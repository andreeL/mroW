{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
module Mixer
  ( startMixer
  , SoundOutput(..)
  , AddSoundWave
  , StopMixer
  , SoundWave
  , testMixer -- only for debuging
  , pianoKey -- just for testing
  ) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (MVar(..), newMVar, newEmptyMVar, takeMVar, modifyMVar, modifyMVar_, putMVar, readMVar)
import Control.Monad (forM_)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Vector.Storable as VC
import qualified Data.Vector.Storable.Mutable as VM
import qualified SDL.Init (initialize, InitFlag(..), quit)
import SDL.Audio (openAudioDevice, setAudioDeviceLocked, setAudioDevicePlaybackState, closeAudioDevice)
import SDL.Audio (OpenDeviceSpec(..), AudioFormat(..), Changeable(..), Channels(..), AudioDeviceUsage(..), PlaybackState(..), LockState(..))

data SoundOutput = SoundOutput {
  _sampleRate :: Int,
  _noOfChannels :: Int
}

-- TODO: this quick hack is not really a ringbuffer, switch to a proper one with preallocated vectors
type RingBuffer = MVar [VC.Vector Float]

createRingBuffer :: IO RingBuffer
createRingBuffer = newMVar ([] :: [VC.Vector Float])

popChunk :: RingBuffer -> IO (Maybe (VC.Vector Float))
popChunk ringBuffer = do
  chunks <- takeMVar ringBuffer
  let (chunk, !chunks') = case chunks of
                          [] -> (Nothing, [])
                          (x:xs) -> (Just x, xs)
  putMVar ringBuffer chunks'
  pure chunk

pushChunk :: RingBuffer -> IO (VC.Vector Float) -> IO Bool
pushChunk ringBuffer getChunk = do
  chunks <- readMVar ringBuffer
  if (length chunks < 2)
    then do
      chunk <- getChunk
      -- To avoid crackling sound, we need the chunk to be evaluated (ie. we need it in NF) before we send it off to the buffer.
      -- I leave it to my feature self to figure out how to do it correctly! But for now, this "seq" seems to do the trick..
      modifyMVar_ ringBuffer $ (VC.length chunk) `seq` \chunks -> let !chunks' = (chunks ++ [chunk]) in pure chunks'
      pure True
    else pure False

type SoundWave = VC.Vector Float

data PlayingSoundWave = PlayingSoundWave {
  _soundWave :: SoundWave,
  _position :: Int
}

type AddSoundWave = SoundWave -> IO ()
type StopMixer = IO ()

startMixer :: Int -> SoundOutput -> IO (AddSoundWave, StopMixer)
startMixer noOfSamplesInSoundBuffer SoundOutput{..} = do
  ringBuffer <- createRingBuffer

  let noOfElementsInSoundBuffer = noOfSamplesInSoundBuffer * _noOfChannels

  let fillFloatBuffer :: VM.IOVector Float -> IO()
      fillFloatBuffer samples = do
        maybeChunk <- popChunk ringBuffer
        case maybeChunk of
          Just chunk -> VC.copy samples chunk
          Nothing -> VM.set samples 0

  let deviceCallback :: AudioFormat a -> VM.IOVector a -> IO ()
      deviceCallback FloatingLEAudio samples = fillFloatBuffer samples
      deviceCallback FloatingBEAudio samples = fillFloatBuffer samples
      deviceCallback _ _ = pure () -- unsupported

  SDL.Init.initialize $ Just SDL.Init.InitAudio
    
  (audioDevice, audioSpec) <- openAudioDevice OpenDeviceSpec {
    openDeviceFreq = Desire (fromIntegral _sampleRate),
    openDeviceFormat = Desire FloatingNativeAudio,
    openDeviceChannels = if (_noOfChannels == 1) then Desire Mono else Desire Stereo,
    openDeviceSamples = fromIntegral noOfSamplesInSoundBuffer, -- no of samples in buffer
    openDeviceCallback = deviceCallback,
    openDeviceUsage = ForPlayback,
    openDeviceName = Nothing
  }

  setAudioDeviceLocked audioDevice Unlocked
  setAudioDevicePlaybackState audioDevice Play

  let createNextChunk :: [PlayingSoundWave] -> IO SoundWave
      createNextChunk soundWaves = do
        pure $ VC.generate noOfElementsInSoundBuffer $ \j -> foldl' (\a PlayingSoundWave{..} -> a + (fromMaybe 0 (_soundWave VC.!? (_position + j)))) 0 soundWaves

  sharedSoundWaveQueue <- newMVar ([] :: [SoundWave])

  -- For the mixer to work reasonably in GHCi it has to run in a separate thread.
  sharedStopMixer <- newMVar False
  sharedMixerStopped <- newEmptyMVar :: IO (MVar ())
  let mixerTick :: [PlayingSoundWave] -> IO()
      mixerTick playingSoundWaves = do
        stopMixer <- readMVar sharedStopMixer
        if stopMixer
          then do
            closeAudioDevice audioDevice
            SDL.Init.quit
            putMVar sharedMixerStopped ()
          else do
            threadDelay 10000
            soundWaveQueue <- modifyMVar sharedSoundWaveQueue $ \x -> pure ([], x)
            let playingSoundWaves' = (fmap (\soundWave -> PlayingSoundWave soundWave 0) soundWaveQueue) ++ playingSoundWaves
            chunkPushed <- pushChunk ringBuffer (createNextChunk playingSoundWaves')
            let playingSoundWaves'' = if chunkPushed
                  then
                    flip mapMaybe playingSoundWaves' $ \PlayingSoundWave{..} ->
                      let newPosition = _position + noOfElementsInSoundBuffer
                      in if newPosition < VC.length _soundWave
                          then Just (PlayingSoundWave _soundWave newPosition)
                          else Nothing
                  else
                    playingSoundWaves'
            mixerTick playingSoundWaves''

  putStrLn "starting mixer"
  mainThreadId <- forkIO $ mixerTick []
  
  -- callbacks
  let addSoundWave wave = modifyMVar_ sharedSoundWaveQueue $ \xs -> pure (wave:xs)
  let stopMixer = do
        putStrLn "stopping mixer"
        modifyMVar_ sharedStopMixer $ pure . const True
        takeMVar sharedMixerStopped
  
  pure (addSoundWave, stopMixer)






  
-- some code for testing the mixer
type Seconds = Float
data ADSRParameters = ADSRParameters {
  _attack :: Seconds,
  _decay :: Seconds,
  _sustain :: Float,
  _release :: Seconds
}

speakersNoOfChannels = 1 :: Int
speakerSampleRate = 22500 :: Int

generateADSREffect :: SoundOutput -> ADSRParameters -> Float -> (Seconds -> Int -> Float) -> VC.Vector Float
generateADSREffect SoundOutput{..} ADSRParameters{..} amplitude wave = VC.generate (totalNoOfSamples * _noOfChannels) $ \i -> getSample (i `divMod` _noOfChannels)
  where
    toNoOfSamples seconds = round $ seconds * (fromIntegral _sampleRate)
    noOfAttackSamples = toNoOfSamples _attack
    noOfDecaySamples = toNoOfSamples _decay
    noOfReleaseSamples = toNoOfSamples _release
    totalNoOfSamples = noOfAttackSamples + noOfDecaySamples + noOfReleaseSamples
    divideBy a b = ((fromIntegral a) / (fromIntegral b)) :: Float
    getSample :: (Int, Int) -> Float
    getSample (i, c) = let
      amp :: Float
      amp
        | i < noOfAttackSamples                    = i `divideBy` noOfAttackSamples
        | i < noOfAttackSamples + noOfDecaySamples = let t = (i - noOfAttackSamples) `divideBy` noOfDecaySamples in (1.0 - t) + (t * _sustain)
        | otherwise                                = let t = (i - noOfAttackSamples - noOfDecaySamples) `divideBy` noOfReleaseSamples in (1 - t) * _sustain
      in amp * amplitude * (wave (i `divideBy` _sampleRate) c)

sinWave frequency t _ = sin $ t * 2 * pi * frequency
triangleWave frequency t _ =
  let t' = t * frequency
      f = 4 * ( t' - (fromIntegral (floor t')))
  in if f < 1 then f else (if f < 3 then (2 - f) else (f - 4))
squareWave frequency t _ =
  let t' = t * frequency
      f = 4 * ( t' - (fromIntegral (floor t')))
  in if f < 1 then 1 else (if f < 3 then (-1) else 1)

keyFrequency n = 2 ** ((fromIntegral n - 49) / 12) * 440
[c2, c'2, d2, d'2, e2, f2, f'2, g2, g'2, a2, a'2, b2,
  c3, c'3, d3, d'3, e3, f3, f'3, g3, g'3, a3, a'3, b3,
  c4, c'4, d4, d'4, e4, f4, f'4, g4, g'4, a4, a'4, b4,
  c5, c'5, d5, d'5, e5, f5, f'5, g5, g'5, a5, a'5, b5
  ] = fmap keyFrequency [16..16 + 4 * 12 - 1]

p = generateADSREffect (SoundOutput speakerSampleRate speakersNoOfChannels) (ADSRParameters 0.025 0.05 0.75 0.2) 1 . sinWave
l x = generateADSREffect (SoundOutput speakerSampleRate speakersNoOfChannels) (ADSRParameters 0.025 0.05 0.75 x) 1 . triangleWave 

songHallelujah = [
  ([p e4], 1),
  ([l 2 c3, p g4], 2), ([p g4], 1), ([p g4], 2), ([p g4], 1),
  ([l 2 a2, p a4], 1), ([p a4], 1), ([p a4], 3), ([p e4], 1),
  ([l 2 c3, p g4], 2), ([p g4], 1), ([p g4], 1), ([p g4], 1), ([p g4], 1),
  ([l 2 a2, p a4], 2), ([p a4], 1), ([p a4], 2), ([p e4], 1),
  ([l 2 f2, p a4], 2), ([p a4], 1), ([p a4], 2), ([p e4], 1),
  ([l 2 g2, p a4], 2), ([p g4], 1), ([p g4], 1), ([p f4], 2),
  ([l 1 c2, p g4], 1), -- and so on...
  ([p g4], 1)
  ]

testMixer :: IO ()
testMixer = do
  (addSoundWave, stopMixer) <- startMixer 2048 SoundOutput{
    _sampleRate = speakerSampleRate,
    _noOfChannels = speakersNoOfChannels    
  }
  forM_ songHallelujah $ \(effects, delay) -> do
    forM_ effects $ \effect -> addSoundWave effect
    threadDelay (delay * 400000)
  threadDelay 1000000
  stopMixer
  
pianoKey :: SoundOutput -> Int -> SoundWave
pianoKey soundOutput = generateADSREffect soundOutput (ADSRParameters 0.025 0.05 0.75 0.2) 1 . sinWave . keyFrequency
