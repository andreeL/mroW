module Platform
 ( withPlatform
 ) where

import Foreign
import Graphics.Win32
import System.Win32
import System.Win32.DLL
import Data.Bits ((.|.))

type Platform = () -- extend...

windowClosure :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
windowClosure hWnd uMsg wParam lParam
    | uMsg == wM_DESTROY = do
        postQuitMessage 0
        return 0
    | otherwise = do
        defWindowProc (Just hWnd) uMsg wParam lParam

extractMessage :: Ptr WindowMessage -> IO (WindowMessage)
extractMessage msgPtr =
-- struct tagMSG {
--   HWND   hwnd;
--   UINT   message;
--   WPARAM wParam;
--   LPARAM lParam;
--   DWORD  time;
--   POINT  pt;
-- }
    let hwndSize = sizeOf (undefined :: HWND)
    in peekByteOff msgPtr hwndSize

handleMessages :: IO Bool
handleMessages = do
    allocaMessage $ \msgPtr -> do
        hasMessage <- c_PeekMessage msgPtr nullPtr 0 0 1 -- 1 = pM_REMOVE
        if (hasMessage == 0) then return False
        else do
            messageValue <- extractMessage (castPtr msgPtr :: Ptr WindowMessage)
            if (messageValue == wM_QUIT) then return True
            else do
                _ <- translateMessage msgPtr
                _ <- dispatchMessage msgPtr
                handleMessages

withPlatform :: String -> a -> ((a, Platform) -> IO (a, Bool)) -> IO ()
withPlatform gameName programState programFunc = do
    let className = mkClassName gameName
    hModule <- getModuleHandle Nothing
    atom <- registerClass (0, hModule, Nothing, Nothing, Nothing, Nothing, className)
    let style = wS_OVERLAPPED .|. wS_SIZEBOX .|. wS_SYSMENU .|. wS_VISIBLE
    hWnd <- createWindowEx 0 className gameName style Nothing Nothing Nothing Nothing Nothing Nothing hModule windowClosure
    --_ <- showWindow hWnd sW_SHOW
    let mainLoop state = do
            quitDetected <- handleMessages
            (newState, done) <- programFunc (state, ())
            if (quitDetected || done) then return () else mainLoop newState

    mainLoop programState
