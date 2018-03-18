{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
-- This module will do for now! But later, when the project grows,
-- this stuff might be replaced by some high level OpenGL lib instead!
--
-- Just some basic stuff to get us started...
module OpenGLHelpers (
    GLSLProgram,
    createGLSLProgram,
    recreateGLSLProgram,
    getProgramId,
    deleteGLSLProgram,
    createEmptyBO,
    createVAO,
    setInt,
    setFloat,
    setFloat2,
    setFloat3,
    setFloat4Array,
    setMatrix33,
    createSceneTargetTexture,
    createSceneTargetBuffer,
    createFontTexture,
    createImageTexture
) where

import Control.Monad (forM_, when)
import Data.Vector.Storable (Vector, unsafeWith)
import Foreign
import Foreign.C.String
import Graphics.GL
import Logger (Logger)

data GLSLProgram = GLSLProgram {
    vertexShaderId :: GLuint,
    fragmentShaderId :: GLuint,
    programId :: GLuint
    }

#define CHECK_GL (checkForGlError logger __FILE__ __LINE__) $
checkForGlError :: Logger -> String -> Int -> IO a -> IO a
checkForGlError logger file line action = do
    check "before"
    result <- action
    check "after"
    return result
    where
        check placement = do
            glError' <- glGetError
            when (glError' /= 0) $ logString logger "OpenGL" (Just $ "Error " ++ show glError' ++ " " ++ placement ++ " @" ++ file ++ ":" ++ show line)

createGLSLProgram :: Logger -> (FilePath, FilePath) -> IO (GLSLProgram)
createGLSLProgram logger (vertexShaderSource, fragmentShaderSource) = do
    vertexShaderId <- CHECK_GL glCreateShader GL_VERTEX_SHADER
    fragmentShaderId <- CHECK_GL glCreateShader GL_FRAGMENT_SHADER
    programId <- CHECK_GL glCreateProgram
    CHECK_GL (forM_ [vertexShaderId, fragmentShaderId] $ glAttachShader programId)
    buildShader logger vertexShaderId vertexShaderSource
    buildShader logger fragmentShaderId fragmentShaderSource
    linkProgram logger programId
    return GLSLProgram{..}

recreateGLSLProgram :: Logger -> GLSLProgram -> (FilePath, FilePath) -> IO ()
recreateGLSLProgram logger GLSLProgram{..} (vertexShaderSource, fragmentShaderSource) = do
    buildShader logger vertexShaderId vertexShaderSource
    buildShader logger fragmentShaderId fragmentShaderSource
    linkProgram logger programId

getProgramId :: GLSLProgram -> GLuint
getProgramId = programId

deleteGLSLProgram :: GLSLProgram -> IO ()
deleteGLSLProgram GLSLProgram{..} = do
    glDeleteShader vertexShaderId
    glDeleteShader fragmentShaderId
    glDeleteProgram programId

createEmptyBO :: Logger -> GLsizei -> IO GLuint
createEmptyBO logger noOfBytes = do
    bufferId <- alloca $ \ptr -> glGenBuffers 1 ptr >> peek ptr
    glBindBuffer GL_ARRAY_BUFFER bufferId
    CHECK_GL glBufferData GL_ARRAY_BUFFER (fromIntegral noOfBytes) nullPtr GL_STATIC_DRAW
    glBindBuffer GL_ARRAY_BUFFER 0
    return bufferId

createVAO :: IO GLuint
createVAO = do
    id <- alloca $ \ptr -> glGenVertexArrays 1 ptr >> peek ptr
    glBindVertexArray id
    return id

withUniformLocation :: Logger -> GLuint -> String -> Maybe GLint -> (GLint -> IO()) -> IO (GLint)
withUniformLocation logger oId name mLoc action = do
    loc <- case mLoc of
        Just cachedLoc -> return cachedLoc
        Nothing        -> CHECK_GL withCString name $ glGetUniformLocation oId
    action loc
    return loc

setInt :: Logger -> GLuint -> String -> Maybe GLint -> GLint -> IO (GLint)
setInt logger oId name mLoc x = withUniformLocation logger oId name mLoc (\loc -> glUniform1i loc x)

setFloat :: Logger -> GLuint -> String -> Maybe GLint -> GLfloat -> IO (GLint)
setFloat logger oId name mLoc x = withUniformLocation logger oId name mLoc (\loc -> glUniform1f loc x)

setFloat2 :: Logger -> GLuint -> String -> Maybe GLint -> GLfloat -> GLfloat -> IO (GLint)
setFloat2 logger oId name mLoc x y = withUniformLocation logger oId name mLoc (\loc -> glUniform2f loc x y)

setFloat3 :: Logger -> GLuint -> String -> Maybe GLint -> GLfloat -> GLfloat -> GLfloat -> IO (GLint)
setFloat3 logger oId name mLoc x y z = withUniformLocation logger oId name mLoc (\loc -> glUniform3f loc x y z)

setFloat4Array :: Logger -> GLuint -> String -> Maybe GLint -> [GLfloat] -> IO (GLint)
setFloat4Array logger oId name mLoc array = do
    withUniformLocation logger oId name mLoc $ \loc -> do
        withArrayLen array $ \len arrayPtr -> do
            if (len `mod` 4 == 0)
                then glUniform4fv loc (fromIntegral $ len `div` 4) arrayPtr
                else logString logger "called setFloat4Array with wrong length" (Just len)
                
setMatrix33 :: Logger -> GLuint -> String -> Maybe GLint -> [GLfloat] -> IO (GLint)
setMatrix33 logger oId name mLoc matrix = do
    withUniformLocation logger oId name mLoc $ \loc -> do
        withArrayLen matrix $ \len matrixPtr -> do
            if len == 9
                then glUniformMatrix3fv loc 1 GL_FALSE matrixPtr
                else logString logger "called setMatrix33 with wrong length" (Just len)

createSceneTargetTexture :: Logger -> GLsizei -> GLsizei -> IO (GLuint)
createSceneTargetTexture logger width height = do
    glActiveTexture GL_TEXTURE0
    textureId <- alloca $ \ptr -> glGenVertexArrays 1 ptr >> peek ptr
    glBindTexture GL_TEXTURE_2D textureId
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral GL_REPEAT)
    CHECK_GL glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGB16F) width height 0 GL_RGB GL_FLOAT nullPtr
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL  0
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR)
    return textureId

createSceneTargetBuffer :: GLuint -> IO GLuint
createSceneTargetBuffer textureId = do
    bufferPointer <- malloc :: IO (Ptr GLuint)
    glGenFramebuffers 1 bufferPointer
    frameBufferId <- peek bufferPointer
    glBindFramebuffer GL_FRAMEBUFFER frameBufferId
    glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D textureId 0
    glBindFramebuffer GL_FRAMEBUFFER 0
    return frameBufferId

createFontTexture :: Logger -> GLsizei -> GLsizei -> Vector Word8 -> IO (GLuint)
createFontTexture logger width height textureData = do
    glActiveTexture GL_TEXTURE0
    textureId <- alloca $ \ptr -> glGenVertexArrays 1 ptr >> peek ptr
    glBindTexture GL_TEXTURE_2D textureId
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral GL_REPEAT)
    unsafeWith textureData $ \bufferPtr -> do
        CHECK_GL glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_R8) width height 0 GL_RED GL_UNSIGNED_BYTE bufferPtr
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL  0
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR)
    return textureId

createImageTexture :: Logger -> GLsizei -> GLsizei -> Vector Word8 -> IO (GLuint)
createImageTexture logger width height textureData = do
    glActiveTexture GL_TEXTURE0
    textureId <- alloca $ \ptr -> glGenVertexArrays 1 ptr >> peek ptr
    glBindTexture GL_TEXTURE_2D textureId
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral GL_REPEAT)
    unsafeWith textureData $ \bufferPtr -> do
        CHECK_GL glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA8) width height 0 GL_RGB GL_UNSIGNED_BYTE bufferPtr
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL  0
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR)
    return textureId
    
-- private stuff
logString :: (Show b) => Logger -> String -> Maybe b -> IO () 
logString logger info Nothing = logString' logger info
logString logger info (Just var) = logString' logger $ info ++ " - " ++ show var
logString' logger info = do
    logger info

buildShader :: Logger -> GLuint -> FilePath -> IO ()
buildShader logger shaderId filename = do
    sourceCode <- readFile filename
    withCString sourceCode $ \source -> do
        alloca $ \shaderOk -> do
            alloca $ \sourceList -> do
                alloca $ \len -> do
                    poke sourceList source
                    poke len $ fromIntegral $ length sourceCode
                    CHECK_GL glShaderSource shaderId 1 sourceList len
                    CHECK_GL glCompileShader shaderId
                    CHECK_GL glGetShaderiv shaderId GL_COMPILE_STATUS shaderOk
                    shaderOkVal <- peek shaderOk

                    when (shaderOkVal == 0) $ do
                        logLengthPointer <- malloc :: IO (Ptr GLint)
                        CHECK_GL glGetShaderiv shaderId GL_INFO_LOG_LENGTH logLengthPointer
                        logLength <- peek logLengthPointer
                        logMessagePointer <- mallocArray (fromIntegral logLength) :: IO (Ptr GLchar)
                        CHECK_GL glGetShaderInfoLog shaderId logLength nullPtr logMessagePointer
                        logMessage <- peekCString logMessagePointer
                        logString' logger $ filename ++ " - " ++ logMessage
                        error $ "Failed to compile shader " ++ filename ++ "\n"

linkProgram :: Logger -> GLuint -> IO ()
linkProgram logger programId = do
    alloca $ \programOk -> do
        CHECK_GL glLinkProgram programId
        CHECK_GL glGetProgramiv programId GL_LINK_STATUS programOk
        programOkVal <- peek programOk
        when (programOkVal == 0) $ do
            logLengthPointer <- malloc :: IO (Ptr GLint)
            CHECK_GL glGetProgramiv programId GL_INFO_LOG_LENGTH logLengthPointer
            logLength <- peek logLengthPointer
            logMessagePointer <- mallocArray (fromIntegral logLength) :: IO (Ptr GLchar)
            CHECK_GL glGetProgramInfoLog programId logLength nullPtr logMessagePointer
            logMessage <- peekCString logMessagePointer
            logString' logger $ "linking - " ++ logMessage
            error $ "Failed to link program\n"
