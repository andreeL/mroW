{-# LANGUAGE CPP #-}
#define CHECK_GL (checkForGlError __FILE__ __LINE__) $
-- This module will do for now! But later, when the project grows,
-- this stuff might be replaced by some high level OpenGL lib instead!
--
-- Just some basic stuff to get us started...
module OpenGLHelpers (
    GLSLProgram,
    createGLSLProgram,
    getProgramId,
    deleteGLSLProgram,
    createEmptyBO,
    createVAO,
    setInt,
    setFloat,
    setFloat2,
    createSceneTargetTexture,
    createSceneTargetBuffer
) where

import Control.Monad (forM_, when)
import Foreign
import Foreign.C.String
import Graphics.GL
import System.IO (hFlush, stdout)

data GLSLProgram = GLSLProgram {
    vertexShaderId :: GLuint,
    fragmentShaderId :: GLuint,
    programId :: GLuint
    }

checkForGlError :: String -> Int -> IO a -> IO a
checkForGlError file line action = do
    check "before"
    result <- action
    check "after"
    return result
    where
        check placement = do
            glError' <- glGetError
            when (glError' /= 0) $ logString "OpenGL" (Just $ "Error " ++ show glError' ++ " " ++ placement ++ " @" ++ file ++ ":" ++ show line)

createGLSLProgram :: FilePath -> FilePath -> IO (GLSLProgram)
createGLSLProgram vertexShaderSource fragmentShaderSource = do
    vertexShader   <- createShader GL_VERTEX_SHADER vertexShaderSource
    fragmentShader <- createShader GL_FRAGMENT_SHADER fragmentShaderSource
    program        <- compileProgram [vertexShader, fragmentShader]
    -- TODO!!! when do we delete the shaders?
    return $ GLSLProgram vertexShader fragmentShader program

getProgramId :: GLSLProgram -> GLuint
getProgramId = programId

deleteGLSLProgram :: GLSLProgram -> IO ()
deleteGLSLProgram = undefined -- TODO

createEmptyBO :: GLsizei -> IO GLuint
createEmptyBO noOfBytes = do
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

withUniformLocation :: GLuint -> String -> Maybe GLint -> (GLint -> IO()) -> IO (GLint)
withUniformLocation oId name mLoc action = do
    loc <- case mLoc of
        Just cachedLoc -> return cachedLoc
        Nothing        -> CHECK_GL withCString name $ glGetUniformLocation oId
    action loc
    return loc

setInt :: GLuint -> String -> Maybe GLint -> GLint -> IO (GLint)
setInt oId name mLoc x = withUniformLocation oId name mLoc (\loc -> glUniform1i loc x)

setFloat :: GLuint -> String -> Maybe GLint -> GLfloat -> IO (GLint)
setFloat oId name mLoc x = withUniformLocation oId name mLoc (\loc -> glUniform1f loc x)

setFloat2 :: GLuint -> String -> Maybe GLint -> GLfloat -> GLfloat -> IO (GLint)
setFloat2 oId name mLoc x y = withUniformLocation oId name mLoc (\loc -> glUniform2f loc x y)

createSceneTargetTexture :: GLsizei -> GLsizei -> IO (GLuint)
createSceneTargetTexture width height = do
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

-- private stuff
logString :: (Show b) => String -> Maybe b -> IO () 
logString info Nothing = logString' $ info
logString info (Just var) = logString' $ info ++ " - " ++ show var
logString' info = do
    putStrLn info
    hFlush stdout -- TODO!!! just log to error??

createShader :: GLenum -> FilePath -> IO GLuint
createShader shaderType filename = do
    sourceCode <- readFile filename
    shader     <- CHECK_GL glCreateShader shaderType
    withCString sourceCode $ \source -> do
        alloca $ \shaderOk -> do
            alloca $ \sourceList -> do
                alloca $ \len -> do
                    poke sourceList source
                    poke len $ fromIntegral $ length sourceCode
                    CHECK_GL glShaderSource shader 1 sourceList len
                    CHECK_GL glCompileShader shader
                    CHECK_GL glGetShaderiv shader GL_COMPILE_STATUS shaderOk
                    shaderOkVal <- peek shaderOk

                    if shaderOkVal == 0
                    then do
                        logLengthPointer <- malloc :: IO (Ptr GLint)
                        CHECK_GL glGetShaderiv shader GL_INFO_LOG_LENGTH logLengthPointer
                        logLength <- peek logLengthPointer
                        logMessagePointer <- mallocArray (fromIntegral logLength) :: IO (Ptr GLchar)
                        CHECK_GL glGetShaderInfoLog shader logLength nullPtr logMessagePointer
                        logMessage <- peekCString logMessagePointer
                        error $ "Shader " ++ filename ++ " failed to compile" ++ "\n" ++ logMessage
                    else return shader

compileProgram :: Foldable t => t GLuint -> IO GLuint
compileProgram shaders = do
    alloca $ \programOk -> do
        program <- CHECK_GL glCreateProgram
        CHECK_GL (forM_ shaders $ glAttachShader program)
        CHECK_GL glLinkProgram program
        CHECK_GL glGetProgramiv program GL_LINK_STATUS programOk
        programOkVal <- peek programOk
        if programOkVal == 0
            then do
                logLengthPointer <- malloc :: IO (Ptr GLint)
                CHECK_GL glGetProgramiv program GL_INFO_LOG_LENGTH logLengthPointer
                logLength <- peek logLengthPointer
                logMessagePointer <- mallocArray (fromIntegral logLength) :: IO (Ptr GLchar)
                CHECK_GL glGetProgramInfoLog program logLength nullPtr logMessagePointer
                logMessage <- peekCString logMessagePointer
                error $ "Program failed to compile" ++ "\n" ++ logMessage
            else return program
