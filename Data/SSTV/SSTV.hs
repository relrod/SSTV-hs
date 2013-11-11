module Data.SSTV where

import Data.Monoid (mappend)
import Data.WAVE
import Data.Int (Int32)
import Data.List (group)

samplesPS = 48000
bitrate = 16

header = WAVEHeader 1 samplesPS bitrate Nothing

-- TODO: I suspect there is a nicer way of writing this.
-- This was taken from http://stackoverflow.com/questions/5658391/generating-wav-sound-data-in-haskell
sound :: Double -- | Frequency
      -> Double -- | Lenght of sound in seconds
      -> Int32  -- | Volume, (maxBound :: Int32) for highest, 0 for lowest
      -> [Int32]
sound freq len volume =
  take (round $ len * fromIntegral samplesPS) $
  map ((round . (* fromIntegral volume)) . sin)
    [0.0, (freq * 2 * pi / fromIntegral samplesPS) ..]

vox :: [[Int32]]
vox = samples
  where
    tones = [1900, 1500, 1900, 1500, 2300, 1500, 2300, 1500]
    samples = map (\x -> sound x 0.1 (maxBound `div` 2)) tones

sync :: [[Int32]]
sync = [ sound 1900 0.3 (maxBound `div` 2)
       , sound 1200 0.01 (maxBound `div` 2) ]

concatAudio :: [[[Int32]]] -> [[Int32]]
concatAudio = group . concat . concat

waveData = WAVE header (concatAudio audioData)
  where
    audioData = [ vox
                , replicate 2 . concat $ sync ]

makeWavFile :: WAVE -> IO ()
makeWavFile = putWAVEFile "temp.wav"

main = makeWavFile waveData
