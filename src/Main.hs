module Main {-(main)-} where

import qualified Input
import qualified Analyze
import qualified Midi

import Control.Monad

import qualified System
	( getArgs )

-- Pretend to know these for now
bps :: RealFrac a => a
bps = 60

-- audio file gets passed into main

-- End



f = Midi.toMidi . Analyze.basic . Input.input

main = do
	wavPath <- liftM head System.getArgs
	putStrLn $ "converting " ++ wavPath ++ " to MIDI file at " ++ wavPath ++ ".mid"
	a <- liftM f (Input.getInput wavPath) >>= Midi.putMidi (wavPath ++ ".mid")
	putStrLn "ok!"

	return a


{-

freq <- load freqPath
amp <- load ampPath

plotList [] freq

-}
