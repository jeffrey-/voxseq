module Main {-(main)-} where

import qualified Input
import qualified FFT
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
	wavPath <- head `liftM` System.getArgs
--	p <- print "converting " ++ wavPath ++ " to MIDI file at " ++ wavPath ++ ".mid"
	a <- Midi.putMidi (wavPath ++ ".mid") =<< (f `liftM` Input.getInput wavPath)

	return a

	print "ok!"

{-

freq <- load freqPath
amp <- load ampPath

plotList [] freq

-}
