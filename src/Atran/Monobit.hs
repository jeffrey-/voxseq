module Monobit

where

data Monobit =
	NoteOn      Integer Int Int -- elapsed time, pitch, vel
	| NoteOff   Integer Int Int -- elapsed time, pitch, vel
	| PitchBend Integer Int     -- elapsed time, bendAmount
	deriving (Show)

-- todo: include header
type Melody = [Monobit]
