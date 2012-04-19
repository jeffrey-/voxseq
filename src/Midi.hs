module Midi
--	( toMidi
--	, putMidi
--	)

where

import qualified Sound.MIDI.File as MF
	( T (Cons)
	, Type (Parallel)
	, Division (Ticks)
	, ElapsedTime
	, toElapsedTime
	)

import qualified Sound.MIDI.File.Save as Save
	( toByteString )

import qualified Sound.MIDI.File.Event as Event
	( T (MIDIEvent, MetaEvent) )

import qualified Sound.MIDI.File.Event.Meta as MetaEvent
	( T (SetTempo) )

import qualified Sound.MIDI.Message.Channel as ChMsg
	( T (Cons)
	, Body (Voice)
	, toChannel
	)

import qualified Sound.MIDI.Message.Channel.Voice as VcMsg
	( T (ProgramChange, NoteOn, NoteOff, PitchBend, Control)
	, Pitch
	, toPitch
	, fromPitch
	, toVelocity
	, normalVelocity
	, toProgram
	, Controller
	, toController
	, PitchBendRange
	)

import qualified Sound.MIDI.Controller as C
	( portamento
	, portamentoTime
	)

import qualified Data.EventList.Relative.TimeBody  as ELTB
	( T
	, cons
	, fromPairList
	)

import qualified Data.ByteString.Lazy as B
	( ByteString
	, writeFile
	)


--timeDelta = frameSizeD / sampleFreq / overSampD
timeDelta = 1024 / 44100 / 1

microsecondsPerMinute = 60000000.0

bpm = 60.0

mpqn = microsecondsPerMinute / bpm



toNote :: Integral a => (String, a, a) -> (String, MF.ElapsedTime, VcMsg.Pitch)
toNote (s, t, p) = (s, MF.toElapsedTime $ fromIntegral t, VcMsg.toPitch $ fromIntegral p)


melodyEvents :: Integral a => [(a, (String, (a, a)))] -> ELTB.T MF.ElapsedTime Event.T
melodyEvents mel = ELTB.fromPairList $ concatMap ef mel
	where
	chan  = ChMsg.toChannel 0
	event = Event.MIDIEvent . ChMsg.Cons chan . ChMsg.Voice

	toPitchBendRange :: Int -> VcMsg.PitchBendRange
	toPitchBendRange = id

	ef (t, (s, (x, y)))
		| s=="NoteOn"    = [ 
		                     ( (MF.toElapsedTime . fromIntegral) t
		                     , event $ VcMsg.NoteOn    ((VcMsg.toPitch    . fromIntegral) x)
		                                               ((VcMsg.toVelocity . fromIntegral) y))
		                   ]

		| s=="NoteOff"   = [ 
		                     ( (MF.toElapsedTime . fromIntegral) t
		                     , event $ VcMsg.NoteOff   ((VcMsg.toPitch    . fromIntegral) x)
		                                               ((VcMsg.toVelocity . fromIntegral) y))
		                   , ( (MF.toElapsedTime . fromIntegral) 0
		                     , event $ VcMsg.PitchBend (toPitchBendRange 8192)
		                     )
		                   ]

		| s=="PitchBend" = [ 
		                     ( (MF.toElapsedTime . fromIntegral) t
		                     , event $ VcMsg.PitchBend ((toPitchBendRange . fromIntegral) x)
		                     )
		                   ]


toMidiFile :: Integral a => [(a, (String, (a, a)))] -> MF.T
toMidiFile mel = MF.Cons MF.Parallel (MF.Ticks (round (1/timeDelta)))

	[ ELTB.cons 0 (Event.MetaEvent (MetaEvent.SetTempo (round mpqn))) 
	$ melodyEvents mel
	]



-------------------------------------------------
-- Exported Functions
-------------------------------------------------

toMidi :: Integral a => [(a, (String, (a, a)))] -> B.ByteString
toMidi = Save.toByteString . toMidiFile
 
putMidi :: String -> B.ByteString -> IO ()
putMidi = B.writeFile
