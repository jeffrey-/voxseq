module Midi
	( toMidi
	, putMidi
	)

where

import qualified Sound.MIDI.File      as MidiFile
import qualified Sound.MIDI.File.Save as Save

import Sound.MIDI.File
	( ElapsedTime
	, toElapsedTime
	)

import qualified Sound.MIDI.File.Event.Meta as MetaEvent
import qualified Sound.MIDI.File.Event      as Event

import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Data.EventList.Relative.BodyTime  as EventListBT
import qualified Data.EventList.Relative.MixedTime as EventListMT
import qualified Data.EventList.Relative.TimeBody  as EventList

import qualified Data.ByteString.Lazy as B


--timeDelta = frameSizeD / sampleFreq / overSampD
timeDelta = 1024 / 44100 / 1

microsecondsPerMinute = 60000000.0

bpm = 60.0

mpqn = microsecondsPerMinute / bpm


toNote :: Integral a => (a, a) -> (VoiceMsg.Pitch, ElapsedTime)
toNote (p, t) = (VoiceMsg.toPitch $ fromIntegral p, toElapsedTime $ fromIntegral t)

melodyEvents :: [(VoiceMsg.Pitch, ElapsedTime)] -> EventListBT.T ElapsedTime Event.T
melodyEvents mel =
	let
		chan = ChannelMsg.toChannel 0
		vel = VoiceMsg.toVelocity (VoiceMsg.normalVelocity+25)
		event = Event.MIDIEvent . ChannelMsg.Cons chan . ChannelMsg.Voice
	in
		EventListBT.fromPairList 
			$ concatMap 
				(\(pgm, (p,t)) -> -- (pitch, time)
					[ (event $ VoiceMsg.ProgramChange pgm, 0)
					, (event $ VoiceMsg.NoteOn  p vel, t)
					, (event $ VoiceMsg.NoteOff p vel, 0)
					]
				)
				$ zip (repeat (VoiceMsg.toProgram 1)) mel -- 1 is instrument

-- first argument of solo is instrument --
solo :: [(VoiceMsg.Pitch, ElapsedTime)] -> MidiFile.T
solo mel =
	MidiFile.Cons MidiFile.Parallel (MidiFile.Ticks (round (1/timeDelta)))
		[ EventList.cons 0 (Event.MetaEvent $ MetaEvent.SetTempo (round mpqn))
		$ EventListTM.switchTimeR const
		$ EventListMT.consTime 0
		$ melodyEvents mel
		]



-------------------------------------------------
-- Exported Functions
-------------------------------------------------

toMidi :: Integral a => [(a, a)] -> B.ByteString
toMidi x = Save.toByteString (solo (map toNote x))

putMidi :: String -> B.ByteString -> IO ()
putMidi = B.writeFile
