module Midi
	( toMidi
	, putMidi
	)

where

import qualified Sound.MIDI.File      as MidiFile
import qualified Sound.MIDI.File.Save as Save

import Sound.MIDI.File (ElapsedTime, )

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
timeDelta = 1024 / 44100 / 4

microsecondsPerMinute = 60000000.0

bpm = 60.0

mpqn = microsecondsPerMinute / bpm


melody :: [Int] -> [(VoiceMsg.Pitch, ElapsedTime)]
melody x = zip (map VoiceMsg.toPitch x) [43,43..]


melodyEvents :: Int -> [(VoiceMsg.Pitch, ElapsedTime)] -> EventListBT.T ElapsedTime Event.T
melodyEvents pn mel =
   let chan = ChannelMsg.toChannel 0
       vel = VoiceMsg.toVelocity (VoiceMsg.normalVelocity+25)
       event = Event.MIDIEvent . ChannelMsg.Cons chan . ChannelMsg.Voice
   in  EventListBT.fromPairList 
			$ concatMap 
				(\(pgm, (n,t)) -> [(event $ VoiceMsg.ProgramChange pgm, 0), (event $ VoiceMsg.NoteOn  n vel, t), (event $ VoiceMsg.NoteOff n vel, 0)])
									 $ zip (cycle $ map VoiceMsg.toProgram [pn]) mel


-- first argument of solo is instrument --
solo :: Int -> [(VoiceMsg.Pitch, ElapsedTime)] -> MidiFile.T
solo pn mel =
   MidiFile.Cons MidiFile.Parallel (MidiFile.Ticks (round (1/timeDelta)))
      [EventList.cons 0
          (Event.MetaEvent $ MetaEvent.SetTempo (round mpqn)) $
       EventListTM.switchTimeR const $
       EventListMT.consTime 0 $
       melodyEvents pn mel]



-------------------------------------------------
-- Exported Functions
-------------------------------------------------

toMidi :: [Int] -> B.ByteString
toMidi pitches = Save.toByteString (solo 1 (melody pitches))

putMidi :: String -> B.ByteString -> IO ()
putMidi = B.writeFile
