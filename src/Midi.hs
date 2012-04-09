module Midi

where

import qualified Sound.MIDI.File      as MidiFile
import qualified Sound.MIDI.File.Save as Save

import Sound.MIDI.File (ElapsedTime, )

import qualified Sound.MIDI.File.Event.Meta as MetaEvent
import qualified Sound.MIDI.File.Event      as Event

import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

-- import qualified Sound.MIDI.Parser.Report as Report

import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Data.EventList.Relative.BodyTime  as EventListBT
import qualified Data.EventList.Relative.MixedTime as EventListMT
import qualified Data.EventList.Relative.TimeBody  as EventList
-- import Data.EventList.Relative.MixedBody ((/.), (./), )

import qualified Data.ByteString.Lazy as B

-- import qualified Numeric.NonNegative.Wrapper as NonNeg

import Data.Tuple.HT (mapFst, )

import qualified Control.Monad.Trans.State as State
import Control.Monad (liftM3, )

import qualified System (getArgs)
import Data.Fixed  -- for mod', div'
import Data.List.Split -- for splitWhen

import Convert
--import MathStuff


--let t = Cons Serial (Ticks $ toTempo 60) [

-- 'round' uses banker's rounding...
freqtopitch_p :: (RealFrac a, Floating a) => a -> a
freqtopitch_p x = fromIntegral fix
	where 
	fix = minimum [maximum [num, 0], 127]
	num = round (12 * (logBase 2 (x / 440))) + 57

-- 'round' uses banker's rounding...
freqtopitch :: (RealFrac a, Floating a) => a -> VoiceMsg.Pitch
freqtopitch x = VoiceMsg.toPitch fix
	where 
	fix = minimum [maximum [num, 0], 127]
	num = round (12 * (logBase 2 (x / 440))) + 57

scaleRound :: (RealFrac a, Integral b) => a -> b
scaleRound x
	| x>=0		&& x<=1		= 0		--root
	| x> 1		&& x< 3		= 2		--2
	| x>=3		&& x<=4.5	= 4		--M3
	| x> 4.5	&& x< 6		= 5		--4
	| x>=6		&& x<=8		= 7		--5
	| x> 8		&& x<10		= 9 	--M6
	| x>=10		&& x<11.5	= 11	--M7
	| x>=11		&& x<12		= 12	--octave

freqToScalePitch :: (Integral a, Real b, RealFrac b, Floating b) => a -> b -> a
freqToScalePitch _ 0 = 0
freqToScalePitch s x = scaleRound top + base + 57 {- uh -} 
	where
	base = div' (trans + (fromIntegral s)) 12 * 12 - s
	top = mod' (trans + (fromIntegral s)) 12
	trans = 12 * (logBase 2 (x / 440)) 


mICROSECONDS_PER_MINUTE = 60000000.0

bPM = 60.0

mPQN = mICROSECONDS_PER_MINUTE / bPM

-- bpm
-- npb:	notes per beat
-- td:	time delta (time between frequency markers)
-- len:	length of frequency marker list
--index :: Double -> Double -> Double -> Int -> [Int]
index :: (RealFrac a, Integral b, Integral a1, Enum a) => a -> a -> a -> a1 -> [b]
index bpm npb td len = map round [dunno, 2*dunno..fromIntegral len-dunno]
	where
	dunno = bpm / 60 / npb / td

ind x = index 60 4 timeDelta (length x)
indfreqs x = map (\ k -> x !! k) (ind x)


melody' :: [Double] -> [(VoiceMsg.Pitch, ElapsedTime)]
melody' a = zip (map (VoiceMsg.toPitch . (freqToScalePitch 8)) a) [1,1..]


--converts peaks into elapsed times
peakET :: [Double] -> [ElapsedTime]
peakET x = map (fromIntegral . (+)1 . length) $ splitWhen (>0) x

toET :: (RealFrac a) => [a] -> [ElapsedTime]
toET x = map (fromIntegral . round) x

melody'' :: Double -> [Double] -> [Double] -> [(VoiceMsg.Pitch, ElapsedTime)]
melody'' tdelta freqs peaks = zip (map (VoiceMsg.toPitch . (freqToScalePitch 8)) $ snd dumber) (map (\x->div x 1) $ toET peaks)
	where
	notZero x | x==0 = False | otherwise = True
	g = filter (\ (t, f, p) -> notZero p) $ zip3 [0, tdelta..] freqs peaks
	dumb (a,b,_) = (a,b)
	dumber = dumb $ unzip3 g


--melody'' :: Double -> [Double] -> [Double] -> [(VoiceMsg.Pitch, ElapsedTime)]
--melody'' tdelta freqs peaks = zip (map (VoiceMsg.toPitch . (freqToScalePitch 8)) $ fst dumber) (peakET peaks)
--	where
--	notZero x | x==0 = False | otherwise = True
--	g = filter (\ (t, f, p) -> notZero p) $ zip3 [0, tdelta..] freqs peaks
--	dumb (a,b,_) = (a,b)
--	dumber = dumb $ unzip3 g

-- a <- bla2 ("t1" ++ ".wav")
-- let q x =  round $ mean $ take 43 $ drop (43*x) a
-- map (\ x -> q x) [0..34]


melodyEvents :: Int -> [(VoiceMsg.Pitch, ElapsedTime)] -> EventListBT.T ElapsedTime Event.T
melodyEvents pn mel =
   let chan = ChannelMsg.toChannel 0
       vel = VoiceMsg.toVelocity (VoiceMsg.normalVelocity+25)
       event = Event.MIDIEvent . ChannelMsg.Cons chan . ChannelMsg.Voice
   in  EventListBT.fromPairList 
				$ concatMap 
							(\(pgm, (n,t)) -> [(event $ VoiceMsg.ProgramChange pgm, 0), (event $ VoiceMsg.NoteOn  n vel, t), (event $ VoiceMsg.NoteOff n vel, 0)])
									 $ zip (cycle $ map VoiceMsg.toProgram [pn]) mel

solo :: Int -> [(VoiceMsg.Pitch, ElapsedTime)] -> MidiFile.T
solo pn mel =
   MidiFile.Cons MidiFile.Parallel (MidiFile.Ticks (round (1/timeDelta))) --4)
      [EventList.cons 0
          (Event.MetaEvent $ MetaEvent.SetTempo (round mPQN)) $
       EventListTM.switchTimeR const $
       EventListMT.consTime 0 $
       melodyEvents pn mel]


doer :: String -> IO ()
doer name = do
	(pitch, peak) <- bla2 (name ++ ".wav")
--	let melody = melody' (indfreqs pitch)
	let melody = melody'' 1 (smooth 9 pitch) [8,8..] --peak
	B.writeFile (name ++ ".mid") (Save.toByteString (solo 16 melody))

doertemp :: [Double] -> IO ()
doertemp freq = do
	let melody = melody'' 10 freq (replicate 100 43)
	B.writeFile ("../test/t2.mid") (Save.toByteString (solo 1 melody))


------------------------------------------
-- first argument of solo is instrument --
------------------------------------------

--doertemp freq = solo 16 melody
--    where
--        melody = melody'' 1 freq (replicate 20 1)


--main :: IO ()
--main = do
--	args <- System.getArgs
--	doer (head args)
