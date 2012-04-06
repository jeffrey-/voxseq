import ossaudiodev
import time
import wave
import sys

def ticker(bpm, countin, tickpth, rec):
	f_tick = wave.open(tickpth, 'rb')
	(nc,sw,fr,nf,comptype, compname) = f_tick.getparams()
	tick = f_tick.readframes(nf)
	f_tick.close()
	dev = ossaudiodev.open('rw')
	dev.nonblock()
	dev.setparameters(ossaudiodev.AFMT_S16_NE, 1, 44100)
	def devread():
		try:
			rec.append(dev.read(buff))
		except IOError:
			pass
	buff = 16384
	donecountin = False
	spb = 60.0 / bpm
	tickpad = tick + '\x00' * (buff - len(tick))
	lastmod = 9999999
	start = time.time()
	try:
		while True:
			mod = (time.time() - start) % spb
			if mod < lastmod:
				dev.write(tickpad)
				#dev.flush()
				#print time.time() - start
				if donecountin == False:
					if (time.time() - start) / spb >= countin:
						donecountin = True
			lastmod = mod
			if donecountin == True:
				devread()
	except KeyboardInterrupt:
		devread()
		dev.close()
		print ' done recording'

def recmerge(rec):
	result = ''
	for k in rec:
		result += k
	return result
		
def saveprompt(r):
	pin = ''
	pin += raw_input('save to file named (blank for no save): ')
	if pin != '':
		f = wave.open(pin, 'w')
		f.setnchannels(1)
		f.setsampwidth(2)
		f.setframerate(44100)
		f.writeframes(r)
		f.close()

def doit(bpm, countin, tickpth):
	rec = []
	ticker(bpm, countin, tickpth, rec)
	r = recmerge(rec)
	saveprompt(r)

argv = sys.argv
doit(int(argv[1]), int(argv[2]), argv[3])
