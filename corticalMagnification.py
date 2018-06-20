from math import sqrt

def corticalMagnification(stimulus, ltrHeight, cue=False):
	e2 = 2
	####S  = (1+E/E_2)S_0

	###The 2 vs 8 experiment had .9 degree letters at 3 degrees of eccentricity

	###Foveated letters would have to be this large to get a ltrHeight degree high letter at 3 degrees of eccentricity

	#print(str(ltrHeight))
	#print(str(1+3./e2))

	foveatedSize = float(ltrHeight)/(1.+3./e2)
	print('Foveated size = ' + str(foveatedSize))

	ltrHeight = foveatedSize #ltrHeight is now the letter height at fixation (never really happens)

	#baseMagnification = A/(e2) #when E = 0

	stimulusEccentricity = sqrt(stimulus.pos[0]**2 + stimulus.pos[1]**2) #Eccentricity


	#stimulusMagnification = A/(stimulusEccentricity+e2)

	scaledSize = (1+stimulusEccentricity/e2)*foveatedSize #Scaled from foveated size.

	if not cue:
		stimulus.height = scaledSize
		stimulus.text = stimulus.text #Speeds up draw(), see http://www.psychopy.org/api/visual/textstim.html
	else: #If it's a circle cue
		stimulus.radius = scaledSize

	#print('Eccentricity: ' + str(stimulusEccentricity))
	#print('Height: ', str(stimulus.height))
	#print('Scaling factor: ' + str(scaling))
	#print('Stimulus height: ' + str(stimulus.height))

	return stimulus

if __name__ == '__main__':
	###Some tests
	class testStimulus( object ):
	   def __init__( self ):
	       self.height= None
	       self.pos = list()
	       self.text = 'a'

	testStimuli = list()
	sizes = list()

	ltrHeight = 3

	for ecc in xrange(11):
		stimulus = testStimulus()
		stimulus.height = 0
		stimulus.pos = [0,ecc]
		testStimuli.append(stimulus)
		stimulus = corticalMagnification(stimulus, ltrHeight)
		sizes.append(stimulus.height)

	print(sizes)

	if sizes[3] == ltrHeight:
		print('Test passed')
	else:
		print('Test failed. Stimulus size at 3deg is ' + str(sizes[3]))


