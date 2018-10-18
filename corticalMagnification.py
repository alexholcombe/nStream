from math import sqrt

def corticalMagnification(stimulus, ltrHeight, cue=False, sizeOut = False):
	'''
	stimulus is stimulus object
	ltrHeight is height at 3 deg eccentricity (size and eccentricity of 2v8 study)
	cue if stimulus is ring cue
	sizeOut if you want function to return the size , otherwise the function returns a stimulus object
	'''
	e2 = 2.
	####S  = (1+E/E_2)S_0

	###The 2 vs 8 experiment had .9 degree letters at 3 degrees of eccentricity

	###Foveated letters would have to be this large to get a ltrHeight degree high letter at 3 degrees of eccentricity

	#print(str(ltrHeight))
	#print(str(1+3./e2))

	stimulusEccentricity = float(sqrt(stimulus.pos[0]**2 + stimulus.pos[1]**2)) #Eccentricity

	foveatedSize = float(ltrHeight)/(1.+3./e2)

	ltrHeight = foveatedSize #ltrHeight is now the letter height at fixation (never really happens)


	scaledSize = (1+stimulusEccentricity/e2)*foveatedSize #Scaled from foveated size.

	scaledSize = round(scaledSize,2)

	if not cue:
		stimulus.height = scaledSize
		stimulus.text = stimulus.text #Speeds up draw(), see http://www.psychopy.org/api/visual/textstim.html
	else: #If it's a circle cue
		stimulus.radius = scaledSize

	#print('Eccentricity: ' + str(stimulusEccentricity))
	#print('Height: ', str(stimulus.height))
	#print('Scaling factor: ' + str(scaling))
	#print('Stimulus height: ' + str(stimulus.height))
	if sizeOut:
		return scaledSize
	else:
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

	ltrHeight = .9

	for ecc in [3,7,11.5]:
		stimulus = testStimulus()
		stimulus.height = 0
		stimulus.pos = [0,ecc]
		testStimuli.append(stimulus)
		stimulus = corticalMagnification(stimulus, ltrHeight)
		sizes.append(stimulus.height)

	print(sizes)

	if sizes[0] == ltrHeight:
		print('Test passed')
	else:
		print('Test failed. Stimulus size at 3deg is ' + str(sizes[3]))

	class testCue( object ):
		def __init__( self ):
			self.radius = None
			self.pos = list()

	for ecc in [3,7,11.5]:
		stimulus = testCue()
		stimulus.pos = [0, ecc]
		stimulus = corticalMagnification(stimulus, .9810000000000002, cue = True, sizeOut = False)
		print(stimulus.radius)


