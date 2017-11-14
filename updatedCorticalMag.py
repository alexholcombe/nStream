from math import sqrt

def corticalMagnification(stimulus, ltrHeight, cue=False):
	#####Function and parameters from: Robert F. Dougherty, Volker M. Koch, Alyssa A. Brewer, Bernd Fischer, 
 	#								   Jan Modersitzki, Brian A. Wandell; Visual field representations and locations of visual areas V1/2/3 
	# 							       in human visual cortex. Journal of Vision 2003;3(10):1. doi: 10.1167/3.10.1.
	A = 29.2 #Cortical scaling factor
	e2 = 3.67 #degrees

	####The Magnification factor is the cortical area associated with processing the stimulus
	####Magnification Factor = A/(eccentricity+e2)

	###The 2 vs 8 experiment had .9 degree letters at 3 degrees of eccentricity

	###The scaling from the fovea to 3degrees is

	#foveaToThree = (3+e2)/e2

	###Foveated letters would have to be this large to get a ltrHeight degree high letter at 3 degrees of eccentricity

	foveatedSize= (ltrHeight*e2)/(3+e2)

	ltrHeight = foveatedSize #ltrHeight is now the letter height at fixation (never really happens)

	#baseMagnification = A/(e2) #when E = 0

	stimulusEccentricity = sqrt(stimulus.pos[0]**2 + stimulus.pos[1]**2) #Eccentricity


	#stimulusMagnification = A/(stimulusEccentricity+e2)

	scaling = (stimulusEccentricity+e2)/e2 #Scaled from foveated size.

	if not cue:
		stimulus.height = ltrHeight*scaling
		stimulus.text = stimulus.text #Speeds up draw(), see http://www.psychopy.org/api/visual/textstim.html
	else:
		stimulus.radius = ltrHeight*scaling

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

	ltrHeight = 1

	for ecc in [3,7,11.5]:
		stimulus = testStimulus()
		stimulus.height = 0
		stimulus.pos = [0,ecc]
		testStimuli.append(stimulus)
		stimulus = corticalMagnification(stimulus, ltrHeight)
		sizes.append(stimulus.height)

	print(sizes)

	# if sizes[3] == ltrHeight:
	# 	print('Test passed')
	# else:
	# 	print('Test failed. Stimulus size at 3deg is ' + str(sizes[3]))


