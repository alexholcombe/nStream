from math import sqrt

def corticalMagnification(stimulus, ltrHeight):
	A = 29.2 #Cortical scaling factor
	e2 = 3.67 #degrees

	###The 2 vs 8 experiment had 1 degree letters at 3 degrees of eccentricity

	###The scaling from the fovea to 3degrees is

	foveaToThree = (3+e2)/e2

	###Foveated letters would have to be this large to get a 1 degree high letter at 3 degrees of eccentricity

	foveatedSize= 1/foveaToThree

	ltrHeight = foveatedSize #this is the letter height at fixation (never really happens)

	baseMagnification = A/(3+e2) #when E = 3

	stimulusEccentricity = sqrt(stimulus.pos[0]**2 + stimulus.pos[1]**2) #E
	#print('Eccentricity: ' + str(stimulusEccentricity))

	stimulusMagnification = A/(stimulusEccentricity+e2)

	scaling = baseMagnification/stimulusMagnification

	stimulus.height = ltrHeight*scaling

	#print('Scaling factor: ' + str(scaling))
	#print('Stimulus height: ' + str(stimulus.height))

	stimulus.text = stimulus.text #Speeds up draw(), see http://www.psychopy.org/api/visual/textstim.html

	return stimulus