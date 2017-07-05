from math import sqrt

def corticalMagnification(stimulus, ltrHeight):
	A = 29.2 #Cortical scaling factor
	e2 = 3.67 #degrees

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