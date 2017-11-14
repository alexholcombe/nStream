### Work out critical spacing for ring cue ###

eccentricities = [3,7,11.5]

letterHeights = [1.0, 1.5997001499250376, 2.2743628185907045]

cueLineWidth = 0.07521188910048975

def boumas(ecc, height):
	halfEcc = ecc/2
	halfHeight = height/2

	radius = halfHeight + halfEcc + .5

	return radius

cueSizes = list()

for i in range(3):
	ecc = eccentricities[i]
	letterHeight = letterHeights[i]
	cueRad = boumas(ecc, letterHeight)
	cueSizes.append(cueRad)

print(cueSizes)