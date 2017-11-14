
for nStreams in [2,21]:
    for cueTemporalPos in [6,7,8,9,10]: #5 or 6 temporal serial positions for  the cue
        for ring in [0,1,2]:
            for pairAngle in [0,1,2,3]:
                for whichInPair in [0,1]:
                    if nStreams==2:
                        whichStreamCuedAngle = 360/4*pairAngle
                        whichStreamCuedAngle += whichInPair * 180
                        print(ring)
                        print(whichInPair)
                        print('whichStreamCuedAngle' + str(whichStreamCuedAngle))
                    elif nStreams == 21:
                        whichStreamCuedAngle = 360/4*pairAngle
                        whichStreamCuedAngle += whichInPair * 180  