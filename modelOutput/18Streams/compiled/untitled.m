pdfVals = pdf_normmixture(theseErrorsByParticipant,parameterGuess(1),parameterGuess(2),parameterGuess(3))

theseZeros = pdfVals == 0

theseErrorsByParticipant(theseZeros)