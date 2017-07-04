######################
###Alphabet Helpers###
######################
from string import ascii_uppercase

def numberToLetter(number): #0 = A, 25 = Z
    #if it's not really a letter, return @
    #if type(number) != type(5) and type(number) != type(np.array([3])[0]): #not an integer or numpy.int32
    #    return ('@')
    alpha = [i for i in ascii_uppercase]
    #print('Inside numberToLetter, number = ' + str(number))
    if number < 0 or number > len(alpha) or number == 2 or number == 22:
        return '@'
    else:
        return alpha[number]


def letterToNumber(letter): #A = 0, Z = 25
    #if it's not really a letter, return -999
    #HOW CAN I GENERICALLY TEST FOR LENGTH. EVEN IN CASE OF A NUMBER THAT' SNOT PART OF AN ARRAY?
    alpha = [i for i in ascii_uppercase]
    alpha.remove('C')
    alpha.remove('W')
    if letter == 'C' or letter == 'W':
        return '@'
    else:
        return alpha.index(letter)
