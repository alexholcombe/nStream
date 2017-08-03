######################
###Alphabet Helpers###
######################
from string import ascii_uppercase

def numberToLetter(number, alpha): #0 = A, 25 = Z
    #if it's not really a letter, return @
    #if type(number) != type(5) and type(number) != type(np.array([3])[0]): #not an integer or numpy.int32
    #    return ('@')
    #print('Inside numberToLetter, number = ' + str(number))
    if number < 0 or number > len(alpha):
        return '@'
    else:
        print(number)
        return alpha[number]


def letterToNumber(letter,alpha): #A = 0, Z = 25
    #if it's not really a letter, return -999
    #HOW CAN I GENERICALLY TEST FOR LENGTH. EVEN IN CASE OF A NUMBER THAT' SNOT PART OF AN ARRAY?
    if letter not in alpha:
        return '@'
    else:
        return alpha.index(letter)
