import random
import string


with open("words.txt") as f:
  bpDict = f.read().splitlines() 


print("")
print("Welcome to BombParty!")
print("You can play the original game at https://jklm.fun")
print("To play, type a word that contains the given sequence of letters.")
print("Use #skip or #end to skip or end the game.")
print("")
print("What difficulty would you like?")


def getDiff():
    while True:
        diff = input("Type either #easy, #medium, or #hard." + "\n")
        if diff == "#easy":
            return 100
        elif diff == "#medium":
            return 70
        elif diff == "#hard":
            return 35
        else:
            print("Invalid input, try agian.")


def getSeq(seqLen):
    while True:
        seq = ""
        for i in range(3):
            seq += random.choice(string.ascii_lowercase)
        filtered = [x for x in bpDict if seq in x]
        if len(filtered) > seqLen:
            return seq


def playGame():
    diff = getDiff()
    seq = getSeq(diff)
    score = 0
    while True:
        inputWord = input("Sequence: " + seq + "\n").lower()
        if inputWord == "#skip":
            print("Okay, word skipped.")
            seq = getSeq(diff)
        elif inputWord == "#end":
            return print("The game has ended. Here's your final score:", str(score))
        elif (inputWord in bpDict) and (seq in inputWord):
            score += 1
            seq = getSeq(diff)
            print("Correct! Current score:", str(score))
        elif inputWord in bpDict:
            print("That doesn't have the sequence! Try again.")
        else:
            print("That's not a word! Try again.")
        

playGame()