'''
stable.py
Test whether putative solution to stable roommates problem is, in fact, 
a solution.
Inputs:
  1. File of prefernce lists.  First string on a line is the name of the person 
  whose list this is.  Remaining strings are the preference lists, in
  decreasing order of desirability.
  2. File with same base name as prefrence file, but with .out.txt appended.  
  Each line lists a person and his roommate.
'''

import sys, os.path
from itertools import takewhile

if len(sys.argv) != 2:
    print("Usgae: python stable.py filename")
    exit()

prefs = dict()
with open(sys.argv[1]) as fin:
    for line in fin:
        line = line.split()
        prefs[line[0]] = line[1:]

roommates = dict()
with open(os.path.splitext(sys.argv[1])[0]+'.out.txt') as fin:
    for line in fin:
        line = line.split()
        roommates[line[0]] = line[1]
       
oneOne = True 
for r1 in roommates:
    r2 = roommates[r1]
    if roommates[r2] != r1:
        print("Error: %s's roommate is %s and %s's roommate is %s" % r1, r2, r2, roommates[r2]) 
        oneOne = False
if not oneOne: exit()

stable = True
wish = dict()
for p in prefs:
    r = roommates[p]
    wish[p] = list(takewhile(lambda x: x != r, prefs[p]))
    
for w in wish:
    for r in wish[w]:
        if w in wish[r]:
            print("Blocking pair %s %s" % (w, r))
            stable = False
            
if stable: print("Matching is stable")

            