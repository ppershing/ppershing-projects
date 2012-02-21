#!/usr/bin/python
import math
import random
from termcolor import colored

f = open('slovka.dat', 'r')
f.readline() # version
sloviek = int(f.readline())

# the higher the number, the more slowly will knowledge adapt to new results
# setting 0 means "entirely last try", setting 1 means no adaptation at all

KNOWLEDGE_FACTOR = 0.7
RANDOMNESS = -2.3
POKUSOV = 10

class Entry:
	def __init__(self, query, answer):
		self.query = query
		self.answer = answer
		# number of times a correct answer was given
		self.tries_ok = 0
		# number of times this entry was shown
		self.tries_all = 0
		# number of entries shown before the last occurence of this entry
		self.last_try_age = 0
		# knowledge is a number between 0 and 1 describing how well
		# you know the entry. Most recent occurences of this entry
		# weight more towards the knowledge score
		self.knowledge_score = 0

	def answerWasCorrect(self):
		self.tries_all += 1
		self.tries_ok += 1
		self.knowledge_score = self.knowledge_score * KNOWLEDGE_FACTOR + \
								(1 - KNOWLEDGE_FACTOR)

	def answerWasBad(self):
		self.tries_all += 1
		self.knowledge_score = self.knowledge_score * KNOWLEDGE_FACTOR

	def advanceAge(self):
		self.last_try_age += 1

	def getRating(self):
		base = self.knowledge_score
		exponent = 1.5 + math.sqrt(self.last_try_age) / 15.0
		return math.pow(base, exponent)

	def __str__(self):
		return "%s -> %s (rating %.2f, knowledge %.2f (%d/%d), age: %d)" % (
			self.query, self.answer, self.getRating(), self.knowledge_score,
			self.tries_ok, self.tries_all, self.last_try_age)

entries = []
for i in range(sloviek):
	en = f.readline().strip()
	sk = f.readline().strip()
	tmp = f.readline().strip().split(" ")
	id = int(tmp[0])
	ok = int(tmp[1])
	all = int(tmp[2])
	last = int(tmp[3])
	rating = int(tmp[4])
	hist = float(f.readline())
	f.readline()

	e = Entry(sk, en)
	e.tries_ok = ok
	e.tries_all = all
	e.knowledge_score = hist
	entries.append(e)

def chooseEntryAccept(step):
	koef = math.exp(RANDOMNESS) + 1
	return math.pow(koef, step)

def chooseEntry(entries):
	if not entries:
		return None
	have = False
	for step in range(POKUSOV):
		e = random.choice(entries)
		if e.getRating() <= chooseEntryAccept(step):
			break
	print "choose step:", step
	return e

while True:
	e = chooseEntry(entries)
	print
	print
	print
	print colored(e.query, 'yellow', attrs=['bold'])
	answer = raw_input("Answer:")
	if len(answer) > 1 and answer[0]=='/':
		print "special command"
		continue

	for x in entries:
		x.advanceAge()

	print e
	if answer == e.answer:
		e.answerWasCorrect()
		print colored(e.answer, 'green', attrs=['bold'])
	else:
		e.answerWasBad()
		print colored(e.answer, 'red', attrs=['bold'])
	e.last_try_age = 0

	avg_rating = sum(map(lambda x: x.getRating(), entries)) / len(entries)
	print "average rating: %d" % (avg_rating * 1000)
