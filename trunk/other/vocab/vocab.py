#!/usr/bin/python
# vim: et:ts=4:sw=4:sts=4
import math
import random
import sys
from termcolor import colored
import time


class config:
    # the higher the number, the more slowly will knowledge adapt to new results
    # setting 0 means "entirely last try", setting 1 means no adaptation at all
    KNOWLEDGE_FACTOR = 0.7
    # TODO
    RANDOMNESS = -2.3
    # TODO
    POKUSOV = 10
    #
    VOCABLUARY = "slovka.dat"

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
        # unused
        self.id = 0

    def answerWasCorrect(self):
        self.last_try_age = 0
        self.tries_all += 1
        self.tries_ok += 1
        self.knowledge_score = self.knowledge_score * config.KNOWLEDGE_FACTOR + \
                                (1 - config.KNOWLEDGE_FACTOR)

    def answerWasBad(self):
        self.last_try_age = 0
        self.tries_all += 1
        self.knowledge_score = self.knowledge_score * config.KNOWLEDGE_FACTOR

    def advanceAge(self, age = 1):
        self.last_try_age += age

    def getRating(self):
        base = self.knowledge_score
        exponent = 1.5 + math.sqrt(self.last_try_age) / 15.0
        return math.pow(base, exponent)

    def __str__(self):
        return "%s -> %s (rating %.2f, knowledge %.2f (%d/%d), age: %d)" % (
            self.query, self.answer, self.getRating(), self.knowledge_score,
            self.tries_ok, self.tries_all, self.last_try_age)

class Vocabluary:
    def __init__(self):
        self.entries = []
        self.epoch = 0

    def loadFromFile(self, filename):
        f = open(filename, 'r')
        version = f.readline().strip()
        assert version == "ver.2.0"
        sloviek = int(f.readline())
        self.entries = []
        self.epoch = 0
        for i in range(sloviek):
            en = f.readline().strip()
            sk = f.readline().strip()
            tmp = f.readline().strip().split(" ")
            id = int(tmp[0])
            ok = int(tmp[1])
            all = int(tmp[2])
            last = int(tmp[3])
            #rating = int(tmp[4])
            hist = float(f.readline())
            f.readline()

            e = Entry(sk, en)
            e.id = id
            e.tries_ok = ok
            e.tries_all = all
            e.knowledge_score = hist
            e.last_try_age = -last
            self.epoch = max([self.epoch, last])
            self.entries.append(e)
        for e in self.entries:
            e.advanceAge(self.epoch)
        print "loaded %d entries" % sloviek

    def saveToFile(self, filename):
        f = open(filename, 'w')
        print >>f, "ver.2.0"
        print >>f, len(self.entries)
        for entry in self.entries:
            print >>f, entry.answer
            print >>f, entry.query
            print >>f, entry.id, entry.tries_ok, entry.tries_all, \
                self.epoch - entry.last_try_age, \
                int (1000 * entry.getRating())
            print >>f, "%.5f" % entry.knowledge_score
            print >>f

    def chooseEntry(self, chooseFunction):
        assert self.entries
        return chooseFunction(self.entries)

    def advance(self):
        self.epoch += 1
        for e in self.entries:
            e.advanceAge()

    def getAverageRating(self):
        return sum(map(lambda x: x.getRating(), self.entries)) / len(self.entries)

class MagicChooser:
    @classmethod
    def chooseEntryAccept(cls, step):
        koef = math.exp(config.RANDOMNESS) + 1
        return math.pow(koef, step)

    @classmethod
    def chooseEntry(cls, entries):
        assert entries
        for step in range(config.POKUSOV):
            e = random.choice(entries)
            if e.getRating() <= cls.chooseEntryAccept(step):
                break
        return e

class Histogram:
    def __init__(self, width, height, char):
        self.width = width
        self.height = height
        self.char = char

    def printHistogram(self, values, stream):
        values = [x for x in values] # copy
        values.sort();
        values.reverse()
        data = [ ['.' for y in range(self.height)] for x in range(self.width)]
        maxval = max(values)
        for x in range(self.width):
            sample = int(x * 1.0 / self.width * len(values))
            for y in range(int(values[sample] * 1.0 / maxval  * self.height)):
                data[x][y] = self.char
        for y in range(self.height):
            for x in range(self.width):
                stream.write(data[x][self.height - 1 - y])
            stream.write('\n')
        return

vocab = Vocabluary()
vocab.loadFromFile(config.VOCABLUARY)

h = Histogram(80, 10, '*')

while True:
    e = vocab.chooseEntry(MagicChooser.chooseEntry)
    print
    print
    print
    print e.getRating(), e.knowledge_score
    print colored(e.query, 'yellow', attrs=['bold'])
    answer = raw_input("Answer:")
    if len(answer) > 1 and answer[0]=='/':
        if (answer == "/save"):
            print "saving..."
            vocab.saveToFile(config.VOCABLUARY)
        elif (answer == "/wq"):
            print "exitting..."
            vocab.saveToFile(config.VOCABLUARY)
            sys.exit()
        elif (answer == "/exit"):
            print "exitting..."
            print "really?"
            a = raw_input()
            if a == "yes":
                sys.exit()
        else:
            print "unknown command"
        continue


    print e
    if answer == e.answer:
        e.answerWasCorrect()
        print colored(e.answer, 'green', attrs=['bold'])
    else:
        e.answerWasBad()
        print colored(e.answer, 'red', attrs=['bold'])
        time.sleep(1)
    
    vocab.advance()

    print
    print "average rating: %d" % (vocab.getAverageRating() * 1000)
    h.printHistogram(map(lambda x:x.getRating(), vocab.entries), sys.stdout)
