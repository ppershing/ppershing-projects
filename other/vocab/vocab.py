#!/usr/bin/python
# vim: set fileencoding=utf-8 :
# vim: et:ts=4:sw=4:sts=4
import math
import random
import sys
from termcolor import colored
import time
import speak
import os.path
from config import Config
import utils
import graphic

class Entry:
    def __init__(self, query, answer):
        self.query = utils.fix_accents(query)
        self.answer = utils.fix_accents(answer)
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
        self.knowledge_score = self.knowledge_score * Config.KNOWLEDGE_FACTOR + \
                                (1 - Config.KNOWLEDGE_FACTOR)

    def answerWasBad(self):
        self.last_try_age = 0
        self.tries_all += 1
        self.knowledge_score = self.knowledge_score * Config.KNOWLEDGE_FACTOR

    def advanceAge(self, age = 1):
        self.last_try_age += age

    def getKnowledge(self):
        return self.knowledge_score

    def getRating(self):
        base = self.knowledge_score
        exponent = 1.5 + math.sqrt(self.last_try_age) / 15.0
        return math.pow(base, exponent)

    def __str__(self):
        return "%s -> %s (rating %.1f, knowledge %.1f (%d/%d), age: %d)" % (
            self.query, self.answer,
            100* self.getRating(), 100 * self.knowledge_score,
            self.tries_ok, self.tries_all, self.last_try_age)

class Vocabluary:
    def __init__(self, lang):
        self.entries = []
        self.epoch = 0
        self.lang = lang
        self.dictionaries = {}
        self.default_dict = ""

    def getFileName(self, dictname):
        return self.lang + '/' + dictname + '.dat'

    def addDict(self, dictname, train):
        if not self.default_dict:
            self.default_dict = dictname
        entries = self.loadFromFile(self.getFileName(dictname))
        self.dictionaries[dictname] = entries
        if train:
            self.entries += entries

    def loadFromFile(self, filename):
        f = open(filename, 'r')
        version = f.readline().strip()
        assert version == "ver.2.0"
        sloviek = int(f.readline())
        entries = []
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
            entries.append(e)
        for e in entries:
            e.advanceAge(self.epoch)
        print "loaded %d entries from %s" % (sloviek, filename)
        return entries

    def save(self):
        for key in self.dictionaries:
            self.saveToFile(self.getFileName(key), self.dictionaries[key])

    def saveToFile(self, filename, entries):
        f = open(filename, 'w')
        print >>f, "ver.2.0"
        print >>f, len(entries)
        for entry in entries:
            print >>f, entry.answer
            print >>f, entry.query
            print >>f, entry.id, entry.tries_ok, entry.tries_all, \
                self.epoch - entry.last_try_age, \
                -1 # int (1000 * entry.getRating()) # no rating provided
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

    def getMaxRating(self):
        return max(map(lambda x: x.getRating(), self.entries))

    def add(self, entry):
        self.entries.append(entry)
        self.dictionaries[self.default_dict].append(entry)

    def move(self, entry, dictionary):
        assert dictionary in self.dictionaries
        for d in self.dictionaries:
            if entry in self.dictionaries[d]:
                self.dictionaries[d].remove(entry)
        self.dictionaries[dictionary].append(entry)

class MagicChooser:
    @classmethod
    def getThreshold(cls, step):
        koef = math.exp(Config.RANDOMNESS) + 1
        return math.pow(koef, step) - 1

    @classmethod
    def chooseEntry(cls, entries):
        assert entries
        for step in range(Config.POKUSOV):
            e = random.choice(entries)
            if e.getRating() <= cls.getThreshold(step):
                break
        return e

    @classmethod
    def getThresholds(cls):
        return [cls.getThreshold(step) for step in range(Config.POKUSOV)]

    

vocab = Vocabluary(Config.LANG)
for f in Config.VOCABLUARY_FILES:
    vocab.addDict(f, f in Config.VOCABLUARY_TRAIN)

h = graphic.Histogram(80, 10, '*')
print MagicChooser.getThresholds()

e = None
while True:
    old_e = e
    e = vocab.chooseEntry(MagicChooser.chooseEntry)
    print
    print
    print
    print "Rating: %.1f Score: %.1f" % (
        100* e.getRating(), 100 * e.knowledge_score)
    print colored(e.query, 'yellow', attrs=['bold'])
    answer = raw_input("Answer:")

    CMD_SAVE = "/save"
    CMD_PLAY = "/play"
    CMD_EXIT = "/exit"
    CMD_SAVE_EXIT = "/wq"
    CMD_ADD = "/add/"
    CMD_SEARCH = "/s/"
    CMD_MOVE = "/mv/"
    if len(answer) > 0 and answer[0]=='/':
        if (answer == CMD_SAVE):
            print "saving..."
            vocab.save()
        elif (answer == CMD_SAVE_EXIT):
            print "exitting..."
            vocab.save()
            sys.exit()
        elif (answer == CMD_EXIT):
            print "exitting..."
            print "really?"
            a = raw_input()
            if a == "yes":
                sys.exit()
        elif (answer[0:len(CMD_PLAY)] == CMD_PLAY):
            if len(answer) == len(CMD_PLAY):
                text = old_e.answer
            else:
                text = answer[len(CMD_PLAY):]
            text = utils.fix_accents(text)
            speak.play(vocab.lang, text)
        elif (answer[0:len(CMD_ADD)] == CMD_ADD):
            tokens = answer[len(CMD_ADD):].split("/")
            if len(tokens) == 2:
                query = tokens[0]
                response = tokens[1]
                new_e = Entry(query, response)
                vocab.add(new_e)
                print colored('added %s' % new_e, 'green')
                if Config.ADD_AUTOPLAY:
                    speak.play(vocab.lang, utils.fix_accents(response))
            else:
                print colored('wrong add', 'red')
            print tokens
        elif (answer[0:len(CMD_SEARCH)] == CMD_SEARCH):
            text = utils.fix_accents(answer[len(CMD_SEARCH):])
            print colored('searching for ' + text, 'yellow')
            for x in vocab.entries:
                if x.answer.find(text) != -1:
                    print x
            print "EOF"
        elif (answer[0:len(CMD_MOVE)] == CMD_MOVE):
            d = answer[len(CMD_MOVE):]
            if d in vocab.dictionaries:
                vocab.move(e, d)
                print colored('moved...', 'green')
            else:
                print colored("no such dictionary!", 'red')
        else:
            print "unknown command"
        e = old_e # go back
        continue


    print e
    answer = utils.fix_accents(answer)
    if answer == e.answer:
        e.answerWasCorrect()
        print colored(e.answer, 'green', attrs=['bold'])
    else:
        e.answerWasBad()
        print answer
        print colored(e.answer, 'red', attrs=['bold'])
        if Config.FAIL_AUTOPLAY:
            speak.play(Config.NATIVE_LANG, e.query)
            speak.play(vocab.lang, e.answer)
        time.sleep(1)
    
    vocab.advance()

    print
    print "average rating: %d, max: %d" % (
        vocab.getAverageRating() * 1000, vocab.getMaxRating() * 1000)

    if Config.SHOW_RATING_HISTOGRAM:
        h.printHistogram(map(lambda x:x.getRating() * 1000, vocab.entries),
                         "%03.0f", sys.stdout)
    if Config.SHOW_KNOWLEDGE_HISTOGRAM:
        h.printHistogram(map(lambda x:x.getKnowledge(), vocab.entries),
                         "%1.2f", sys.stdout)
