#!/usr/bin/python
#encoding:utf-8
# vim: et:ts=4:sw=4:sts=4
import urllib
import httplib
import re
import os

class MyOpener(urllib.URLopener):
    version = 'Mozilla/5.0 (Windows; U; Windows NT 5.1; it; rv:1.8.1.11) Gecko/20071127 Firefox/2.0.0.11'

def ensure_dir(filename):
    d = os.path.dirname(filename)
    if not os.path.exists(d):
        os.makedirs(d)

def download_speech(lang, text, filename):
    print "Fetching file ..."
    baseUrl = "http://translate.google.com/translate_tts?"
    params = ({'tl' : lang, 'q' : text})
    if os.path.exists(filename):
        os.system("rm '%s'" %filename);
    ensure_dir(filename)
    stream = MyOpener().retrieve(baseUrl + urllib.urlencode(params),
        filename)
    print "Done"


def play_file(filename, wait = False):
    if wait:
        os.system("mplayer \"%s\" 2>&1 >/dev/null" % filename)
    else:
        os.system("mplayer \"%s\" 2>&1 >/dev/null &" % filename)

def play(lang, text):
    print "play", lang, text
    CACHE_DIR = "cache"
    filename = CACHE_DIR + "/" + lang + "-" + text
    if not os.path.exists(filename):
        download_speech(lang, text, filename)
    play_file(filename, True)

"""
download_speech('sk', "Najvyšším vrchom Južnej Ameriky je:", "/tmp/question.mp3");
download_speech('sk', "a, Ojos del Salado", "/tmp/answer_a.mp3");

play_speech("/tmp/question.mp3");
play_speech("/tmp/answer_a.mp3");
"""
