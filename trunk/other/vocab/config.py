#!/usr/bin/python
# vim: set fileencoding=utf-8 :
# vim: et:ts=4:sw=4:sts=4
class Config:
    # the higher the number, the more slowly will knowledge adapt to new results
    # setting 0 means "entirely last try", setting 1 means no adaptation at all
    KNOWLEDGE_FACTOR = 0.7
    # TODO
    RANDOMNESS = -2.3
    # TODO
    POKUSOV = 10
    #
    VOCABLUARY = "de-slovka.dat"
    #VOCABLUARY = "fr-skola.dat"
    FAIL_AUTOPLAY = True
    ADD_AUTOPLAY = True
