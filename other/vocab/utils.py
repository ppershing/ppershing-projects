#!/usr/bin/python
# vim: set fileencoding=utf-8 :
# vim: et:ts=4:sw=4:sts=4
def fix_accents(text):
    ACCENT_REPLACE = [
         [":a", "ä"],
         [":o", "ö"],
         [":u", "ü"],

         [",a", "á"],
         [",e", "é"],
         [",i", "í"],
         [",o", "ó"],
         [",u", "ú"],
         [",y", "ý"],

         [",r", "ŕ"],
         [",l", "ĺ"],

         [">c", "č"],
         [">d", "ď"],
         [">l", "ľ"],
         [">n", "ň"],
         [">s", "š"],
         [">t", "ť"],
         [">z", "ž"],

         ["^e", "ê"],
         ["^o", "ô"],

         ["`a", "à"],
         ["`e", "è"],

         ["SS", "ß"],

         ["+c" ,"ç"],
        ]
    #ÈÇ
    for entry in ACCENT_REPLACE:
        text = text.replace(entry[0], entry[1])
    return text
