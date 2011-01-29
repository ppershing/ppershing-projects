// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "Font.h"
#include "exception"
#include "Exceptions.h"

#include "Errors.h"
#include "NoDebug.h"
#include "SDL/SDL_ttf.h"

#include "MyStringUtils.h"

// {{{ Font
Font::Font():referenceCount(new int),font(NULL){
    *referenceCount=1;
}
// }}}

// {{{ Font(fontName,pt)
Font::Font(const std::string& fontName,const int pt):referenceCount(new int){
    DEBUG("Load font " +MyStringUtils::intToString(pt));
    font=TTF_OpenFont(fontName.c_str(),pt);
    if (font==NULL) {        
        delete referenceCount;
        throw EIllegalArgument(std::string("Font load failed (")+
                fontName+","+MyStringUtils::intToString(pt)+") "+
                +TTF_GetError());
    }
    *referenceCount=1;
}
// }}}

// {{{ cleanup
void Font::cleanup(){
    if (--(*referenceCount)==0) {
        delete referenceCount;
        TTF_CloseFont(font);
    }
}
// }}}

// {{{ assign
void Font::assign(const Font& f){
    font=f.font;
    referenceCount=f.referenceCount;
    (*referenceCount)++;
}
// }}}

// {{{ getFont 
TTF_Font* Font::getFont(){
    if (font==NULL) throw EIllegalArgument("Trying to do something on "
            "null font");
    return font;
}
// }}}

// {{{ getLineHeight 
int Font::getLineHeight(){
    return TTF_FontLineSkip(getFont());
}
// }}}

// {{{ getHeight 
int Font::getHeight(){
    return TTF_FontHeight(getFont());

}
// }}}

// {{{ getWidth 
int Font::getWidth(const std::string& text){
    int w,h;
    if (TTF_SizeText(getFont(),text.c_str(),&w,&h)!=0) {
        throw Exception(std::string("Font::getHeight failed,") 
                + TTF_GetError());
    }
    return w;
}
// }}}

// {{{ ~Font 
Font::~Font(){
    cleanup();
}
// }}}

// {{{ operator=
Font& Font::operator=(const Font& f){
    if (this!=&f){
      cleanup();
      assign(f);
    }
    return *this;
}
// }}}

// {{{ Font(const Font& )
Font::Font(const Font& f){
    assign(f);
}
// }}}

// {{{ getMaxFontSize
int Font::getMaxFontSize(const std::string& fontName, const std::string&
        text, const int width, const int height){
    const int initialGuess=20;
    Font guessFont=Font(fontName,initialGuess);
    int guess= initialGuess*(double) width/ guessFont.getWidth(text);
    if (guess>=height) return height-1; else return guess;
}
// }}}

// {{{ getMaxFontSize
int Font::getMaxFontSize(const std::string& fontName, const std::string&
        text, const int width){
  return getMaxFontSize(fontName,text,width,99999999);
}
// }}}
