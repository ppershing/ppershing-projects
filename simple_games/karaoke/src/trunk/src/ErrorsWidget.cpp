// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "ErrorsWidget.h"
#include "NoDebug.h"
#include "Exceptions.h"

// {{{ ErrorsWidget
ErrorsWidget::ErrorsWidget(){
    fontR=fontG=fontB=fontA=255;
}
// }}}

// {{{ ~ErrorsWidget
ErrorsWidget::~ErrorsWidget(){
}
// }}}

// {{{ setFontColor
void ErrorsWidget::setFontColor(const unsigned char R, const unsigned
        char G, const unsigned char B,const unsigned char A){
    fontR=R;
    fontG=G;
    fontB=B;
    fontA=A;
}
// }}}

// {{{ setFont
void ErrorsWidget::setFont(Font& f){
    font=f;
}
// }}}

// {{{ clippedDraw
void ErrorsWidget::clippedDraw(Surface& surface){
    try {
        int fontSize=font.getLineHeight();
        std::vector<std::string> errors=Errors::_getLastErrors();
        int cnt=errors.size();
        if (cnt*fontSize>rectangle.h) cnt=rectangle.h/fontSize-1;

        for (int i=0; i<cnt; i++){
            Graphics::drawText(surface,font,errors[i],
                    0,(cnt-i-1)*fontSize,fontR,fontG,fontB,fontA);
        }
    } catch (Exception& e){
        Errors::_addError(std::string("There was an error during "
                    "ErrorsWidget::draw :\n")+e.what(),Errors::ERROR);
    }
}
// }}}
