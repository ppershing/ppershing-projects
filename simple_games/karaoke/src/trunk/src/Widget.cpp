// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "Widget.h"
#include "Errors.h"

// {{ Widget(rect)
Widget::Widget(const SDL_Rect& rect):rectangle(rect),active(true){
    frameR=frameG=frameB=frameA=255;
}
// }}}

// {{{ Widget()
Widget::Widget():active(true){
    rectangle.x=0;
    rectangle.y=0;
    rectangle.w=0;
    rectangle.h=0;
    frameR=frameG=frameB=frameA=255;
}
// }}}

// {{{ Widget(x1,y1,x2,y2)
Widget::Widget(const int x1,const int y1,
               const int x2,const int y2):active(true){ 
    rectangle.x=x1;
    rectangle.y=y1;
    rectangle.w=x2-x1+1;
    rectangle.h=y2-y1+1;
}
// }}}

// {{{ setFrameColor
void Widget::setFrameColor(const unsigned char R, const unsigned char G,
            const unsigned char B,const unsigned char A){
    frameR=R;
    frameG=G;
    frameB=B;
    frameA=A;
}
// }}}


// {{{ setRect(rect)
void Widget::setRect(const SDL_Rect& rect){
    rectangle=rect;
}
// }}}

// {{{ setRect(x1,y1,x2,y2)
void Widget::setRect(const int x1,const int y1,
               const int x2,const int y2){ 
    rectangle.x=x1;
    rectangle.y=y1;
    rectangle.w=x2-x1+1;
    rectangle.h=y2-y1+1;
}
// }}}

// {{{ draw
void Widget::draw(Surface& surface){
    if (!active) return;
    SDL_Rect oldRectangle;
    SDL_GetClipRect(surface.getSurface(),&oldRectangle);
    SDL_SetClipRect(surface.getSurface(),&rectangle);
    try{
      clippedDraw(surface);
    }catch(Exception e){
      Errors::_addError("Drawing widget failed:\n"+std::string(e.what()),Errors::ERROR);
    }
    Graphics::rectangle(surface,&rectangle,frameR,frameG,frameB,frameA);
    SDL_SetClipRect(surface.getSurface(),&oldRectangle);
}
// }}}

// {{{ isActive
bool Widget::isActive() const {
    return active;
}
// }}}

// {{{ activate
void Widget::activate(){
    active=true;
}
// }}}

// {{{ deactivate
void Widget::deactivate(){
    active=false;
}    
// }}}
