// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#include "ScreenManager.h"
#include "Errors.h"
#include "Graphics.h"
#include "Font.h"
#include "Preferences.h"
#include "MyStringUtils.h"
#include "Surface.h"
#include "Debug.h"

#include <SDL/SDL.h>
#include <SDL/SDL_gfxPrimitives.h>



#define SUCCESS 0

// FIXME: BUG: really we want * in  *SDL_GetError()?
// {{{ checkPoint
void Graphics::checkPoint(Surface& screen, const int x,const int y){
/*  if (x<0 || x>=screen.getWidth() ||
      y<0 || y>=screen.getHeight())
    throw EIllegalArgument("pixel " 
        +MyStringUtils::intToString(x)+","
        +MyStringUtils::intToString(y)+
        " lies outside screen: " + *SDL_GetError());

  SDL_Rect r=screen.getSurface()->clip_rect;
  if (x<r.x || y<r.y ||
      x>=(r.x+r.w) || y>=(r.y+r.h))
    throw EIllegalArgument("pixel " 
        +MyStringUtils::intToString(x)+","
        +MyStringUtils::intToString(y)+
        " lies outside clipping rect: " + *SDL_GetError());
        */
}
// }}}

// {{{  drawPixel
void Graphics::drawPixel(Surface& screen, const int x, const int y, const int R, const int G, const int B, const int A){
  checkPoint(screen,x,y);

  if(pixelRGBA(screen.getSurface(),x,y,R,G,B,A)!=SUCCESS)
    throw Exception("Pixel drawing failed: " + *SDL_GetError());
}
// }}}

// {{{ drawText
void Graphics::drawText(Surface& screen, Font& font, const std::string text, const int x,const int y, const int R, const int G, const int B, const int A) {
  checkPoint(screen,x,y);

  if (text=="") return; // no draw at all

  if (font.font==NULL) throw EIllegalArgument("Null font!");
  SDL_Color tmpfontcolor = {R,G,B,A};
  SDL_Rect textBg;
  textBg.x=x; textBg.y=y;
  Surface message;
  try {
    message=Surface(TTF_RenderText_Blended(font.font, text.c_str(), tmpfontcolor),1);
  } catch (EIllegalArgument& e){
    throw Exception(std::string("Drawing font failed: ")+TTF_GetError());

  }
  if (SDL_BlitSurface(message.getSurface(), NULL, screen.getSurface(), &textBg)
      !=SUCCESS) throw Exception(std::string("Graphics: can't blit: ") + SDL_GetError());
}
// }}}

// {{{ textToSurface
Surface Graphics::textToSurface(Surface& screen, Font& font, const std::string text, 
    const int x,const int y, const int R, const int G, const int B, const int A){

  checkPoint(screen,x,y);

  if (font.font==NULL) throw EIllegalArgument("Null font!");
  SDL_Color tmpfontcolor = {R,G,B,A};
  SDL_Rect textBg;
  textBg.x=x; textBg.y=y;
  Surface message=Surface(TTF_RenderText_Blended(font.font, text.c_str(), tmpfontcolor),1);
  Surface temp=Surface(screen.getSurface(),1);
  if (SDL_BlitSurface(message.getSurface(), NULL, temp.getSurface(), &textBg)
      !=SUCCESS) throw Exception("Graphics: can't blit: " + *SDL_GetError());
  return temp;
}
// }}}

//FIXME: function ignores RGBA
// {{{ filledRectangle( x1,y1,x2,y2)
void Graphics::filledRectangle(Surface& screen, const int x1, const
    int y1, const int x2, const int y2,
    const int R,const int G, const int B, const int A){
  checkPoint(screen,x1,y1);
  checkPoint(screen,x2,y2);

  if (x1>x2 || y1>y2) throw EIllegalArgument("Rectangle: "
      "invalid points order: " + *SDL_GetError());

  if (A==255) {

    SDL_Rect rect={x1,y1,x2-x1+1,y2-y1+1}; // width,height is +1 !

    if (SDL_FillRect(screen.getSurface(),&rect,0)!=SUCCESS)
      throw Exception("Filled Rectangle drawing failed: " + *SDL_GetError());


  } else {
    if(boxRGBA(screen.getSurface(),x1,y1,x2,y2,R,G,B,A)!=SUCCESS)
      throw Exception("Filled Rectangle drawing failed: " + *SDL_GetError());
  }
}
// }}}

// {{{ filledRectangle( rect )
void Graphics::filledRectangle(Surface& screen, SDL_Rect *rect, const int R, const int G, const int B, const int A){
  if(rect==NULL) {
    filledRectangle(screen,0,0,screen.getWidth()-1,screen.getHeight()-1,R,G,B,A);
  } else {
    filledRectangle(screen,rect->x,rect->y,rect->x+rect->w-1,rect->y+rect->h-1,R,G,B,A);
  }
}
// }}}

// {{{ rectangle (x1,y1,x2,y2)
void Graphics::rectangle(Surface& screen, const int x1, const int y1, const int x2, const int y2, const int R, const int G, const int B, const int A){
  checkPoint(screen,x1,y1);
  checkPoint(screen,x2,y2);

  if (x1>x2 || y1>y2) throw EIllegalArgument("Rectangle: "
      "invalid points order");

  if(rectangleRGBA(screen.getSurface(),x1,y1,x2,y2,R,G,B,A)!=SUCCESS)
    throw Exception("Rectangle drawing failed: " + *SDL_GetError());
}
// }}}

// {{{ rectangle (rect)
void Graphics::rectangle(Surface& screen, SDL_Rect *rect, const int R, const int G, const int B, const int A){
  if(rect==NULL) {
    rectangle(screen,0,0,screen.getWidth()-1,screen.getHeight()-1,R,G,B,A);
  } else {
    rectangle(screen,rect->x,rect->y,rect->x+rect->w-1,rect->y+rect->h-1,R,G,B,A);
  }
}
// }}}

// {{{ line (x1,y1,x2,y2)
void Graphics::line(Surface& screen, const int x1, const int y1, const int x2, const int y2, const int R, const int G, const int B, const int A){

  checkPoint(screen,x1,y1);
  checkPoint(screen,x2,y2);
  if(lineRGBA(screen.getSurface(),x1,y1,x2,y2,R,G,B,A)!=SUCCESS)
    throw Exception("Line drawing failed: " + *SDL_GetError());	
}
// }}}

// {{{ circle
void Graphics::circle(Surface& screen, const int x, const int y,
    const int r, const int R, const int G, const int B, const int A){
  checkPoint(screen,x,y-r);
  checkPoint(screen,x-r,y);
  checkPoint(screen,x,y+r);
  checkPoint(screen,x+r,y);
  if(circleRGBA(screen.getSurface(),x,y,r,R,G,B,A)!=SUCCESS)
    throw Exception("Circle drawing failed: " + *SDL_GetError());
}
// }}}

// {{{ filledCircle
void Graphics::filledCircle(Surface& screen, const int x, const int y,
    const int r, const int R, const int G, const int B, const int A){
  checkPoint(screen,x,y-r);
  checkPoint(screen,x-r,y);
  checkPoint(screen,x,y+r);
  checkPoint(screen,x+r,y);
  if(filledCircleRGBA(screen.getSurface(),x,y,r,R,G,B,A)!=SUCCESS)
    throw Exception("Filles circle drawing failed: " + *SDL_GetError());
}
// }}}

//FIXME: check return
// {{{ flip
void Graphics::flip(Surface& screen){
	SDL_Flip(screen.getSurface());
}
// }}}
