
// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk
#ifndef H_WIDGET
#define H_WIDGET

#include "Graphics.h"


/**
  base class for all small "miniapplications"
  embedded into project
  */
class Widget{
  public:
  /**
    constructor, rect will be position on screen
    */
    Widget(const SDL_Rect& rect);

    /**
      constructor, default rect is 0,0, width 0, height 0
      shoud be instantly replaced (otherwise calling draw
      may cause exceptions
      */
    Widget();

    /**
      constructor, parameters forms position on screen
      */

    Widget(const int x1,const int y1,
           const int x2,const int y2);

    /**
      set new position
      */
    void setRect(const SDL_Rect& rect);

    /**
      set new position
      */
    void setRect(const int x1,const int y1,
           const int x2,const int y2);

    /**
      draw widget to surface
      */
    void draw(Surface& surface);

  protected:
    /**
      called by draw after clipping surface,
      implements each widget on it's own
      */
    virtual void clippedDraw(Surface& surface)=0;
  public:

    /**
      return if widget is actively drawing on surface
      */
    bool isActive() const;

    /**
      activate widget
      */
    void activate();

    /**
      deactivate widget
      */
    void deactivate();

    void setFrameColor(const unsigned char R, const unsigned char G,
            const unsigned char B,const unsigned char A);

    protected:

    /**
      cliping rectangle
      */
    SDL_Rect rectangle;

    unsigned char frameR,frameG,frameB,frameA;

    bool active;
};
#endif
