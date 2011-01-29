// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_SCREEN
#define H_SCREEN

#include "Surface.h"
#include <string>
#include <SDL/SDL.h>

/**
  Abstract class implementing various strategies for
  showing screens from ScreenManager.
  All screens are derived from this class
  */
class Screen{
  public:
    /**
      Method used by ScreenManager to obtain name of screen
      */
    virtual std::string getScreenName() const =0;

    /**
      Method called by ScreenManager when it registeres new event
      */
    virtual void handleEvent(const SDL_Event* const event)=0;

    /**
      Draw actual screen to canvas
      @param screen Screen to draw to
      */
    virtual void drawScreen(Surface& surface)=0;

    /**
      Method called when screen is activated
      */
    virtual void activateScreen()=0;

    /**
      Method called when screen is deactivated
      */
    virtual void deactivateScreen()=0;
    /**
      Method called when there is need to update audio ringbuffers
      */
    virtual void updateAudioBuffers();

    /**
      destructor
      */
    virtual ~Screen();

  private:
    static const int UPDATE_BUFFER_SIZE=1024;
};

#endif
