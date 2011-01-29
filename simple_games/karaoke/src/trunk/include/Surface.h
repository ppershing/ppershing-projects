// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk
#ifndef H_SURFACE
#define H_SURFACE

#include <string>
#include <SDL/SDL.h>
#include "Exceptions.h"
class Surface;
/**
  class for wrapping SDL_Surface*
  */
class Surface{
  public:
    friend class Graphics;
    Surface();
    /**
      create Surface from given surface,
      does not copy this surface on assign/copy,
      only maintains referenceCount,
      if autoFree is true, then last instance pointing
      to Surface* automatically frees that surface
      */
    Surface(SDL_Surface* surface,bool autoFree);

    /**
      create own surface,
      which is automatically copied if assignment is made
      */
    Surface(int width,int height,int depth);

    /**
      assignment operator
      */
    Surface& operator=(const Surface& s);

    /**
      copy constructor
      */
    Surface(const Surface& s);

    /**
      get actual surface, if null, throw IllegalArgument
      */
    SDL_Surface* getSurface();

    /**
      get width of actual surface, if surface is null, 
      throw EIllegalArgument
      */
    int getWidth();

    /**
      get height of actual surface, if surface is null,
      throw EIllegalArgument
      */
    int getHeight();
	
	
    /**
      destructor
      */
    ~Surface();
  
  private:
    /**
      cleans data
      */
    void cleanup();

    /**
      assign new data
      */
    void assign(const Surface& s);

    /**
      number of references to current surface
      */
    int* referenceCount;

    /**
      surface itself
      */
    SDL_Surface* surface;


    /**
      if copy surface
      */
    bool copySurface;

    /**
      if auto-free surface
      */
    bool freeSurface;
};

// {{{ getSurface
inline SDL_Surface* Surface::getSurface(){
	if (surface==NULL) throw EIllegalArgument("Asking for no surface");
	return surface;
}
// }}}

// {{{ getWidth
inline int Surface::getWidth(){
	return getSurface()->w;
}
// }}}

// {{{ getHeight
inline int Surface::getHeight(){
	return getSurface()->h;
}
// }}}


#endif
