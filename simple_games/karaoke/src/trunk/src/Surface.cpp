// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "Surface.h"
#include "exception"
#include "Exceptions.h"

#include "Errors.h"
#include "NoDebug.h"
#include "SDL/SDL_ttf.h"

#include "StackTrace.h"

// {{{ Surface()
Surface::Surface():referenceCount(new int),surface(NULL),
copySurface(false),freeSurface(false){
    DEBUG("new surface");
    *referenceCount=1;
}
// }}}

// {{{ Surface(SDL_Surface*, autoFree)
Surface::Surface(SDL_Surface* surface, bool autoFree)
:referenceCount(new int),surface(surface),copySurface(false),
freeSurface(autoFree){
    StackTrace trace;
    DEBUG("new external surface"+trace.getStringData());
    if (surface==NULL) { 
        delete referenceCount;
        throw EIllegalArgument("Surface is NULL");
    }
    *referenceCount=1;
}
// }}}

// {{{ Surface(width,height,depth)
Surface::Surface(int width,int height,int depth):
referenceCount(new int),copySurface(true),freeSurface(true){
    surface=SDL_CreateRGBSurface(SDL_SWSURFACE,width,height,depth,0,0,0,0);
    if (surface==NULL) {
        delete referenceCount;
        throw EIllegalArgument("Sufrace creation problem");
    }
    *referenceCount=1;
}
// }}}

// {{{ cleanup
void Surface::cleanup(){
    if (--(*referenceCount)==0) {
        delete referenceCount;
        if (freeSurface && surface!=NULL){ // we want to free it
            SDL_FreeSurface(surface);
        }
    }
}
// }}}

// {{{ assign
void Surface::assign(const Surface& s){
    if (s.copySurface) {
        surface=SDL_ConvertSurface(s.surface,
                s.surface->format,s.surface->flags);
        referenceCount=new int;
        *referenceCount=1;
        if (surface==NULL) {
            throw Exception("FATAL: Can't copy "
                "surface - maybe memory limit or "
                " wrong bit-depth - check this");
        }
    } else {
        surface=s.surface;
        referenceCount=s.referenceCount;
        (*referenceCount)++;
    }
    copySurface=s.copySurface;
    freeSurface=s.freeSurface;
}
// }}}

// {{{ ~Surface
Surface::~Surface(){
    cleanup();
}
// }}}

// {{{ operator=
Surface& Surface::operator=(const Surface& s){
    if (this!=&s){
      cleanup();
      assign(s);
    }
    return *this;
}
// }}}

// {{{ Surface( const Surface& )
Surface::Surface(const Surface& s){
    assign(s);
}
// }}}
