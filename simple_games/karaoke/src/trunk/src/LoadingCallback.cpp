// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

// {{{ include
//
#include<string>

#include "LoadingCallback.h"
#include "ScreenManager.h"
#include "Surface.h"

LoadingCallback::LoadingCallback* LoadingCallback::instance=NULL;

// {{{ getInstance
LoadingCallback::LoadingCallback* LoadingCallback::getInstance(){
 // throw (ESingletonInstance)
    if (instance==NULL) throw ESingletonInstance(
            "LoadingCallback: get null instance");
    return instance;
}
// }}}

// {{{ createInstance
void LoadingCallback::createInstance(){ //throw(ESingletonInstance)
    if (instance!=NULL) throw ESingletonInstance(
            "LoadingCallback: double instantiating");
    instance=new LoadingCallback();
}

// }}}

// {{{ destroyInstance
void LoadingCallback::destroyInstance(){ //throw (ESingletonInstance)
    if (instance==NULL) throw ESingletonInstance (
                "LoadingCallback: destroying null instance");
    delete instance;
    instance=NULL;
}
// }}}

// {{{ itemLoaded
void LoadingCallback::itemLoaded(const std::string& loadingText,int line){
  if(line!=0 && line!=1)
    throw EIllegalArgument("LoadingCallback(): Line "+MyStringUtils::intToString(line)+" is not set.");
  text[line]=loadingText;
  Graphics::filledRectangle(surface,NULL,0,0,0,255);
  ScreenManager::_drawScreen(surface);
  //errorConsole.draw(surface);//TO-DO: chceme to tu?
  Graphics::flip(surface);
}
// }}}

// {{{ _itemLoaded
void LoadingCallback::_itemLoaded(const std::string& loadingText,int line){
  return getInstance()->itemLoaded(loadingText,line);
}
// }}}

// {{{ getItemLoaded
std::string LoadingCallback::getItemLoaded(int line){
  if(line!=0 && line!=1)
    throw EIllegalArgument("LoadingCallback(): Line "+MyStringUtils::intToString(line)+" is not set.");
  return text[line];
}
// }}}

// {{{ _getItemLoaded
std::string LoadingCallback::_getItemLoaded(int line){
  return getInstance()->getItemLoaded(line);
}
// }}}

// {{{ setSurface
void LoadingCallback::setSurface(Surface& _surface){
  surface = _surface;
}
// }}}

// {{{ _setSurface
void LoadingCallback::_setSurface(Surface& _surface){
  return getInstance()->setSurface(_surface);
}
// }}}

// {{{ LoadingCallback
LoadingCallback::LoadingCallback(){
}
// }}}

// {{{ ~LoadingCallback
LoadingCallback::~LoadingCallback(){
}
// }}}


