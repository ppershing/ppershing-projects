// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk


#include "ScreenManager.h"
#include <stdlib.h>
#include "Errors.h"
#include "Exceptions.h"
#include <string>
#include "Debug.h"
#include "Graphics.h"
#include "Preferences.h"
#include "Surface.h"

ScreenManager::ScreenManager* ScreenManager::instance=NULL;

//FIXME: document NULL returns

// {{{ getInstance
ScreenManager::ScreenManager* ScreenManager::getInstance(){
 // throw (ESingletonInstance)
    if (instance==NULL) throw ESingletonInstance(
            "ScreenManager: get null instance");
    return instance;
}
// }}}

// {{{ createInstance
void ScreenManager::createInstance(){ //throw(ESingletonInstance)
    if (instance!=NULL) throw ESingletonInstance(
            "ScreenManager: double instantiating");
    instance=new ScreenManager();
}

// }}}

// {{{ destroyInstance
void ScreenManager::destroyInstance(){ //throw (ESingletonInstance)
    if (instance==NULL) throw ESingletonInstance (
                "ScreenManager: destroying null instance");
    delete instance;
    instance=NULL;
}
// }}}

// {{{ registerScreen
void ScreenManager::registerScreen(Screen* const screen){
    if (getScreenFromName(screen->getScreenName())!=NULL)
        throw new EIllegalArgument(
                "register screen - screen "+screen->getScreenName()+
                " already exists");
    screenList.push_back(screen);
}

void ScreenManager::_registerScreen(Screen* const screen){ //throw (ESingletonInstance)
    return getInstance()->registerScreen(screen);
}

// }}}

// {{{ unregisterScreen
void ScreenManager::unregisterScreen(Screen *screen){
    Screen* s;
    if (screen==activeScreen) throw EIllegalArgument(
            "ScreenManager::unregisterScreen "+screen->getScreenName()+
            " is active");
    
    s=getScreenFromName(screen->getScreenName());
    
    if (s==0) throw EIllegalArgument(
            "unregisterScreen - screen "+screen->getScreenName()+
            " not registered");
    
    std::vector<Screen::Screen*>::iterator it=screenList.begin();
    while ((*it)!=s) it++;
    screenList.erase(it);
}

void ScreenManager::_unregisterScreen(Screen *screen){ //throw (ESingletonInstance)
    return getInstance()->unregisterScreen(screen);
}
// }}}

//FIXME: not valid screen name, check first
// {{{ activateScreen
void ScreenManager::activateScreen(const std::string& screenName){
    if (activeScreen!=NULL) activeScreen->deactivateScreen();
    activeScreen=getScreenFromName(screenName);
    activeScreen->activateScreen();
}

void ScreenManager::_activateScreen(const std::string& screenName){
    // throw (ESingletonInstance)
    return getInstance()->activateScreen(screenName);
}
// }}}

//FIXME: null
// {{{ getActiveScreenName
std::string ScreenManager::getActiveScreenName() const{
    return activeScreen->getScreenName();
}

std::string ScreenManager::_getActiveScreenName(){
    //throw (ESingletonInstance)
    return getInstance()->getActiveScreenName();
}
// }}}

// {{{ getActiveScreen
const Screen::Screen* ScreenManager::getActiveScreen() const{
    return activeScreen;
}

const Screen::Screen* ScreenManager::_getActiveScreen(){
    //throw (ESingletonInstance)
    return getInstance()->getActiveScreen();
}
// }}}

// {{{ getScreenFromName
Screen::Screen* ScreenManager::getScreenFromName(const
        std::string& screenName) {
    std::vector<Screen::Screen*>::iterator i;
    for (i=screenList.begin();i!=screenList.end();i++)
        if ((*i)->getScreenName()==screenName) return (*i);
    return NULL;
}

Screen::Screen* ScreenManager::_getScreenFromName(
 const std::string& screenName){ //throw (ESingletonInstance)
    return getInstance()->getScreenFromName(screenName);
}
// }}}


// {{{ drawScreen
void ScreenManager::drawScreen(Surface& surface){
    Assert(activeScreen!=NULL,"ScreenManager::drawScreen "+
             " there is no active screen");
    activeScreen->drawScreen(surface);
}

void ScreenManager::_drawScreen(Surface& surface){
    //throw (ESingletonInstance)
    return getInstance()->drawScreen(surface);
}
// }}}

// {{{  handleEvent
void ScreenManager::handleEvent(const SDL_Event* const event){
    Assert(activeScreen!=NULL,"ScreenManager::handleEvent "+
             " there is no active screen");
    activeScreen->handleEvent(event);
}

void ScreenManager::_handleEvent(const SDL_Event* const event){
    // throw (ESingletonInstance)
    return getInstance()->handleEvent(event);
}
// }}}

// {{{  updateAudioBuffers
void ScreenManager::updateAudioBuffers(){
    Assert(activeScreen!=NULL,"ScreenManager::updateAudioBuffers "+
             " there is no active screen");
    activeScreen->updateAudioBuffers();
}

void ScreenManager::_updateAudioBuffers(){
    // throw (ESingletonInstance)
    return getInstance()->updateAudioBuffers();
}
// }}}


// {{{ init
void ScreenManager::init(){
}

void ScreenManager::_init(){ //throw (ESingletonInstance)
    getInstance()->init();
}
// }}}

// {{{ ScreenManager
ScreenManager::ScreenManager():activeScreen(NULL){

}
// }}}

// {{{ ~ScreenManager
ScreenManager::~ScreenManager(){
}
// }}}

// {{{ finalize
void ScreenManager::finalize(){
    if (activeScreen!=NULL) activeScreen->deactivateScreen();
    std::vector<Screen::Screen*>::iterator it=screenList.begin();

    while (it!=screenList.end()) {
        DEBUG(std::string("ScreenManager: autoclearing screen ")+
                (*it)->getScreenName());

        delete (*it);
        it++;
    }
}

void ScreenManager::_finalize(){ //throw (ESingletonInstance)
    ScreenManager::getInstance()->finalize();
}
// }}}
