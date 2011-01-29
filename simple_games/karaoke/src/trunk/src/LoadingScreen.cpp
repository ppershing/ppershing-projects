#include "ScreenManager.h"
#include "LoadingScreen.h"
#include "Errors.h"
#include <SDL/SDL.h>
#include "Graphics.h"
#include "Surface.h"
#include "Font.h"
#include "LoadingCallback.h"

LoadingScreen::LoadingScreen(){
  songsLoaded=0;
  font=Font("fonts/DejaVuSans.ttf",25);
}

std::string LoadingScreen::getScreenName() const{
    return std::string("Loading");
}

void LoadingScreen::handleEvent(const SDL_Event* const event){
    if (event->type==SDL_KEYDOWN) {
    }
}

void LoadingScreen::drawScreen(Surface& surface){
  Graphics::drawText(surface,font,LoadingCallback::_getItemLoaded(0),100,100,255,255,0,255);
  Graphics::drawText(surface,font,LoadingCallback::_getItemLoaded(1),100,150,255,255,0,255);
}

void LoadingScreen::activateScreen(){
    Errors::_addError("Loading begins",
           Errors::NOTICE);
}

void LoadingScreen::deactivateScreen(){
    Errors::_addError("Loading ends",
           Errors::NOTICE);
}

