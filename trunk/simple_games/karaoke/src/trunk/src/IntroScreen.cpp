#include "ScreenManager.h"
#include "IntroScreen.h"
#include "Errors.h"
#include <SDL/SDL.h>
#include "Graphics.h"
#include "Surface.h"

IntroScreen::IntroScreen(){
 x=0;
}

std::string IntroScreen::getScreenName() const{
    return std::string("Intro");
}

void IntroScreen::handleEvent(const SDL_Event* const event){
    if (event->type==SDL_KEYDOWN) {
	ScreenManager::getInstance()->activateScreen("MainMenu");
    }
}

void IntroScreen::drawScreen(Surface& surface){
    if (x<639) x++; else x=0; 
     int q=x;
     //Graphics::circle(surface,100,100,20,255,255,0,255);
        for (int w=100;w<170;w++)
            Graphics::drawPixel(surface,q,w,q/3,w/3,abs(q-w)/3,255);
}

void IntroScreen::activateScreen(){
    Errors::_addError("Intro begins",
           Errors::NOTICE);
}

void IntroScreen::deactivateScreen(){
    Errors::_addError("Intro ends",
           Errors::NOTICE);
}

