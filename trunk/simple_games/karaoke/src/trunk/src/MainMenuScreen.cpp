// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#include "MainMenuScreen.h"
#include "ScreenManager.h"
#include "Errors.h"
#include "Graphics.h"
#include "Preferences.h"
#include "GameDataPointers.h"
#include <SDL/SDL.h>
#include "Surface.h"
#include "Font.h"

//FIXME: 1. font should be configurable, size should be from metric or
//       2. use Metric
//       3. SM->getInstance should be abbreviated by _

MainMenuScreen::~MainMenuScreen(){
}

MainMenuScreen::MainMenuScreen(){
 font=Font("fonts/DejaVuSans.ttf",42);
 currentSelection=0;
 items=5;
 item.push_back(" SING");
 item.push_back(" OPTIONS");
 item.push_back(" INTRO");
 item.push_back(" PPScreen");
 item.push_back(" QUIT");
}

std::string MainMenuScreen::getScreenName() const{
    return std::string("MainMenu");
}

void MainMenuScreen::handleEvent(const SDL_Event* const event){
    if (event->type==SDL_KEYDOWN) {
        switch(event->key.keysym.sym){
            case SDLK_UP:
                currentSelection=(currentSelection+items-1)%items;
                break;
            case SDLK_DOWN:
                currentSelection=(currentSelection+1)%items;
                break;
            case SDLK_RETURN:
                select();
                break;
            case SDLK_ESCAPE:
                Errors::_addError("ESC pressed - exiting application",Errors::NOTICE);
                SDL_Event event;
                event.type=SDL_QUIT;
                SDL_PushEvent(&event);
                break;
            default:
                break;
        }
    }
}

void MainMenuScreen::drawScreen(Surface& surface){
    
    Graphics::rectangle(surface,100,currentSelection*80+50,400,currentSelection*80+120,0,0,255,255);

    for (int i=0;i<items;i++)
        Graphics::drawText(surface,font,item[i].c_str(),100,i*80+50,255,255,0,255);
}

void MainMenuScreen::activateScreen(){
    Errors::_addError("MainMenu begins",
           Errors::NOTICE);
}

void MainMenuScreen::deactivateScreen(){
    Errors::_addError("MainMenu ends",
           Errors::NOTICE);
}

void MainMenuScreen::select(){
    Errors::_addError("Item selected",
           Errors::NOTICE);
switch(currentSelection){
    case 0://SING
        ScreenManager::getInstance()->activateScreen("SongSelection");
        break;
    case 1://OPTIONS
        break;
    case 2://INTRO
        ScreenManager::getInstance()->activateScreen("Intro");
	break;
    case 3://PPScreen
        ScreenManager::getInstance()->activateScreen("PP");
        break;
    case 4://QUIT
        Errors::_addError("QUIT selected",Errors::NOTICE);
        SDL_Event event;
        event.type=SDL_QUIT;
        SDL_PushEvent(&event);
	break;
    default:
        break;
    }
}

