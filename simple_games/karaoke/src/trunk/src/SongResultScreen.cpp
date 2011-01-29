// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#include "SongResultScreen.h"
#include "GameDataPointers.h"
#include "ScreenManager.h"
#include "Errors.h"
#include "Exceptions.h"
#include "NoDebug.h"
#include "Graphics.h"
#include "Preferences.h"
#include <SDL/SDL.h>
#include "Surface.h"
#include "Font.h"
#include "SongList.h"
#include "Directory.h"

// {{{ desctructor
SongResultScreen::~SongResultScreen(){
}
// }}}

// {{{ constructor
SongResultScreen::SongResultScreen(){
  items=2;
  item.push_back(" SING AGAIN");
  item.push_back(" MAIN MENU");
  font=Font("fonts/DejaVuSans.ttf",30);//GameDataPointers::_getSongList()->getMaxFontSize("fonts/DejaVuSans.ttf",300,70));
}
// }}}

// {{{ getScreenName
std::string SongResultScreen::getScreenName() const{
  return std::string("SongResult");
}
// }}}

// {{{ handleEvent
void SongResultScreen::handleEvent(const SDL_Event* const event){
  if (event->type==SDL_KEYDOWN) {
    switch(event->key.keysym.sym){
      case SDLK_UP:
        currentSelection=(currentSelection+items-1)%items;
        break;
      case SDLK_DOWN:
        currentSelection=(currentSelection+1)%items;
        break;
      case SDLK_RIGHT:
        break;
      case SDLK_LEFT:
        break;
      case SDLK_RETURN:
        select();
        break;
      case SDLK_ESCAPE:
        ScreenManager::getInstance()->activateScreen("MainMenu");
        break;
      default:
        break;
    }
  }
}
// }}}

// {{{ drawScreen
void SongResultScreen::drawScreen(Surface& surface){
  Graphics::drawText(surface,font,songname,10,40,255,255,255,255);
  Graphics::drawText(surface,font,MyStringUtils::intToString(score)+"/"+MyStringUtils::intToString(possible)+": "+rank,10,70,255,255,255,255);
  Graphics::rectangle(surface,100,currentSelection*80+150,400,currentSelection*80+220,0,0,255,255);

    for (int i=0;i<items;i++)
        Graphics::drawText(surface,font,item[i].c_str(),100,i*80+150,255,255,0,255);
}
// }}}

// {{{ activateScreen
void SongResultScreen::activateScreen(){
  Errors::_addError("SongResult activate",Errors::NOTICE);
  int currentSongResult=GameDataPointers::_getCurrentSongId();
  songname = GameDataPointers::_getSongList()->getSong(currentSongResult).title;
  currentSelection=0;
  if(GameDataPointers::_getSongList()->song.empty())
    throw Exception("Empty songlist, but some song was played.");
  else
    item[0]=" SING AGAIN";
  font=Font("fonts/DejaVuSans.ttf",30);
  score = GameDataPointers::_getCurrentPlayer()->score;
  DEBUG("\n"+GameDataPointers::_getSongList()->getSong(GameDataPointers::_getCurrentSongId()).getSongInfo());
  try{
    possible = 4.0 * GameDataPointers::_getSongList()->getSong(GameDataPointers::_getCurrentSongId()).activeLength / Preferences::_getDefaultDouble("/audio/timeunit",0.05);
  }catch(Exception e){
    Errors::_addError("Error trying to get possible score:\n"+std::string(e.what()),Errors::ERROR);
  }
  int efficiency = (possible!=0)?100*score/possible:101;
  if(efficiency>100)rank="HAXXOR";
  else if(efficiency>50)rank="SUPERSTAR";
  else if(efficiency>30)rank="LEAD SINGER";
  else if(efficiency>20)rank="RISING STAR";
  else if(efficiency>10)rank="AMATEUR";
  else if(efficiency>5)rank="DUCK";
  else if(efficiency>2)rank="FROG BELCHING";
  else if(efficiency>1)rank="LLAMA";
  else if(efficiency>0)rank="TOTAL LLAMA";
  else if(efficiency==0)rank="TONE DEAF";
  else rank="BUG IN SYSTEM";
  Errors::_addError("SongResult begins",Errors::NOTICE);
}
// }}}

// {{{ deactivateScreen
void SongResultScreen::deactivateScreen(){
    Errors::_addError("SongResult ends",
           Errors::NOTICE);
}
// }}}

// {{{ select
void SongResultScreen::select(){

  Errors::_addError("Item selected",Errors::NOTICE);
  switch(currentSelection){
    case 0://SING AGAIN
      ScreenManager::getInstance()->activateScreen("SongSelection");
      break;
    case 1://MAIN MENU
      ScreenManager::getInstance()->activateScreen("MainMenu");
      break;
    default:
        break;
    }
}
// }}}
