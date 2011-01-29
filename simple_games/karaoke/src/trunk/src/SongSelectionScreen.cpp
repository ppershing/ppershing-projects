// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#include "SongSelectionScreen.h"
#include "GameDataPointers.h"
#include "ScreenManager.h"
#include "Errors.h"
#include "Graphics.h"
#include "Preferences.h"
#include <SDL/SDL.h>
#include "Surface.h"
#include "Font.h"
#include "SongList.h"
#include "Directory.h"
#include "SongComparator.h"

// {{{ desctructor
SongSelectionScreen::~SongSelectionScreen(){
}
// }}}

// {{{ constructor
SongSelectionScreen::SongSelectionScreen(){
  currentSelection=0;
  currentSongSelection=0;
  items=2;
  fprintf(stderr,"OK1\n");
  //fprintf(stderr,"%d\n",GameDataPointers::_getSongList()->song.empty());
  /*if(GameDataPointers::_getSongList()->song.empty())
    item.push_back(" NO SONGS");
  else
    item.push_back(GameDataPointers::_getSongList()->song[currentSongSelection].dirname);
    */
  fprintf(stderr,"OK2\n");
  item.push_back(" NO SONGS");
  item.push_back(" BACK");
  font=Font("fonts/DejaVuSans.ttf",30);//GameDataPointers::_getSongList()->getMaxFontSize("fonts/DejaVuSans.ttf",300,70));
  fprintf(stderr,"OK3\n");
}
// }}}

// {{{ getScreenName
std::string SongSelectionScreen::getScreenName() const{
  return std::string("SongSelection");
}
// }}}

// {{{ handleEvent
void SongSelectionScreen::handleEvent(const SDL_Event* const event){
  if (event->type==SDL_KEYDOWN) {
    if(event->key.keysym.sym>=SDLK_a && event->key.keysym.sym<=SDLK_z){
      int selLookUp = GameDataPointers::_getSongList()->getNextIndex(currentSongSelection);
      while(selLookUp!=currentSongSelection && GameDataPointers::_getSongList()->getSong(selLookUp).title[0]+32!=event->key.keysym.sym){ //map key to ascii value
        selLookUp = GameDataPointers::_getSongList()->getNextIndex(selLookUp);
      }//iterates at most all songs to find first with title starting with pressed key
      currentSongSelection = selLookUp;
      item[0] = GameDataPointers::_getSongList()->getSong(currentSongSelection).title;
    }else
    switch(event->key.keysym.sym){
      case SDLK_UP:
        currentSelection=(currentSelection+items-1)%items;
        break;
      case SDLK_DOWN:
        currentSelection=(currentSelection+1)%items;
        break;
      case SDLK_RIGHT:
        if(currentSelection==0){
          currentSongSelection = GameDataPointers::_getSongList()->getNextIndex(currentSongSelection);
          item[0]=GameDataPointers::_getSongList()->getSong(currentSongSelection).title;
        }
        break;
      case SDLK_LEFT:
        if(currentSelection==0){
          currentSongSelection = GameDataPointers::_getSongList()->getPrevIndex(currentSongSelection);
          item[0]=GameDataPointers::_getSongList()->getSong(currentSongSelection).title;
        }
        break;
      case SDLK_RETURN:
        select();
        break;
      case SDLK_SPACE:{
        GameDataPointers::_getSongList()->compareByNext();
        int selUID = GameDataPointers::_getSongList()->getSong(currentSongSelection).UID;
        GameDataPointers::_getSongList()->sort();
        currentSongSelection = GameDataPointers::_getSongList()->getIndexFromUID(selUID);
        Errors::_addError("Songs sorted by "+GameDataPointers::_getSongList()->getSortAttribute(),Errors::NOTICE);
        break;
        }
      case SDLK_ESCAPE:
        GameDataPointers::_setCurrentSongId(currentSongSelection);
        ScreenManager::getInstance()->activateScreen("MainMenu");
        break;
      default:
        break;
    }
  }
}
// }}}

// {{{ drawScreen
void SongSelectionScreen::drawScreen(Surface& surface){
  Graphics::drawText(surface,font,"Sorted by "+GameDataPointers::_getSongList()->getSortAttribute(),150,20,255,255,0,255);
  Graphics::drawText(surface,font,MyStringUtils::intToString(GameDataPointers::_getSongList()->song.size()),20,20,255,255,0,255);  
  Graphics::rectangle(surface,100,currentSelection*80+50,400,currentSelection*80+120,0,0,255,255);

    for (int i=0;i<items;i++)
        Graphics::drawText(surface,font,item[i].c_str(),100,i*80+50,255,255,0,255);
}
// }}}

// {{{ activateScreen
void SongSelectionScreen::activateScreen(){
  Errors::_addError("SongSelection begins",Errors::NOTICE);
  currentSongSelection=GameDataPointers::_getCurrentSongId();
  if(GameDataPointers::_getSongList()->song.empty())
    item[0]="NO SONGS";
  else
    item[0]=GameDataPointers::_getSongList()->getSong(currentSongSelection).title;
  font=Font("fonts/DejaVuSans.ttf",20);//GameDataPointers::_getSongList()->getMaxFontSize("fonts/DejaVuSans.ttf",300,70));
  Errors::_addError("SongSelection really begins",Errors::NOTICE);
}
// }}}

// {{{ deactivateScreen
void SongSelectionScreen::deactivateScreen(){
    Errors::_addError("SongSelection ends",
           Errors::NOTICE);
}
// }}}

// {{{ select
void SongSelectionScreen::select(){

  Errors::_addError("Item selected",Errors::NOTICE);
  switch(currentSelection){
    case 0://SING
      GameDataPointers::_setCurrentSongId(currentSongSelection);
      ScreenManager::getInstance()->activateScreen("Sing");
      break;
    case 1://BACK
      GameDataPointers::_setCurrentSongId(currentSongSelection);
      ScreenManager::getInstance()->activateScreen("MainMenu");
      break;
    default:
        break;
    }
}
// }}}
