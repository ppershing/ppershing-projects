// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#include "GameDataPointers.h"
#include "SongList.h"
#include "Font.h"
#include "Player.h"
#include "Preferences.h"
#include "Errors.h"
#include "Debug.h"
#include<string>

GameDataPointers::GameDataPointers* GameDataPointers::instance=NULL;

// {{{ constructor
GameDataPointers::GameDataPointers(){
}
// }}}

// {{{ destructor
GameDataPointers::~GameDataPointers(){
}
// }}}

// {{{ getInstance
GameDataPointers::GameDataPointers* GameDataPointers::getInstance(){ //throw (ESingletonInstance) 
    if (instance==NULL) {
        throw ESingletonInstance(std::string(
                    "GameDataPointers: no instance to get"));
    }
    return instance;
}
// }}}

// {{{ createInstance
void GameDataPointers::createInstance(){
   // throw (ESingletonInstance) 
    if (instance!=NULL) 
        throw ESingletonInstance("GameDataPointers: create instance called twice");

    instance=new GameDataPointers::GameDataPointers();
}
// }}}

// {{{ destroyInstace
void GameDataPointers::destroyInstance(){ //throw (ESingletonInstance) 
   if (instance==NULL) 
       throw ESingletonInstance("GameDataPointers: destroying null class");
   delete instance;
   instance=NULL;
}
// }}}

// {{{ initPointers
void GameDataPointers::initPointers(){
  songList=new SongList();
  //int menuFontSize = Preferences::_getInt("/video/resolution/height")/Preferences::_getInt("/themes/menu/itemCount")/2;
        //std::string fontName=Preferences::_getString("/themes/menu/fontName");
  menuFont=new Font(Preferences::_getDefaultString("/themes/menu/fontName","fonts/Errors.ttf"),(Preferences::_getInt("/video/resolution/height")-Preferences::_getInt("/themes/menu/offsetY"))/Preferences::_getInt("/themes/menu/itemCount")/2);
  player=new Player();
  Errors::_addError("Data Pointers init OK",Errors::NOTICE);
}
// }}}

// {{{ _initPointers
void GameDataPointers::_initPointers(){
  getInstance()->initPointers();
}
// }}}

// {{{ getSongList
SongList* GameDataPointers::getSongList(){
  if (songList==NULL) throw EIllegalArgument("Trying to do something on null songlist");
  return songList;
}
// }}}

// {{{ _getSongList
SongList* GameDataPointers::_getSongList(){
  return getInstance()->getSongList();
}
// }}}

// {{{ getCurrentSongId
int GameDataPointers::getCurrentSongId(){
  return currentSongId;
}
// }}}

// {{{ _getCurrentSongid
int GameDataPointers::_getCurrentSongId(){
  return getInstance()->getCurrentSongId();
}
// }}}

// {{{ setCurrentSongId
void GameDataPointers::setCurrentSongId(const int id){
  currentSongId=id;
}
// }}}

// {{{ _setCurrentSongId
void GameDataPointers::_setCurrentSongId(const int id){
  getInstance()->setCurrentSongId(id);
}
// }}}

// {{{ getMenuFont
Font* GameDataPointers::getMenuFont(){
  if(menuFont==NULL)throw EIllegalArgument("menuFont is not initialized");
  return menuFont;
}
// }}}

// {{{ _getMenuFont
Font* GameDataPointers::_getMenuFont(){
  return getInstance()->getMenuFont();
}
// }}}

// {{{ getCurrentPlayer
Player* GameDataPointers::getCurrentPlayer(){
  if(player==NULL)throw EIllegalArgument("player is not initialized");
  return player;
}
// }}}

// {{{ _getCurrentPlayer
Player* GameDataPointers::_getCurrentPlayer(){
  return getInstance()->getCurrentPlayer();
}
// }}}

// {{{ finalize
void GameDataPointers::finalize(){
  DEBUG("GameDataPointers: finalize SongList");
  delete player;
  delete menuFont;
  delete songList;
}
// }}}

// {{{ _finalize
void GameDataPointers::_finalize(){
  getInstance()->finalize();
}
// }}}

