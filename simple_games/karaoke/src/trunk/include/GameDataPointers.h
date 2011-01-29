// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_GAME_DATA_POINTERS
#define H_GAME_DATA_POINTERS

#include "Exceptions.h"
#include "SongList.h"
#include "Font.h"
#include "Player.h"

/**
  Class for wrapping pointers to common data,
  such as songlist, player.
  @author Miso
  @see GameEngine
  */
class GameDataPointers{
public:
    /**
      Constructor.
    */
    GameDataPointers();
    /**
      Destructor.
    */
    ~GameDataPointers();
	
    /**
      Creates instance
      */
    static void createInstance();
        // throws (ESingletonInstance);

    /**
      Gets basic static instance
      */
    static GameDataPointers* getInstance();
    // throws (ESingletonInstance);
    
    /**
      destroy static instance
      */
    static void destroyInstance();
    
    /**
      Creates data structures and stores pointers to them.
      @author Miso
      */
    void initPointers();
    /**
      Shorthand.
      @author Miso
    */
    static void _initPointers();

    SongList* getSongList();
    static SongList* _getSongList();
    
    int getCurrentSongId();
    static int _getCurrentSongId();
    
    void setCurrentSongId(const int id);
    static void _setCurrentSongId(const int id);
    
    Font* getMenuFont();
    static Font* _getMenuFont();

    Player* getCurrentPlayer();
    static Player* _getCurrentPlayer();
    /**
      finalizes GameDataPointers.
      @author Miso
    */
    void finalize();
    /**
      Shorthand.
      @author Miso
    */
    static void _finalize();
    //throws (ESingletonInstance);

private:
    static GameDataPointers* instance;
    SongList* songList;
    int currentSongId;
    Font* menuFont;
    Player* player;
};

#endif
