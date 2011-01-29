// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_SONG_SELECTION_SCREEN
#define H_SONG_SELECTION_SCREEN
#include<vector>

#include "SongList.h"
#include "Screen.h"
#include "Surface.h"
#include "Font.h"

/**
  Screen that allows users to select song to sing. There exists only one instance during the runtime.
  @author Miso
  @see SongList
  @see SingScreen
  @see MainMenuScreen
  */	

class SongSelectionScreen:public Screen{
  public:
    /**
      Constructor.
      */	
    SongSelectionScreen();
    /**
      Destructor.
      */	
    ~SongSelectionScreen();
    /**
      Returns the name of this screen 'SongSelection'.
      */	
    std::string getScreenName() const;
    /**
      Handles events such as keypressed, SDL_quit.
      @author Miso
      @see ScreenManager
      @see GameEngine
      */
    void handleEvent(const SDL_Event* const event);
    /**
      Draws screen on the surface.
      @author Miso
      @return void
      @param Surface to draw on.
      @see Surface
      @see Graphics
      */
    void drawScreen(Surface& surface);
    void activateScreen();
    void deactivateScreen();
    /**
      Performs some action according to selected menu item.
      @author Miso
      @return void
      */
    void select();
  private:
    /**
      Index of the current selected menu item.
      */
    int currentSelection;
    /**
      Index of the current selected song in songlist.
      @see SongList
      */
    int currentSongSelection;
    /**
      Font to draw texts with.
      @see Font
      */
    Font font;
    /**
      List of the menu items.
      */
    std::vector<std::string> item;
    /**
      Number of the menu items.
      */
    int items;
    std::string fontname;
};

#endif
