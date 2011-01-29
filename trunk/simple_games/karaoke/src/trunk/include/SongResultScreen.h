// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_SONG_RESULT_SCREEN
#define H_SONG_RESULT_SCREEN
#include<vector>

#include "SongList.h"
#include "Screen.h"
#include "Surface.h"
#include "Font.h"

/**
  Screen that displays results of played song. There exists only one instance during the runtime.
  @author Miso
  @see SongList
  @see SingScreen
  @see MainMenuScreen
  */	

class SongResultScreen:public Screen{
  public:
    /**
      Constructor.
      */	
    SongResultScreen();
    /**
      Destructor.
      */	
    ~SongResultScreen();
    /**
      Returns the name of this screen 'SongResult'.
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
    /**
      Maximum possible score of the last song.
    */
    int possible;
    /**
      Score managed last at last sing.
    */
    int score;
    /**
      Rank of the singer. Depends on quotient of score and maximum possible score.
    */
    std::string rank;
    std::string songname;
    std::string fontname;
};

#endif
