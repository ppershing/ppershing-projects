// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_MAIN_MENU_SCREEN
#define H_MAIN_MENU_SCREEN
#include<vector>

#include "Screen.h"
#include "Surface.h"
#include "Font.h"

/**
  Screen that provides basic interface to game options. There exists only one instance during the runtime.
  */	

class MainMenuScreen:public Screen{
  public:
    /**
      Constructor. Creates screen with font and menu items.
      @author Miso
      */	
    MainMenuScreen();
    /**
      Destructor
      @author Miso
      */	
    ~MainMenuScreen();
    /**
      Returns the name of this screen 'MainMenu'.
      @author Miso
      */	
    std::string getScreenName() const;
    /**
      Handles events such as keypressed, SDL_quit
      @author Miso
      @return void
      */
    void handleEvent(const SDL_Event* const event);
    /**
      Draw the screen on the surface.
      @author Miso
      @return void
      @param surface surface to draw on
      @see Surface
      @see Graphics
      @see SongSelectionScreen
      @see Screen
      @see ScreenManager
      @see IntroScreen
      */
    void drawScreen(Surface& surface);
    /**
      Does some thing when this screen becomes active.
      @author Miso
    */
    void activateScreen();
    /**
      Does some things when this screen becomes inactive.
      @author Miso
    */
    void deactivateScreen();
    /**
      Performs some action according to selected menu item
      @author Miso
      */
    void select();
  private:
    /**
      Represents current selected menu item
      */
    int currentSelection;
    Font font;
    /**
      List of the menu items
      */
    std::vector<std::string> item;
    /**
      Number of the menu items
      */
    int items;
    std::string fontname;
};

#endif
