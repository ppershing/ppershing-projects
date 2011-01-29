// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_GAME_ENGINE
#define H_GAME_ENGINE

#include <SDL/SDL.h>
#include "ScreenManager.h"
#include "Exceptions.h"
#include "Graphics.h"
#include "Surface.h"
#include "ErrorsWidget.h"
/**
  Singleton class for basic game engine,
  which implements all basic functionality
  */
class GameEngine{
  public:
      
      /**
        Method to obtain active instance
        */
      static GameEngine* getInstance();
        //throws (ESingletonInstance);

      /**
        Create an instance of game engine
        */
      static void createInstance();
       //throws (ESingletonInstance);

      /**
        Init all.(ie. graphics, audio, screenmanager, screens)
        @returns 0 on false
        */
      int init();

      /**
        Run game loop
        */
      void run();

      /**
        Set save preferences state
        */
      void setSavePreferences(const int state);



      /**
        Method for destroying active instance
        */
      static void destroyInstance();
       //throws (ESingletonInstance);

  private:
      /**
        Private constructor (is Singleton)
        */
      GameEngine();

      /**
        Private destructor (is Singleton)
        */
      ~GameEngine();

      /**
        init and return state of sdl initialization, for finalization
        */
      int initSDL();

      /**
        init and return state of ttf initializaiotn, for finalization
        */
      int initTTF();

      /**
        init and return state of sdl video initialization
        */
      int initGraphics();

      /**
        init and return state of audio initialization
        */
      int initAudio();

      /**
        init and return state of screens&manager initialization
        */
      int initScreens();

      /**
        prepares error console if set up in preferences
        */
      int initErrorConsole();

      /**
        load songs and return the state of loading
	@author Miso
	@see SongList
        */
      int loadSongs();

      /**
        init and return state of timer initialization
        */
      int initTimer();
      
      /**
        Finalize all
        */
      void finalize();

      /**
        perform task required by event
        */
      int doEvent(SDL_Event& event);

      /**
        main instance
        */

      static GameEngine* instance;

      /**
        id of timer, if more timers present
        */
      SDL_TimerID timer_id;        

     /**
       video screen
       */
      Surface surface;

      /**
        true if we want to save actial preferences
        */
      int savePreferences;

      /**
        saved state of initializations
        */
      int SDLInitOk;    
      int TTFInitOk;
      int GraphicsInitOk;
      int AudioInitOk;
      int ScreensInitOk;
      int TimerInitOk;
      int SongsLoadedOk;

      /**
        error console
        */
      ErrorsWidget errorConsole;

};

#endif
