// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_SCREEN_MANAGER
#define H_SCREEN_MANAGER

#include "Screen.h"
#include <string>
#include <vector>
#include <SDL/SDL.h>
#include "Exceptions.h"
#include "Graphics.h"
#include <SDL/SDL_ttf.h>
/**
  Singleton class for managing various screens.
  It is created as strategy pattern class.
  Various independent tasks are added as screens.
  Screens may switch between by calling setActiveScreen();
  */
class ScreenManager{
    public:

        /**
          Method for creating main instance of screenManager
          throws ESingletonInstance on double instantiating
          */
        static void createInstance();
        //throws (ESingletonInstance);

        /**
          Method for getting instance of Singleton,
          throws ESingletonInstance if there is no instance
          */
        static ScreenManager* getInstance();
        //throws (ESingletonInstance);

        /**
          Method for destructing ScreenManager instance
          throws ESingletonInstance if destroying null
          */
        static void destroyInstance();
        //throws (ESingletonInstance);


        /**
          Registers new screen, throws EIllegalException on
          addiyng screen with same name
          */
        void registerScreen(Screen* const screen);

        /**
          shorthand (do not need  explicitly write getInstance())
          */
        static void _registerScreen(Screen* const screen);
            // throws (ESingletonInstance);

        /**
          Unregister screen
          */
        void unregisterScreen(Screen* const screen);

        /**
          shorthand
          */
        static void _unregisterScreen(Screen* const screen);
            // throws (ESingletonInstance);

        /**
          Method for changing active screen
          throws EIllegalException on nonexists screen
          @param screenName name of screen to be activated
          */
        void activateScreen(const std::string& screenName);

        /**
          shorthand
          */
        static void _activateScreen(const std::string& screenName);
            //throws (ESingletonInstance);

        /**
          Method for querying for active screen
          */
        std::string getActiveScreenName() const;
        
        /**
          shorthand
          */
        static std::string _getActiveScreenName();
        //throws(ESingletonInstance);

        /**
          Method for querying active screen
          */
        const Screen::Screen* getActiveScreen() const;
        
        /**
          shorthand
          */
        static const Screen::Screen* _getActiveScreen();
        //throws(ESingletonInstance);

        /**
          Method for getting obtaining specific screen from name
          */
        Screen::Screen* getScreenFromName(const std::string&
                screenName) ;
        
        /**
          shorthand
          */
        static Screen::Screen* 
             _getScreenFromName(const std::string& screenName);
             //throws (ESingletonInstance);


        /**
          Method for forwarding event to active screen
          */
        void handleEvent(const SDL_Event* const event);
        
        /**
          shorthand
          */
        static void _handleEvent(const SDL_Event* const event);
        //throws(ESingletonInstance);

        /**
          Method for forwarding drawing to active screen
          */
        void drawScreen(Surface& surface);

        /**
          shorthand
          */
        static void _drawScreen(Surface& surface);
        //throws (ESingletonInstance);

        /**
          Method called when there is need of refilling ringbuffers
          */
        void updateAudioBuffers();

        /**
          shorthand
          */
        static void _updateAudioBuffers();
        //throws(ESingletonInstance);

        /**
          initialize screenManager
          */
        void init();

        /**
          shorthand
          */
        static void _init();
        //throws (ESingletonInstance);

        /**
          finalize ScreenManager
          */
        void finalize();

        /**
          shorthand
          */
        static void _finalize();
        //throws (ESingletonInstance);

    private:
        /**
          Private constructor (class is Singleton)
          */
        ScreenManager();

        /**
          Private destructor (class is Singleton)
          Deactivates actual screen.
          Note that if there are some registered screens,
          they are automatically destroyed
          */
        ~ScreenManager();

        /**
          instance 
          */
        static ScreenManager* instance;

        /**
          list of screens actually registered
          */
        std::vector<Screen::Screen*> screenList;

        /**
          pointer to active screen
          */
        Screen::Screen* activeScreen;

};

#endif
