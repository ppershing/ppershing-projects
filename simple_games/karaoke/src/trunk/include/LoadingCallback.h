// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef LOADING_CALLBACK
#define LOADING_CALLBACK

#include<string>

#include "Surface.h"
#include "Graphics.h"
#include "ScreenManager.h"

class LoadingCallback{
  public:
    /**
      Method for creating main instance of LoadingCallBack
      throws ESingletonInstance on double instantiating
      */
    static void createInstance();
    //throws (ESingletonInstance);

    /**
      Method for getting instance of Singleton,
      throws ESingletonInstance if there is no instance
      */
    static LoadingCallback* getInstance();
    //throws (ESingletonInstance);

    /**
      Method for destructing LoadingCallback instance
      throws ESingletonInstance if destroying null
      */
    static void destroyInstance();
    /**
      Sets the current loading item and calls
      the function draw screen of screen manager. LoadingScreen should
      be active so it display the text of current load.
      @author Miso
      @param loadingText Text of current loaded item to display.
      @see LoadingScreen
    */
    void itemLoaded(const std::string& loadingText,int line);
    /**
      shorthand
    */
    static void _itemLoaded(const std::string& loadingText,int line);
    /**
      Returns the text of current loaded item.
      @author Miso
    */
    std::string getItemLoaded(int line);
    /**
      shorthand
    */
    static std::string _getItemLoaded(int line);
    /**
      Sets the video screen.
      @author Miso
    */
    void setSurface(Surface& _surface);
    /**
      shorthand
    */
    static void _setSurface(Surface& _surface);
  private:
    /**
      Private constructor (class is Singleton)
    */
    LoadingCallback();
    /**
      Private destructor (class is Singleton)
    */
    ~LoadingCallback();
    /**
      instance
    */
    static LoadingCallback* instance;
    /**
      Text of the current loading item.
    */
    std::string text[2];
    /**
      video screen
    */
    Surface surface;
};
#endif

