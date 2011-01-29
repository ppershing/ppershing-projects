// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

// {{{ include
#include "GameEngine.h"
#include "Errors.h"
#include "Graphics.h"
#include <stdlib.h>
#include <SDL/SDL.h>
#include "SDL/SDL_ttf.h"
#include "ScreenManager.h"
#include "LoadingCallback.h"
#include "LoadingScreen.h"
#include "IntroScreen.h"
#include "MainMenuScreen.h"
#include "SongSelectionScreen.h"
#include "SingScreen.h"
#include "SongResultScreen.h"
#include "GameDataPointers.h"

#include "PPScreen.h"

#include "Exceptions.h"
#include "NoDebug.h"
#include "Preferences.h"
#include "Audio.h"
#include "MyAssert.h"
// }}}

#define TIMER_EVENT 1
#define TIMER_TIME 50
#define MAX_TIMER_EVENTS_IN_QUEUE 3

#define INI_FILE "Karaoke.ini"

#include "Tests.h"


GameEngine::GameEngine* GameEngine::instance=NULL;

volatile int timerEventsInQueue=0;


// {{{ timerCallback
Uint32 timerCallback(Uint32 interval,void* param){
    (void) interval;
    (void) param;

    if (timerEventsInQueue>=MAX_TIMER_EVENTS_IN_QUEUE) {
      Errors::_addError("Too many timer events. Try decreasing timer speed.",
           Errors::NOTICE);
      return TIMER_TIME;
    }
    timerEventsInQueue++;
    SDL_Event event;
    event.type=SDL_USEREVENT;
    event.user.code=TIMER_EVENT;
    SDL_PushEvent(&event);
    return TIMER_TIME;
}
// }}}

// {{{ GameEngine
GameEngine::GameEngine():savePreferences(false),SDLInitOk(0),TTFInitOk(0),GraphicsInitOk(0),
    AudioInitOk(0),ScreensInitOk(0),TimerInitOk(0){
    ScreenManager::createInstance();
    Preferences::createInstance();
    Audio::createInstance();
}
// }}}

// {{{ ~GameEngine
GameEngine::~GameEngine(){
    finalize();
    Audio::destroyInstance();
    ScreenManager::destroyInstance();
    Preferences::destroyInstance();
    GameDataPointers::destroyInstance();
}
// }}}

// {{{ createInstance
void GameEngine::createInstance(){ //throw (ESingletonInstance) 
    if (instance!=NULL) throw ESingletonInstance(
            "GameEngine: called create instance twice");
    instance=new GameEngine();
}
// }}}

// {{{ getInstance
GameEngine::GameEngine* GameEngine::getInstance() //throw (ESingletonInstance) 
{
    if (instance==NULL) {
        throw ESingletonInstance(
            std::string("GameEngine: getInstance with null instance"));
    }
    return instance;
}
// }}}

// {{{ destroyInstance
void GameEngine::destroyInstance(){ //throw (ESingletonInstance)
    if (instance==NULL) throw ESingletonInstance(
            "GameEngine: destroy null instance");
    delete instance;
    instance=NULL;
}
// }}}

// {{{ initSDL
int GameEngine::initSDL(){
    if (SDL_Init(SDL_INIT_VIDEO|SDL_INIT_TIMER)<0){
        Errors::_addError("Can't init SDL",Errors::ERROR);
        return 0;
    }
    Errors::_addError("SDL init done",Errors::NOTICE);
    return 1;
}
// }}}

// {{{ initGraphics
int GameEngine::initGraphics(){
    int width,height,bitDepth,fullScreen;
    int flags;
    width=Preferences::     _getInt("/video/resolution/width");
    height=Preferences::    _getInt("/video/resolution/height");
    bitDepth=Preferences::  _getInt("/video/resolution/bitdepth");
    fullScreen=Preferences::_getInt("/video/resolution/fullscreen");

    flags=SDL_HWSURFACE|SDL_DOUBLEBUF;
    if (fullScreen) flags|=SDL_FULLSCREEN;

    try {
        surface=Surface(SDL_SetVideoMode(width,height,bitDepth,flags),false);
    }catch(Exception &e){
        Errors::_addError(
                std::string("Can't init video mode") +
                e.what(),Errors::ERROR);
        return 0;
    }
	
    Errors::_addError("Video mode OK",Errors::NOTICE);

    return 1;
}
// }}}

// {{{ initTTF
int GameEngine::initTTF(){
    if (TTF_Init() == -1) {
        Errors::_addError("Unable to initialize SDL_ttf",Errors::ERROR);
        return 0;
    }
    Errors::_addError("Font library OK",Errors::NOTICE);
    return 1;
}
// }}}

// {{{ initScreens
int GameEngine::initScreens(){
    ScreenManager::_init();
    DEBUG("creating screen PP");
    PPScreen::PPScreen* PP=new PPScreen();
    ScreenManager::_registerScreen(PP);
    #ifdef DEBUG_PP
    ScreenManager::_activateScreen("PP");
    #else
    DEBUG("created");
    LoadingScreen::LoadingScreen* loadingScreen = new LoadingScreen();
    IntroScreen::IntroScreen* introScreen=new IntroScreen();
    MainMenuScreen::MainMenuScreen* mainMenuScreen=new MainMenuScreen();
    DEBUG("loading intro and mainMenu created, going to sing...");
    SingScreen::SingScreen* singScreen=new SingScreen();
    DEBUG("singScreen created");
    SongSelectionScreen::SongSelectionScreen* songSelectionScreen=new SongSelectionScreen();
    DEBUG("songSelection created");
    SongResultScreen::SongResultScreen* songResultScreen=new SongResultScreen();
    DEBUG("songResult created");

    ScreenManager::_registerScreen(loadingScreen);
    ScreenManager::_registerScreen(introScreen);
    ScreenManager::_registerScreen(mainMenuScreen);
    ScreenManager::_registerScreen(singScreen);
    ScreenManager::_registerScreen(songSelectionScreen);
    ScreenManager::_registerScreen(songResultScreen);
    ScreenManager::_activateScreen("Loading");
    #endif


    Errors::_addError("Screens created and activated",Errors::NOTICE);
    return 1;
}
// }}}

// {{{ initErrorConsole
int GameEngine::initErrorConsole(){
    errorConsole.deactivate();
    if (Preferences::_getDefaultInt(
                "/widget/errorConsole/enabled",0)==0)
            return 1;

    SDL_Rect r;
    r.x=0; r.y=0;
    r.w=surface.getWidth();
    r.h=surface.getHeight();
    errorConsole.setRect(r);
    errorConsole.setFrameColor(0,0,0,0); // no frame

    Errors::_setErrorCacheSize(
            Preferences::_getDefaultInt(
                "/widget/errorConsole/errorCount",10));
    Font font;            
    try {
    font=Font(Preferences::_getDefaultString(
            "/widget/errorConsole/fontName",""),
            Preferences::_getDefaultInt(
            "/widget/errorConsole/fontSize",10));
    } catch (Exception& e){
        Errors::_addError(
                std::string("ErrorWidget: font load failed:")+
                e.what(), Errors::ERROR);
        Errors::_addError("ErrorWidget: function disabled",
                Errors::NOTICE);
        return 1;
    }

    errorConsole.setFont(font);
    errorConsole.setFontColor(
            Preferences::_getDefaultInt(
                "/widget/errorConsole/fontColorR",255),
            Preferences::_getDefaultInt(
                "/widget/errorConsole/fontColorG",255),
            Preferences::_getDefaultInt(
                "/widget/errorConsole/fontColorB",255),
            Preferences::_getDefaultInt(
                "/widget/errorConsole/fontColorA",255));

    errorConsole.activate();
    return 1;
}

// }}}

// {{{ loadSongs
int GameEngine::loadSongs(){
  GameDataPointers::_setCurrentSongId(0);
  int returnValue = GameDataPointers::_getSongList()->loadFromDir("songs");
  ScreenManager::_activateScreen("MainMenu");
  return returnValue;
}
// }}}

//  {{{ initTimer
int GameEngine::initTimer(){
    timer_id=SDL_AddTimer((Uint32)TIMER_TIME,timerCallback,NULL);
    Errors::_addError("Timer set up",Errors::NOTICE);
    return 1;
}
// }}}

// {{{ initAudio
int GameEngine::initAudio(){
    std::string audioError;
    int sampleRate=Preferences::_getInt("/audio/samplerate");
    double
        micCorrection=Preferences::_getDouble("/audio/micTimeCorrection");
    if (Audio::_init(sampleRate,2,2,micCorrection,audioError)==0) { 
        // audio must be called after activating screen
        Errors::_addError(audioError,Errors::ERROR);
        return 0;
    }
    Errors::_addError("Audio OK",Errors::NOTICE);
    return 1;
}
// }}}

// {{{  init
int GameEngine::init(){    
    Errors::_addError("Game engine init",Errors::NOTICE);
    Preferences::_loadFromFile(INI_FILE);
    GameDataPointers::createInstance();
    LoadingCallback::createInstance();

    if (!(SDLInitOk=initSDL()))     return 0;
    if (!(GraphicsInitOk=initGraphics())) return 0;
    if (!(TTFInitOk=initTTF())) return 0;
    //#ifndef DEBUG_MISO
    if (!(AudioInitOk=initAudio())) return 0;
    //#endif
    if (!(TimerInitOk=initTimer())) return 0;
    GameDataPointers::_initPointers();
    LoadingCallback::_setSurface(surface);
    Errors::_addError("LoadingCallback init OK",Errors::NOTICE);
    if (!(ScreensInitOk=initScreens())) return 0;

    if (! initErrorConsole()) return 0;

    if (!(SongsLoadedOk=loadSongs())) return 0;

    Errors::_addError("init OK",Errors::NOTICE);
    return 1;
}
// }}}

// {{{ doEvent
int GameEngine::doEvent(SDL_Event& event){
    switch (event.type){
        case SDL_QUIT: 
                return 0;

        case SDL_USEREVENT:
            if (event.user.code==TIMER_EVENT) {
                timerEventsInQueue--;
                Assert(timerEventsInQueue>=0,
                        "negative number of timer events!");

                Graphics::filledRectangle(surface,NULL,0,0,0,255);
                ScreenManager::_drawScreen(surface);
                errorConsole.draw(surface);
                Graphics::flip(surface);

                ScreenManager::_updateAudioBuffers();
                return 1;
            }
        default:
            ScreenManager::_handleEvent(&event);
    }
    return 1;

}
// }}}

// {{{ run
void GameEngine::run(){
    Errors::_addError("Game engine run",Errors::NOTICE);
    Tests::instance.init();
    Tests::instance.do_tests();
    Tests::instance.do_benchmarks();

    int loop=1;
    while (loop){
        SDL_Event event;
        if (!SDL_WaitEvent(&event)) continue;

        loop=doEvent(event);
    }

    Tests::instance.finalize();
}
// }}}

// {{{ setSavePreferences
void GameEngine::setSavePreferences(int state){
    savePreferences=state;
}
// }}}

// {{{ finalize
void GameEngine::finalize(){
    Errors::_addError("Game engine finalize",Errors::NOTICE);
    Font tmp=Font();
    errorConsole.setFont(tmp);

    //#ifndef DEBUG_MISO
    if (AudioInitOk){
        std::string audioError;
        if (Audio::_finalize(audioError)==0) 
            Errors::_addError("Audio: " +audioError,Errors::ERROR);
        else Errors::_addError("Audio finalize OK",Errors::NOTICE);
    }
    //#endif

    if (ScreensInitOk) {
        ScreenManager::_finalize();
        Errors::_addError("ScrenManager&screens finalize OK",Errors::NOTICE);
    }

    if (savePreferences) Preferences::_saveToFile(INI_FILE);

    LoadingCallback::destroyInstance();
    Errors::_addError("LadingCallback destroyed",Errors::NOTICE);
    GameDataPointers::_finalize();
    Errors::_addError("GameDataPointers closed",Errors::NOTICE);

    if (TimerInitOk){
        SDL_RemoveTimer(timer_id);
        Errors::_addError("Remove timer OK",Errors::NOTICE);
    }

    if (TTFInitOk){
        TTF_Quit();
        Errors::_addError("TTF library closed",Errors::NOTICE);
    }

    if (SDLInitOk){
        SDL_Quit();
        Errors::_addError("SDL closed",Errors::NOTICE);
    }

}
// }}}
