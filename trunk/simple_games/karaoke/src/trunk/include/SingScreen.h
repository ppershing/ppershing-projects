// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_SING_SCREEN
#define H_SING_SCREEN

#include "Screen.h"
#include "Surface.h"
#include "Font.h"
#include "GameDataPointers.h"
#include "Song.h"
#include "MP3Decoder.h"
#include "Timer.h"
#include "NoteAnalyzer.h"
#include "StaveWidget.h"
#include "TimeBarWidget.h"
#include "LyricsWidget.h"
#include "Spectrogram.h"

/**
  Screen where the Song is playing and player singing. There exists only one instance during the runtime.
  @author Miso
  @see Song
  @see ScreenManager
  @see Screen
  */	

class SingScreen:public Screen{
  public:
    /**
      Constructor
      @author Miso
      @see Screen
      @see ScreenManager
      */
    SingScreen();
    /**
      Destructor
      @author Miso
      @see Screen
      @see ScreenManager
      */
    ~SingScreen();
    /**
      Returns the name of this screen 'SingScreen'
      @author Miso
      @return Name of this screen 'SingScreen'
      @see Screen
      @see ScreenManager
      */
    std::string getScreenName() const;
    /**
      Handles events such as keypressed, SDL_quit. On keypressed, it only returns to main menu.
      @author Miso
      @return void
      @param event Pointer to structure SDL_Event
      @see Screen
      @see ScreenManager
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
      Inits the stave widget, that shows notes to sing and singed notes and time bar widget.
      @author Miso
      @return 1 if success, -1 if failed.
      @see StaveWidget
      @see TimeBarWidget
    */
    int initWidgets();
    void activateScreen();
    void deactivateScreen();
    void updateAudioBuffers();
  private:
    std::string fontname;
    Font font;
    /**
      Id of the current playing song in GameDataPointers
      @author Miso
      @see GameDataPointers
      */
    int songId;
    /**
      Current playing song.
      @author Miso
      @see Song
      */
    Song song;
    /**
      Pointer to MP3 decoder.
      @author Miso
      @see MP3Decoder
    */
    MP3Decoder* music;
    AudioSamples musicData;
    Timer timer;
    NoteAnalyzer analyzer;
    StaveWidget staveWidget;
    TimeBarWidget timeBarWidget;
    LyricsWidget lyricsWidget;
    Spectrogram spectrogram;
    
    /**
      Current sentence index in playing song.
      @author Miso
      @see Sentence
      @see Song
      */
    int currentSentenceIndex;
    /**
      Current note in plaing song.
      @see Note
      @see Song
      */
    //Note currentNote;
};

#endif
