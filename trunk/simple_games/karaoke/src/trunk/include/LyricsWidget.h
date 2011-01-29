// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_LYRICS_WIDGET
#define H_LYRICS_WIDGET

#include "Widget.h"
#include "Graphics.h"
#include "Metric.h"
#include "Errors.h"
#include "Font.h"
#include "Song.h"
#include "Preferences.h"

#include<string>

/**
  Widget showing syllables of next 2 sentences.
  */
class LyricsWidget: public Widget {
  public:
    /**
      constructor, default font is null
      */
    LyricsWidget();

    /**
      Set font to Widget. Calculates fontsize from widget dimensions. Call after setting the song!
      @author Miso
      @param fontName Name of the font to use.
      @see Font
      */
    void setFont(const std::string fontName);

    /**
      destructor
      */
    virtual ~LyricsWidget();

    /**
      Sets colors to font, default is 255,255,255,255...
      @author Miso
      @param fR,fG,fB,fA Color of the first line.
      @param sR,sG,sB,sA Color of the second line.
      @param mR,mG,mB,mA Color of the marked syllable.
      @see Font
      */
    void setFontColor(const unsigned char fR, const unsigned char fG, const unsigned char fB,const unsigned char fA,
        const unsigned char sR, const unsigned char sG, const unsigned char sB,const unsigned char sA,
        const unsigned char mR, const unsigned char mG, const unsigned char mB,const unsigned char mA);
    
    /**
      Inits the metric of the rectangle. Called usually after setRect
      @author Miso
      @see Widget
      @see Metric
      */
    void setMetric();

    /**
      Sets current time in song.
      @author Miso
      @param time Time to set.
    */
    void setTime(double time);

    /**
      Sets song to widget.
      @author Miso
      @param _song Pointer to song to sing.
      @see Song
    */
    void setSong(Song* _song);
    /**
      Sets space between lines. Default is 5.
      @author Miso
      @param spacing Line spacing to set.
    */
    void setLineSpacing(int spacing);
    /**
      Sets index of the sentence that is currently singed.
      @author Miso
      @param index Index of that sentence.
    */
    void setSentenceIndex(int index);
  private:

    /**
      draw after clipping
      */
    virtual void clippedDraw(Surface& surface);
    double currentTime;
    Font font;
    Metric metric;
    Song* song;
    int currentSentenceIndex;
    int lineSpacing;
    /**
      Color of the first line.
      */
    unsigned char firstR,firstG,firstB,firstA;
    /**
      Color of the second line.
      */
    unsigned char secondR,secondG,secondB,secondA;
    /**
      Color of the marked syllable. (currently singing)
      */
    unsigned char markedR,markedG,markedB,markedA;
};

#endif
