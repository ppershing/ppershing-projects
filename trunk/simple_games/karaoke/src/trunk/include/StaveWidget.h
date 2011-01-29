// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_STAVE_WIDGET
#define H_STAVE_WIDGET

#include "Widget.h"
#include "Graphics.h"
#include "Metric.h"
#include "Errors.h"
#include "Preferences.h"
#include "Sentence.h"

/**
  Widget showing notes to sing and singed notes.
  @author Miso
  */
class StaveWidget: public Widget {
    public:
        /**
          constructor, default font is null
          */
        StaveWidget();

        /**
          set font to Widget
          */
        void setFont(Font& f);

        /**
          destructor
          */
        virtual ~StaveWidget();
        /**
          activate widget
        */
        void activate();
    
        /**
          sets color to font,
          default is 255,255,255,255
          */
        void setFontColor(const unsigned char R, const unsigned char G,
            const unsigned char B,const unsigned char A);
        /**
          Sets current sentence, where are notes to sing.
          @author Miso
          @param sentence Sentence to set.
          @see Sentence
        */
        void setSentence(Sentence newSentence);
        /**
          Sets current time in song.
          @author Miso
          @param time Time to set.
        */
        void setTime(double time);
        /**
          Inits the metric of the rectangle. Called usually after setRect
          @author Miso
          @see Widget
          @see Metric
        */
        void setMetric();
        /**
          Sets width of notes. Difference of y-coordinates of top and bottom edges.
          @author Miso
          @param width Width of the note to set.
        */
        void setNoteWidth(int width);
        /**
          Sets padding of the pitch in the widget. So bottom of the widget is minimum pitch of the sentence minus padding.
          @author Miso
          @param padding Padding to set.
        */
        void setPitchPadding(double padding);
        /**
          Returns the x-coordinate at surface of the corresponding point to given time.
          @author Miso
          @param time Time for which we want to find x-coordinate.
          @return x-coordinate of that point.
          @see Metric
        */
        int timeToX(double time);
        /**
          Returns the y-coordinate at surface of the corresponding point to given pitch.
          @author Miso
          @param pitch Pitch for which we want to find y-coordinate.
          @return y-coordinate of that point.
          @see Metric
        */
        int pitchToY(double pitch);
    private:
        
        /**
          draw after clipping
          */
        virtual void clippedDraw(Surface& surface);
        /**
          Draws note with given parameters on the surface.
          @author Miso
          @see Note
        */
        void drawNote(Surface& surface,double start,double duration,double pitch,
            const int R, const int G, const int B, const int A);
        void drawNote(Surface& surface,Note& note,
            const int R, const int G, const int B, const int A);
        Sentence sentence;
        double leftTime,timeWidth,bottomPitch,pitchRange,pitchPadding,currentTime,timeUnit;
        int noteWidth;
        Metric metric;
        Font font;
        unsigned char fontR,fontG,fontB,fontA;
};

#endif
