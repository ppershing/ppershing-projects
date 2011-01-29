// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_TIME_BAR_WIDGET
#define H_TIME_BAR_WIDGET

#include "Widget.h"
#include "Graphics.h"
#include "Metric.h"
#include "Errors.h"
#include "Preferences.h"
#include "Song.h"

/**
  Widget showing time progress bar of playing song.
  @author Miso
  */
class TimeBarWidget: public Widget {
    public:
        /**
          constructor, default font is null
          */
        TimeBarWidget();

        /**
          set font to Widget
          */
        void setFont(Font& f);

        /**
          destructor
          */
        virtual ~TimeBarWidget();
    
        /**
          sets color to font,
          default is 255,255,255,255
          */
        void setFontColor(const unsigned char R, const unsigned char G,
            const unsigned char B,const unsigned char A);
        /**
          Sets current time in song.
          @author Miso
          @param time Time to set.
        */
        void setTime(double time);
        /**
          Sets time when the song ends.
          @author Miso
          @param time Time to set.
          @see Song
        */
        void setEndTime(double time);
        /**
          Inits the metric of the rectangle. Called usually after setRect
          @author Miso
          @see Widget
          @see Metric
        */
        void setMetric();
        void setSong(Song* _song);
        /**
          Returns the x-coordinate at surface of the corresponding point to given time.
          @author Miso
          @param time Time for which we want to find x-coordinate.
          @return x-coordinate of that point.
          @see Metric
        */
        int timeToX(double time);
    private:
        
        /**
          draw after clipping
          */
        virtual void clippedDraw(Surface& surface);
        double endTime,currentTime;
        Metric metric;
        Font font;
        Song* song;
        unsigned char fontR,fontG,fontB,fontA;
};

#endif
