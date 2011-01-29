// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_OSCILLOSCOPE
#define H_OSCILLOSCOPE

#include "Graphics.h"
#include "AudioSamples.h"
#include "AudioCircularBuffer.h"
#include "Surface.h"
#include "Widget.h"
#include "DotGraph.h"
#include <vector>


/**
  Widget - shows standard oscilloscope
  */

class Oscilloscope: public Widget{
    public:
        /**
          constructor,
          defaults - frequency 44100, channleCount 1,
          data size 2, colors 255/255/255/255
          */
        Oscilloscope();

        /**
          destructor
          */
        virtual ~Oscilloscope();

        /**
          inserts new data to oscilloscope,
          performs check against rate&channels match
          */
        void sendNewData(AudioSamples& newData);

        /**
          actual drawing routing
          */
        void draw(Surface& surface);

        /**
          sets new type of audio characteristic
          */
        void setAudioCharacteristics(const double rate, int channels);

        /**
          set surface destination rectangle
          */
        void setRect(const SDL_Rect& rect);

        /**
          set surface destination rectangle
          */
        void setRect(const int x1,const int y1, const int x2, 
                const int y2);

        /**
           set new size of data buffer
           */
        void setDataSize(const int newSize); 

        /**
          set color of oscilloscope
          */
        void setColor(const int R,const int G,const int B,const int
                A);

    private:
        /**
          drawing to clipped surface - redirected from draw()
          */
        void clippedDraw(Surface& surface);

        /**
          data buffer
          */
        AudioCircularBuffer data;

        /**
          use dot graph, or maybe line graph
          */
        DotGraph graph;
};

#endif
