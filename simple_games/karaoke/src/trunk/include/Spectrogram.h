// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_SPECTROGRAM
#define H_SPECTROGRAM

#include "Graphics.h"
#include "AudioSamples.h"
#include "AudioCircularBuffer.h"
#include "Surface.h"
#include "Widget.h"
#include "BarGraph.h"
#include <vector>
#include "SpectrumAnalyzer.h"
#include "YGradientColorFunction.h"

/**
  Widget - shows standard oscilloscope
  */

class Spectrogram: public Widget{
    public:
        /**
          constructor, default spectrumAnalyzer is null
          colors 255/255/255/255
          */
        Spectrogram();

        /**
          destructor
          */
        virtual ~Spectrogram();



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
          set color of oscilloscope
          */
        void setColor(const int R,const int G,const int B,const int
                A);

        void setAnalyzer(SpectrumAnalyzer* newAnalyzer);

    private:
        /**
          drawing to clipped surface - redirected from draw()
          */
        void clippedDraw(Surface& surface);


        SpectrumAnalyzer* analyzerPtr;
        /**
          use dot graph, or maybe line graph
          */
        BarGraph graph;
        YGradientColorFunction coloring;
};

#endif
