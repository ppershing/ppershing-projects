// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_MEMORY_WIDGET
#define H_MEMORY_WIDGET

#include "Widget.h"
#include "MyStringUtils.h"
#include "Graphics.h"
#include "Metric.h"
#include "CircularBuffer.h"
#include "BarGraph.h"

/**
  Widget showing actual memory consumption and
  graph of last few seconds
  */
class MemoryWidget: private Widget {
    public:
        /**
          destructor
          */
        virtual ~MemoryWidget();

        MemoryWidget(const SDL_Rect& rect);
        
        void updateMemoryInfo();

        /**
          redirect to widget->draw
          */
        void draw(Surface& surface);

    private:
        int mem;
        int max;
        /**
          actual drawing routine
          */
        virtual void clippedDraw(Surface& surface);


        /**
          used font
          */
        Font font;

        /**
          metric for drawing
          */
        Metric metric;

        /**
          holder of history
          */
        CircularBuffer<int> history;

        /**
          we also graph history
          */
        BarGraph graph;
};

#endif

