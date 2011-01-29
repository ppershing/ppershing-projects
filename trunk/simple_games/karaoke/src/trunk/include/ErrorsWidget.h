// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_ERRORS_WIDGET
#define H_ERRORS_WIDGET

#include "Widget.h"
#include "Graphics.h"
#include "Metric.h"
#include "Errors.h"
#include "Preferences.h"

/**
  Widget showing last n errors
  */
class ErrorsWidget: public Widget {
    public:
        /**
          constructor, default font is null
          */
        ErrorsWidget();

        /**
          set font to Widget
          */
        void setFont(Font& f);

        /**
          destructor
          */
        virtual ~ErrorsWidget();
    
        
        /**
          sets color to font,
          default is 255,255,255,255
          */
        void setFontColor(const unsigned char R, const unsigned char G,
            const unsigned char B,const unsigned char A);

    private:
        
        /**
          draw after clipping
          */
        virtual void clippedDraw(Surface& surface);

        Font font;
        unsigned char fontR,fontG,fontB,fontA;
};

#endif
