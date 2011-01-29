// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk
#ifndef H_METRIC
#define H_METRIC
#include "Metric.h"
#include <SDL/SDL.h>
#include "Font.h"

/**
  Metric is class for providing unified
  access to drawing for various resolutions,
  you specify destination region and
  metric will translate points from percentage 0..100 to
  real points
  */
class Metric {
    public:
        /**
          default metric is (0,0,100,100)
          */
        Metric();
        /**
          construct metric from corner points
          */
        Metric(const int x1,const int y1,const int x2,const int y2);

        /**
          construct metric from rectangle
          */
        Metric(const SDL_Rect& rect);

        /**
          destructor
          */
        ~Metric();

        /**
          set new Rectangle, throws EIllegalArgument 
          it is not correct
          */
        void setRect(const SDL_Rect& rect);

        /**
          set new Rectangle, throws  EIllegalArgument
          if not corrent
          */
        void setRect(const int x1,const int y1,
                const int x2, const int y2);

        /**
          return x-position of pixel.
          @param x percentage
          */
        int x(const double x);

        /**
          return y-position of pixel.
          Note that this function is from bottom to up
          @param y percentage
          */
        int y(const double y);

        /**
          return x-position of pixel.
          @param x percentage
          */
        int xx(const double x);

        /**
          return y-position of pixel.
          Note that this function is from up to bottom
          @param y percentage
          */
        int yy(const double y);

        /**
          returns left point of drawing frame
          */
        int x1();

        /**
          returns top (numerically smallest) point
          of drawing frame
          */
        int y1();

        /**
          returns right point of drawing frame
          */
        int x2();

        /**
          returns bottom (numerically greatest) point
          of drawing frame
          */
        int y2();

        /**
          returns width of frame
          note that width!=x2-x1 (+1 correction)
          */
        int width();

        /**
          returns height of frame
          note that height!=y2-y1 (+1 correction)
          */
        int height();

        /**
          return x/y aspect ratio of frame
          */
        double aspectRatio();

            
    private:
        /**
          internal borders
          */
        int _x1,_y1,_x2,_y2;

        /**
          zooming used for fast computations
          */
        double zoomx,zoomy;
};

inline int Metric::x(const double x){
    return (int)(x*zoomx)+_x1;
}

inline int Metric::y(const double y){
    return (int)((100.0-y)*zoomy)+_y1;
}

inline int Metric::xx(const double x){
    return (int)(x*zoomx)+_x1;
}

inline int Metric::yy(const double y){
    return (int)(y*zoomy)+_y1;
}

inline int Metric::x1(){
    return _x1;
}

inline int Metric::y1(){
    return _y1;
}

inline int Metric::x2(){
    return _x2;
}

inline int Metric::y2(){
    return _y2;
}

inline int Metric::width(){
    return _x2-_x1+1;
}

inline int Metric::height(){
    return _y2-_y1+1;
}

inline double Metric::aspectRatio(){
	return zoomx/zoomy;
}

#endif
