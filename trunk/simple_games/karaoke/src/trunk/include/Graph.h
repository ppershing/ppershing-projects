// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_GRAPH
#define H_GRAPH

#include "Metric.h"
#include "Graphics.h"
#include "RangeStatistics.h"

// {{{ Graph
class Graph{
    public:
        /**
          constructor with metric
          */
        Graph(const Metric& m);

        /**
          default metric is used
          */
        Graph();

        /**
          set metric
          */
        void setMetric(const Metric& m);

        /**
          set maximum y-value. Note that if
          automatic maximum is selected,
          this value will be overwritten
          next call to draw()
          */
        void setMax(const double m);

        /**
          set minimum y-value. Note that if
          automatic minimum is selected,
          this value will be overwritten
          next call to draw()
          */
        void setMin(const double m);

        /**
          sets color and alpha
          */
        void setColor(const unsigned char R,const unsigned char G,
                const unsigned char B, const unsigned char A);

        /**
          set automatic y-value adjustment
          */
        void setAuto(const bool min,const bool max);


        /**
          draw data from range between <first,last),
          first, last must be an iterator to T and
          T must be cast-able to double.
          this function is not implemented 
          (it is only template for hierarchy)
          */
    template<typename T>
    void draw(Surface& surface,T first, T last);

    protected:

    /**
      update minimum and maximum y-value
      */
    template<typename T>
    void updateMinMax(T first,T last){
        if (auto_ymin) {
            ymin=(double)
                *(RangeStatistics::getRangeMin(first,last));
        }
        if (auto_ymax) {
            ymax=(double)
                *(RangeStatistics::getRangeMax(first,last));
        }

        if (ymax-ymin<1e-10) {
            if (auto_ymax) {
                ymax=ymin+1e-10;
            } else
            if (auto_ymin){
                ymin=ymax-1e-10;
            }
        }
    }


        /**
          actual metric
        */
        Metric metric;
        /**
          color data
          */
        int R,G,B,A;
        /**
          y minimum and maximum
          */
        double ymin,ymax;

        /**
          y min/max auto-adjustment
          */
        bool auto_ymin,auto_ymax;

};
// }}}

#endif
