// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk
#ifndef H_BAR_GRAPH
#define H_BAR_GRAPH

#include "MyAssert.h"
#include "Errors.h"
#include "Graph.h"
#include "ColorFunction.h"
/**
  class providing drawing 'dotted' graphs
  */
class BarGraph:public Graph{
    public:
        /**
          constructor
          */
        BarGraph();

        /**
          draw data from range between <first,last),
          first, last must be an iterator to T and
          T must be cast-able to double
          @see Graph::draw
          */

    template<typename T>
    void draw(Surface& surface,T first, T last){
        updateMinMax(first,last);
        if (ymax<=ymin) throw EIllegalArgument(
                "DotGraph: ymax<=ymin, can't plot points");
        int size=last-first;
        T it=first;

        Assert(barDrawWidth<=1,"Whoops, wana to draw biggers bar then"
                " they are");

        double left=(1-barDrawWidth)/2;
        double right=1-left;
        double s=size+1;

        try {
        for (int i=0;i<size;i++){            
            Graphics::rectangle(surface,
                    metric.x(100*((i+left)/s)),
                    metric.y(100* (*it-ymin)/(ymax-ymin)),
                    metric.x(100*((i+right)/s)),
                    metric.y(0),R,G,B,A);
            it++;
        }
        } catch (Exception& e){
            throw Exception("Failed to draw graph",e);
        }
    }

    /**
      template for colorFunction
      */
    template<typename T>
    void draw(Surface& surface,T first, T last,ColorFunction* coloring){
        updateMinMax(first,last);
        if (ymax<=ymin) throw EIllegalArgument(
                "DotGraph: ymax<=ymin, can't plot points");
        int size=last-first;
        T it=first;

        Assert(barDrawWidth<=1,"Whoops, wana to draw biggers bar then"
                " they are");

        double left=(1-barDrawWidth)/2;
        double right=1-left;
        double s=size+1;

        try {
        for (int i=0;i<size;i++){  
            double x=i/s;
            double y=(*it-ymin)/(ymax-ymin);
            Graphics::rectangle(surface,
                    metric.x(100*((i+left)/s)),
                    metric.y(100*y),
                    metric.x(100*((i+right)/s)),
                    metric.y(0),
                    coloring->R(x,y),
                    coloring->G(x,y),
                    coloring->B(x,y),
                    coloring->A(x,y));
            it++;
        }
        } catch (Exception& e){
            throw Exception("Failed to draw graph",e);
        }
    }

    /**
      set width of individual bar
      0 means nothing, 1 means full width (agains number of samples)
      0.625 means bars and spaces between them are eqaul-length
      */
    void setBarSize(const double size);

    private:
        double barDrawWidth;
};

#endif
