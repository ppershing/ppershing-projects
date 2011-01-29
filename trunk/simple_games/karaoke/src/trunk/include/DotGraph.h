// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk
#ifndef H_DOT_GRAPH
#define H_DOT_GRAPH

#include "Graph.h"

/**
  class providing drawing 'dotted' graphs
  */
class DotGraph:public Graph{
    public:
        /**
          constructor
          */
        DotGraph();

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
        for (int i=0;i<size;i++){
            if (ymin> (*it) || ymax< (*it)) throw
                Exception(
                        "Draw graph - there is point below ymin"
                        "or above ymax");
        }

        try {
        for (int i=0;i<size;i++){
            Graphics::drawPixel(surface,
                    metric.x(100*(double)i/(double)size),
                    metric.y(100* (*it-ymin)/(ymax-ymin)),R,G,B,A);
            it++;
        }
        } catch (Exception& e){
            throw Exception("Failed to draw graph",e);

        }
    }
};

#endif
