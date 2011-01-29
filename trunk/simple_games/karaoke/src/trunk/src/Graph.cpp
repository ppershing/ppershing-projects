// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "Graph.h"
//FIXME: check input
//TODO: rethrow drawing errors

// {{{ Graph
Graph::Graph(){
}
// }}}

// {{{ Graph( Metric)
Graph::Graph(const Metric& m):metric(m){
}
// }}}

// {{{ setMetric
void Graph::setMetric(const Metric& m){
    metric=m;
}
// }}}

// {{{ setColor 
void Graph::setColor(const unsigned char R,const unsigned char G,
        const unsigned char  B, const unsigned char A){
    this->R=R;
    this->G=G;
    this->B=B;
    this->A=A;
}
// }}}

// {{{ setAuto 
void Graph::setAuto(const bool min,const bool max){
    auto_ymin=min;
    auto_ymax=max;
}
// }}}

// {{{ setMin 
void Graph::setMin(const double m){
    ymin=m;
}
// }}}

// {{{ setMax 
void Graph::setMax(const double m){
    ymax=m;
}
// }}}
