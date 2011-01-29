// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "BarGraph.h"
#include "MyAssert.h"

// {{{ BarGraph
BarGraph::BarGraph():Graph(){
    R=G=B=A=255;
    ymin=0;
    ymax=1;
    auto_ymin=auto_ymax=1;
    barDrawWidth=1;
}
// }}}

// {{{ setBarSize
void BarGraph::setBarSize(const double size){
    if (size<0 || size>1) throw
        EIllegalArgument(
                "BarGraph::setBarSize illegal value");
    barDrawWidth=size;
}
// }}}
