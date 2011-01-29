// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "Metric.h"
#include "SDL/SDL_ttf.h"
#include "Exceptions.h"

// {{{ Metric
Metric::Metric():_x1(0),_y1(0),_x2(100),_y2(100),zoomx(1.0),zoomy(1.0){
}
// }}}

// {{{ Metric(x1,y1,x2,y2)
Metric::Metric(const int x1,const int y1,const int x2,const int y2):
    _x1(x1),_y1(y1),_x2(x2),_y2(y2),zoomx((x2-x1)/100.0),zoomy((y2-y1)/100.0){
        if (y1>=y2 || x1>=x2) throw
            EIllegalArgument("Metric: not valid rectangle");
}
// }}}

// {{{ Metric( SDL_Rect)
Metric::Metric(const SDL_Rect& rect):_x1(rect.x),_y1(rect.y),
    _x2(rect.x+rect.w-1),_y2(rect.y+rect.h-1),
    zoomx((_x2-_x1)/100.0),zoomy((_y2-_y1)/100.0){
        if (rect.w<=0 || rect.h<=0) throw
            EIllegalArgument("Metric: not valid rectangle");
}
// }}}

// {{{ ~Metric 
Metric::~Metric(){
}
// }}}
