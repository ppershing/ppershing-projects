// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "MemoryWidget.h"
#include "MemoryInfo.h"
#include "MyStringUtils.h"
#include "Metric.h"
#include "NoDebug.h"
#include "Errors.h"
#include "stdlib.h"

// {{{ clippedDraw
void MemoryWidget::clippedDraw(Surface& surface){
    DEBUG("widget draw");

    try {
    graph.draw(surface,history.rbegin(),history.rend());

    Graphics::drawText(surface,font,"Mem: "+
            MyStringUtils::intToStringIT(mem),
            metric.x(0),metric.y(100),
            255,255,255,255);

    Graphics::drawText(surface,font,"Max: "+
            MyStringUtils::intToStringIT(max),
            metric.x(50),metric.y(100),
            255,255,255,255);
    } catch (Exception &e){
        Errors::_addError(
                std::string("MemoryWidget: draw failed:\n")
                +e.what(),Errors::ERROR);
    }
}
// }}}

// {{{ draw
void MemoryWidget::draw(Surface& surface){
    Widget::draw(surface);
}
// }}}

// {{{ updateMemoryInfo
void MemoryWidget::updateMemoryInfo(){
    mem=MemoryInfo::getOccupiedMemory();
    max=MemoryInfo::getMaxMemory();

    history.insert(mem);
}
// }}}

// {{{ MemoryWidget(rect)
MemoryWidget::MemoryWidget(const SDL_Rect&
        rect):Widget(rect),metric(rect),
    history(CircularBuffer<int>(400))
{
    mem=0;
    max=0;

    std::string fontName="fonts/DeJaVuSans.ttf";
    int fontSize=Font::getMaxFontSize(fontName,"Mem: XX.XXMB",
            rect.w/2,rect.h);
    font=Font(fontName,fontSize);

    Metric
        graphMetric=Metric(metric.x1(),metric.y1()+
                (int) (fontSize*1.2)
                ,metric.x2(),metric.y2());

    graph=BarGraph();
    graph.setMetric(graphMetric);
    graph.setColor(50,255,100,255);
    graph.setAuto(0,1);
    graph.setMin(0);
    DEBUG("widget init ok");

}
// }}}

// {{{ ~MemoryWidget
MemoryWidget::~MemoryWidget(){
}
// }}}
