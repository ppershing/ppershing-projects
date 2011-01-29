// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#include "TimeBarWidget.h"
#include "Debug.h"
#include "Exceptions.h"
#include "MyStringUtils.h"
#include "GameDataPointers.h"

// {{{ TimeBarWidget
TimeBarWidget::TimeBarWidget(){
    fontR=fontG=fontB=fontA=255;
}
// }}}

// {{{ ~TimeBarWidget
TimeBarWidget::~TimeBarWidget(){
}
// }}}

// {{{ setFontColor
void TimeBarWidget::setFontColor(const unsigned char R, const unsigned
    char G, const unsigned char B,const unsigned char A){
  fontR=R;
  fontG=G;
  fontB=B;
  fontA=A;
}
// }}}

// {{{ setFont
void TimeBarWidget::setFont(Font& f){
  font=f;
}
// }}}

// {{{ clippedDraw
void TimeBarWidget::clippedDraw(Surface& surface){
  try {

    //DEBUG(MyStringUtils::intToString(timeToX(currentTime)));
    try{
      Graphics::rectangle(surface,metric.x(0.0),metric.y(80.0),timeToX(currentTime),metric.y(20.0),255,255,0,255);
    }catch(Exception e){
      throw EIllegalArgument("time rectangle failed at time "+MyStringUtils::doubleToString(currentTime)+", endTime="+MyStringUtils::doubleToString(endTime)+"\n"+std::string(e.what()));
    }
    /*for(unsigned int i=0;i<song->sentence.size();i++){
      try{
        Graphics::drawPixel(surface,timeToX(song->sentence.at(i).start),metric.y(50.0),255,255,255,255);
      }catch(Exception e){
        throw EIllegalArgument(MyStringUtils::intToString(i)+"-th sentence border point drawing failed:\n"+std::string(e.what()));
      }
    }*/
  } catch (Exception& e){
    throw EIllegalArgument(std::string("There was an error during TimeBarWidget::draw():\n")+e.what());
  }
}
// }}}

// {{{ setTime
void TimeBarWidget::setTime(double time){
  if(time<0 || time>endTime)
    Errors::_addError(MyStringUtils::doubleToString(time)+" is not valid. endTime="+MyStringUtils::doubleToString(endTime),Errors::WARNING);
  currentTime = time;
}
// }}}

// {{{ setEndTime
void TimeBarWidget::setEndTime(double time){
  endTime = time;
}
// }}}

// {{{ setMetric
void TimeBarWidget::setMetric(){
  metric = Metric(rectangle);
}
// }}}

// {{{ setSong
void TimeBarWidget::setSong(Song* _song){
  song = _song;
  DEBUG("set endTime:"+MyStringUtils::doubleToString(song->endTime));
  setEndTime(song->endTime);
}
// }}}

// {{{ timeToX
int TimeBarWidget::timeToX(double time){
  return metric.x(100.0*time/endTime);
}
// }}}

