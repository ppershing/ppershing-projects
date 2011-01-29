#include "Timer.h"
#include <SDL/SDL.h>

double Timer::fpsTime=0.5;

// {{{ start
void Timer::start(){
    startTime=getSystemTime();
    lastTime=startTime;
    fpsStartTime=lastTime;
    lastFPS=0;
    ticks=0;
    fpsTicks=0;
}
// }}}

// {{{ tick
double Timer::tick(){
    double oldTime=lastTime;
    lastTime=getSystemTime();
    ticks++;
    fpsTicks++;
    if (lastTime-fpsStartTime>fpsTime) {
         lastFPS=fpsTicks/(lastTime-fpsStartTime);
         fpsStartTime=lastTime;
         fpsTicks=0;
    }

    return lastTime-oldTime;
}
// }}}


// {{{ getTimeFromStart
double Timer::getTimeFromStart(){
    return lastTime-startTime;
}
// }}}

// {{{ getFPS
double Timer::getFPS(){
    return lastFPS;
}
// }}}

// {{{ getSystemTime
double Timer::getSystemTime(){
    return SDL_GetTicks()/1000.0;
}
// }}} 

// {{{ getCalibratedSystemTime
double Timer::getCalibratedSystemTime(){
    return getSystemTime()+callibration;
}
// }}}

// {{{ getCalibratedSystemTimeAt
double Timer::getCalibratedSystemTimeAt(double time){
  return time+callibration;
}
// }}}

// {{{ setCalibratedSystemTime
double Timer::setCalibratedSystemTime(double time){
    callibration=time-getSystemTime();
    return callibration;
}
// }}}
