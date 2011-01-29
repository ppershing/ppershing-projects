// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "Oscilloscope.h"
#include "MyAssert.h"
#include "Surface.h"

// {{{ Oscilloscope
Oscilloscope::Oscilloscope(){
    graph.setAuto(false,false);
    graph.setColor(0,255,255,255);
    graph.setMin(-1.0);
    graph.setMax(1.0);
    setAudioCharacteristics(44100,1); // default audio characteristic
    setRect(0,0,1,1);
}
// }}}

//FIXME: check
// {{{ setDataSize 
void Oscilloscope::setDataSize(const int newSize){
    data.resize(newSize);
}
// }}}

// {{{ setAudioCharacteristics 
void Oscilloscope::setAudioCharacteristics(const double rate, const
        int channels){
    data.setAudioCharacteristics(rate,channels);
}
// }}}

// {{{ ~Oscilloscope 
Oscilloscope::~Oscilloscope(){
}
// }}}

// {{{ draw 
void Oscilloscope::draw(Surface& surface){
    Widget::draw(surface);
}
// }}}

// FIXME: rethrow/suppress
// {{{ clippedDraw 
void Oscilloscope::clippedDraw(Surface& surface){
    graph.draw(surface,data.rbegin(),data.rend());
}
// }}}

// {{{ sendNewData 
void Oscilloscope::sendNewData(AudioSamples& newData){
    data.insertAudioSamples(newData);
}
// }}}

// {{{ setRect
void Oscilloscope::setRect(const SDL_Rect& rect){
    Widget::setRect(rect);
    graph.setMetric(Metric(rect));
}
// }}}

// {{{ setColor 
void Oscilloscope::setColor(const int R,const int G,
        const int B,const int A){
    graph.setColor(R,G,B,A);
}
// }}}

// {{{ setRect 
void Oscilloscope::setRect(const int x1, const int y1,
        const int x2, const int y2){
    Widget::setRect(x1,y1,x2,y2);
    graph.setMetric(Metric(x1,y1,x2,y2));
}
// }}}
