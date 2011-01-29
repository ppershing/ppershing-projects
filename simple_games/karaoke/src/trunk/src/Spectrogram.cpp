// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "Spectrogram.h"
//TODO: rethrow drawing errors


// {{{ Spectrogram
Spectrogram::Spectrogram(){
    graph.setMin(0);
    graph.setMax(1);
    graph.setAuto(0,0);
//    graph.setAuto(0,1);
    analyzerPtr=NULL;
}
// }}}

// {{{ ~Spectrogram
Spectrogram::~Spectrogram(){
}
// }}}

// {{{ setRect(rect)
void Spectrogram::setRect(const SDL_Rect& rect){
    Widget::setRect(rect);
    graph.setMetric(Metric(rect));
}
// }}}

// {{{ setRect(x1,y1,x2,y2)
void Spectrogram::setRect(const int x1, const int y1,
        const int x2, const int y2){
    Widget::setRect(x1,y1,x2,y2);
    graph.setMetric(Metric(x1,y1,x2,y2));
}
// }}}

// {{{ setColor
void Spectrogram::setColor(const int R,const int G,
        const int B,const int A){
    graph.setColor(R,G,B,A);
}
// }}}


// {{{ setAnalyzer
void Spectrogram::setAnalyzer(SpectrumAnalyzer* newAnalyzer){
    analyzerPtr=newAnalyzer;
}
// }}}

// {{{ clippedDraw
void Spectrogram::clippedDraw(Surface& surface){
    coloring.setGradient(0,255,0,255,255,0,0,255);
    if (analyzerPtr==NULL) return ; // no analyzer
    int min=90; //analyzerPtr->getSpectrumMin();
    int size=410; // analyzerPtr->getSpectrumSize();
    std::vector<double> tmp=std::vector<double>(size);
    for (int q=0;q<size;q++) {
        tmp[q]=analyzerPtr->getAmplitude(q+min);
//        fprintf(stderr,"%lf\n",tmp[q]);
    }

    graph.draw(surface,tmp.begin(),tmp.end(),&coloring);
}
// }}}
