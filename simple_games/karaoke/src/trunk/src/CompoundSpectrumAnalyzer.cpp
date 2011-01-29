// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "CompoundSpectrumAnalyzer.h"
#include "Exceptions.h"
#include <algorithm>
#include "limits.h"

// {{{ CompoundSpectrumAnalyzer
CompoundSpectrumAnalyzer::CompoundSpectrumAnalyzer(){
    analyzerPtr.resize(analyzerCount);
    analyzerWeight.resize(analyzerCount);
    for (int q=0; q<analyzerCount; q++){
        analyzerPtr[q]=NULL;
        analyzerWeight[q]=1.0/analyzerCount;
    }
}
// }}}

// {{{ ~CompoundSpectrumAnalyzer
CompoundSpectrumAnalyzer::~CompoundSpectrumAnalyzer(){
}
// }}}

// {{{ setAnalyzer
void CompoundSpectrumAnalyzer::setAnalyzer(const int id, 
        SpectrumAnalyzer* ptr){
    if (id<0 || id>=analyzerCount) throw EIllegalArgument(
            "invalid id");
    analyzerPtr[id]=ptr;
}
// }}}

// {{{ setWeight
void CompoundSpectrumAnalyzer::setWeight(const int id, const
        double weight){
    if (id<0 || id>=analyzerCount) throw EIllegalArgument(
            "invalid id");
    analyzerWeight[id]=weight;
}
// }}}

// {{{ getAmplitude 
double CompoundSpectrumAnalyzer::getAmplitude(const int freq){
    if (freq<getSpectrumMin() || freq>=getSpectrumMax()) throw
        EIllegalArgument("frequency out of range");

    double result=0;
    for (int q=0; q<analyzerCount; q++)
        if (analyzerPtr[q]!=NULL)
             result+=analyzerPtr[q]->getAmplitude(freq)*analyzerWeight[q];
    return result;
}
// }}}

// {{{ analyzeData 
void CompoundSpectrumAnalyzer::analyzeData(){
    for (int q=0; q<analyzerCount; q++)
        if (analyzerPtr[q]!=NULL)
           analyzerPtr[q]->analyzeData();
}
// }}}

// {{{ sendNewData 
void CompoundSpectrumAnalyzer::sendNewData(AudioSamples& data){
    for (int q=0; q<analyzerCount; q++)
        if (analyzerPtr[q]!=NULL)
           analyzerPtr[q]->sendNewData(data);
}
// }}}

// {{{ setAudioCharacteristics
void CompoundSpectrumAnalyzer::setAudioCharacteristics(const double
        sampleRate, const int channelCount){
    for (int q=0; q<analyzerCount; q++)
        if (analyzerPtr[q]!=NULL)
          analyzerPtr[q]->setAudioCharacteristics(sampleRate,channelCount);
}
// }}}

// {{{ getSpectrumMin
int CompoundSpectrumAnalyzer::getSpectrumMin(){
    int min=0;
    
    for (int q=0; q<analyzerCount; q++)
        if (analyzerPtr[q]!=NULL){
            min=std::max(min,analyzerPtr[q]->getSpectrumMin());
        }
    return min;
}
// }}}

// {{{ getSpectrumMax
int CompoundSpectrumAnalyzer::getSpectrumMax(){
    int max=INT_MAX;
    
    for (int q=0; q<analyzerCount; q++)
        if (analyzerPtr[q]!=NULL){
            max=std::min(max,analyzerPtr[q]->getSpectrumMax());
        }
    if (max==INT_MAX) return 0;
    return max;
}
// }}}

// {{{ getTime
double CompoundSpectrumAnalyzer::getTime(){
    double result=0;
    for (int q=0; q<analyzerCount; q++)
        if (analyzerPtr[q]!=NULL)
             result+=analyzerPtr[q]->getTime()*analyzerWeight[q];
    return result;
}
// }}}
