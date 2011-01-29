// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

//FIXME: sqr
//FIXME: analyzeSingleFrequency() assertions on frequency

#include "SimpleSpectrumAnalyzer.h"
#include "Exceptions.h"
#include "NoDebug.h"
#include <cmath>
#include "MyMath.h"

const double SimpleSpectrumAnalyzer::idealMatch=0.009;

// {{{ setPrecision
void SimpleSpectrumAnalyzer::setPrecision(const int precision){
    Assert(precision>0,"wrong precision");
    Assert(precision< buffer.getSampleRate()/minFreq*minNumberOfWaves/10,
            "wrong precision");
    this->precision=precision;
}
// }}}

// {{{ SimpleSpectrumAnayzer
SimpleSpectrumAnalyzer::SimpleSpectrumAnalyzer(){
    buffer.setAudioCharacteristics(44100,1);
    analyzedData.resize(getSpectrumSize());
    precision=5;
}
// }}}

// {{{ ~SimpleSpectrumAnalyzer
SimpleSpectrumAnalyzer::~SimpleSpectrumAnalyzer(){

}
// }}}

// {{{ sendNewData
void SimpleSpectrumAnalyzer::sendNewData(AudioSamples& data){
    buffer.insertAudioSamples(data);
    endTime=data.getEndTime();
}
// }}}


// {{{ getTime
double SimpleSpectrumAnalyzer::getTime(){
    return endTime-analyzedData.size()/buffer.getSampleRate()/2.0;
}
// }}}

// {{{ setAudioCharacteristics
void SimpleSpectrumAnalyzer::setAudioCharacteristics(const double sampleRate,
    const int channelCount){
    DEBUG("set");
    if (channelCount!=1) throw EIllegalArgument(
            "currently supporting only 1-channel input");
    buffer.setAudioCharacteristics(sampleRate,channelCount);
    buffer.resize((int)
            (minNumberOfWaves*sampleRate/getSpectrumMin()));
    DEBUG("ok");
}
// }}}


// {{{ getSpectrumMin
int SimpleSpectrumAnalyzer::getSpectrumMin(){
    return minFreq;
}
// }}}

// {{{ getSpectrumMax
int SimpleSpectrumAnalyzer::getSpectrumMax(){
    return maxFreq;
}
// }}}

// {{{ getAmplitude
double SimpleSpectrumAnalyzer::getAmplitude(const int freq){
    DEBUG("get amp");
    if (freq<getSpectrumMin() || freq>=getSpectrumMax())
        throw EIllegalArgument("get amplitude - wrong frequency");
    return analyzedData[freq-getSpectrumMin()];    
}
// }}}

// {{{ analyzeData
void SimpleSpectrumAnalyzer::analyzeData(){
    const int size=getSpectrumSize();
    const int min=getSpectrumMin();
    for (int q=0; q<size; q++){
        analyzedData[q]=analyzeSingleFrequency(q+min,precision);
    }
}
// }}}

// {{{ analyzeSingleFrequency
double SimpleSpectrumAnalyzer::analyzeSingleFrequency(const int
        freq,const int precision){
    const int length=buffer.size(); // length of buffer
    const int offset=(int)buffer.getSampleRate()/freq; 
    long double diff=1e-10; // square difference
    long double self=1e-10; // mean of self signal (eg. DC offset)
    long double selfQuad=1e-10; // square of self signal -to normalize diff
    int n=0; // number of samples

    for (int i=0;i<length-offset;i+=precision){
        diff+=MyMath::sqr((long double)(buffer[i]-buffer[i+offset]));
        self+=buffer[i];
        selfQuad+=MyMath::sqr((long double)buffer[i]);
        n++;
    }
        selfQuad-=MyMath::sqr(self)/n;

    return idealMatch/(idealMatch+diff/selfQuad);
}
// }}}
