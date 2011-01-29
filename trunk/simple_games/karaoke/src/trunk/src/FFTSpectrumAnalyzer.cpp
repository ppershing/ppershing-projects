// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "FFTSpectrumAnalyzer.h"
#include "MyStringUtils.h"
#include "Debug.h"

// {{{ FFTSpectrumAnalyzer
FFTSpectrumAnalyzer::FFTSpectrumAnalyzer(){
    buffer.setAudioCharacteristics(44100,1); // default
    setFFTSize(16384);
}
// }}}

// {{{ setFFTSize
void FFTSpectrumAnalyzer::setFFTSize(const int size){
  if (!MyMath::isPowerOf2(size)) throw EIllegalArgument(
          "setFFTSize - size is not power of 2");
  FFTSize=size;
  precompute();
}
// }}}

// {{{ precompute
void FFTSpectrumAnalyzer::precompute(){
   buffer.resize(FFTSize);
   FFTData.resize(FFTSize);
   precomputeComplexes();
   precomputeBitReverses();
}
// }}}

// {{{ precomputeBitReverses
void FFTSpectrumAnalyzer::precomputeBitReverses(){
    bitReversal.resize(FFTSize);

    int rev=0;
    for (int i=0;i<FFTSize-1;i++) {
        bitReversal[i]=rev;

        int mask=FFTSize/2;
        while (rev>=mask){
            rev-=mask;
            mask/=2;
        }
        rev+=mask;
    }
    bitReversal[FFTSize-1]=FFTSize-1;
}
// }}}

// {{{ precomputeComplexes
void FFTSpectrumAnalyzer::precomputeComplexes(){
   int logSize=MyMath::log2(FFTSize);
   precomputedComplex.resize(logSize+1);

   for (int q=1;q<=logSize; q++){
       precomputedComplex[q].resize(FFTSize);

       int p=1<<q;
       for (int w=0; w<FFTSize; w++){
           double re=cos(2*M_PI*w/p);
           double im=-sin(2*M_PI*w/p);
           precomputedComplex[q][w]=
               std::complex<double>(re,im);
       }
   }
}
// }}}

// {{{ ~FFTSpectrumAnalyzer
FFTSpectrumAnalyzer::~FFTSpectrumAnalyzer(){
}
// }}}

// {{{ sendNewData
void FFTSpectrumAnalyzer::sendNewData(AudioSamples& data){
    buffer.insertAudioSamples(data);
    endTime=data.getEndTime();
}
// }}}

// {{{ setAudioCharacteristics
void FFTSpectrumAnalyzer::setAudioCharacteristics(const double sampleRate,
        const int channelCount){
    if (channelCount!=1) throw EIllegalArgument(
            "currently supporting only 1-channel input");
    buffer.setAudioCharacteristics(sampleRate,channelCount);
}
// }}}

// {{{ getSpectrumMin 
int FFTSpectrumAnalyzer::getSpectrumMin(){
    return (int)(FFTSize-1+buffer.getSampleRate())/FFTSize;
}
// }}}

// {{{ getSpectrumMax
int FFTSpectrumAnalyzer::getSpectrumMax(){
    return (int)(buffer.getSampleRate()/2);
}
// }}}

// {{{ analyzeData
void FFTSpectrumAnalyzer::analyzeData(){
    copyData();
    doTransform();
    calculateVolume();
}
// }}}

// {{{ calculateVolume
void FFTSpectrumAnalyzer::calculateVolume(){
    volume=1e-5;
    for (int q=0;q<FFTSize; q++)
        volume+=std::abs(buffer[q]);
    volume/=FFTSize;
}
// }}}

// {{{ copyData
void FFTSpectrumAnalyzer::copyData(){
  for (int i=0;i<FFTSize; i++){
      FFTData[bitReversal[i]]=buffer[i];
  }
}
// }}}

// {{{ getAmplitude
double FFTSpectrumAnalyzer::getAmplitude(const int freq){
        if (freq<getSpectrumMin() || freq>=getSpectrumMax())
            throw EIllegalArgument("frequency out of range");

        double requestedPoint=freq/buffer.getSampleRate()*buffer.size();
        int x=(int) requestedPoint;
        double weight=(requestedPoint-x);

        return (std::abs(FFTData[x])*(1-weight)+
               std::abs(FFTData[x+1])*weight)/FFTSize/volume;
}
// }}}

// {{{ doTransform 
void FFTSpectrumAnalyzer::doTransform(){
    int step=1;
    int logSize=MyMath::log2(FFTSize);
    for (int level=1; level<=logSize; level++){
        int increment=step*2;

        for (int q=0; q<step; q++) {
            for (int i=q; i<FFTSize; i+=increment){
            std::complex<long double> t=
                precomputedComplex[level][i]*FFTData[i+step];
            FFTData[i+step]=FFTData[i]-t;
            FFTData[i]+=t;
            }
        }
        step*=2;
    }
}
// }}}

// {{{
double FFTSpectrumAnalyzer::getTime(){
    return endTime-FFTSize/buffer.getSampleRate()/2.0;
}
// }}}
