// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_SIMPLE_SPECTRUM_ANALYZER
#define H_SIMPLE_SPECTRUM_ANALYZER

#include "SpectrumAnalyzer.h"
#include "AudioCircularBuffer.h"
#include "AudioSamples.h"
#include <vector>

/**
  basic spectrum analyzer - 
  computes self-signal square differences for
  all frequencies
  */
class SimpleSpectrumAnalyzer: public SpectrumAnalyzer{
    public:
    /**
      send new data to analyzer
      */
    virtual void sendNewData(AudioSamples& data);

    /**
      set new audio type, currently support only one channel
      @throws EIllegalArgument on wrong data type
      */
    virtual void setAudioCharacteristics(const double sampleRate,
            const int channelCount);

    /**
      analyze buffer
      */
    virtual void analyzeData();

    /**
      return smallest analyzed frequency
      */
    virtual int getSpectrumMin();

    /**
      return 1+ largest analyzed frequency
      */
    virtual int getSpectrumMax();

    /**
      return center of analyzed time
      */
    virtual double getTime();

    /**
      return amplitude of analized frequency
      @throws EIllegalArgument if freq is out of range
      */
    virtual double getAmplitude(const int freq);
    
    /**
      costructor,
      default audio is 44100,1
      default buffer size is sampleRate/minFreq*minNumberOfWaves
      */
    SimpleSpectrumAnalyzer();

    /**
      destructor
      */
    virtual ~SimpleSpectrumAnalyzer();

    /**
      set precision of computation,
      default is 10,
      minimum precision is 1,
      maximum precision is sampleRate/minFreq*minNumberOfWaves/10
      (we want at least ten points to statistics)
      note that time is exactly proportional to 1/precision 
      @throws EIllegalArgument on wrong precision
      */
    void setPrecision(const int precision);

    private:

    /**
      internal function, returns amplitude of frequency
      @param precesion 1 means all samples, n means 
      calculate with only n-th samples
      */
    double analyzeSingleFrequency(const int freq, const int precision);

    /**
      minimum supported frequency
      */
    static const int minFreq=90;

    /**
      maximum supported frequency
      */
    static const int maxFreq=500;

    /**
      minimum number of waves to analyze 
      note that it should be at least two
      (otherwise we can't do whole differences
      */
    static const int minNumberOfWaves=3;

    /**
      magic bulgar constant,
      it is best possible match of signals
      (due to noise, we do not have zero diff).
      It is used to transform diff into amplitude
      */
    static const double idealMatch;

    /**
      actual audio buffer
      */
    AudioCircularBuffer buffer;

    /**
      holds analyzed data
      */
    std::vector<double> analyzedData;

    /**
      actual setting of precision
      */
    int precision;

    /**
      time of last inserted sample
      */
    double endTime;
};

#endif
