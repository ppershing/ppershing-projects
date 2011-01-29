// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_SPECTRUM_ANALYZER
#define H_SPECTRUM_ANALYZER

#include "AudioSamples.h"

/**
  base class for all spectrum analyzers
  */
class SpectrumAnalyzer{
    public:
    /**
      insert new data to analyzer
      @throws EIllegalArgument data type do not match
      */
    virtual void sendNewData(AudioSamples& data)=0;

    /**
      set audio data type used for analysis
      */
    virtual void setAudioCharacteristics(const double sampleRate,
            const int channelCount)=0;
    /**
      perform analysis of current data
      */
    virtual void analyzeData()=0;

    /**
      return minimun analyzed frequency
      */
    virtual int getSpectrumMin()=0;

    /**
      return 1+highest analyzed frequency
      */
    virtual int getSpectrumMax()=0;

    /**
      return center of analyzed data time
      */
    virtual double getTime()=0;

    /**
      return size of spectrum
      */
    int getSpectrumSize();

    /**
      return amplitude of frequency in data
      */
    virtual double getAmplitude(const int freq)=0;
};

#endif
