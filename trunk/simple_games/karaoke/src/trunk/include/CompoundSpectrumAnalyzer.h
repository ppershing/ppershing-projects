// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_COMPOUND_SPECTRUM_ANALYZER
#define H_COMPOUND_SPECTRUM_ANALYZER

#include "AudioSamples.h"
#include "SpectrumAnalyzer.h"

/**
  SpectrumAnalyzer that is result of
  linear combination of some (actually two)
  other analyzers
  */
class CompoundSpectrumAnalyzer:public SpectrumAnalyzer{
    public:
    /**
      forwards data to analyzers
      */
    virtual void sendNewData(AudioSamples& data);

    /**
      forwards new audio characteristics to analyzers
      */
    virtual void setAudioCharacteristics(const double sampleRate,
            const int channelCount);

    /**
      forwards request to analyze data to analyzers
      */
    virtual void analyzeData();

    /**
      return maximum of analyzers getSpectrumMin()
      */
    virtual int getSpectrumMin();

    /**
      return minimum of analyzers getSpectrumMax()
      */
    virtual int getSpectrumMax();

    /**
      return linear combination of analyzers getAmplitude()
      */
    virtual double getAmplitude(const int freq);

    /**
      return center time of inserted date
      */
    virtual double getTime();

    /**
      constructor,
      default analyzers are NULL,
      default weigths are 1/analyzerCount
      */
    CompoundSpectrumAnalyzer();

    /**
      destructor
      */
    virtual ~CompoundSpectrumAnalyzer();

    /**
      assign analyzer
      @param id is in <0, analyzerCount)
      */
    void setAnalyzer(const int id, SpectrumAnalyzer* ptr);

    /**
      set analyzer's weight
      @param id is in <0, analyzerCount)
      */
    void setWeight(const int id, const double weight);

    private:

    /**
      number of analyzers supproted
      */
    static const int analyzerCount=2;

    /**
      pointers to analyzers
      */
    std::vector<SpectrumAnalyzer*> analyzerPtr;

    /**
      analyzers weights
      */
    std::vector<double> analyzerWeight;

};

#endif
