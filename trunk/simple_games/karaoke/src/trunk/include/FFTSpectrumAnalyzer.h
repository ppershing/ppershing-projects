// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_FFT_SPECTRUM_ANALYZER
#define H_FFT_SPECTRUM_ANALYZER

#include <cmath>
#include "SpectrumAnalyzer.h"
#include "AudioCircularBuffer.h"
#include "AudioSamples.h"
#include <vector>
#include <complex>
#include "MyMath.h"

/**
   FFT Spectrum analysis,
   performs 2-radix fast fourier transform on data.
   Note that providing big transform size (greater that 64k)
   is not optimalized, especially because of
   memory - we held long double * n * log n 
   precomputed complex values, which is great deal
   */

class FFTSpectrumAnalyzer: public SpectrumAnalyzer{
    public:
    /**
      add data to internal buffer,
      @throws EIllegalArgument if data type do not match
      */
    virtual void sendNewData(AudioSamples& data);

    /**
      set new size of FFT
      @param size must be power of 2
      @throws EIllegalArgument on wrong size
      */
    void setFFTSize(const int size);

    /**
      set new type of data, note that currently is supported
      only one channel
      @throws EIllegalArgument if invalid specification
      */
    virtual void setAudioCharacteristics(const double sampleRate,
            const int channelCount);

    /**
      performs actual FFT analysis on buffer
      */
    virtual void analyzeData();

    /**
      returns minimal analyzed frequency
      */
    virtual int getSpectrumMin();

    /**
      returns maximum analyzed requency
      */
    virtual int getSpectrumMax();

    /**
      returns center of analyzed data
      */
    virtual double getTime();

    /**
      returns analyzed strength of frequency
      */
    virtual double getAmplitude(const int freq);
    
    /**
      constructor,
      default audio is 44100,1
      default FFTSize is 2^16
      */
    FFTSpectrumAnalyzer();

    /**
      destructor
      */
    virtual ~FFTSpectrumAnalyzer();
    private:

    /**
      perform all precomputations and
      resizing of arrays
      */
    void precompute();

    /**
      precompute complex values and resize
      arrays
      */
    void precomputeComplexes();

    /**
      precompute bit reversal, so we can
      fast sort according inverse bit order
      */
    void precomputeBitReverses();

    /**
      copy data from internal buffer to analyzer
      */
    void copyData();

    /**
      do actual transform
      */
    void doTransform();

    /**
      calculate "volume", used for internal normalizing
      of result.
      "volume" is mean absolutute value of samples
      */
    void calculateVolume();

    /**
      internal buffer
      */
    AudioCircularBuffer buffer;

    /**
      data analysis buffer
      */
    std::vector<std::complex<long double> > FFTData;

    /**
      bit reversal array
      */
    std::vector<int> bitReversal;

    /**
      precomputed complex values
      */
    std::vector<std::vector<std::complex<long double> > >
        precomputedComplex;

    /**
      size of FFT
      */
    int FFTSize;

    /**
      calculated volume
      */
    double volume;

    /**
      time of last inserted sample
      */
    double endTime;
};

#endif
