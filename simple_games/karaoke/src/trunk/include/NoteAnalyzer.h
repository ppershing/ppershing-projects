// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_NOTE_ANALYZER
#define H_NOTE_ANALYZER

#include "AudioSamples.h"
#include "SpectrumAnalyzer.h"
#include "SimpleSpectrumAnalyzer.h"
#include "FFTSpectrumAnalyzer.h"
#include "CompoundSpectrumAnalyzer.h"
#include <vector>

class NoteAnalyzer{
  public:
      NoteAnalyzer();
      ~NoteAnalyzer();
      void setAudioCharacteristics(const int sampleRate);
      void sendNewData(AudioSamples& data);
      void analyzeData();
      int getFrequency();
      double getTime();

      double threshold;

  private:
    static const int historySize=3;
    static const int frequencyDiffOK=3;
    std::vector<int> history;

    SimpleSpectrumAnalyzer simpleAnalyzer;
    FFTSpectrumAnalyzer fftAnalyzer;
    CompoundSpectrumAnalyzer analyzer;
};

#endif
