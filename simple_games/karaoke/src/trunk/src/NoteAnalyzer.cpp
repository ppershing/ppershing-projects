#include "NoteAnalyzer.h"
#include <cmath>

//TODO: threshold from ini file

// {{{ NoteAnalyzer
NoteAnalyzer::NoteAnalyzer(){
  analyzer.setAnalyzer(0,&simpleAnalyzer);
  analyzer.setAnalyzer(1,&fftAnalyzer);
  analyzer.setWeight(0,0.6);
  analyzer.setWeight(1,0.4);
  history.resize(historySize);
  threshold=0.007;
}
// }}}

// {{{ setAudioCharacteristics
void NoteAnalyzer::setAudioCharacteristics(const int sampleRate){
  simpleAnalyzer.setAudioCharacteristics(sampleRate,1);
  fftAnalyzer.setAudioCharacteristics(sampleRate,1);
}
// }}}

// {{{ sendNewData
void NoteAnalyzer::sendNewData(AudioSamples& data){
  analyzer.sendNewData(data);
}
// }}}

// {{{ analyzeData
void NoteAnalyzer::analyzeData(){
  analyzer.analyzeData();

  int lo=analyzer.getSpectrumMin();
  int hi=analyzer.getSpectrumMax();
  int maximum=lo;
  double best=analyzer.getAmplitude(maximum);

  for (int q=lo; q<hi; q++) {
      double amplitude=analyzer.getAmplitude(q);
        if (amplitude>best) {
            best=amplitude;
            maximum=q;
            }
  }

  history.erase(history.begin());
//  printf("%d %lf\n",maximum,best);
  if (best<threshold) maximum=0; // only noise
  history.push_back(maximum);
}
// }}}

// {{{ getFrequency
int NoteAnalyzer::getFrequency(){
    int bestCount=0;
    int best=0;
    for (unsigned int q=0; q<history.size(); q++ ) {
        int cnt=0;
        for (unsigned int w=0; w<history.size(); w++)
            if (std::abs(history[q]-history[w])<frequencyDiffOK)
                cnt++;

        if (cnt>bestCount) {
            bestCount=cnt;
            best=history[q];
        }
    }

    return best;
}
// }}}

// {{{ getTime
double NoteAnalyzer::getTime(){
  return analyzer.getTime();
}
// }}}


// {{{ ~NoteAnalyzer
NoteAnalyzer::~NoteAnalyzer(){
}
// }}}
