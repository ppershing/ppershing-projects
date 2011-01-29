// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#include <string>
#include <vector>
#include <math.h>
#include "Note.h"
#include "NoDebug.h"
#include "MyStringUtils.h"
#include "MyMath.h"

// {{{ constructor
Note::Note(){
  pitch=0;
  start=0;
  duration=0;
}
// }}}

// {{{ destructor
Note::~Note(){
}
// }}}

// {{{ pitchToFrequencyWithTranspose
int Note::pitchToFrequencyWithTranspose(double pitch,int transpose){
  if(pitch<=0)return 0;
  int t=0;
  if(pitch<30)t=55;//69 is A440
  return 440*exp(log(2)/12*(pitch+transpose+t-69));
}
// }}}

// {{{ pitchToFrequency
int Note::pitchToFrequency(double pitch){
  return pitchToFrequencyWithTranspose(pitch,0);
}
// }}}

// {{{ frequencyToPitchWithTranspose
double Note::frequencyToPitchWithTranspose(int frequency,int transpose){
  if(frequency==0)
    return -1.0;
  return 12.0*log((double)frequency/440)/log(2) - (double)transpose + 69.0;
}
// }}}

// {{{ frequencyToPitch
double Note::frequencyToPitch(int frequency){
  return frequencyToPitchWithTranspose(frequency,0);
}
// }}}

// {{{ normalizePitch
double Note::normalizePitch(double pitch,double target){
  if(pitch<0)return pitch;
  double normalizedPitch = MyMath::mod((pitch-target+6.0),12.0);
  if(normalizedPitch<0)
    normalizedPitch += 12.0;
  normalizedPitch += target-6.0;
  return normalizedPitch;
}
// }}}
