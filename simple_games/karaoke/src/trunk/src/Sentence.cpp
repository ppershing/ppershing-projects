// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#include <string>
#include <vector>
#include <iostream>
#include <SDL/SDL.h>
#include "Note.h"
#include "Sentence.h"
#include "NoDebug.h"
#include "Exceptions.h"
#include "MyStringUtils.h"

// {{{ Sentence
Sentence::Sentence(){
  start=0;
  duration=0;
}
// }}}

// {{{ EmptySentence
Sentence::Sentence(const std::string& text,double st,double dur){
  start=st;
  duration=dur;
  Syllable oneSyllable;
  oneSyllable.start=start;
  oneSyllable.duration=duration;
  oneSyllable.text=text;
  syllable.push_back(oneSyllable);
}
// }}}

// {{{ clear
void Sentence::clear(){
  start=0;
  duration=0;
  note.clear();
  syllable.clear();
}
// }}}

// {{{ isSyllableAt
bool Sentence::isNoteAt(double time){
  for(unsigned int i=0;i<note.size();i++){
    if(time>=note.at(i).start && time<note.at(i).start+note.at(i).duration)
      return true;
  }
  return false;
}
// }}}


// {{{ getNoteAt
Note Sentence::getNoteAt(double time){
  for(unsigned int i=0;i<note.size();i++)
    if(time>=note.at(i).start && time<note.at(i).start+note.at(i).duration)
      return note.at(i);
  throw Exception("getNoteAt: no note at time "+MyStringUtils::doubleToString(time));
  return Note();
}
// }}}

// {{{ isSyllableAt
bool Sentence::isSyllableAt(double time){
  for(unsigned int i=0;i<syllable.size();i++){
    if(time>=syllable.at(i).start && time<syllable.at(i).start+syllable.at(i).duration)
      return true;
  }
  return false;
}
// }}}

// {{{ getSyllableIndexAt
int Sentence::getSyllableIndexAt(double time){
  for(unsigned int i=0;i<syllable.size();i++){
    if(time>=syllable.at(i).start && time<syllable.at(i).start+syllable.at(i).duration)
      return i;
  }
  return -1;
}
// }}}

// {{{ getSyllableAt
Syllable Sentence::getSyllableAt(double time){
  int i=getSyllableIndexAt(time);
  if(i==-1)
    throw Exception("getSyllableAt: no syllable at time "+MyStringUtils::doubleToString(time));
  return syllable.at(i);
}
// }}}

// {{{ getMinPitch
double Sentence::getMinPitch(){
  double min=99999999;
  for(unsigned int i=0;i<note.size();i++)
    if(min>note.at(i).pitch)
      min=note.at(i).pitch;
  if(min>=999999)
    return 60.0;//empty sentence shows pitches 60-70
  return min;
}
// }}}

// {{{ getMaxPitch
double Sentence::getMaxPitch(){
  double max=-99999999;
  for(unsigned int i=0;i<note.size();i++)
    if(max<note.at(i).pitch)
      max=note.at(i).pitch;
  if(max<=-999999)
    return 70.0;//empty sentence shows pitches 60-70
  return max;
}
// }}}

// {{{ getPitchRange
double Sentence::getPitchRange(){
  return getMaxPitch()-getMinPitch();
}
// }}}

// {{{ getMidPitch
double Sentence::getMidPitch(){
  return (getMaxPitch()+getMinPitch())/2.0;
}
// }}}

// {{{ setEnds
void Sentence::setEnds(double time){
  for(unsigned int i=0;i<note.size();i++)
    if(note.at(i).start<=time && note.at(i).start + note.at(i).duration>time)
      note.at(i).duration = time - note.at(i).start;
  for(unsigned int i=0;i<syllable.size();i++)
    if(syllable.at(i).start<=time && syllable.at(i).start + syllable.at(i).duration>time)
      syllable.at(i).duration = time - syllable.at(i).start;
}
// }}}

// {{{ getActiveLength
double Sentence::getActiveLength(){
  double sum = 0;
  for(unsigned int i=0;i<note.size();i++){
    sum += note.at(i).duration;
  }
  return sum;
}
// }}}

// {{{ getFullText
std::string Sentence::getFullText(){
  return getFullTextMarked(-999999).at(0);
}
// }}}

// {{{ getFullTextMarked
std::vector<std::string> Sentence::getFullTextMarked(double time){
  //DEBUG("Getting full text marked at time "+MyStringUtils::intToString(time));
  std::vector<std::string> ftm;
  std::string s;
  int currentSyllableIndex=getSyllableIndexAt(time);
  if(currentSyllableIndex==-1){
    //DEBUG("No syllable is at this time, nothing will be marked.");
    for(int i=0;i<syllable.size();i++)
      s+=syllable.at(i).text;
    ftm.push_back(s);
    return ftm;
  }
  for(int i=0;i<currentSyllableIndex;i++)
    s+=syllable.at(i).text;
  ftm.push_back(s);
  ftm.push_back(getSyllableAt(time).text);
  s="";
  for(unsigned int i=currentSyllableIndex+1;i<syllable.size();i++)
    s+=syllable.at(i).text;
  ftm.push_back(s);
  return ftm;
}
// }}}

// {{{ computeDuration
double Sentence::getLastNoteOrSyllableTime(){
  double maxTime=start;
  for(unsigned int i=0;i<note.size();i++)
    if(note.at(i).start + note.at(i).duration > maxTime)
      maxTime = note.at(i).start + note.at(i).duration;
  for(unsigned int i=0;i<syllable.size();i++)
    if(syllable.at(i).start + syllable.at(i).duration > maxTime)
      maxTime = syllable.at(i).start + syllable.at(i).duration;
  DEBUG("Sentence max time = "+MyStringUtils::doubleToString(maxTime));
  return maxTime;
}
// }}}

// {{{
double Sentence::computeDuration(){
  duration=getLastNoteOrSyllableTime()-start;
  return duration;
}
// }}}

