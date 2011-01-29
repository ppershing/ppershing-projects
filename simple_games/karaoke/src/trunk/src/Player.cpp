// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#include <string>
#include <vector>
#include "Player.h"
#include "MyStringUtils.h"
#include "NoDebug.h"
#include "Errors.h"
#include "Exceptions.h"
#include "Preferences.h"

// {{{ constructor
Player::Player(){
  name="New player";
  singing=false;
}
// }}}

// {{{ destructor
Player::~Player(){
}
// }}}

// {{{ getNoteIndexAt
int Player::getNoteIndexAt(double time){
  for(unsigned int i=0;i<singed.size();i++){
    if(singed.at(i).start <= time && time < singed.at(i).start + singed.at(i).duration)
      return i;
  }
  return -1;
}
// }}}

// {{{ getFirstNoteIndexAfter
int Player::getFirstNoteIndexAfter(double time){
  for(unsigned int i=0;i<singed.size();i++){
    if(time < singed.at(i).start + singed.at(i).duration)
      return i;
  }
  return -1;
}
// }}}

// {{{ getLastNoteIndexBefore
int Player::getLastNoteIndexBefore(double time){
  for(int i=singed.size()-1;i>=0;i--){
    if(singed.at(i).start <= time)
      return i;
  }
  return -1;
}
// }}}

// {{{ getNoteAt
Note Player::getNoteAt(double time){
  int noteIndex = getNoteIndexAt(time);
  if(noteIndex<0)
    throw EIllegalArgument("Player::getNoteAt: No note at "+MyStringUtils::intToString(time));
  return singed.at(noteIndex);
}
// }}}

// {{{ getNoteAtIndex
Note Player::getNoteAtIndex(int index){
  if(index<0 || index>=singed.size())
    throw EIllegalArgument("Player::getNoteIndexAt: No note at index "+MyStringUtils::intToString(index));
  return singed.at(index);
}
// }}}

// {{{ getCurrentSingingNote
Note Player::getCurrentSingingNote(){
    return currentSingingNote;//only usable when singing, so check it.
}
// }}}

// {{{ isSinging
bool Player::isSinging(){
  return singing;
}
// }}}

// {{{ sing
void Player::sing(double pitch, double time,double target){
  //int pitch = Note::frequencyToPitch(frequency);
  DEBUG("Sing at normalized pitch "+MyStringUtils::doubleToString(pitch)+", time="+MyStringUtils::doubleToString(time));
  /*if(pitch>=0 && !singing){//start sing new note
    //currentSingingNote=Note();
    currentSingingNote.start = (int) time;
    currentSingingNote.pitch = pitch;
    DEBUG("Started singing new note time="+MyStringUtils::doubleToString(time)+", pitch="+MyStringUtils::intToString(pitch));
    singing=true;
  }
  if(pitch>=0 && singing){//continue singing
    currentSingingNote.duration = (int) time-currentSingingNote.start;
    if(currentSingingNote.duration<0){
      Errors::_addError("Jumped back in time - currentSingingNote: end<start, truncating to 0.",Errors::ERROR);
      currentSingingNote.duration=0;
    }
    if((int)pitch == currentSingingNote.pitch){//at the same pitch
    }else{                                //at other pitch
      singed.push_back(currentSingingNote);
      //printf("old note %d-%d, pitch %d pushed\n",currentSingingNote.start,currentSingingNote.start+currentSingingNote.duration,currentSingingNote.pitch);
      currentSingingNote.start = (int) time;
      currentSingingNote.pitch = pitch;
      currentSingingNote.duration = 0;
      //printf("new note started %d, pitch %d\n",currentSingingNote.start,currentSingingNote.pitch);
      DEBUG("Change of pitch to "+MyStringUtils::intToString(pitch)+", old note pushed, there are "+MyStringUtils::intToString(singed.size())+" notes now.");
    }
  }
  if(pitch<0 && !singing){//continue being quiet
  }
  if(pitch<0 && singing){//stop singing
    currentSingingNote.duration = (int) time-currentSingingNote.start;
    if(currentSingingNote.duration<0){
      Errors::_addError("Jumped back in time - currentSingingNote: end<start, truncating to 0.",Errors::ERROR);
      currentSingingNote.duration=0;
    }
    singed.push_back(currentSingingNote);
    singing = false;
    DEBUG("Silence started. There are "+MyStringUtils::intToString(singed.size())+" notes now");
  }*/
  
  int position = time/timeUnit;
  int last = recorded.size()-1;
  double lastPitch = 0;
  if(recorded.size()>0)
    lastPitch = recorded.at(last).pitch;

  if(last<position){//forward in time
    RecordedData r = RecordedData(lastPitch);
    for(int i=0;i<position-last-1;i++){
      r.setColorByTarget(target);
      recorded.push_back(r);
    }
    r.pitch = pitch;
    r.setColorByTarget(target);
    recorded.push_back(r);
  }else{//back in time
    recorded.at(position).pitch = pitch;
    recorded.at(position).setColorByTarget(target);
  }

}
// }}}

// {{{ sing
void Player::sing(int frequency, double time, int midPitch,bool scoring){
  double pitch = Note::frequencyToPitch(frequency);
  DEBUG("Sing at pitch "+MyStringUtils::doubleToString(pitch)+", target="+MyStringUtils::doubleToString(midPitch));
  double normalizedPitch = Note::normalizePitch(pitch,(double)midPitch);
  if(scoring){
    score += 4.0/((normalizedPitch-midPitch)*(normalizedPitch-midPitch)+1.0);
    sing(normalizedPitch,time,midPitch);
  }else sing(normalizedPitch,time,-1);
}
// }}}

// {{{ splitAtTime
void Player::splitAtTime(double time,int newTargetPitch){
  if(currentSingingNote.start<time){
    currentSingingNote.duration = (int)time-currentSingingNote.start;
    singed.push_back(currentSingingNote);
    currentSingingNote.start = (int)time;
    currentSingingNote.pitch = Note::normalizePitch((double)currentSingingNote.pitch,(double)newTargetPitch);
  }
}
// }}}

// {{{ clearNotes
void Player::clearNotes(){
  singed.clear();
  singing=false;
  timeUnit = Preferences::_getDefaultDouble("/audio/timeunit",0.05);
  recorded.clear();
  score = 0;
}
// }}}

// {{{ recordAt
RecordedData Player::recordAt(int index){
  return recorded.at(index);
}
// }}}

// {{{ recordedSize
int Player::recordedSize(){
  return recorded.size();
}
// }}}

