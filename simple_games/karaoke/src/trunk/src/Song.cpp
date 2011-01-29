// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#include <string>
#include <vector>
#include <fstream>
#include <iostream>
#include <SDL/SDL.h>

#include "Song.h"
#include "Sentence.h"
#include "NoDebug.h"
#include "Exceptions.h"
#include "Errors.h"
#include "MyStringUtils.h"
#include "MyMath.h"

// {{{ constructor
Song::Song(){
  UID=0;
  endTime=0;
  gap=0;
  bpm=0;
  notesgap=0;
  resolution=0;
  relative=false;
  title="";
  artist="";
  fileName="";
  musicFileName="";
  dirname="";
  familyDirName="";
  cover="";
  background="";
}
// }}}

// {{{ destructor
Song::~Song(){
}
// }}}

// {{{ constructor
Song::Song(const std::string name){
  UID=0;
  endTime=0;
  gap=0;
  bpm=1;
  notesgap=0;
  resolution=0;
  relative=false;
  title=name;
  artist="";
  fileName=name;
  musicFileName="";
  dirname=name;
  familyDirName="";
  cover="";
  background="";
}
// }}}

// {{{ getSentenceIndexAt
int Song::getSentenceIndexAt(double time){
  for(unsigned int i=0;i<sentence.size();i++)
    if(time>=sentence.at(i).start && time<sentence.at(i).start+sentence.at(i).duration)
      return i;
  return -1;
}
// }}}

// {{{ getSentenceAt
Sentence Song::getSentenceAt(double time){
  int i=getSentenceIndexAt(time);
  if(i==-1)
    throw Exception("getSentenceAt: no sentence at time "+MyStringUtils::doubleToString(time));
  return sentence.at(i);
}
// }}}

// {{{
double Song::getTimeSlot(int time){
  //std::cout << "gap=" << gap << " /+" << (1000*time/4)*60/bpm << std::endl;
  return (double)(gap + (1000*time/4)*60/bpm)/1000.0;
}
// }}}

// {{{ hasEnded
bool Song::hasEnded(double time){
  return time>=endTime;
}
// }}}

// {{{ getActiveLength
double Song::getActiveLength(){
  double sum = 0;
  for(unsigned int i=0;i<sentence.size();i++){
    sum += sentence.at(i).getActiveLength();
  }
  return sum;
}
// }}}

// {{{ getSongInfo
std::string Song::getSongInfo(){
  std::string info;
  info += "Title: ";
  info += title;
  info += "\nArtist: ";
  info += artist;
  info += "\nLength: ";
  info += MyStringUtils::doubleToString(endTime);
  info += "\nActive length: ";
  info += MyStringUtils::doubleToString(activeLength);
  return info;
}
// }}}

// {{{ loadHeader
int Song::loadHeader(std::string filename){
  fileName=filename;
  std::ifstream sourceFile;
  sourceFile.open(filename.c_str(),std::ifstream::in);
  DEBUG(filename+" opened");
  if (sourceFile.fail()) throw ECantOpenFile("Song::loadFromFile: cant open file "+filename);
  char line[1001];
  while(true){
    sourceFile.getline(line,1000);
    DEBUG("line: "+line);
    if(line[0]!=':' && line[0]!='#' && std::string(line).length()>0 && !sourceFile.eof()){
      DEBUG("Wrong line in header");
      return -1;
    }
    if(line[0]==':'){
      DEBUG("Line starting with :");
      if(musicFileName==std::string(""))return -1;
      return 1;//field name is zero-length and has no leading #
    }
    std::string field=MyStringUtils::splitString(line,":").at(0).substr(1);//remove first "#"
    std::string value=MyStringUtils::splitString(line,":").at(1);
    if(value[value.length()-1]<32)
      value=value.substr(0,value.length()-1);//getline dal divny znak na konci...zobrazoval sa ako stvorec -> nepatri sem:)
    DEBUG(field+": \""+value+"\"");
    if(field=="TITLE")title=value;
    if(field=="ARTIST")artist=value;
    if(field=="MP3")musicFileName=getDirPath()+"/"+value;
    if(field=="COVER")cover=value;
    if(field=="BACKGROUND")background=value;
    if(field=="RELATIVE")relative=(value==std::string("YES") || value==std::string("yes"));
    if(field=="GAP")gap=(double)MyStringUtils::stringToInt(value)/1000.0;
    if(field=="BPM")bpm=MyStringUtils::stringToInt(MyStringUtils::replace(value,',','.'));
    if(field=="RESOLUTION")resolution=MyStringUtils::stringToInt(value);
    if(field=="NOTESGAP")notesgap=MyStringUtils::stringToInt(value);
  }
  DEBUG("Header ends");
  return 1;
}
// }}}

// {{{ loadFull
int Song::loadFull(){
  std::ifstream sourceFile;
  sourceFile.open(fileName.c_str(),std::ifstream::in);
  DEBUG(fileName+" opened");
  if(sourceFile.fail())throw ECantOpenFile("Song::loadFromFile: cant open file "+fileName);
  char line[1001];
  sentence.push_back(Sentence("( OUTTRO )",0,-1));  //sentence.at(0) = ( OUTTRO )
  sentence.push_back(Sentence("",0,-1));             //sentence.at(1) = <empty> (behind the outtro)
  if(gap>0)
    sentence.push_back(Sentence("( INTRO )",0,gap));

  Sentence newSentence;
  newSentence.start=gap;

  while (!sourceFile.eof()){
    DEBUG("getting line...");
    std::string lineAsString;
    try{
      sourceFile.getline(line,1000);
      lineAsString = line;
      DEBUG("\""+lineAsString+"\" "+MyStringUtils::intToString(lineAsString.length())+"-'"+lineAsString[0]+"'");
      try{
      if(lineAsString.length()>0)
        if(lineAsString.at(lineAsString.length()-1)<32){
          lineAsString = lineAsString.substr(0,lineAsString.length()-1); //posledny znak je divny
          DEBUG("Posledny znak je divny, skracujem na "+lineAsString);
        }
      }catch(Exception e){
        throw EIOError("Problem with length or substr:\n"+std::string(e.what()));
      }
      if (sourceFile.fail()) throw EIOError("Song::loadFromFile caused IO error(fail) while loading "+fileName);
      if (sourceFile.bad()) throw EIOError("Song::loadFromFile caused IO error(bad) while loading "+fileName);
    }catch(Exception e){
      Errors::_addError("Problem with getting line: "+std::string(e.what()),Errors::ERROR);
      break;
    
    }
    if(line[0]=='#')continue;
    if(lineAsString[0]=='E'){//koniec suboru
      DEBUG("KONIEC!!!");
      endTime=newSentence.start+newSentence.computeDuration();
      sentence.push_back(newSentence);
      DEBUG("Last sentence created. start="+MyStringUtils::doubleToString(newSentence.start)+", dur="+MyStringUtils::doubleToString(newSentence.duration));
      break;
    }

    if(line[0]==':'){//TO-DO osetrenie zlych vstupov
      std::vector<std::string> lineItems=MyStringUtils::splitString(lineAsString," ");
      
      Note newNote;
      if(relative)
        newNote.start = getTimeSlot(MyStringUtils::stringToInt(lineItems.at(1))) + newSentence.start;
      else
        newNote.start = getTimeSlot(MyStringUtils::stringToInt(lineItems.at(1))) + gap;
      //std::cout << sentence.size() << " " << lineItems.at(1) << " " << newNote.start << std::endl;
      newNote.duration = getTimeSlot(MyStringUtils::stringToInt(lineItems.at(2)));
      newNote.pitch = MyStringUtils::stringToInt(lineItems.at(3));
      
      Syllable newSyllable;
      newSyllable.start = newNote.start;
      newSyllable.duration = newNote.duration;
      //newSyllable.text=lineItems.at(4);
      int substrStart = lineItems.at(0).length()+lineItems.at(1).length()+lineItems.at(2).length()+lineItems.at(3).length()+4;
      newSyllable.text = lineAsString.substr(substrStart);
      //if(lineAsString.at(lineAsString.length()-1)==' ')
      //  newSyllable.text+=" ";
      newSentence.note.push_back(newNote);
      newSentence.syllable.push_back(newSyllable);
      DEBUG("Syllable and Note created, start="+MyStringUtils::doubleToString(newNote.start)+", duration="+MyStringUtils::doubleToString(newNote.duration));
    }

    if(line[0]=='-'){
      double tempTime = getTimeSlot(MyStringUtils::stringToInt(lineAsString.substr(1)));
      if(relative)
        tempTime += newSentence.start;
      else tempTime += gap;
      //std::cout << "sentence end beat=" << lineAsString.substr(1) << "tempTime=" << tempTime << std::endl;
      newSentence.duration = tempTime-newSentence.start;
      if(newSentence.getLastNoteOrSyllableTime() > newSentence.start + newSentence.duration){
        Errors::_addError("Song loading: Some notes are active after the sentence ends.",Errors::WARNING);
        if(newSentence.getLastNoteOrSyllableTime() > newSentence.start + newSentence.duration+0.05){
          Errors::_addError("Song loading: Some notes are active quite long after the sentence ends. - adjusting sentence end",Errors::ERROR);
          newSentence.computeDuration();
          tempTime=newSentence.start+newSentence.duration;
        }else{
          Errors::_addError("Song loading: Some notes are active only a bit after the sentence ends. - adjusting notes end",Errors::ERROR);
          newSentence.setEnds(tempTime);
        }
      }
      //std::cout << "sen dur=" << newSentence.duration << std::endl;
      //std::cout << newSentence.start << "-" << newSentence.start+newSentence.duration << std::endl;
      sentence.push_back(newSentence);
      DEBUG("Old Sentence pushed. start="+MyStringUtils::doubleToString(newSentence.start)+", dur="+MyStringUtils::doubleToString(newSentence.duration));
      newSentence.clear();
      newSentence.start = tempTime;//zaciatok uz dalsej sentence
      DEBUG("New Sentence created. start="+MyStringUtils::doubleToString(newSentence.start));
    }
  }
  sourceFile.close();
  DEBUG(title+" succesfully loaded.");
  activeLength = getActiveLength();
  Errors::_addError("\n"+getSongInfo(),Errors::NOTICE);
  return 0;
}
// }}}

// {{{ getDirPath()
std::string Song::getDirPath(){
  return familyDirName;
}
// }}}
