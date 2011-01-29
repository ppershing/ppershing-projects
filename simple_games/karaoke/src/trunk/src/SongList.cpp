// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#include <vector>
#include "Font.h"
#include "SongList.h"
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"
#include "boost/filesystem/fstream.hpp"
#include "boost/filesystem.hpp"
#include "Song.h"
#include "Debug.h"
#include "Errors.h"
#include "Exceptions.h"
#include "MyStringUtils.h"
#include "LoadingCallback.h"
#include <fstream>

#define SUCCESS 0 
#define bfs boost::filesystem

// {{{ destructor
SongList::~SongList(){
  song.clear();
}
// }}}

// {{{ constructor
SongList::SongList(){
}
// }}}

// {{{ loadFromDirPath
int SongList::loadFromDirPath(const bfs::path & dir_path){
  //DEBUG("Entering "+dir_path.native_file_string());
  if (!exists(dir_path)){//wrong path
    Errors::_addError(dir_path.native_file_string()+" does not exist.",Errors::ERROR);
    return -1;
  }
  bfs::directory_iterator end_itr; // default construction yields past-the-end
  for (bfs::directory_iterator itr( dir_path ); itr != end_itr; ++itr )
  {
    if (bfs::is_directory(itr->status())){
      //DEBUG("Vnaram sa do "+itr->path().native_file_string());
      if (loadFromDirPath(itr->path())<0)
        Errors::_addError("SongList::loadFromDirPath(): "+itr->path().native_file_string()+" could not be loaded.",Errors::NOTICE);
      //DEBUG("Vynaram sa z "+itr->path().native_file_string());
    }
    else
      if(boost::filesystem::extension(*itr)==".txt" || boost::filesystem::extension(*itr)==".TXT"){
        std::string dirPathString = dir_path.native_file_string();
        std::string sourceFileString = itr->path().native_file_string();
        if(!bfs::is_regular(itr->path())){//source file problem
          Errors::_addError("SongList::loadFromDirPath(): Source "+sourceFileString+" is not regular.",Errors::NOTICE);
          numSongsFailed++;
          continue;
        }
        Song nextSong = Song(sourceFileString);
        nextSong.familyDirName=dirPathString;
        DEBUG("Loading header of: "+itr->path().native_file_string());
        if(nextSong.loadHeader(sourceFileString)<0){//loading header failed
          Errors::_addError("SongList::loadFromDirPath(): Could not load header of "+sourceFileString,Errors::NOTICE);
          numSongsFailed++;
          continue;
        }
        DEBUG("Header loaded.");
        bfs::path song_path(nextSong.musicFileName);
        if(!bfs::is_regular(song_path)){//music file problem
          Errors::_addError("SongList::loadFromDirPath(): MusicFile "+nextSong.musicFileName+" is not regular.",Errors::NOTICE);
          numSongsFailed++;
          continue;
        }
        maxUID++;
        nextSong.UID = maxUID;
        DEBUG("file closed, pushing to songlist");
        song.push_back(nextSong);
        DEBUG("pushed");
        LoadingCallback::_itemLoaded("Loading song header - "+MyStringUtils::intToString(song.size()),0);
        LoadingCallback::_itemLoaded(itr->path().native_file_string(),1);

      }
  }
  //DEBUG("Returning from "+dir_path.native_file_string());
  return 1;
}
// }}}

// {{{ loadFromDir
int SongList::loadFromDir(const std::string dirname){
  DEBUG("loadFromDir");
  familyDirname=dirname;
  bfs::path dir_path(dirname);
  numSongsFailed=0;
  maxUID=47;//IDs will start with value 47
  if(loadFromDirPath(dirname)<0)
    Errors::_addError("SongList::loadFromDir(): Loading from "+dirname+" failed.",Errors::ERROR);
  Errors::_addError(MyStringUtils::intToString(numSongsFailed)+" songs failed to load.",Errors::NOTICE);
  Errors::_addError("Loaded..."+MyStringUtils::intToString(song.size())+" songs OK",Errors::NOTICE);
  compareBy("title");
  sort();
  Errors::_addError("Sorted by title OK",Errors::NOTICE);
  return 1;
}

int SongList::getMaxFontSize(const std::string& fontName,const int width, const int height){
  int max=42;
  for(unsigned int i=0;i<song.size();i++){
    int pom=Font::getMaxFontSize(fontName,song[i].title,width,height);
    if(pom<max)max=pom;
  }
  return max;
}
// }}}

// {{{ compareByNext
void SongList::compareByNext(){
  songComparator.compareByNext();
}
// }}}

// {{{ compareBy
void SongList::compareBy(const std::string attr){
  songComparator.compareBy(attr);
}
// }}}

// {{{ getSortAttribute
std::string SongList::getSortAttribute(){
  return songComparator.getAttr();
}
// }}}

// {{{ sort
void SongList::sort(){
  std::sort(song.begin(),song.end(),songComparator);
}
// }}}

// {{{ getSong
Song SongList::getSong(int index){
  if(index<0 || index>song.size())
    throw EIllegalArgument("SongList::getSong(): No song at index "+MyStringUtils::intToString(index)+", indexes are from 0 to "+MyStringUtils::intToString(song.size()));
  return song[index];
}
// }}}

// {{{ getNext
Song SongList::getNext(int index){
  return getSong(getNextIndex(index));
}
// }}}

// {{{ getNextIndex
int SongList::getNextIndex(int index){
  return (index+1)%song.size();
}
// }}}

// {{{ getPrev
Song SongList::getPrev(int index){
  return getSong(getPrevIndex(index));
}
// }}}

// {{{ getPrevIndex
int SongList::getPrevIndex(int index){
  return (index+song.size()-1)%song.size();
}
// }}}

// {{{ getIndexFromUID
int SongList::getIndexFromUID(int UID){
  for(unsigned int i=0;i<song.size();i++)
    if(song.at(i).UID==UID)
      return i;
  return -1;
}
// }}}
