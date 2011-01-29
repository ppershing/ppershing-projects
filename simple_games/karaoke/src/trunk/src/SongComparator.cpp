// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#include <string>
#include <vector>
#include "Song.h"
#include "SongComparator.h"

// {{{ constructor
SongComparator::SongComparator(){
  compareBy("title");
}
// }}}

// {{{ constructor
SongComparator::SongComparator(const std::string attr){
  compareBy(attr);
}
// }}}

// {{{ destructor
SongComparator::~SongComparator(){
}
// }}}

// {{{ operator()<
bool SongComparator::operator()(const Song& s1, const Song& s2){
  switch(compareByAttribute){
    case SORT_TITLE:
      return s1.title<s2.title;
    break;
    case SORT_ARTIST:
      return s1.artist<s2.artist;
    break;
    case SORT_BPM:
      return s1.bpm<s2.bpm;
    break;
    default:
      return s1.title<s2.title;
    break;
  }
}
// }}}

// {{{ compareBy
void SongComparator::compareBy(const std::string attr){
  compareByAttribute = sortAttrToInt(attr);
}
// }}}

// {{{ compareByNext
void SongComparator::compareByNext(){
  compareByAttribute = (compareByAttribute + 1) % SORT_ATTRIBUTES;
}
// }}}

// {{{ sortAttrToInt
int SongComparator::sortAttrToInt(const std::string attr){
  if(attr=="title")
    return SORT_TITLE;
  if(attr=="artist")
    return SORT_ARTIST;
  if(attr=="bpm")
    return SORT_BPM;
}
// }}}

// {{{ intToSortAttr
std::string SongComparator::intToSortAttr(int attr){
  if(attr==SORT_TITLE)
    return "title";
  if(attr==SORT_ARTIST)
    return "artist";
  if(attr==SORT_BPM)
    return "bpm";
}
// }}}

// {{{ getAttr
std::string SongComparator::getAttr(){
  return intToSortAttr(compareByAttribute);
}
// }}}
