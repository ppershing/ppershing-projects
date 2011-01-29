// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_SONG_COMPARATOR
#define H_SONG_COMPARATOR

class SongComparator;

#include <string>
#include <vector>
#include <map>
#include "Song.h"

#define SORT_TITLE 0
#define SORT_ARTIST 1
#define SORT_BPM 2

#define SORT_ATTRIBUTES 3


/**
  Class providing comparisons of songs. Useful for sorting.
  @author Miso
  @see Song
  */

class SongComparator{
  public:
    /**
      Default constructor witch compareBy title.
      @author Miso
    */
    SongComparator();
    /**
      Constructor with custom compareBy.
      @author Miso
    */
    SongComparator(const std::string attr);
    /**
      Destructor.
      @author Miso
    */
    ~SongComparator();
    /**
      Operator for compare two songs by attribute stored in compareBy.
      @author Miso
      @param s1 First song to compare.
      @param s2 Second song to compare.
      @return true if the first song is greater.
      @see Song
    */
    bool operator()(const Song& s1,const Song& s2);
    /**
      Sets compareBy given attribute.
      @author Miso
      @param attr Attribute to compare by.
    */
    void compareBy(const std::string attr);
    /**
      Sets compareBy the next attribute that can be used for comparison.
      @author Miso
    */
    void compareByNext();
    /**
      Converts string label of the attribute to local representation as int.
      @author Miso
      @param attr String label of attribute.
      @return Local representation as int.
    */
    static int sortAttrToInt(const std::string attr);
    /**
      Converts local represetation of attribute to string. Useful for displaying attribute of current sorting.
      @author Miso
      @param attr Local representation of attribute (i.e. SongComparison.compareByAttribute).
      @return String label of the attribute.
    */
    static std::string intToSortAttr(int attr);
    /**
      Gets the current attribute of comparison.
      @author Miso
      @return String label of that attribute.
    */
    std::string getAttr();
    /**
      Attribute that is used as criterium for sorting.
    */
    int compareByAttribute;
};

#endif
