// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_SONG
#define H_SONG

class Song;

#include <string>
#include <vector>
#include <SDL/SDL.h>
#include "Sentence.h"
#include <map>

/**
  Class with all informations about song.
  @author Miso
  @see SongList
  */

class Song{
  public:
    /**
      Default constructor.
      @author Miso
      @return Instance of the song.
      */
    Song();
    /**
      Destructor.
      @author Miso
      */
    ~Song();
    /**
      Constructs an instance with the name of the directory song is stored in.
      @author Miso
      @return Instance of the song.
      @param Name of the song directory.
      */
    Song(const std::string name);
    /**
      Loads all meta informations about song from file.
      @author Miso
      @return 0 if succeed, -1 if failed.
      @param Name of the file information is stored in.
    */
    int loadHeader(std::string filename);
    /**
      Loads all information about song(notes and lyrics) from file.
      @author Miso
      @return 0 if success, -1 if failed.
      @param Name of the file information is stored in.
      */
    int loadFull();
    /**
      Finds and returns the index of the sentence played at the time
      @param time Time at which the sentence is played.
      @return Index played at the time.
      @author Miso
      */
    int getSentenceIndexAt(double time);
    /**
      Finds and returns the sentence played at the time
      @param time Time at which the sentence is played.
      @return Sentence played at the time
      @author Miso
      */
    Sentence getSentenceAt(double time);
    /**
      Returns the full path to song directory.
      @return Concatented names of directories in path with "/".
      @author Miso
    */
    std::string getDirPath();
    /**
      Converts current beat to time in seconds from the start.
      @author Miso
      @param time Beat to convert.
      @return Time in seconds.
    */
    double getTimeSlot(int time);
    /**
      Checks if the song has ended by the time.
      @author Miso
      @param time Time to check song end at.
      @return true if the song ended, time>endTime
      */
    bool hasEnded(double time);
    /**
      Sums the length of all notes.
      @author Miso
      @return That sum.
    */
    double getActiveLength();
    /**
      Gets multiline info about song.
      @author Miso
      @return String containing all that info.
    */
    std::string getSongInfo();
    /**
      Name of the file information about song is stored in.
      */
    std::string fileName;
    /**
      Name of the file song can be played from.
      */
    std::string musicFileName;
    /**
      Name of the file cover of cd is stored in.
      */
    std::string cover;
    /**
      Name of the file song background is stored in.
      */
    std::string background;
    /**
      Edition of the song.
    */
    std::string edition;
    /**
      Name of the directory where are stored all files about song.
      */
    std::string dirname;
    /**
      Name of the directory where are stored all other songs.
    */
    std::string familyDirName;
    /**
      Title of the song.
      */
    std::string title;
    /**
      Artist of the song.
      */
    std::string artist;
    /**
      Time gap at the start of song in seconds.
    */
    double gap;
    /**
      Unique ID of the song.
    */
    int UID;
    int notesgap;
    int resolution;
    /**
      Bits per Minute.
    */
    int bpm;
    /**
      Flag if a source file is written in relative mode. Used in loading song.
      @see Song
      */
    bool relative;
    /**
      Time of end of the song in seconds.
      */
    double endTime;
    /**
      Sum of lengths of all notes in song.
    */
    double activeLength;
    //std::map<std::string,std::string> field;
    /**
      List of the sentences in the song.
      @see Sentence.
      */
    std::vector<Sentence> sentence;
};

#endif
