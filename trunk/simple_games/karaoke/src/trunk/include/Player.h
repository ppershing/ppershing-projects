// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_PLAYER
#define H_PLAYER

#include <string>
#include <vector>
#include "RecordedData.h"
#include "Note.h"

class Player{
  public:
    /**
      constructor
    */
    Player();
    /**
      destructor
    */
    ~Player();
    /**
      Finds index of the note player singed at given time.
      @author Miso
      @return Index of that note. -1 if none.
      @param time Time the note was singed.
      @see Note
    */
    int getNoteIndexAt(double time);
    /**
      Finds index of the first note player singed after the given time.
      @author Miso
      @return Index of that note. -1 if none.
      @param time Time after that we want to find singed note.
      @see Note
    */
    int getFirstNoteIndexAfter(double time);
    /**
      Finds index of the last note player singed before the given time.
      @author Miso
      @return Index of that note. -1 if none.
      @param time Time before that we want to find singed note.
      @see Note
    */
    int getLastNoteIndexBefore(double time);
    /**
      Finds note the player singed at given time.
      @author Miso
      @return Note signed at that time. -1 if none.
      @param time Time the note was singed.
      @see Note
    */
    Note getNoteAt(double time);
    /**
      Access to list of singed notes.
      @author Miso
      @param index Index of wanted note.
      @return Note at given position.
      @see Note
    */
    Note getNoteAtIndex(int index);
    /**
      Access to note currently is singing. Only usable when singing, so check it.
      @author Miso
      @return That note.
    */
    Note getCurrentSingingNote();
    /**
      Checks if the player is currently singing.
      @author Miso
      @return true if is singing, false if is quiet.
    */
    bool isSinging();
    /**
      Player is active and is singing. So update his sing history.
      If he is singing at the same pitch, do not push current singing note, only enlang it.
      Otherwise push that note and make new.
      @author Miso
      @param pitch Pitch the player is singing.
      @param time Current time of singing.
      @see Note
    */
    void sing(double pitch, double time, double target);
    /**
      Player is active and is singing. So update his sing history.
      If he is singing at the same pitch, do not push current singing note, only enlang it.
      Otherwise push that note and make new.
      Pitch is moved to octave (midPitch-6, midPitch+6).
      @author Miso
      @param frequency Frequency the player is singing.
      @param time Current time of singing.
      @param midPitch Frequency is converted to pitch and that is transposed by octaves so it will be
      the one that is nearest to midPitch.
      @see Note
    */
    void sing(int frequency, double time, int midPitch,bool scoring);
    /**
      If is a note active at given time, this splits it into two parts - one ends at given time, second start at that time.
      @author Miso
      @param time The time note would split.
      @see Note
    */
    void splitAtTime(double time,int newTargetPitch);
    /**
      Everything stays as is, but notes are cleared. Maybe next song started.
      @author Miso
    */
    void clearNotes();
    /**
      Read access to recorded data.
      @author Miso
      @param index Index in the array of records.
      @return Recorded data at given index.
    */
    RecordedData recordAt(int index);
    /**
      Returns the size of recorded data.
      @author Miso
      @return That size.
    */
    int recordedSize();
    int score;
  private:
    /**
      List of the notes the player singed.
    */
    std::vector<Note> singed;
    /**
      Name of the player.
    */
    std::string name;
    /**
      Note the player is singing at current time.
    */
    Note currentSingingNote;
    /**
      True if player is singing now. False otherwise (player is quiet).
    */
    bool singing;
    /**
      Stores recorded pitches through time.
    */
    std::vector<RecordedData> recorded;
    double timeUnit;
};

#endif
