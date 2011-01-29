// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_SENTENCE
#define H_SENTENCE

class Sentence;

#include <string>
#include <vector>
#include <SDL/SDL.h>
#include "Note.h"
#include "Syllable.h"

class Sentence{
  public:
    /**
      Default constructor.
    */
    Sentence();
    /**
      Constructor of empty sentence with one full-time syllable and zero notes.
    */
    Sentence(const std::string& text,double st,double dur);
    /**
      Resets all data in sentence.
      @author Miso
    */
    void clear();
    /**
      Finds if any note is playing at given time.
      @author Miso
      @param time Time the note should be played at.
      @return True if any note is playing at the time, false otherwise.
      @see Note

    */
    bool isNoteAt(double time);
    /**
      Finds note playing at given time. If none, throws an exception.
      @author Miso
      @param time Time the note is playing at.
      @return Note playing at the time.
      @see Note
    */
    Note getNoteAt(double time);
    /**
      Finds if any syllable is playing at given time.
      @author Miso
      @param time Time the syllable should be played at.
      @return True if any syllable is playing at the time, false otherwise.
      @see Syllable
    */
    bool isSyllableAt(double time);
    /**
      Finds the syllable played at given time and returns it's index.
      @author Miso
      @param time Time the syllable should be played at.
      @return Index of the syllable playing at given time. -1 if none playing.
      @see Syllable
    */
    int getSyllableIndexAt(double time);
    /**
      Finds the syllable played at given time and returns it. If none, throws an exception.
      @author Miso
      @param time Time the syllable should be played at.
      @return Syllable playing at given time.
      @see Syllable
    */
    Syllable getSyllableAt(double time);
    /**
      Iterates through all notes and finds the one with the lowest pitch.
      @author Miso
      @return The lowest pitch of that note.
      @see Note
    */
    double getMinPitch();
    /**
      Iterates through all notes and finds the one with the highest pitch.
      @author Miso
      @return The highest pitch of that note.
      @see Note
    */
    double getMaxPitch();
    /**
      Finds the range of pitches of all notes in this sentence.
      @author Miso
      @return The difference of highest and lowest pitch.
      @see Note
    */
    double getPitchRange();
    /**
      Finds the middle point of min and max pitch.
      @author Miso
      @return The middle pitch.
      @see Note
    */
    double getMidPitch();
    /**
      Force sets end to all notes and syllables to given time.
      @author Miso
      @param time Time in seconds where they must end.
      @see Note
      @see Syllable
    */
    void setEnds(double time);
    /**
      Sums the lengths of all the notes.
      @author Miso
      @return That sum in seconds.
    */
    double getActiveLength();
    /**
      Iterates through syllables and concatenates their text.
      @author Miso
      @return The concatenated text of syllables.
      @see Syllable
    */
    std::string getFullText();
    /**
      Iterates through syllables and divides them to three groups:
      In the first are syllables played before given time,
      in the second syllable played at given time
      and in the last are syllables playing after given time.
      @author Miso
      @return Vector of three strings - concatenated texts of syllables in particular group.
      @see Syllable
    */
    std::vector<std::string> getFullTextMarked(double time);
    /**
      Iterates through notes and syllables and finds smallest time
      when all of them are finished.
      @author Miso
      @return Time of the last note or syllable end.
      @see Note
      @see Syllable
    */
    double getLastNoteOrSyllableTime();
    /**
      Finds time when all notes and syllables are finished and updates own duration.
      @author Miso
      @return Duration of this Sentence.
      @see Note
      @see Syllable
    */
    double computeDuration();
    /**
      List of notes in the sentence.
      @see Note
    */
    std::vector<Note> note;
    /**
      List of syllables int the sentence.
      @see Syllable
    */
    std::vector<Syllable> syllable;
    /**
      Time of start in seconds.
    */
    double start;
    /**
      Duration time in seconds.
    */
    double duration;
};

#endif
