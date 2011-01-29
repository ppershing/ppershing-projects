// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_NOTE
#define H_NOTE

#include <string>
#include <vector>
#include <SDL/SDL.h>

class Note{
  public:
    /**
      constructor
    */
    Note();
    /**
      destructor
    */
    ~Note();
    /**
      Converts pitch to frequency with respect to transpose.
      @author Miso
      @param pitch Pitch which will be transposed and converted to frequency.
      @param transpose Difference in pitch.
      @return Converted frequency.
    */
    static int pitchToFrequencyWithTranspose(double pitch, int transpose);
    /**
      Converts pitch to frequency with default no transpose.
      @author Miso
      @param pitch Pitch which will be converted to frequency.
      @return Converted frequency.
    */
    static int pitchToFrequency(double pitch);
    /**
      Converts frequency to pitch and transposes it.
      @author Miso
      @param frequency Frequency to convert.
      @param tranpose Difference in pitch.
      @return Converted pitch.
    */
    static double frequencyToPitchWithTranspose(int frequency, int transpose);
    /**
      Converts frequency to pitch with default no transpose.
      @author Miso
      @param frequency Frequency to convert.
      @return Converted pitch.
    */
    static double frequencyToPitch(int frequency);
    /**
      Normalizes pitch so it will be the one of pitches transposed by
      multiple of 12, which is one octave. Returns the pitch that is the most close
      to target.
      @author Miso
      @param pitch Pitch to normalize.
      @param target Target pitch which we want to be close to.
      @return Normalized pitch most close to target.
    */
    static double normalizePitch(double pitch,double target);
    /**
      Time of start in seconds.
    */
    double start;
    /**
      Duration time in seconds.
    */
    double duration;
    int pitch;
};

#endif
