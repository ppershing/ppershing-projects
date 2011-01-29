// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_AUDIO_CIRCULAR_BUFFER
#define H_AUDIO_CIRCULAR_BUFFER

#include "AudioSamples.h"
#include "CircularBuffer.h"
#include <vector>
#include <string>


/**
  class providing CircularBuffer specialization
  for AudioSamples 
  specilization includes
  importing from AudioSamples,
  fully checked against sample rate and channel count
  */
class AudioCircularBuffer: public CircularBuffer<AudioSample>{
  public:
      /**
        inserts data into buffer,
        automatically checks sampleRate and channel count
        */
        void insertAudioSamples(AudioSamples& data);

        /**
          constructor,
          default sample rate is 44100, default channel count is 1
          */
        AudioCircularBuffer();

        /**
          destructor
          */
        ~AudioCircularBuffer();

        /**
          set new audio sample rate and channel count
          */
        void setAudioCharacteristics(const double rate,const int channels);

        /**
          resize internal buffer
          */
        void resize(const int newSize);

        int getChannelCount() const;
        double getSampleRate() const;
        int size() const;

  private:
        double sampleRate;
        int channelCount;
};

#endif
