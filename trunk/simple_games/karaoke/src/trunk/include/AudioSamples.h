// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_AUDIO_SAMPLES
#define H_AUDIO_SAMPLES

#include "Exceptions.h"
#include <vector>

typedef double AudioSample;

/*
   ok, this workaround is to allow Ringbuffer to be friend of
   AudioSamples for providing fastest access to data (no need of copy)
   */
class AudioCircularBuffer;
class AudioSamples;
#include "RingBuffer.h"

/*
   basic class for transfering chunks of audio samples
   between various parts of program
   */
class AudioSamples{
    public:
        friend void RingBuffer::writeFromSamples(AudioSamples& data);
        friend class AudioCircularBuffer;
        friend class AudioUtils;
        /**
           constructor, copies buffer to internal buffer
           */
        AudioSamples(AudioSample *buffer,
                     const int length,const double sampleRate,
                     const int channelCount); 
        //throws(EIllegalArgument);

        /**
          constructor, with time field
          */
        AudioSamples(AudioSample *buffer,
                     const int length,const double time,
                     const double sampleRate,
                     const int channelCount);
        //throws(EIllegalArgument);

        /**
          constructor from vector
          */
        AudioSamples(std::vector<AudioSample>& buffer,const double
                sampleRate,const int channelCount);

        /**
          constructor from vector with time field
          */
        AudioSamples(std::vector<AudioSample>& buffer,const double
                time,const double
                sampleRate,const int channelCount);
        /*
           destructor, deallocates internal buffer
           */
        ~AudioSamples();

        static AudioSamples zeroSamples(const int length,
                 const double time,
                 const double sampleRate,
                 const int channelCount);

        /*
           method for writing out buffer,
           throws EIllegalArgument if start and length not aligned 
           properly to channels
           */
        void writeTo(AudioSample *buffer,const int start,const int
                length) const;
        //throws (EIllegalArgument);

        /*
           method for appending another buffer to this,
           throws EIllegalArgument if trying to append incompatible 
           data
            */
        void append(const AudioSample *buffer,const int length,
                const double sampleRate, const int channelCount);
          //  throws (EIllegalArgument);

        /*
           method for moving data, automatically update reference time
            */
        void writeToAndDelete(AudioSample *buffer,
                const int start,const int length);
        //throws (EIllegalArgument);
        
        /*
           returns the length of the buffer
           */
        int getLength() const;

        /**
          returns the length of the buffer
          but in samples (i.e divide by channel count)
          */
        int getSampleCount() const;

        /*
           time reference of buffer start
           */
        double referenceTime;

        /**
          reference to empty data (because of lack of default
          constructor)
          */
        static AudioSamples EmptyAudioSamples;

        /**
          internal buffer
          */
        std::vector<AudioSample> buffer;

        /**
          return sample rate (note that this is not actual
          data rate, data rate=sample rate* channels
          */
        double getSampleRate() const;

        /**
          return channel count of data
          */
        int getChannelCount() const;

        double getEndTime() const;

    private:
        /**
          'constructor' - other constructors
          are redirected here
          */
        void AudioSamplesInit(AudioSample *buffer,
            const int length);
        //throws (EIllegalArgument);


        /**
          number of channels
          */
        int channelCount;

        /**
          sample rate
          */
        double sampleRate;

};



#endif
