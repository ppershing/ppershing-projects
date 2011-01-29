// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

// TODO: mixing SFX to playback and SFX functions


#ifndef H_AUDIO
#define H_AUDIO

#include "Exceptions.h"
#include "Errors.h"
#include <string>
#include <portaudio.h>
#include "AudioSamples.h"



/**
  callback data that are passed to internal function
  which is portaudio callback, we need to pass pointers
  to ringbuffers so we can use it (callback is not
  within Audio class
  */
typedef struct {
    RingBuffer* play;
    RingBuffer* rec;
    int inputChannels;
    int outputChannels;
    PaStream *stream;
    double microphoneTimeCorrection;
    double *playbackVolume;
    double *recordVolume;
    double *SFXVolume;
    double *lastTime;
} CallbackData;

/**
  Main class for Audio
  initializes and plays audio,
  currently are two independent methods for playing data
  a) SFX (Simple sounds, played asynchrounly, instantly, only few at
  time)
  b) ringbuffer - previous data are mixed against ringbuffer which
  whould be periodically filled by application.
  This technique allows program to play various data without 
  dependence of function that generates data
  */
class Audio{
	public:
            /**
              create instance, throw ESingletonInstance if one exists
              */

            static void createInstance(); // throws (ESingletonInstance);

            /**
              destroy instance, throw ESingletonInstance if no one
              exists
              */
            static void destroyInstance(); // throw (ESingletonInstance);

            /**
              returs instance, or throw ESingletonInstance if no one
              exists
              */
            static Audio* getInstance(); // throw (ESingletonInstance);

            /**
              init audio
              @param sampleRate inits desired sample rate
              @param errorMessage will be filled with error (if
              occured)
              @returns true on success, false otherwise
              */
            int init(const int sampleRate,const int inputChannels,
                    const int outputChannels, const double
                    micTimeCorrection,std::string& errorMessage);

            /**
              shorthand to init
              */
            static int _init(const int sampleRate,const int inputChannels,
                    const int outputChannels, const double
                    micTimeCorrection,std::string& errorMessage);
               // throws (ESingletonInstance);

            /**
              closes audio streams and disable audio
              @param errorMessage will be filled if error occured
              @returns true on success, false otherwise
              */
            int finalize(std::string& ErrorMessage);

            /**
              shorthand to finalize
              */
            static int _finalize(std::string& ErrorMessage);
                //throws (ESingletonInstance);

            /**
              @return sample rate of audio
              */
            int getSampleRate() const;

            /**
              shorthand to getSampleRate
              */
            static int _getSampleRate(); // throws(ESingletonInstance);

            /**
              @return playback ring
              */
            RingBuffer* getPlaybackRing() const;

            /**
              shorthand to getPlaybackRing
              */
            static RingBuffer* _getPlaybackRing(); // throws(ESingletonInstance);

            /**
              @return record ring
              */
            RingBuffer* getRecordRing() const;

            /**
              shorthand to getRecordRing
              */
            static RingBuffer* _getRecordRing(); // throws(ESingletonInstance);


            /**
              set playback volume - allowed range 0.0-1.0
              */
            void setPlaybackVolume(const double volume);
               // throws(EIllegalArgument);

            /**
              shorthand to setPlaybackVolume
              */
            static void _setPlaybackVolume(const double volume);
               // throws(EIllegalArgument,ESingletonInstance);

            /**
              returns actual playback volume
              */
            double getPlaybackVolume() const;

            /**
              shorthand to getPlaybackVolume
              */
            static double _getPlaybackVolume();
                //throws(ESingletonInstance);


            /**
              set record volume - allowed range 0.0-1.0
              */
            void setRecordVolume(const double volume);
               // throws(EIllegalArgument);

            /**
              shorthand to setRecordVolume
              */
            static void _setRecordVolume(const double volume);
              //  throws(EIllegalArgument,ESingletonInstance);

            /**
              returns actual record volume
              */
            double getRecordVolume() const;

            /**
              shorthand to getRecordVolume
              */
            static double _getRecordVolume();
               // throws(ESingletonInstance);


            /**
              set SFX volume - allowed range 0.0-1.0
              */
            void setSFXVolume(const double volume);
               // throws(EIllegalArgument);

            /**
              shorthand to setSFXVolume
              */
            static void _setSFXVolume(const double volume);
               // throws(EIllegalArgument,ESingletonInstance);

            /**
              returns actual SFX volume
              */
            double getSFXVolume() const;

            /**
              shorthand to getSFXVolume
              */
            static double _getSFXVolume();
               // throws(ESingletonInstance);

            /**
              get current time
              */
            double getCurrentTime();

            /**
              shorthand to getCurrentTime
            */
            static double _getCurrentTime();

	private:
            /**
              constructor
              */
            Audio();

            /**
              destructor
              */
            ~Audio();


            /**
              not to be implemented, it is singleton
              */
            Audio(const Audio&);

            /**
              not to be implemented, it is singleton
              */
            Audio& operator= (const Audio&);
            
            /**
              audio sample rate
              */
            int sampleRate;

            /**
              singleton instance
              */
            static Audio* instance;	

            /**
              portaudio playback/record stream
              */
            PaStream* stream;

            /**
              playback ringbuffer
              */
            RingBuffer* playbackRing;

            /**
              record ringbuffer
              */
            RingBuffer* recordRing;

            /**
              size of buffer to be passed by callback function
              */
            static const int BUFFER_SIZE=1024;

            /**
              structure passed to callback function
              */
            CallbackData callbackData;

            /**
              actual volumes
              */
            double playbackVolume,recordVolume,SFXVolume;

            /**
              microphone channels
              note that currently we support only mono input, shoud be
              enought
              */
            int inputChannels;

            /**
              playback channels
              note that currently we support only stereo output,
              should be enough
              */
            int outputChannels;

            double microphoneTimeCorrection;

            /**
              create one second long ring buffers
              */
            void createRings(const double sampleRate,const int
                    inputChannels,const int outputChannels);

            /**
              delete allocated ringbuffers
              */
            void deleteRings();

            void fillCallbackData();

            double lastTime;
};


#endif
