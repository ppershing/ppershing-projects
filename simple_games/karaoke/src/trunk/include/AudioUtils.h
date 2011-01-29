// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_AUDIOUTILS
#define H_AUDIOUTILS

#include "AudioSamples.h"
#include "Exceptions.h"

/**
  static class implementing needed audio transformations
  */
class AudioUtils{
    public:
    /**
      resample data from one sample rate to another,
    TODO: not implemented yet
      */
    static AudioSamples resample(
            AudioSamples& data,
            const double targetRate);

    /**
      resample data from one sample rate to another
    TODO: not implemented yet
      */
    static AudioSamples resample(
            std::vector<AudioSample>& data,
            const double sourceRate,
            const double targetRate,const int channelCount,
            const double referenceTime);

    /**
      mix data together
    TODO: not implemented yet
      */
    static AudioSamples mix(
            const AudioSamples& data1, const double volume1, 
            const AudioSamples& data2, const double volume2);    

    static AudioSamples rechannel(AudioSamples& data, const int
        targetChannels);


};

#endif
