// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk


#include "AudioUtils.h"
#include "Exceptions.h"
#include "Debug.h"
#include "MyStringUtils.h"
#include "AudioSamples.h"
#include <cmath>

#include <stdio.h>

// TODO: mix, sfx
// TODO: rechannel other than 2->1

AudioSamples::AudioSamples AudioUtils::mix(
  const AudioSamples::AudioSamples& data1, const double volume1,
  const AudioSamples::AudioSamples& data2, const double volume2){
    throw ENotImplemented("AudioUtils::mix");
}


// {{{ resample(AudioSamples,targetRate)
AudioSamples::AudioSamples AudioUtils::resample(
  AudioSamples::AudioSamples& data,
  const double targetRate){
    return
        resample(data.buffer,data.sampleRate,targetRate,data.channelCount,data.referenceTime);
}
// }}}

// {{{ resample(vector data,sourceRate,targetRate,channelCount,time)
AudioSamples::AudioSamples AudioUtils::resample(
  std::vector<AudioSample>& data,const double sourceRate,
  const double targetRate,const int channelCount,const double
  referenceTime){

    if ( std::abs(sourceRate-targetRate)<0.1) 
        return AudioSamples(data,referenceTime,targetRate,channelCount);

    if (data.size()==0) {
        return AudioSamples::EmptyAudioSamples;
    }

  /**
    slow resampling, TODO: use some library?
    */


  Assert(data.size()>0,"no data");
  Assert(channelCount>0,"wrong number of channels");
  Assert(sourceRate>0,"wrong source rate");
  Assert(data.size()%channelCount==0,"resample:: illegal buffer "
          "size");

  const int sourceLength=data.size()/channelCount;
  const double timeFactor=targetRate/sourceRate;

  const int targetLength=(int) (sourceLength* timeFactor+0.5);

  std::vector<AudioSample> buffer;
  buffer.resize(targetLength*channelCount);

  for (int channel=0; channel< channelCount; channel++) {
      for (int q=0; q<targetLength; q++) {
          double sourcePos=(int) (q / timeFactor);
          int p1=(int) sourcePos;
          double weight=sourcePos-p1;

          while (p1>=sourceLength-1) p1=sourceLength-2;
          int p2=p1+1;
          
          
          buffer[channel+q*channelCount]=
               weight*data[channel+p1*channelCount] +
               (1-weight)*data[channel+p2*channelCount];
               

      }
  }


  return
      AudioSamples(buffer,referenceTime,targetRate,channelCount);

}
// }}}

// {{{ rechannel
AudioSamples AudioUtils::rechannel(AudioSamples& data, const int
        targetChannels){
    if (data.getLength()==0) return data;
    if (data.getChannelCount()==targetChannels) return data;
    if (targetChannels!=1) throw ENotImplemented("rechannel only 2->1");

    int sourceChannels=data.getChannelCount();

    std::vector<AudioSample> result;
    for(int q=0; q<data.getLength()/2; q++)
        result.push_back(data.buffer[q*sourceChannels]);

    return AudioSamples(result,data.referenceTime,data.getSampleRate(),targetChannels);
}
// }}}
        
