// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "AudioSamples.h"
#include "MyStringUtils.h"
#include <algorithm>

#include "Debug.h"

AudioSamples AudioSamples::EmptyAudioSamples((AudioSample*) 1,0,0,1); // emty


// {{{ AudioSamplesInit
void AudioSamples::AudioSamplesInit(AudioSample* buffer,
  const int length){
    //throw (EIllegalArgument)

    if (buffer==NULL) throw
        EIllegalArgument("AudioSamples: null buffer");
    if (length%channelCount!=0) throw
        EIllegalArgument("AudioSamples: try to"
                " construct samples not aligned to channels");


    this->buffer.resize(length);
    std::copy(buffer,buffer+length,this->buffer.begin());
}
// }}}

// {{{ AudioSamples(AudioSample* buffer, length,
// sampleRate,channelCount
AudioSamples::AudioSamples(AudioSample* buffer,
        const int length, const double sampleRate,
        const int channelCount): //throw (EIllegalArgument)
referenceTime(0),channelCount(channelCount),sampleRate(sampleRate)
{
    AudioSamplesInit(buffer,length);
}
// }}}

// {{{ AudioSamples(AudioSample* buffer, length, time
AudioSamples::AudioSamples(AudioSample* buffer,
        const int length, const double time, const double sampleRate,
        const int channelCount): //throw (EIllegalArgument)
referenceTime(time),channelCount(channelCount),sampleRate(sampleRate)
{
    AudioSamplesInit(buffer,length);
}
// }}}

// {{{ AudioSamples(vector<AudioSample>buffer, sampleRate,
// channelCount
AudioSamples::AudioSamples(std::vector<AudioSample>& buffer,
        const double sampleRate, const int channelCount):
referenceTime(0),channelCount(channelCount),sampleRate(sampleRate)
{
    
    AudioSamplesInit(&buffer[0],buffer.size());
}
// }}}

// {{{ AudioSamples(vector<AudioSample>buffer, time
// channelCount
AudioSamples::AudioSamples(std::vector<AudioSample>& buffer,
        const double time,
        const double sampleRate, const int channelCount):
referenceTime(time),channelCount(channelCount),sampleRate(sampleRate)
{
    
    AudioSamplesInit(&buffer[0],buffer.size());
}
// }}}

// {{{ ~AudioSamples
AudioSamples::~AudioSamples(){
}
// }}}

// {{{ writeTo
void AudioSamples::writeTo(AudioSample* buffer,const int start,
  const int length) const { //throw (EIllegalArgument)
    if (start%channelCount!=0) throw
        EIllegalArgument("AudioSamples:writeTo start not aligned");
    if (length%channelCount!=0) throw
        EIllegalArgument("AudioSamples:writeTo length not aligned");

    if (start+length> (int) this->buffer.size()) 
         throw EIllegalArgument("AudioSamples:writeTo (length out of "
             "bounds ");

   std::copy(this->buffer.begin()+start,
             this->buffer.begin()+start+length,
             buffer);
}
// }}}

// {{{ append
void AudioSamples::append(const AudioSample* buffer,const int length,
        const double sampleRate, const int channelCount) {
  //throw (EIllegalArgument)

    if (buffer==NULL) throw 
        EIllegalArgument("AudioSamples::append: null buffer");

    if (length==0) return; // not going to append

    if (this->sampleRate!=sampleRate) throw
         EIllegalArgument("AudioSamples:: append: "
                 "not compatible sampleRate");

    if (this->channelCount!=channelCount) throw
        EIllegalArgument("AudioSamples::append: "
                "not compatible channelCount");

    std::vector<AudioSample>::iterator it=this->buffer.end();
    this->buffer.resize(this->buffer.size()+length);
    std::copy(buffer,buffer+length,it);
}
// }}}

// {{{ writeToAndDelete
void AudioSamples::writeToAndDelete(AudioSample* buffer,const int start,
  const int length){ //throw (EIllegalArgument)

    writeTo(buffer,start,length); // we have checked input
    this->buffer.erase(this->buffer.begin()+start,
            this->buffer.begin()+start+length);

    if (this->sampleRate!=0) {
        referenceTime+=((double)length / (double) channelCount /
                sampleRate);

    }
}

// }}}

// {{{ getLength
int AudioSamples::getLength() const{
    return this->buffer.size();
}
// }}}

// {{{ getSampleCount
int AudioSamples::getSampleCount() const {
    return getLength()/channelCount;
}
// }}}

// {{{ getSampleRate
double AudioSamples::getSampleRate() const{
    return sampleRate;
}
// }}}

// {{{ getChannelCount
int AudioSamples::getChannelCount() const{
    return channelCount;
}
// }}}

// {{{ zeroSamples
AudioSamples AudioSamples::zeroSamples( const int length,
        const double time, const double sampleRate,
        const int channelCount){
    std::vector<AudioSample> tmp=std::vector<AudioSample>(length);
    return AudioSamples(tmp,time,sampleRate,channelCount);

}

// }}}

// {{{ getEndTime
double AudioSamples::getEndTime() const {
  return referenceTime+getLength()/sampleRate/channelCount;

}
// }}}
