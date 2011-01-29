// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "AudioCircularBuffer.h"

// {{{ AudioCircularBuffer
AudioCircularBuffer::AudioCircularBuffer(){
    sampleRate=44100;
    channelCount=1;
}
// }}}

// {{{ ~AudioCircularBuffer
AudioCircularBuffer::~AudioCircularBuffer(){
}
// }}}

// {{{ size
int AudioCircularBuffer::size() const{
    return CircularBuffer<AudioSample>::size();

}
// }}}

// {{{ getSampleRate
double AudioCircularBuffer::getSampleRate() const{
    return sampleRate;
}
// }}}

// {{{ getChannelCount
int AudioCircularBuffer::getChannelCount() const{
    return channelCount;
}
// }}}

// {{{ insertAudioSamples
void AudioCircularBuffer::insertAudioSamples(AudioSamples& data){
    if (data.getChannelCount()!=channelCount) 
        throw EIllegalArgument("channel count mismatch");
    if (data.getSampleRate()!=sampleRate)
        throw EIllegalArgument("sample rate mismatch");
    for (int i=0; i<(int)data.buffer.size(); i++)
        insert(data.buffer[i]);
}
// }}}

// {{{ setAudioCharacteristics
void AudioCircularBuffer::setAudioCharacteristics(
        double rate, int channels){
    if (rate<10000) throw EIllegalArgument("sample rate should be at"
            " least 10000");
    if (channels<1) throw EIllegalArgument(
            "channel count should be at least 1");
    sampleRate=rate;
    channelCount=channels;
}
// }}}

// {{{ resize
void AudioCircularBuffer::resize(const int newSize){
    CircularBuffer<AudioSample>::resize(newSize);
}
// }}}
