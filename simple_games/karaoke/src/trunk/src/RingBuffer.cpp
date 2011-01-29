// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

// AudioSamples include RingBuffer.h, can't be changed because of
// friend functions
#include "AudioSamples.h"
#include "Exceptions.h"
#include "MyAssert.h"
#include "Exceptions.h"
#include "NoDebug.h"
#include "MyStringUtils.h"
#include <cmath>


// {{{ RingBuffer
RingBuffer::RingBuffer(const int bufferSize, const double
        sampleRate,const int channels):
    timeReference(0),
    sampleRate(sampleRate),
    channelCount(channels),
    data(std::vector<AudioSample>(bufferSize)),
    start(0),
    end(0),
    isReading(0),
    isWriting(0)
{
    if (sampleRate<=0) throw EIllegalArgument(
            "Sample rate is non positive");
}
// }}}

// {{{ ~RingBuffer
RingBuffer::~RingBuffer(){
}
// }}}

// {{{ extractStart
inline AudioSample RingBuffer::extractStart(){
   // DEBUG("extract");
   // assert removed for performance reasons - modulo is slow!
   //  assert(getDataSize()>0,"RingBuffer:extractStart no data");
    AudioSample a=data[start];
    ++start;
    if (start>=getSize()) start-=getSize();
    return a;
}
// }}}

// {{{ writeEnd
inline void RingBuffer::writeEnd(AudioSample sample){
 //DEBUG("write end");
    // assert removed for performance reasons - modulo is slow
    //assert(getFreeSize()>0,"RingBuffer:WriteEnd no free space");
    data[end]=sample;
    ++end;
    if (end>=getSize()) end-=getSize();
}
// }}}

// {{{ writeToFloats
void RingBuffer::writeToFloats(float* buffer,int length, const double
        volume){

    DEBUG("write to buffer");
    if (volume<0 || volume>1) throw EIllegalArgument(
            "volume not in 0..1");

    if (length>getDataSize()) throw EIllegalArgument(
        "Ringbuffer::writeToFloats wants to write more data than in ring");
    ++isReading;
    Assert(isReading==1,"RingBuffer: concurrent reads");

    double oldTimeReference = timeReference;

    for (int i=0;i<length; i++) {
        buffer[i]= volume*extractStart();
     }
    
    timeReference=oldTimeReference+length/sampleRate/channelCount; // this is nasty
    // hack - we want to allow +-correct time reference on concurrent
    // setting time by audio callback

    --isReading;
}
// }}}

// {{{ writeFromFloats
void RingBuffer::writeFromFloats(float* buffer, int length,const
        double volume){
    DEBUG("write from buffer");

    if (volume<0 || volume>1) throw EIllegalArgument(
            "volume not in 0..1");

if (length>getFreeSize()) throw EIllegalArgument(
        "Ringbuffer::writeFromFloats wants to write more data than "
        "free in ring");
    ++isWriting;
    Assert(isWriting==1,"RingBuffer: concurrent writes");

    for (int i=0;i<length;i++){
        writeEnd(buffer[i]*volume);
    }

    --isWriting;
}
// }}}

// {{{ writeToSamples
AudioSamples RingBuffer::writeToSamples(int length){
    // throw(EIllegalArgument)

    DEBUG("write to buffer");

    if (length%channelCount!=0) throw EIllegalArgument(
            "Ringbuffer::writeToSamples length not aligned");


    if (length>getDataSize()) throw EIllegalArgument(
        "Ringbuffer::writeToSamples wants to write more data than in ring");

    ++isReading;
    Assert(isReading==1,"RingBuffer: concurrent reads");

    double oldTimeReference = timeReference;

    AudioSamples data=
         AudioSamples((double*)1,0,sampleRate,channelCount);

    data.buffer.resize(length);

    for (int i=0;i<length; i++) {
        data.buffer[i]= extractStart();
     }
    data.referenceTime=oldTimeReference;

    timeReference=oldTimeReference+length/sampleRate/channelCount; // this is nasty
    // hack - we want to allow +-correct time reference on concurrent
    // setting time by audio callback

    --isReading;
    return data;
}
// }}}

// {{{ writeFromSamples
void RingBuffer::writeFromSamples(AudioSamples& data){
   // throw (EIllegalArgument)
    DEBUG("write from buffer");

    const int length=data.getLength();
    if (length==0) return; // nothing to do, suppress warnings

    if (data.getChannelCount()!=channelCount) throw
        EIllegalArgument("Ringbuffer::writeFromSamples "
                "wrong number of channels");
    if (std::abs(data.getSampleRate()-sampleRate)>0.1) throw
        EIllegalArgument("Ringbuffer::writeFromSamples "
                "wrong sample rate");

    Assert(length%channelCount==0,"AudioSamples corrupted - not"
           " aligned");

    if (length>getFreeSize()) throw EIllegalArgument(
        "Ringbuffer::writeFromSamples wants to write more data than "
        "free in ring");

    ++isWriting;
    Assert(isWriting==1,"RingBuffer: concurrent writes");

    for (int i=0;i<length;i++){
        writeEnd(data.buffer[i]);
    }

    --isWriting;
}
// }}}

// {{{ getEndTimeReference
double RingBuffer::getEndTimeReference(){
    return timeReference+getDataSize()/sampleRate/channelCount;
}
// }}}

// {{{ setEndTimeReference
void RingBuffer::setEndTimeReference(const double time){
    timeReference=time-getDataSize()/sampleRate/channelCount;
}
// }}
