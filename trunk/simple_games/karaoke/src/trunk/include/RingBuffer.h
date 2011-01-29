// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk
#ifndef H_RING_BUFFER
#define H_RING_BUFFER

#include "AudioSamples.h"
#include "Errors.h"
#include "MyAssert.h"
#include <vector>

/**
  General cyclic buffer for audio synchronizing,
  should be fast enough to handle 50M writes/sec
  optimal inserting/writing operation is 100-200samples per call
  */
class RingBuffer {
  public:
    /**
      constructor
      */
    RingBuffer(const int bufferSize,const double sampleRate,const int
            channels);

    /**
      destructor
      */
    ~RingBuffer();

    /**
      get total size of ringbuffer
      */
    int getSize() const;

    /**
      get free storage size
      */
    int getFreeSize() const;

    /**
      get stored data size
      */
    int getDataSize() const;

    /**
      writes (and deletes) samples from ringbuffer
      to float* buffer, performs multiplying by volume
      note that this function should by used only
      in privite (but we can't make callback in class,
      so can't be friend)
      */
    void writeToFloats(float* buffer,const int length, const double volume);

    /**
      inserts samples from float* buffer, performs multiplying by
      volume
      note that this function should by used only by audio callback
      */
    void writeFromFloats(float* buffer,const int length, const double volume);

    /**
      default way to get data from ringbuffer - 
      creates AudioSamples of given length (sampleRate and
      channelCount) and deletes
      readed data,
      if length not aligned, throw EIllegalArgument
      */
    AudioSamples writeToSamples(const int length);
    //throws(EIllegalArgument);

    /**
      default way to insert data to ringbuffer,
      checks data against same sample rate, channel count and aligment
      of length
      */
    void writeFromSamples(AudioSamples& data);
    //throws (EIllegalArgument);

    /**
      time reference to buffer start - should be used for
      synchronizing streams, it is automatically
      updated by read operation (and audio callback)
      */
    volatile double timeReference;

    /**
      automaticallt calculates time reference of one after last
      byte (half open interval)
      */
    double getEndTimeReference();

    /**
      set timeReference when we have only time to end of the buffer
      */

    void setEndTimeReference(const double time);



    /**
      sample rate of buffer, used to update timeReference
      */
    const double sampleRate;

    /**
      number of channels of buffer
      */

    const int channelCount;

  private:
    /**
      extracts one sample
      */
    AudioSample extractStart();

    /**
      stores one sample
      */
    void writeEnd(const AudioSample sample);

    /**
      actual stored data
      */
    std::vector<AudioSample> data;

    /**
      indexes to start and (end+1) positions of data
      */
    int start,end;

    /**
      synchronization checking - ringbuffer
      is capable of simultaneous read/write but
      not read/read or write/write - in this
      case it throw concurrent modification error
      */
    volatile int isReading,isWriting;
};

// {{{ getDataSize
inline int RingBuffer::getDataSize() const{
    return (data.size()+end-start)%data.size();
}
// }}}

// {{{ getSize
inline int RingBuffer::getSize() const{
    Assert(data.size()>0,"ringbuffer problem");
    return data.size();
}
// }}}

// {{{ getFreeSize
inline int RingBuffer::getFreeSize() const{
    return getSize()-getDataSize()-1; 
}
// }}}


#endif
