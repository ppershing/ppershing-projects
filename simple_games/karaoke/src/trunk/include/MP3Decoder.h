// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_MP3_DECODER
#define H_MP3_DECODER

#include <fstream>
#include <string>
#include <vector>
#include "Exceptions.h"
#include "AudioSamples.h"
#include <mad.h>

/*
MP3Decoder is class designed for reading mp3 files
Warning: MP3Decoder isn't thread-safe, please call it only from one thread
   */

class MP3Decoder {
  public:        
    /**
      constructor, opens file (if error occured throw ECantOpenFile)
      and initializes all internal data
      */
    MP3Decoder(const std::string& fileName);
     //throws(ECantOpenFile);

    /**
      get next frame from mp3 file,
      throw EIOError, if there was problem reading data
      throw EAudioDecode, if there was problem with corrupted audio
      data
      */
    AudioSamples getNextFrame();
    //throws(EIOError,EAudioDecode);

    /**
      returns false, if decoder can't do more decoding
      */
    bool haveNextFrame();

    /**
      destructor
      */
    ~MP3Decoder();

  private:
    /**
      private copy/assignment operator not implemented,
      class isn't designed to be copyable
      */
    MP3Decoder& operator=(const MP3Decoder& decoder);
    MP3Decoder(const MP3Decoder& decoder);

    /**
      internal buffer size, shoud be enough to hold one frame of any 
      mp3 file
      */
    static const int INPUT_BUFFER_SIZE=5*8192;

    /**
      internal constants - error states 
      */
    static const int DECODE_FRAME_OK=1;
    static const int DECODE_FRAME_BUFFER=2;
    static const int DECODE_FRAME_RECOVERABLE=3;
    static const int DECODE_FRAME_UNRECOVERABLE=4;
    
    /**
      mad structures
      */
    struct mad_stream	Stream;
    struct mad_frame	Frame;
    struct mad_synth	Synth;

    /**
      file reading stream
      */
    std::ifstream       inputFileStream;

    /**
      mp3 name, (used to log)
      */
    const std::string         inputFileName;

    /**
      actual input buffer
      */
    unsigned char  InputBuffer[INPUT_BUFFER_SIZE+MAD_BUFFER_GUARD];

    
    unsigned char *GuardPtr;

    /**
      holds actual time of start of first non-decoded frame
      */
    double actualTime;

    /**
      convert mad error code to string representation
      */
    std::string getMadErrors(const int errorCode);

    /**
      if input buffer underrun, refill buffer
      */
    int readFileIfNeeded();
     //throws(EIOError);

    /**
      converts mad data to pcm data
      */
    std::vector<AudioSample> synthetize();

    /**
      decodes one frame, return error code
      */
    int decodeFrame();

    /**
      we have ended decoding
      */
    bool eof;

    /**
      suppress first frame warning
      */
    bool firstFrame;
};

#endif
