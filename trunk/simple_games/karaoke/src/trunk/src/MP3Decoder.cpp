// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include <cmath>
#include <stdlib.h>
#include <string>
#include <mad.h>

#include "MP3Decoder.h"
#include "Errors.h"
#include "AudioUtils.h"
#include "Audio.h"
#include "Debug.h"

// {{{ int readFileIfNeeded, returns true if readed something
int MP3Decoder::readFileIfNeeded(){ //throw (EIOError)
    if(Stream.buffer==NULL || Stream.error==MAD_ERROR_BUFLEN) {
        size_t		ReadSize,Remaining;
        unsigned char	*ReadStart;

        /* {{{ {2} libmad may not consume all bytes of the input
         * buffer. If the last frame in the buffer is not wholly
         * contained by it, then that frame's start is pointed by
         * the next_frame member of the Stream structure. This
         * common situation occurs when mad_frame_decode() fails,
         * sets the stream error code to MAD_ERROR_BUFLEN, and
         * sets the next_frame pointer to a non NULL value. (See
         * also the comment marked {4} bellow.)
         *
         * When this occurs, the remaining unused bytes must be
         * put back at the beginning of the buffer and taken in
         * account before refilling the buffer. This means that
         * the input buffer must be large enough to hold a whole
         * frame at the highest observable bit-rate (currently 448
         * kb/s). XXX=XXX Is 2016 bytes the size of the largest
         * frame? (448000*(1152/32000))/8
         }}} */ 

        if(Stream.next_frame!=NULL) {
            Remaining=Stream.bufend-Stream.next_frame;
            memmove(InputBuffer,Stream.next_frame,Remaining);
            ReadStart=InputBuffer+Remaining;
            ReadSize=INPUT_BUFFER_SIZE-Remaining;
        } else {
            ReadSize=INPUT_BUFFER_SIZE;
            ReadStart=InputBuffer;
            Remaining=0;
        }

       
        inputFileStream.read((char *)InputBuffer+Remaining,ReadSize);
        ReadSize=inputFileStream.gcount();
        
        if(ReadSize<=0 && inputFileStream.fail())
        {
            throw new EIOError("MP3Decoder: problem reading file" +
                    inputFileName);
        }

        /* {{{ {3} When decoding the last frame of a file, it must be
         * followed by MAD_BUFFER_GUARD zero bytes if one wants to
         * decode that last frame. When the end of file is
         * detected we append that quantity of bytes at the end of
         * the available data. Note that the buffer can't overflow
         * as the guard size was allocated but not used the the
         * buffer management code. (See also the comment marked
         * {1}.)
         *
         * In a message to the mad-dev mailing list on May 29th,
         * 2001, Rob Leslie explains the guard zone as follows:
         *
         *    "The reason for MAD_BUFFER_GUARD has to do with the
         *    way decoding is performed. In Layer III, Huffman
         *    decoding may inadvertently read a few bytes beyond
         *    the end of the buffer in the case of certain invalid
         *    input. This is not detected until after the fact. To
         *    prevent this from causing problems, and also to
         *    ensure the next frame's main_data_begin pointer is
         *    always accessible, MAD requires MAD_BUFFER_GUARD
         *    (currently 8) bytes to be present in the buffer past
         *    the end of the current frame in order to decode the
         *    frame."
         }}} */
        if (inputFileStream.eof()) {
            GuardPtr=ReadStart+ReadSize;
            memset(GuardPtr,0,MAD_BUFFER_GUARD);
            ReadSize+=MAD_BUFFER_GUARD;
        }
        
        /* Pipe the new buffer content to libmad's stream decoder
         * facility.
         */
       
    
        mad_stream_buffer(&Stream,InputBuffer,ReadSize+Remaining);
        Stream.error=MAD_ERROR_NONE;
        
        return true;
    }
    return false;
}
// }}}

// {{{ getMadError
std::string getMadError(const int errorCode)
{
  switch(errorCode)
        {
                /* Generic unrecoverable errors. */
                case MAD_ERROR_BUFLEN:
                        return std::string("input buffer too small (or EOF)");
                case MAD_ERROR_BUFPTR:
                        return std::string("invalid (null) buffer pointer");
                case MAD_ERROR_NOMEM:
                        return std::string("not enough memory");

                /* Frame header related unrecoverable errors. */
                case MAD_ERROR_LOSTSYNC:
                        return std::string("lost synchronization");
                case MAD_ERROR_BADLAYER:
                        return std::string("reserved header layer value");
                case MAD_ERROR_BADBITRATE:
                        return std::string
                            ("forbidden bitrate value");
                case MAD_ERROR_BADSAMPLERATE:
                        return std::string("reserved sample frequency value");
                case MAD_ERROR_BADEMPHASIS:
                        return std::string("reserved emphasis value");

                /* Recoverable errors */
                case MAD_ERROR_BADCRC:
                        return std::string("CRC check failed");
                case MAD_ERROR_BADBITALLOC:
                        return std::string("forbidden bit allocation value");
                case MAD_ERROR_BADSCALEFACTOR:
                        return std::string("bad scalefactor index");
                case MAD_ERROR_BADFRAMELEN:
                        return std::string("bad frame length");
                case MAD_ERROR_BADBIGVALUES:
                        return std::string("bad big_values count");
                case MAD_ERROR_BADBLOCKTYPE:
                        return std::string("reserved block_type");
                case MAD_ERROR_BADSCFSI:
                        return std::string("bad scalefactor selection info");
                case MAD_ERROR_BADDATAPTR:
                        return std::string("bad main_data_begin pointer");
                case MAD_ERROR_BADPART3LEN:
                        return std::string("bad audio data length");
                case MAD_ERROR_BADHUFFTABLE:
                        return std::string("bad Huffman table select");
                case MAD_ERROR_BADHUFFDATA:
                        return std::string("Huffman data overrun");
                case MAD_ERROR_BADSTEREO:
                        return std::string("incompatible block_type for JS");

                /* Unknown error. This switch may be out of sync with libmad's
                 * defined error codes.
                 */
                default:
                        return std::string("Unknown error code");
        }
}
// }}}

// {{{ getNextFrame()
AudioSamples MP3Decoder::getNextFrame(){ //throw (EIOError,EAudioDecode)
    if (eof) return AudioSamples::EmptyAudioSamples;

    readFileIfNeeded();
    int r=decodeFrame();
    
    if (r==DECODE_FRAME_BUFFER){
        if (inputFileStream.eof()) {
            eof=true;
            return AudioSamples::EmptyAudioSamples;
        }
        readFileIfNeeded();
        r=decodeFrame();
    }

    if (r==DECODE_FRAME_UNRECOVERABLE) {
        eof=true; // unrecoverable - we cannot continue either
        throw new EAudioDecode("MP3 unrecoverable error:" +
                getMadError(Stream.error)+inputFileName);
    }   

    /**
      if error was recoverable, should be logged
      except if first or last frame errors
      */
    if (r==DECODE_FRAME_RECOVERABLE && !firstFrame &&
            !inputFileStream.eof()) {
                Errors::_addError("MP3 recoverable error:" +
                        getMadError(Stream.error), Errors::NOTICE);
    }

    if (Frame.header.samplerate==0) {
        // something went wrong
        return AudioSamples::EmptyAudioSamples;
    }


    firstFrame=false; // we have sucesfully decoded at least one frame
    std::vector<AudioSample> data=synthetize();

    const int sampleRate=Frame.header.samplerate;
    const int channels=MAD_NCHANNELS(&Frame.header);

    if (sampleRate==0) return AudioSamples::EmptyAudioSamples; 

    const double
        time=(double)data.size()/(double)sampleRate/(double)channels;
    const double resultTime=actualTime;
    actualTime+=time;
    //printf("actualTime: %lf %lf\n",actualTime,time);

    return
        AudioSamples(data,resultTime,sampleRate,channels);
}
// }}}

// {{{ MP3Decoder::synthetize()
std::vector<AudioSample> MP3Decoder::synthetize(){

    std::vector<AudioSample> result;
    mad_synth_frame(&Synth,&Frame);

    for(int i=0;i<Synth.pcm.length;i++)
     for (int w=0;w<MAD_NCHANNELS(&Frame.header);w++)
       {
        AudioSample sample=mad_f_todouble(Synth.pcm.samples[w][i]);
        result.push_back(sample);
       }
    return result;
}    
// }}}

// {{{ decodeFrame
int MP3Decoder::decodeFrame(){
   Stream.error=MAD_ERROR_NONE;

    /* {{{ Decode the next MPEG frame. The streams is read from the
     * buffer, its constituents are break down and stored the the
     * Frame structure, ready for examination/alteration or PCM
     * synthesis. Decoding options are carried in the Frame
     * structure from the Stream structure.
     *
     * Error handling: mad_frame_decode() returns a non zero value
     * when an error occurs. The error condition can be checked in
     * the error member of the Stream structure. A mad error is
     * recoverable or fatal, the error status is checked with the
     * MAD_RECOVERABLE macro.
     *
     * {4} When a fatal error is encountered all decoding
     * activities shall be stopped, except when a MAD_ERROR_BUFLEN
     * is signaled. This condition means that the
     * mad_frame_decode() function needs more input to complete
     * its work. One shall refill the buffer and repeat the
     * mad_frame_decode() call. Some bytes may be left unused at
     * the end of the buffer if those bytes forms an incomplete
     * frame. Before refilling, the remaining bytes must be moved
     * to the beginning of the buffer and used for input for the
     * next mad_frame_decode() invocation. (See the comments
     * marked {2} earlier for more details.)
     *
     * Recoverable errors are caused by malformed bit-streams, in
     * this case one can call again mad_frame_decode() in order to
     * skip the faulty part and re-sync to the next frame.
     }}} */

    if(mad_frame_decode(&Frame,&Stream))
    {
        if(MAD_RECOVERABLE(Stream.error)) {
            /* Do not print a message if the error is a loss of
             * synchronization and this loss is due to the end of
             * stream guard bytes. (See the comments marked {3}
             * supra for more informations about guard bytes.)
             */
            if(Stream.error!=MAD_ERROR_LOSTSYNC ||
                    Stream.this_frame!=GuardPtr) {
                return DECODE_FRAME_RECOVERABLE;
            } else
            
            return DECODE_FRAME_BUFFER; // can't decode frame
        }
        else
            if(Stream.error==MAD_ERROR_BUFLEN)
                return DECODE_FRAME_BUFFER;
        else {
            return DECODE_FRAME_UNRECOVERABLE;
        }
    }
    
    return DECODE_FRAME_OK; // we have sucesfully decoded frame
}
// }}}

// {{{ MP3Decoder(string FileName)
MP3Decoder::MP3Decoder(const std::string& fileName):
    //throw(ECantOpenFile)
inputFileName(fileName),actualTime(0),eof(false),firstFrame(true)
{
    /* First the structures used by libmad must be initialized. */
    mad_stream_init(&Stream);
    mad_frame_init(&Frame);
    mad_synth_init(&Synth);
    inputFileStream.open(fileName.c_str(),std::ios::binary);
    if (inputFileStream.fail()) throw ECantOpenFile(fileName);
    actualTime=0;
}
// }}}

// {{{ ~MP3Decoder()
MP3Decoder::~MP3Decoder(){
    /* Mad is no longer used, the structures that were initialized must
     * now be cleared.
     */
    mad_synth_finish(&Synth);
    mad_frame_finish(&Frame);
    mad_stream_finish(&Stream);
}
// }}}

// {{{ getEof
bool MP3Decoder::haveNextFrame(){
    return !eof;
}
// }}}
