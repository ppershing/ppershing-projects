// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "Screen.h"
#include "Audio.h"
#include "AudioSamples.h"

// {{{ updateAudioBuffers
void Screen::updateAudioBuffers(){
    static AudioSample data[UPDATE_BUFFER_SIZE];
    AudioSamples s(data,UPDATE_BUFFER_SIZE,44100,2);

    RingBuffer* ring;

    for (int q=0;q<UPDATE_BUFFER_SIZE;q++)
        data[q]=0;

    
    // pass null data to playback ring
    ring=Audio::_getPlaybackRing();
    while (ring->getFreeSize()>=UPDATE_BUFFER_SIZE)
        ring->writeFromSamples(s);
      

    // clear recorded data from recording ring
    ring=Audio::_getRecordRing();
    while (ring->getDataSize()>=UPDATE_BUFFER_SIZE)
        ring->writeToSamples(UPDATE_BUFFER_SIZE);
    

}
// }}}

// {{{ ~Screen
Screen::~Screen(){
}
// }}
