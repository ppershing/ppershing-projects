// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "Audio.h"
#include <cmath>
#include "Debug.h"
#include "SDL/SDL.h"
#include "Preferences.h"
#include "portaudio.h"
#include "Timer.h"
Audio::Audio* Audio::instance=NULL;

//TODO: SFX playback

//{{{ callback
static int paCallback( const void *inputBuffer, void *outputBuffer,
    unsigned long framesPerBuffer, const PaStreamCallbackTimeInfo* _timeInfo,
    PaStreamCallbackFlags statusFlags, void *userData )
{
    (void) statusFlags;
    PaStreamCallbackTimeInfo timeInfo=(*_timeInfo);

    CallbackData *data = (CallbackData*)userData; 
    float *out = (float*)outputBuffer;
    float *in  = (float*)inputBuffer;

    int inFrames=framesPerBuffer*data->inputChannels;
    int outFrames=framesPerBuffer*data->outputChannels;

    if (timeInfo.currentTime==0)
        timeInfo.currentTime=Timer::getSystemTime();

    *(data->lastTime)=timeInfo.currentTime;
    if (timeInfo.outputBufferDacTime==0)
        timeInfo.outputBufferDacTime=timeInfo.currentTime;
    if (timeInfo.inputBufferAdcTime==0)
        timeInfo.inputBufferAdcTime=timeInfo.currentTime;
    printf("%f %f\n",timeInfo.inputBufferAdcTime,data->microphoneTimeCorrection);
    if (statusFlags!=0) Errors::_addError(std::string("Audio out of sync"),
                Errors::ERROR);

    data->rec->setEndTimeReference(timeInfo.inputBufferAdcTime+
            data->microphoneTimeCorrection);

    if (data->rec->getFreeSize()< inFrames) {
        Errors::_addError(std::string( "Recording buffer overflow."), Errors::ERROR);
    } else {
        data->rec->writeFromFloats(in,inFrames,1);
    }


    if (data->play->getDataSize()<outFrames) {
       Errors::_addError(std::string("Playback buffer underflow."),Errors::ERROR);
    } else {        
        data->play->writeToFloats(out,outFrames,1);
    }

    data->play->timeReference=timeInfo.outputBufferDacTime;
  
    return 0;
}
// }}}

// {{{ getSampleRate
int Audio::_getSampleRate() {
    //throw (ESingletonInstance)
    return getInstance()->getSampleRate();
}

int Audio::getSampleRate() const {
    return sampleRate;
}
// }}}

// {{{ createRings 
void Audio::createRings(const double sampleRate, const int
        inputChannels,const int outputChannels){

    Assert(playbackRing==NULL,"allocated ring?");
    playbackRing=new
        RingBuffer((int) (sampleRate*outputChannels/4),sampleRate,outputChannels);

    Assert(recordRing==NULL,"allocated ring?");
    recordRing=new
        RingBuffer((int) (sampleRate*inputChannels),sampleRate,inputChannels);
}
// }}}

// {{{ deleteRings
void Audio::deleteRings(){
    Assert(playbackRing!=NULL,"delete null ring");
    delete playbackRing;
    playbackRing=NULL;

    Assert(recordRing!=NULL,"delete null ring");
    delete recordRing;
    recordRing=NULL;

}
// }}}

// {{{ fillCallbackData
void Audio::fillCallbackData(){
    callbackData.play=playbackRing;
    callbackData.rec=recordRing;
    callbackData.inputChannels=inputChannels;
    callbackData.outputChannels=outputChannels;
    callbackData.stream=stream;
    callbackData.microphoneTimeCorrection=microphoneTimeCorrection;
    callbackData.playbackVolume=&playbackVolume;
    callbackData.recordVolume=&recordVolume;
    callbackData.SFXVolume=&SFXVolume;
    callbackData.lastTime=&lastTime;
}
// }}}

// {{{ init
int Audio::init(const int sampleRate,const int inputChannels,
        const int outputChannels, const double
        micTimeCorrection,std::string& errorMessage){
    this->sampleRate=sampleRate;
    this->inputChannels=inputChannels;
    this->outputChannels=outputChannels;
    this->microphoneTimeCorrection=micTimeCorrection;

    createRings(sampleRate,inputChannels,outputChannels);

    PaError err;
    
    try {
        if ((err=Pa_Initialize())!=paNoError) throw Exception("audio");
        fillCallbackData();
        err=Pa_OpenDefaultStream(&stream,inputChannels,outputChannels,
                paFloat32, sampleRate, BUFFER_SIZE, paCallback, &callbackData); 
        if (err!=paNoError) throw Exception("audio");
        if ((err=Pa_StartStream(stream))!=paNoError) throw Exception("audio");

    } catch (Exception e){
        errorMessage=Pa_GetErrorText(err);
        deleteRings();
    }

    return 1;
    
}

int Audio::_init(const int sampleRate,const int inputChannels,
                    const int outputChannels, const double
                    micTimeCorrection,std::string& errorMessage) {
  // throw (ESingletonInstance)
   return
       getInstance()->init(sampleRate,inputChannels,outputChannels,micTimeCorrection,errorMessage);
}
// }}}

// {{{ finalize
int Audio::finalize(std::string& errorMessage){

    PaError err=Pa_StopStream( stream );

    deleteRings();

    if (err!=paNoError) {
        errorMessage=Pa_GetErrorText(err);
        return 0;
    }

    err= Pa_Terminate();

    if (err!=paNoError) {
        errorMessage=Pa_GetErrorText(err);
        return 0;
    }

    
  return 1;
}

int Audio::_finalize(std::string& errorMessage) {
    //throw (ESingletonInstance)
    return getInstance()->finalize(errorMessage);
}
// }}}

// {{{ createInstance
void Audio::createInstance() {
    //throw (ESingletonInstance)
    if (instance!=NULL) throw ESingletonInstance(
            "Audio:: createInstance called twice");
    instance=new Audio;
}
// }}}

// {{{ getInstance
Audio::Audio* Audio::getInstance() {
    //throw (ESingletonInstance)
    if (instance==NULL) throw ESingletonInstance(
            "Audio::get null instance");
    return instance;
}
// }}}

// {{{ destroyInstance()
void Audio::destroyInstance() {
    //throw (ESingletonInstance)
    if (instance==NULL) throw ESingletonInstance(
            "Audio:: destroy null instance");
    delete instance;
    instance=NULL;
}
// }}}

// {{{ Audio()
Audio::Audio():playbackRing(NULL),recordRing(NULL),playbackVolume(1),recordVolume(1),SFXVolume(1),
inputChannels(0),outputChannels(0){
}
// }}}

// {{{ ~Audio
Audio::~Audio(){

}
// }}}

// {{{ getPlaybackRing
RingBuffer* Audio::getPlaybackRing() const {
    return playbackRing;
}

RingBuffer* Audio::_getPlaybackRing() {
    //throw (ESingletonInstance)
    return getInstance()->getPlaybackRing();
}
// }}}

// {{{ getRecordRing
RingBuffer* Audio::getRecordRing() const {
    return recordRing;
}

RingBuffer* Audio::_getRecordRing() {
    //throw (ESingletonInstance)
    return getInstance()->getRecordRing();
}
// }}}


// {{{ setPlaybackVolume
void Audio::setPlaybackVolume(const double volume){
    //throw (EIllegalArgument)
    if (volume<0 || volume>1) throw EIllegalArgument("Audio:playback "
            "volume out of range");
    playbackVolume=volume;
}

void Audio::_setPlaybackVolume(const double volume){
 // throw (EIllegalArgument,ESingletonInstance)
    getInstance()->setPlaybackVolume(volume);

}
// }}}

// {{{ getPlaybackVolume 
double Audio::getPlaybackVolume() const {
    return playbackVolume; 
}

double Audio::_getPlaybackVolume(){
 //throw (ESingletonInstance)
    return getInstance()->getPlaybackVolume();
}
// }}}


// {{{ setRecordVolume
void Audio::setRecordVolume(const double volume){
    //throw (EIllegalArgument)
    if (volume<0 || volume>1) throw EIllegalArgument("Audio:record "
            "volume out of range");
    recordVolume=volume;
}

void Audio::_setRecordVolume(const double volume){
    //throw (EIllegalArgument,ESingletonInstance){
    getInstance()->setRecordVolume(volume);

}
// }}}

// {{{ getRecordVolume 
double Audio::getRecordVolume() const {
    return recordVolume; 
}

double Audio::_getRecordVolume(){
 //throw (ESingletonInstance)
    return getInstance()->getRecordVolume();
}
// }}}


// {{{ setSFXVolume
void Audio::setSFXVolume(const double volume){
    //throw (EIllegalArgument)
    if (volume<0 || volume>1) throw EIllegalArgument("Audio:SFX "
            "volume out of range");
    SFXVolume=volume;
}

void Audio::_setSFXVolume(const double volume){
  // throw (EIllegalArgument,ESingletonInstance)
    getInstance()->setSFXVolume(volume);

}
// }}}

// {{{ getSFXVolume 
double Audio::getSFXVolume() const {
    return SFXVolume; 
}

double Audio::_getSFXVolume(){
 //throw (ESingletonInstance) 
    return getInstance()->getSFXVolume();
}
// }}}

// {{{ getCurrentTime
double Audio::getCurrentTime(){
   // return Pa_GetStreamTime(stream);
   // this is nasty fix to buggy portaudio
   return lastTime;
}
// }}}

// {{{ _getCurrentTime
double Audio::_getCurrentTime(){
  return getInstance()->getCurrentTime();
}
// }}}
