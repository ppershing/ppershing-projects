#include "ScreenManager.h"
#include "PPScreen.h"
#include "Errors.h"
#include <SDL/SDL.h>
#include <SDL/SDL_video.h>
#include "Graphics.h"
#include "Debug.h"
#include "MyStringUtils.h"
#include "MyAssert.h"

#include "MemoryInfo.h"

#include <cmath>
#include "Audio.h"
#include "Surface.h"
#include "AudioUtils.h"

PPScreen::PPScreen():oscilloscope(Oscilloscope()),
musicData(AudioSamples::EmptyAudioSamples){
    music=NULL;
    font=Font("fonts/DeJaVuSans.ttf",24);
    testTime=0;
    SDL_Rect r;
    r.x=30;
    r.y=300;
    r.w=200;
    r.h=50;
    memoryWidget= new MemoryWidget(r);
    oscilloscope.setDataSize(10*600);
    oscilloscope.setRect(10,10,600,200);
    oscilloscope.setColor(0,0,255,255);
    DEBUG("oscilloscope ok");
    analyzer1.setAudioCharacteristics(44100,1);
    DEBUG("a1 ok");
    analyzer2.setAudioCharacteristics(44100,1);
    DEBUG("a2 ok");
    spectrogram1.setAnalyzer(&analyzer1);
    spectrogram1.setRect(0,200,600,300);
    spectrogram2.setAnalyzer(&analyzer2);
    spectrogram2.setRect(0,300,600,400);

    analyzer3.setAnalyzer(0,&analyzer1);
    analyzer3.setAnalyzer(1,&analyzer2);
    analyzer3.setWeight(0,0.6);
    analyzer3.setWeight(1,0.4);

    spectrogram3.setAnalyzer(&analyzer3);
    spectrogram3.setRect(0,400,600,475);
}
    
PPScreen::~PPScreen(){
    delete memoryWidget;
    DEBUG("delete music");
    if (music!=NULL) delete music;
    DEBUG("ok");
}

std::string PPScreen::getScreenName() const{
    return std::string("PP");
}

void PPScreen::handleEvent(const SDL_Event* const event){
    if (event->type==SDL_KEYDOWN) {
        if (event->key.keysym.sym==SDLK_ESCAPE) {
          ScreenManager::getInstance()->activateScreen("MainMenu");
        }
        if (event->key.keysym.sym==SDLK_SPACE){
            delete music;
            music=new MP3Decoder(std::string("sounds/test.mp3"));

        }
    }
}

void PPScreen::drawScreen(Surface& surface){
    memoryWidget->updateMemoryInfo();
    memoryWidget->draw(surface);

//    analyzer1.analyzeData();
//    analyzer2.analyzeData();
    analyzer3.analyzeData();

    spectrogram1.draw(surface);
    spectrogram2.draw(surface);
    spectrogram3.draw(surface);

   
    std::string
        text=MyStringUtils::doubleToString(testTime);
    
    Graphics::drawText(surface,
            font,text,50,250,
            255,255,255,100);

    RingBuffer *rb=Audio::_getPlaybackRing();

        text=MyStringUtils::doubleToString(rb->getDataSize()/(double)rb->getSize());
    
    Graphics::drawText(surface,
            font,
            text,300,350,255,255,255,100);
    

    text=MyStringUtils::doubleToString(Audio::_getPlaybackRing()->timeReference*1.0);

    Graphics::drawText(surface,
            font,
            text,300,370,255,255,255,100);

    text=MyStringUtils::doubleToString(Audio::_getRecordRing()->timeReference*1.0);

    Graphics::drawText(surface,
            font,
            text,300,390,255,255,255,100);

    
  //  SDL_Surface* osc=SDL_CreateRGBSurface(SDL_SWSURFACE, 400,200, 32,
 //           0, 0, 0, 0);
   
    oscilloscope.draw(surface);

    
    
    Graphics::drawText(surface,font,
            (MyStringUtils::intToString(MemoryInfo::getOccupiedMemory())+
            " "+
            MyStringUtils::intToString(MemoryInfo::getMaxMemory())),200,300,255,255,0,255);
     
}

void PPScreen::activateScreen(){
    Errors::_addError("PPScreen begins",
           Errors::NOTICE);
}

void PPScreen::deactivateScreen(){
    Errors::_addError("PPScreen ends",
           Errors::NOTICE);
}

void PPScreen::updateAudioBuffers(){
    //return Screen::updateAudioBuffers();
    #define L 1000


    float tmp[L];
    for (int i=0;i<L;i++)
      tmp[i]=1;

    testTime=
                Audio::_getPlaybackRing()->getDataSize()/88200.0+
                Audio::_getPlaybackRing()->timeReference-
               Audio::_getRecordRing()->timeReference ;
    
    RingBuffer* r;

    for (int i=0;i<100;i++) {

    r=Audio::_getRecordRing();
    if (r->getDataSize()>L) {

        AudioSamples tmp=r->writeToSamples(L);
        AudioSamples s=AudioUtils::rechannel(tmp,1);
        oscilloscope.sendNewData(s);
        analyzer3.sendNewData(s);
//        analyzer2.sendNewData(s);
    }
    }

    r=Audio::_getPlaybackRing();

    while(r->getFreeSize()>musicData.getLength()) {
        r->writeFromSamples(musicData);
        if (music!=NULL && music->haveNextFrame()) {
         musicData=music->getNextFrame();
        } else {
            musicData=AudioSamples((double*) tmp,L/4,44100,2);
        }
    }

}

