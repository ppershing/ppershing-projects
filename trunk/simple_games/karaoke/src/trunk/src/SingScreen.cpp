// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#include "SingScreen.h"
#include "ScreenManager.h"
#include "Errors.h"
#include "NoDebug.h"
#include "MyAssert.h"
#include "MyStringUtils.h"
#include "Graphics.h"
#include <SDL/SDL.h>
#include "SDL/SDL_ttf.h"
#include "Surface.h"
#include "Font.h"
#include "GameDataPointers.h"
#include "Song.h"
#include "Audio.h"
#include "AudioUtils.h"
#include "Timer.h"
#include "CompoundSpectrumAnalyzer.h"
#include "StaveWidget.h"
#include<iostream>

SingScreen::SingScreen():musicData(AudioSamples::EmptyAudioSamples){
  music=NULL;
  //font=Font("fonts/DejaVuSans.ttf",30);
  analyzer.setAudioCharacteristics(Audio::_getRecordRing()->sampleRate);
}

SingScreen::~SingScreen(){
  DEBUG("delete music");
  delete music;
  music=NULL;
  DEBUG("music deleted");
}

std::string SingScreen::getScreenName() const{
  return std::string("Sing");
}

void SingScreen::handleEvent(const SDL_Event* const event){
  if (event->type==SDL_KEYDOWN) {
    switch(event->key.keysym.sym){
      case SDLK_ESCAPE:
        ScreenManager::getInstance()->activateScreen("SongResult");
        break;
      default:
        break;
    }
  }
}

void SingScreen::drawScreen(Surface& surface){
  double currentTime=/* 1000* */timer.getCalibratedSystemTime();
  if(currentTime<0)
    currentTime=0;

  int oldSentenceIndex = currentSentenceIndex;
  currentSentenceIndex = song.getSentenceIndexAt(currentTime);
  if(currentSentenceIndex < 0){
    if(song.hasEnded(currentTime)){
      if(true){//TO-DO:  if music.hasEnded()
        ScreenManager::getInstance()->activateScreen("SongResult");
        return;
      }else{
        currentSentenceIndex = 0;//outtro
      }
    }else{//medzera medzi sentencami? take by nemalo nastat
      return;
    }
  }
  Assert(currentSentenceIndex>=0,"This should not be reached, index<0");
  Sentence currentSentence = song.sentence.at(currentSentenceIndex);

  int targetPitch;
  if(currentSentence.isNoteAt(currentTime))
    targetPitch = currentSentence.getNoteAt(currentTime).pitch;
  else
    targetPitch = currentSentence.getMidPitch();

  if(oldSentenceIndex != currentSentenceIndex){//new sentence started
    GameDataPointers::_getCurrentPlayer()->splitAtTime(currentSentence.start,targetPitch);
    staveWidget.setSentence(currentSentence);
    lyricsWidget.setSentenceIndex(currentSentenceIndex);
  }

  analyzer.analyzeData();
  int freq = analyzer.getFrequency();
  double micTime = analyzer.getTime();//system time when freq was singed
  printf("Analyzer got frequency=%d at system time=%f\n",freq,micTime);
  double micSongTime = 1000*timer.getCalibratedSystemTimeAt(micTime); //time in song when it was singed
  if(micSongTime<0)micSongTime=0;
  printf("getCal...at(%f)=%f //time in song/1000\n",micTime,micSongTime/1000.0);
  printf("Audio::_getCurrentTime()=%f\n",Audio::_getCurrentTime());
  std::string bestFrequency = MyStringUtils::intToString(freq);
  Graphics::drawText(surface,font,bestFrequency+"/"+MyStringUtils::intToString(Note::normalizePitch((double)Note::frequencyToPitch(freq),(double)targetPitch)),200,20,255,255,255,255);

  micSongTime=currentTime-0.15;
  if(micSongTime<0)micSongTime=0;
  //GameDataPointers::_getCurrentPlayer()->sing(freq,currentTime/*micSongTime*/,targetPitch,currentSentence.isNoteAt(currentTime));
  GameDataPointers::_getCurrentPlayer()->sing(freq,micSongTime,targetPitch,currentSentence.isNoteAt(micSongTime));
  /*  if(currentSentence.isNoteAt(currentTime))
      GameDataPointers::_getCurrentPlayer()->sing(freq,currentTime,currentSentence.getNoteAt(currentTime).pitch);
      else
      GameDataPointers::_getCurrentPlayer()->sing(freq,currentTime,currentSentence.getMidPitch());
      */


  Graphics::drawText(surface,font,"You are singing..."+song.title+"",100,50,255,255,0,255);
  Graphics::drawText(surface,font,"Time= "+MyStringUtils::intToString(currentTime*10)+" / "+MyStringUtils::intToString(song.endTime*10),100,80,255,255,0,255);
  Graphics::drawText(surface,font,"Score= "+MyStringUtils::intToString(GameDataPointers::_getCurrentPlayer()->score),100,110,255,255,0,255);
  try{ 
    try{
      lyricsWidget.setTime(currentTime);
      lyricsWidget.draw(surface);
    }catch(Exception e){
      throw EIllegalArgument("Draw lyrics widget failed:\n"+std::string(e.what()));
    }

    int leftX=10;
    int width=300;
    int topY=150;
    int height=220;
    int leftTime = currentSentence.start;
    int timeWidth = currentSentence.duration+1;
    int topPitch = currentSentence.getMaxPitch()+6;
    int pitchRange = currentSentence.getPitchRange()+12;

    try{
      staveWidget.setTime(currentTime);
      staveWidget.draw(surface);
    }catch(Exception e){
      throw EIllegalArgument("Drawing stave failed\n"+std::string(e.what()));
    }

    try{
      timeBarWidget.setTime(currentTime);
      timeBarWidget.draw(surface);
    }catch(Exception e){
      throw EIllegalArgument("Drawing timeBar failed\n"+std::string(e.what()));
    }

    /*try{
      Graphics::rectangle(surface,leftX,topY-30,leftX+width,topY-20,255,255,0,255);//timebar
      Graphics::rectangle(surface,leftX,topY-28,leftX + width*currentTime/song.endTime,topY-22,255,255,0,255);//timebar
    }catch(Exception e){
      throw EIllegalArgument("Draw timebar failed\n"+std::string(e.what()));
    }*/
   
    try{
      for(int i=currentSentence.getMinPitch()-5;i<=currentSentence.getMaxPitch()+6;i+=(pitchRange/8)){//pitch labels on the left
        Graphics::drawText(surface,font,MyStringUtils::intToString(i),leftX,topY+height*(-i+topPitch)/pitchRange,255,255,0,255);
      }
    }catch(Exception e){
      throw EIllegalArgument("Drawing pitch labels failed\n"+std::string(e.what()));
    }/*
    try{
      for(int i=currentSentence.getMinPitch()-5;i<=currentSentence.getMaxPitch()+6;i++){
        if(i%12==0)
          Graphics::line(surface,leftX,topY+height*(-i+topPitch)/pitchRange,leftX+width,topY+height*(-i+topPitch)/pitchRange,0,0,255,255);
      }
    }catch(Exception e){
      throw EIllegalArgument("Drawing octave horizontal lines failed\n"+std::string(e.what()));
    }*/
  }catch(EIllegalArgument e){
    Errors::_addError("Current time="+MyStringUtils::intToString(currentTime)+"\n"+std::string(e.what()),Errors::ERROR);
  }
}
// }}}

// {{{ activateScreen
void SingScreen::activateScreen(){
  DEBUG("SingScreen::activateScreen()");
  songId=GameDataPointers::_getCurrentSongId();
  DEBUG("Got song ID="+MyStringUtils::intToString(songId));
  song=GameDataPointers::_getSongList()->song.at(songId);
  DEBUG("Got song");
  
  int maxFontSize=Font::getMaxFontSize("fonts/DejaVuSans.ttf","You are singing..."+song.title+" press ESC",420,65);
  //printf("maxFontSize=%d\n",maxFontSize);
  font=Font("fonts/DejaVuSans.ttf",maxFontSize);
  
  Errors::_addError("Loading song",Errors::NOTICE);
  song.loadFull();
  /*
  DEBUG("Printing all sentences..."+MyStringUtils::intToString(song.sentence.size()));
  for(unsigned int i=0;i<song.sentence.size();i++){
    DEBUG("Printing "+MyStringUtils::intToString(i)+"-th sentence.");
    std::cout << song.sentence.at(i).getFullText() << "\n";
  }
  */
  Errors::_addError("Preparing player",Errors::NOTICE);
  GameDataPointers::_getCurrentPlayer()->clearNotes();

  Errors::_addError("Init widgets",Errors::NOTICE);
  if(!initWidgets())
    Errors::_addError("Init widgets failed",Errors::ERROR);

  Errors::_addError("Starting playing music",Errors::NOTICE);
  delete music;
  music=NULL;
  music=new MP3Decoder(song.musicFileName);
  
  Errors::_addError("SingScreen really begins",Errors::NOTICE);
  timer.setCalibratedSystemTime(-1);

}
// }}}

// {{{ deactivateScreen
void SingScreen::deactivateScreen(){
  delete music;
  music=NULL;
  Errors::_addError("SingScreen ends",
      Errors::NOTICE);
}
// }}}

// {{{ updateAudioBuffers()
void SingScreen::updateAudioBuffers(){
  #define L 1000
  float tmp[L];
  for (int i=0;i<L;i++)
    tmp[i]=1;

  RingBuffer* r;

  r=Audio::_getRecordRing();
  while (r->getDataSize()>L) {
      AudioSamples recordedData=r->writeToSamples(L);
      recordedData=AudioUtils::rechannel(recordedData,1);
      analyzer.sendNewData(recordedData);
    }


  r=Audio::_getPlaybackRing();
  while(r->getFreeSize()>musicData.getLength()) {
    double dd=r->getEndTimeReference()-Audio::_getCurrentTime();//kolko z pesnicky je uz v bufferi
    printf("setCal..(%f)\n",musicData.referenceTime-r->getEndTimeReference()+Audio::_getCurrentTime());
    printf("callibration=%f\n",timer.setCalibratedSystemTime(musicData.referenceTime-dd));
    r->writeFromSamples(musicData);
    if (music!=NULL && music->haveNextFrame()) {
      musicData=music->getNextFrame();
      musicData=AudioUtils::resample(musicData,r->sampleRate);
      musicData=AudioUtils::rechannel(musicData,r->channelCount);
    } else {
      musicData=AudioSamples((double*) tmp,L/4,44100,2);
    }
  }

}

// {{{ initWidgets()
int SingScreen::initWidgets(){
  staveWidget.deactivate();
  staveWidget.setRect(10,160,590,370);

  staveWidget.setMetric();
  staveWidget.setNoteWidth(6);
  staveWidget.setPitchPadding(7.0);//half of the octave
  staveWidget.setFrameColor(255,255,0,255);
  staveWidget.activate();
  Errors::_addError("Init stave widget OK",Errors::NOTICE);

  timeBarWidget.deactivate();
  timeBarWidget.setRect(10,140,310,150);
  timeBarWidget.setMetric();
  timeBarWidget.setSong(&song);
  timeBarWidget.setFrameColor(255,255,0,255);
  timeBarWidget.activate();
  Errors::_addError("Init timeBar widget OK",Errors::NOTICE);

  lyricsWidget.deactivate();
  lyricsWidget.setRect(30,390,420,470);
  lyricsWidget.setMetric();
  lyricsWidget.setSong(&song);
  lyricsWidget.setFont("fonts/DejaVuSans.ttf");
  lyricsWidget.setFontColor(255,255,0,255,255,255,0,255,255,0,0,255);
  lyricsWidget.setFrameColor(0,0,0,0);
  lyricsWidget.activate();
  Errors::_addError("Init lyrics widget OK",Errors::NOTICE);

  spectrogram.deactivate();
  spectrogram.setRect(430,400,530,470);

  //spectrogram.activate();
  Errors::_addError("Init spectrogram OK",Errors::NOTICE);
  return 1;
}
// }}}
