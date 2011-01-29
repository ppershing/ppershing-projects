// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#include "StaveWidget.h"
#include "NoDebug.h"
#include "Exceptions.h"
#include "MyStringUtils.h"
#include "Note.h"
#include "GameDataPointers.h"
#include "Preferences.h"
#include "RecordedData.h"

// {{{ StaveWidget
StaveWidget::StaveWidget(){
    fontR=fontG=fontB=fontA=255;
}
// }}}

// {{{ ~StaveWidget
StaveWidget::~StaveWidget(){
}
// }}}

// {{{ actiate
void StaveWidget::activate(){
  timeUnit = Preferences::_getDefaultDouble("/audio/timeunit",0.05);
  active=true;
}
// }}}

// {{{ setFontColor
void StaveWidget::setFontColor(const unsigned char R, const unsigned
    char G, const unsigned char B,const unsigned char A){
  fontR=R;
  fontG=G;
  fontB=B;
  fontA=A;
}
// }}}

// {{{ setFont
void StaveWidget::setFont(Font& f){
  font=f;
}
// }}}

// {{{ clippedDraw
void StaveWidget::clippedDraw(Surface& surface){
  try {

    int lineX = timeToX(currentTime);
    Graphics::line(surface,lineX,metric.y(0.0),lineX,metric.y(100.0),255,0,0,255);

    try{
      for(unsigned int i=0;i<sentence.note.size();i++){
        drawNote(surface,sentence.note.at(i),255,255,0,255);
        DEBUG(MyStringUtils::intToString(i)+"-th note drawn");
      }
    }catch(Exception e){
      throw EIllegalArgument("Drawing notes to sing failed:\n"+std::string(e.what()));
    }
    try{
      for(int pitch = sentence.getMinPitch()-pitchPadding;pitch<sentence.getMaxPitch()+pitchPadding;pitch++){
        if(pitch%12==0)
          Graphics::line(surface,metric.x(0.0),pitchToY(pitch),metric.x(100.0),pitchToY(pitch),0,0,255,255);
      }
    }catch(Exception e){
      throw EIllegalArgument("Drawing octave lines failed:\n"+std::string(e.what()));
    }
/*
    try{
      if(GameDataPointers::_getCurrentPlayer()->isSinging()){//current singing note
        Note noteToDraw = GameDataPointers::_getCurrentPlayer()->getCurrentSingingNote();
        drawNote(surface,noteToDraw,255,0,255,255);
      }
    }catch(Exception e){
      throw EIllegalArgument("Drawing current singing note failed:\n"+std::string(e.what()));
    }

    try{
      int firstSingedIndex = GameDataPointers::_getCurrentPlayer()->getFirstNoteIndexAfter(sentence.start);
      int lastSingedIndex = GameDataPointers::_getCurrentPlayer()->getLastNoteIndexBefore(currentTime);
      for(int i=firstSingedIndex;i<=lastSingedIndex && firstSingedIndex>=0;i++){//all singed notes
        Note noteToDraw = GameDataPointers::_getCurrentPlayer()->getNoteAtIndex(i);
        drawNote(surface,noteToDraw,255,0,255,255);
      }
    }catch(Exception e){
      throw EIllegalArgument("Drawing singed notes failed:\n"+std::string(e.what()));
    }
*/    
    for(int i=sentence.start/timeUnit+1;timeUnit*i<sentence.start+sentence.duration && i<GameDataPointers::_getCurrentPlayer()->recordedSize()-1;i++){
      RecordedData r = GameDataPointers::_getCurrentPlayer()->recordAt(i);
      if(r.pitch>0){
        try{
          Graphics::filledCircle(surface,timeToX(timeUnit*(double)i),pitchToY(r.pitch),2,r.R,r.G,r.B,r.A);
        }catch(Exception e){
          throw EIllegalArgument("Drawing recorded data (pitch="+MyStringUtils::doubleToString(r.pitch)+", time="+MyStringUtils::doubleToString(timeUnit*(double)i)+") failed:\n"+std::string(e.what()));
        }
        int aboveY = pitchToY(r.pitch+12.0);
        int belowY = pitchToY(r.pitch-12.0);
        if(aboveY>rectangle.y)
          try{
            Graphics::filledCircle(surface,timeToX(timeUnit*(double)i),aboveY,2,128,128,128,255);
          }catch(Exception e){
            throw EIllegalArgument("Drawing recorded data octave below failed:\n"+std::string(e.what()));
          }
        if(belowY<rectangle.y+rectangle.h)
          try{
            Graphics::filledCircle(surface,timeToX(timeUnit*(double)i),belowY,2,128,128,128,255);
          }catch(Exception e){
            throw EIllegalArgument("Drawing recorded data octave above failed:\n"+std::string(e.what()));
          }
      }
    }

  } catch (Exception& e){
    throw EIllegalArgument("There was an error during StaveWidget::draw():"
        "\nWidget leftX:"+MyStringUtils::intToString(rectangle.x)+" topY:"+MyStringUtils::intToString(rectangle.y)+" width:"+MyStringUtils::intToString(rectangle.w)+" height:"+MyStringUtils::intToString(rectangle.h)+
        "\nSentence.start:"+MyStringUtils::doubleToString(sentence.start)+" duration:"+MyStringUtils::doubleToString(sentence.duration)+"\n"+e.what());
  }
}
// }}}

// {{{ drawNote
void StaveWidget::drawNote(Surface& surface, double start, double duration, double pitch, const int R,
    const int G, const int B, const int A){
  try{
  int yNoteAxis = pitchToY(pitch);
  Graphics::rectangle(
      surface,
      timeToX(start),
      yNoteAxis-noteWidth/2,
      timeToX(start+duration),
      yNoteAxis+noteWidth/2,
      R,G,B,A);
  }catch(Exception e){
    throw EIllegalArgument("Drawing note failed, start="+MyStringUtils::doubleToString(start)+", duration="+MyStringUtils::doubleToString(duration)+", pitch="+MyStringUtils::doubleToString(pitch)+":\n"+std::string(e.what()));
  }
}
// }}}

// {{{ drawNote
void StaveWidget::drawNote(Surface& surface, Note& note,
           const int R, const int G, const int B, const int A){
  drawNote(surface,note.start,note.duration,note.pitch,R,G,B,A);
}
// }}}

// {{{ setSentence
void StaveWidget::setSentence(Sentence newSentence){
  //DEBUG("setting sentence");
  sentence = newSentence;
  //DEBUG("calculating edges");
  leftTime = newSentence.start;
  timeWidth = newSentence.duration+0.001;
  bottomPitch = newSentence.getMinPitch()-pitchPadding;
  pitchRange = newSentence.getPitchRange()+2*pitchPadding;
  //DEBUG("done");
}
// }}}

// {{{ setTime
void StaveWidget::setTime(double time){
  currentTime = time;
}
// }}}

// {{{ setMetric
void StaveWidget::setMetric(){
  metric = Metric(rectangle);
}
// }}}

// {{{ setNoteWidth
void StaveWidget::setNoteWidth(int width){
  noteWidth = width;
}
// }}}

// {{{ setPitchPadding
void StaveWidget::setPitchPadding(double padding){
  pitchPadding = padding;
}
// }}}

// {{{ timeToX
int StaveWidget::timeToX(double time){
  return metric.x(0.5+99.0*(time-leftTime)/timeWidth);
}
// }}}

// {{{ pitchToX
int StaveWidget::pitchToY(double pitch){
  return metric.y(0.5+99.0*(pitch-bottomPitch)/pitchRange);
}
// }}}
