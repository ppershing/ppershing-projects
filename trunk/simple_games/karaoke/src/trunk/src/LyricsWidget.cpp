// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#include "LyricsWidget.h"
#include "NoDebug.h"
#include "Exceptions.h"

// {{{ LyricsWidget
LyricsWidget::LyricsWidget(){
  secondR=secondG=secondB=secondA=markedR=markedG=markedB=markedA=firstR=firstG=firstB=firstA=255;
  lineSpacing = 5;
}
// }}}

// {{{ ~LyricsWidget
LyricsWidget::~LyricsWidget(){
}
// }}}

// {{{ setFontColor
void LyricsWidget::setFontColor(const unsigned char fR, const unsigned char fG, const unsigned char fB,const unsigned char fA,
    const unsigned char sR, const unsigned char sG, const unsigned char sB,const unsigned char sA,
    const unsigned char mR, const unsigned char mG, const unsigned char mB,const unsigned char mA){
  firstR = fR; firstG = fG; firstB = fB; firstA = fA;
  secondR = sR; secondG = sG; secondB = sB; secondA = sA;
  markedR = mR; markedG = mG; markedB = mB; markedA = mA;
}
// }}}

// {{{ setMetric
void LyricsWidget::setMetric(){
  metric = Metric(rectangle);
}
// }}}

// {{{ setTime
void LyricsWidget::setTime(double time){
  currentTime = time;
}
// }}}

// {{{ setSong
void LyricsWidget::setSong(Song* _song){
  song = _song;
}
// }}}

// {{{ setLineSpacing
void LyricsWidget::setLineSpacing(int spacing){
  lineSpacing = spacing;
}
// }}}

// {{{ setFont
void LyricsWidget::setFont(const std::string fontName){
  int fontSize = 30;//we do not want bigger
  for(unsigned int i=0;i<song->sentence.size();i++){
    int maxFontSize = Font::getMaxFontSize(fontName,song->sentence.at(i).getFullText(),rectangle.w,(rectangle.h-lineSpacing)/2);
    if(maxFontSize < fontSize && maxFontSize>4)
      fontSize = maxFontSize;
  }
  font = Font(fontName, fontSize);
}
// }}}

// {{{ clippedDraw
void LyricsWidget::clippedDraw(Surface& surface){
    try {
      int leftX = metric.x(0.0);
      int topY = metric.y(100.0);
      try{//first line
      std::vector<std::string> fullTextMarked = song->sentence.at(currentSentenceIndex).getFullTextMarked(currentTime);
      if(!song->sentence.at(currentSentenceIndex).isSyllableAt(currentTime)){
        Graphics::drawText(surface,font,fullTextMarked.at(0),leftX,topY,firstR,firstG,firstB,firstA);
      }else{
        Graphics::drawText(surface,font,fullTextMarked.at(0),leftX,topY,firstR,firstG,firstB,firstA);
        int w1=font.getWidth(fullTextMarked.at(0));
        Graphics::drawText(surface,font,fullTextMarked.at(1),leftX+w1,topY,markedR,markedG,markedB,markedA);
        int w2=font.getWidth(fullTextMarked.at(1));
        Graphics::drawText(surface,font,fullTextMarked.at(2),leftX+w1+w2,topY,firstR,firstG,firstB,firstA);
      }
      }catch(Exception e){
        throw EIllegalArgument("Drawing first line of lyrics failed:\n"+std::string(e.what()));
      }
      try{//second line
        std::string secondLine="";
        if(currentSentenceIndex < song->sentence.size()-1){
          secondLine = song->sentence.at(currentSentenceIndex+1).getFullText();
          Graphics::drawText(surface,font,secondLine,leftX,topY+font.getHeight()+lineSpacing,secondR,secondG,secondB,secondA);
        }
      }catch(Exception e){
        throw EIllegalArgument("Drawing second line of lyrics failed:\n"+std::string(e.what()));
      }
    }catch (Exception& e){
      throw EIllegalArgument(std::string("There was an error during LyricsWidget::draw() :\n")+std::string(e.what()));
    }
}
// }}}

// {{{ setSentenceIndex
void LyricsWidget::setSentenceIndex(int index){
  currentSentenceIndex = index;
}
// }}}
