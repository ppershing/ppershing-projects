// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_PP_SCREEN
#define H_PP_SCREEN

#include "Screen.h"
#include "Oscilloscope.h"
#include "MP3Decoder.h"
#include "Surface.h"
#include "MemoryWidget.h"
#include "Spectrogram.h"
#include "SimpleSpectrumAnalyzer.h"
#include "FFTSpectrumAnalyzer.h"
#include "CompoundSpectrumAnalyzer.h"

class PPScreen:public Screen{
    public:
	PPScreen();
        virtual ~PPScreen();
        std::string getScreenName() const;
        void handleEvent(const SDL_Event* const event);
        void drawScreen(Surface& surface);
        void activateScreen();
        void deactivateScreen();
        void updateAudioBuffers();
    private:
        Font font;
        double testTime;
        Oscilloscope oscilloscope;
        Spectrogram spectrogram1;
        Spectrogram spectrogram2;
        Spectrogram spectrogram3;
        SimpleSpectrumAnalyzer analyzer1;
        FFTSpectrumAnalyzer analyzer2;
        CompoundSpectrumAnalyzer analyzer3;
        MP3Decoder* music;
        AudioSamples musicData;
        MemoryWidget* memoryWidget;
};

#endif
