#ifndef H_LOADING_SCREEN
#define H_LOADING_SCREEN

#include "Screen.h"
#include "Surface.h"
#include "Font.h"

class LoadingScreen:public Screen{
    public:
	LoadingScreen();
        std::string getScreenName() const;
        void handleEvent(const SDL_Event* const event);
        void drawScreen(Surface& surface);
        void activateScreen();
        void deactivateScreen();
    private:
        int songsLoaded;
        Font font;
};

#endif
