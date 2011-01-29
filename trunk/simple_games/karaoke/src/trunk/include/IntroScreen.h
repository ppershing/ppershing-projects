#ifndef H_INTRO_SCREEN
#define H_INTRO_SCREEN

#include "Screen.h"
#include "Surface.h"

class IntroScreen:public Screen{
    public:
	IntroScreen();
        std::string getScreenName() const;
        void handleEvent(const SDL_Event* const event);
        void drawScreen(Surface& surface);
        void activateScreen();
        void deactivateScreen();
    private:
        int x;
};

#endif
