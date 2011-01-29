// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#define C_KARAOKE


using namespace std;
// {{{ include
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <string>
#include <SDL/SDL.h>

#include "MemoryTrace.hpp"
#include "Errors.h"
#include "MyAssert.h"
#include "Exceptions.h"
#include "GameEngine.h"
#include "StackTrace.h"
#include "MyStringUtils.h"
#include "Debug.h" // want to show debug sessions
#include "Version.h"
// }}}


#include "Tests.h"

#define LEAK_FILE "Karaoke.leaks"

#include "math.h"

// {{{ init
void init(){
    Errors::createInstance("Karaoke.log");
    DEBUG("perform init");
    Errors::_setErrorCacheSize(5); // init main error log file
    GameEngine::createInstance();
    DEBUG("init done");
}
// }}}

// {{{ finalize
void finalize(){
     DEBUG("finalizing application");

    try{        
        GameEngine::destroyInstance();
        Errors::destroyInstance();
        leaktracer::MemoryTrace::GetInstance().stopAllMonitoring();
        std::ofstream oleaks;
        oleaks.open(LEAK_FILE, std::ios_base::out);
        if (oleaks.is_open()) {
            leaktracer::MemoryTrace::GetInstance().writeLeaks(oleaks);
        } else {
            throw ECantOpenFile(string(LEAK_FILE));
        }
    } catch (exception& e){
        fprintf(stderr,"There was an exception during finalize "
                "phase %s\n",e.what());

    }
}
// }}}

// {{{ run
void run(){
    try {        
        init(); // please dont call DEBUG behind this line

        DEBUG("instantiate game engine");
        GameEngine::GameEngine* gameEngine=GameEngine::getInstance();

        DEBUG("init game engine");
        if (gameEngine->init()){

            DEBUG("run game engine");
            gameEngine->run();
        } else {
            DEBUG("init game engine failed");            
        }
    } catch (Exception& e){

        fprintf(stderr,"There was an exception :\n%s\n stack %s\n" 
                ,e.what(),e.getStringStack().c_str());

    } catch (exception& e){
        fprintf(stderr,"There was an exception :\n%s\n",e.what());
    }
    
    finalize();
}
// }}}


int xmain(){
    printf("Program: %s\n",Version::getFullProductName().c_str());
    Tests::instance._init();

    leaktracer::MemoryTrace::GetInstance().startMonitoringAllThreads();
    run();

    Tests::instance._finalize();

    return 0;
}

volatile int cnt=30;

void makeStackFrames(){
    if (cnt==0) {
        xmain();
        return;

    }
    --cnt;
    makeStackFrames();
    cnt+=2;
    cnt--;
    cnt--;
}




// {{{ main
int main(int argc, char *argv[]) {
    makeStackFrames();
}
// }}}

