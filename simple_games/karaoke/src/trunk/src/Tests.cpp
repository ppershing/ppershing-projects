#include "Tests.h"

#include <stdio.h>
#include "MemoryInfo.h"
#include "Font.h"
#include <vector>
#include <stdlib.h>
#include <time.h>
#include <SDL/SDL.h>
#include "Exceptions.h"
#include "Preferences.h"
#include <string>
#include "Errors.h"
#include "MyStringUtils.h"
#include "AudioSamples.h"
#include "MP3Decoder.h"
#include "Surface.h"


Tests Tests::instance;

// {{{ init, _init
void Tests::_init(){
    mem=MemoryInfo::getOccupiedMemory();
    srand(time(NULL));

}

void Tests::init(){

}
// }}}

// {{{ finalize, _finalize
void Tests::_finalize(){
    fprintf(stderr,"\n total program memory leakage: %d\n"
            "Max memory used %d \n",
            MemoryInfo::getOccupiedMemory()-mem,
            MemoryInfo::getMaxMemory());

}

void Tests::finalize(){

}
// }}}


void Tests::do_tests(){
 if (Preferences::_haveKey("/tests/font/enable") &&
     Preferences::_getInt("/tests/font/enable")) do_font_tests();
 
 if (Preferences::_haveKey("/tests/surface/enable") &&
     Preferences::_getInt("/tests/surface/enable")) do_surface_tests();
}

void Tests::do_benchmarks(){
 if (Preferences::_haveKey("/benchmarks/ringbuffer/enable") &&
     Preferences::_getInt("/benchmarks/ringbuffer/enable")) do_ringbuffer_benchmarks();

}

// {{{ TestBenchmarkStatus:: constructor,destructor,finalize
TestBenchmarkStatus::TestBenchmarkStatus(std::string name){
    Errors::_addError(std::string("Initializing test:")+name,
                Errors::NOTICE);
    benchmark=name;
    memorySize=MemoryInfo::getOccupiedMemory();
    startTime=SDL_GetTicks();
}

void TestBenchmarkStatus::finalize(){
    Errors::_addError(std::string("Benchmark: ")+benchmark+ " - memory leakage "+
         MyStringUtils::intToString(MemoryInfo::getOccupiedMemory()-memorySize)+
         " test time "+MyStringUtils::doubleToString(
            float(SDL_GetTicks()-startTime)/1000.0),Errors::NOTICE);
}

TestBenchmarkStatus::~TestBenchmarkStatus(){
}
// }}}

// {{{ do_font_tests
void Tests::do_font_tests(){
    int NUM_FONTS=Preferences::_getInt("/tests/font/vectorSize");
    int NUM_TESTS=Preferences::_getInt("/tests/font/tests");
    int NUM_FAIL_TESTS=Preferences::_getInt("/tests/font/failTests");
    std::string FONT = Preferences::_getString("/tests/font/fontName"); 

    TestBenchmarkStatus test("Font");
    {
        Errors::_addError("testing exceptions - note that this cause "
                "leakage  by exceptional mechanism\n",Errors::NOTICE);

        int j=0;
        for (int i=0;i<NUM_FAIL_TESTS;i++){
           
        try {
            Font f("tubas",5);

        } catch (Exception& e){
            j++;
        }
        }
        if (j!=NUM_FAIL_TESTS) fprintf(stderr,"something went wrong\n");

        Errors::_addError("testing for leakages",Errors::NOTICE);
    std::vector<Font> data(NUM_FONTS);
    for (int i=0;i<NUM_TESTS; i++){
        int q=rand()%NUM_FONTS;
        int w=rand()%NUM_FONTS;
        switch (rand()%3){
         case 0: data[q]=data[w];
            break;
         case 1: data[q]=Font(FONT.c_str(),rand()%10+5);
            break;
         case 2: data[q]=Font(data[w]);
        }
    }
    }
   test.finalize(); 
    
}
// }}}

// {{{ do_surface_tests
void Tests::do_surface_tests(){
    int NUM_SURFS=Preferences::_getInt("/tests/surface/vectorSize");
    int NUM_TESTS=Preferences::_getInt("/tests/surface/tests");
    int NUM_FAIL_TESTS=Preferences::_getInt("/tests/surface/failTests");

    TestBenchmarkStatus test("Surface");
    {
        Errors::_addError("testing exceptions - note that this cause "
                "leakage  by exceptional mechanism\n",Errors::NOTICE);

        int j=0;
        for (int i=0;i<NUM_FAIL_TESTS;i++){
           
        try {
            Surface f((SDL_Surface*)NULL,true);

        } catch (Exception& e){
            j++;
        }
        }

        if (j!=NUM_FAIL_TESTS) fprintf(stderr,"something went wrong\n");

        Errors::_addError("testing for leakages",Errors::NOTICE);
    std::vector<Surface> data(NUM_SURFS);
    fprintf(stderr,"\n\n");
    for (int i=0;i<NUM_TESTS; i++){
        int q=rand()%NUM_SURFS;
        int w=rand()%NUM_SURFS;
        switch (rand()%4){
         case 0: data[q]=data[w];
            break;
         case 1: data[q]=Surface(10,10,32);
            break;
         case 2: data[q]=Surface((SDL_Surface*)1,false);

            break;
         case 3: data[q]=Surface(data[w]);
            break;
        }
    }
    fprintf(stderr,"finalize\n\n");
    }
   test.finalize(); 
}
// }}}

// {{{ do_ringbuffer_readWrite
void Tests::do_ringbuffer_readWrite(int ringSize,int readSize,int times){
    TestBenchmarkStatus test("RingBuffer "
            "read/write "+
            MyStringUtils::intToString(ringSize)+" "+
            MyStringUtils::intToString(readSize)+" "+
            MyStringUtils::intToString(times));
    { RingBuffer rb=RingBuffer(ringSize,44100,2);
      std::vector<float> data(readSize);
      rb.writeFromFloats(&data[0],100,1);
      for (int i=0;i<times;i++){
          rb.writeFromFloats(&data[0],readSize,1);
          rb.writeToFloats(&data[0],readSize,1);


      }
    }
    test.finalize();
}
// }}}

// {{{ do_ringbuffer_benchmarks
void Tests::do_ringbuffer_benchmarks(){

  do_ringbuffer_readWrite(100000,65536,100);
  do_ringbuffer_readWrite(100000,32768,200);
  do_ringbuffer_readWrite(100000,16384,400);
  do_ringbuffer_readWrite(100000,8184,800);
  do_ringbuffer_readWrite(100000,4096,1600);
  do_ringbuffer_readWrite(100000,2048,3200);
  do_ringbuffer_readWrite(100000,1024,6400);
  do_ringbuffer_readWrite(100000,512,12800);
  do_ringbuffer_readWrite(100000,256,25600);
  do_ringbuffer_readWrite(100000,128,51200);
  do_ringbuffer_readWrite(100000,64,102400);
  do_ringbuffer_readWrite(100000,32,204800);
  do_ringbuffer_readWrite(100000,16,409600);

  do_ringbuffer_readWrite(10000,8184,800);
  do_ringbuffer_readWrite(10000,4096,1600);
  do_ringbuffer_readWrite(10000,2048,3200);
  do_ringbuffer_readWrite(10000,1024,6400);
  do_ringbuffer_readWrite(10000,512,12800);
  do_ringbuffer_readWrite(10000,256,25600);
  do_ringbuffer_readWrite(10000,128,51200);
  do_ringbuffer_readWrite(10000,64,102400);
  do_ringbuffer_readWrite(10000,32,204800);
  do_ringbuffer_readWrite(10000,16,409600);

  do_ringbuffer_readWrite(1000,512,12800);
  do_ringbuffer_readWrite(1000,256,25600);
  do_ringbuffer_readWrite(1000,128,51200);
  do_ringbuffer_readWrite(1000,64,102400);
  do_ringbuffer_readWrite(1000,32,204800);
  do_ringbuffer_readWrite(1000,16,409600);
}
// }}}
