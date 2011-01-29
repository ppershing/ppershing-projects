// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_SYLLABLE
#define H_SYLLABLE

#include <string>

class Syllable{
  public:
    Syllable();
    ~Syllable();
    /**
      Time of starts in seconds.
    */
    double start;
    /**
      Duration time of syllable in seconds.
    */
    double duration;
    std::string text;
};

#endif
