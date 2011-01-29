// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_STACKTRACE
#define H_STACKTRACE
#include <string>
#include <vector>
/*
   note that if you change this value, you must change implementation!
   */
#define _MAX_STACK_DATA 20

/*
   probably we do not want either to see the very top of stack
   */
#define _STRING_SKIP_FRAMES 0

/**
  class for providing stack-tracing
  note that all methods are static
  */
class StackTrace{
    public:
    /**
      constructor, creates instance and acquire actual stack trace
      */
    StackTrace();

    /**
      destructor
      */
    ~StackTrace();

    /**
      return stack addressess acquired in constructor, 0 is top frame
      */
    std::vector<unsigned int> getStackData() const;

    /**
      return string representation of stack addresses
      */
    std::string getStringData() const;

    private:
    void* stackData[_MAX_STACK_DATA];
    int stackDataLength;

};

#endif
