// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include <stdlib.h>

#include <cassert>

/**
  assertion macro, automatically add error to log and throw exception
  */
#define Assert(cond,info) { if (!(cond)) { \
     char line[20]; \
    sprintf(line,"%d",__LINE__); \
     std::string s=std::string("Assertion failed in file ") \
    + __FILE__ + " on line "+line + \
    ". Diagnosis: "+info; \
    Errors::getInstance()->addError(s,Errors::FATAL);  \
    throw EAssertionFailed(s); } ; }


//#endif
