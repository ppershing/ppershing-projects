// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "MyStringUtils.h"

//#ifndef H_DEBUG
//#define H_DEBUG

#include "Errors.h"

/**
  Debug macro, automaticaly adds debug messages to logfile
  */

#define DEBUG(x) Errors::_addError(__FILE__+std::string(" : ")+ \
        MyStringUtils::intToString(__LINE__)+std::string(" >>> ") \
        +x,Errors::DEBUG)

//#endif
