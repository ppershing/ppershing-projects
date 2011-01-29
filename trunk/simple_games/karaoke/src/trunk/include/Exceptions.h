// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_EXCEPTIONS
#define H_EXCEPTIONS

#define THROWS(e) 
#include <exception>
#include <string>
#include "StackTrace.h"

/**
  Standard exception
  */
class Exception: public std::exception {
    public:
    Exception(const std::string& s) throw();
    Exception(const std::string& s, const std::exception& cause) throw();
    virtual const char* what() const throw() {return message.c_str();}

    /**
      returns string representation of stack acquired
      during throwing this exception
      */
    std::string getStringStack() throw() {
        return stackTrace.getStringData(); }
     ~Exception() throw();

    private:
    const std::string message;
    const StackTrace stackTrace;
};


/**
    Standard exception for assert failure
      */
class EAssertionFailed: public Exception {
    public:
        EAssertionFailed(const std::string& s) throw();
        ~EAssertionFailed() throw();
    private:
};



/**
  Standard exception for IO failure during opening file
  */
class ECantOpenFile: public Exception {
    public:
    ECantOpenFile(const std::string& s) throw();
     ~ECantOpenFile() throw();
    private:
};

/**
  Standard exception for failed IO operations
  */
class EIOError: public Exception{
    public:
        EIOError(const std::string& s) throw();
        ~EIOError() throw();
    private:
};

/**
  Standard exception for wrong parameters
  */
class EIllegalArgument: public Exception{
    public:
        EIllegalArgument(const std::string& s) throw();
        ~EIllegalArgument() throw();
    private:
};

/**
  Standard exception for singleton instance problems
  */
class ESingletonInstance: public Exception{
    public:
        ESingletonInstance(const std::string& s) throw();
        ~ESingletonInstance() throw();
    private:
};

/**
  Standard exception for failed audio decoding
  */
class EAudioDecode: public Exception{
    public:
        EAudioDecode(const std::string& s) throw();
        ~EAudioDecode() throw();
    private:
};

/**
  Standard exception for calling not implemented function
  */
class ENotImplemented: public Exception{
    public:
        ENotImplemented(const std::string& s) throw();
        ~ENotImplemented() throw();
    private:
};

#endif
