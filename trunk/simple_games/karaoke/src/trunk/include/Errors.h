// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_ERRORS
#define H_ERRORS 1

#include <string>
#include <vector>
#include "MyAssert.h"
#include "Exceptions.h"

/**
  Base class for logging errors,
  it is used in all other parts of project.
  Also contains one singleton class for default access,
  with quickacces as underscore
  
  @short class for logging errors
  @author PPershing
  @version 1.0
*/
class Errors{
  public:
    /**
      Constructor for errors

      This method creates class, opens file descriptor and
      sets logging level to all errors.
      In case of problems opening log file, throws ECantOpenFile
      
      @throws ECantOpenFile
      @return nothing
      @param logName    Name of file to log into
      */
    Errors(const std::string& logName);
    //throws (ECantOpenFile);
    
    /**
      Destructor

      Closes opened logfile
      */
    ~Errors();

    /**
      Add error to logfile, if the errorType is at least logLevel
      @param s String describing error
      @param errorType - type of error, see Erros::
      */
    void addError(const std::string& s,const int errorType);

    /**
      static link to addError for default instance
      */
    static void _addError(const std::string& s,const int errorType);
      //  throw (ESingletonInstance);

    /**
      @returns last added error
      */
    std::string getLastError();

    /**
      static link to getLastError
      */
    static std::string _getLastError();
      //throws (ESingletonInstance);

    /**
      Returns number of errors we want to retrieve from cache
      @param count how many errors we want
      @returns errors, res[0] = last encountered error
      */
    std::vector<std::string> getLastErrors();

    /**
      static link to getLastErrors
      */
    static std::vector<std::string> _getLastErrors();
        //throws (ESingletonInstance);
    

    /**
      Sets number of errors to remeber
      @param size number of errors to remember
      */
    void setErrorCacheSize(const int size);
        //throws (EIllegalArgument);

    /**
      static link to setErrorCacheSize
      */
    static void _setErrorCacheSize(const int size);
        //throws (EIllegalArgument,ESingletonInstance);

    /**
      Sets log level, below which we don;t want to log errors
      @param level desired log level
      */
    void setLogLevel(const int level);

    /**
      static link to setLogLevel
      */
    static void _setLogLevel(const int level);
    //throws (ESingletonInstance);
    
    /**
      number of levels
      */
    static const int NUM_ERRORS=5;

    /**
      constant for debug level
      */

    static const int DEBUG=0;
    
    /**
      constant for notice level
      */
    static const int NOTICE=1;
    
    /**
      constant for warning level
      */
    static const int WARNING=2;
    
    /**
      constant for error level
      */
    static const int ERROR=3;
    
    /**
      constant for fatal level
      */
    static const int FATAL=4;

    /**
      create instance
      */
    static void createInstance(std::string logFile);
        // throws (ESingletonInstance);

    /**
      get basic static instance
      */
    static Errors* getInstance();
    // throws (ESingletonInstance);
    
    /**
      destroy static instance
      */
    static void destroyInstance();
    // throws (ESingletonInstance);
    
  private:
    /**
      private assignment and copy constructor are not implemented,
      class is not supposed to be copied
      */
    Errors& operator=(const Errors& e);

    /**
      private assignment and copy constructor are not implemented,
      class is not supposed to be copied
      */
    Errors(Errors& e);

    /**
      handle to log file
      */
    FILE *logFile;    

    /**
      name of log file
      */
    const std::string logName;

    /**
      string representation of error levels,
      used when writing to log
      */
    static const char *ERRORS_TEXT[NUM_ERRORS];

    /**
      vector of last errors
    TODO: maybe we can convert it to CircularBuffer
    */
    std::vector<std::string> lastErrors;

    /**
      actual level of log vebrosity
      */
    int logLevel;

    /**
      instance
      */
    static Errors* instance;
};

#endif
