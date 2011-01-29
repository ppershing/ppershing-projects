// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#define C_ERRORS
#include <stdio.h>
#include "Errors.h"
#include <time.h>
#include <string>
#include "MyAssert.h"


Errors::Errors* Errors::instance=NULL;

const char *Errors::ERRORS_TEXT[Errors::NUM_ERRORS]={"Debug","Notice","Warning",
                "Error","Fatal"};

// {{{ addError
void Errors::addError(const std::string& s,const int errorType) {
    const char *text="Fatal - errorType not found";
    time_t now;
    time(&now);

    if (errorType<logLevel) return;
    if (logFile==NULL) return;  
    if (errorType>=0 && errorType<NUM_ERRORS)  text=ERRORS_TEXT[errorType];
    std::string message=std::string(asctime(localtime(&now)))+" "+text+" "+s;
    
    lastErrors.erase(lastErrors.end()-1);
    lastErrors.insert(lastErrors.begin(),message);
            
    if (fprintf(logFile,"%s\n\n",message.c_str())<0)
        fprintf(stderr,"Fatal - can't write into log\n");
    fflush(logFile);
}


void Errors::_addError(const std::string& s,const int errorType){
 //throw (ESingletonInstance)
    return getInstance()->addError(s,errorType);
}
// }}}

// {{{ Errors
Errors::Errors(const std::string& logName) //throw(ECantOpenFile)
:logName(logName),lastErrors(std::vector<std::string>(1)),logLevel(0){
    logFile=fopen(logName.c_str(),"w");
    addError("-------------- Session begins --------------",NOTICE);
    if (logFile==NULL) {
            fprintf(stderr,"Fatal - can't open log file\n");
            throw ECantOpenFile(logName);
    }
}
// }}}

// {{{ ~Errors
Errors::~Errors(){
    if (logFile==NULL) return;
    addError("-------------- Session ends   --------------",NOTICE);
    if (fclose(logFile)==EOF) 
            fprintf(stderr,"FATAL - Can't close log file\n");
}
// }}}

// {{{ setErrorCacheSize
void Errors::setErrorCacheSize(int size){ //throw (EIllegalArgument)
    if (size<1) throw
         EIllegalArgument(
                 "Invalid argument (size<1) to Errors::setErrorCacheSize");
    lastErrors.resize(size);
}

void Errors::_setErrorCacheSize(int size){
 //throw (EIllegalArgument,ESingletonInstance)
    return getInstance()->setErrorCacheSize(size);
}    
// }}}

// {{{ getLastError
std::string Errors::getLastError(){
    return lastErrors[0];    
}

std::string Errors::_getLastError(){
 //throw (ESingletonInstance)
    return getInstance()->getLastError();
}
// }}}

// {{{ getLastErrors
std::vector<std::string> Errors::getLastErrors(){
    std::vector<std::string> result;
    for (int q=0;q<(int)lastErrors.size();q++) result.push_back(lastErrors[q]);
    return result;
}

std::vector<std::string> Errors::_getLastErrors(){
 //throw (ESingletonInstance)
    return getInstance()->getLastErrors();
}
// }}}

// {{{ setLogLevel
void Errors::setLogLevel(int level){
    logLevel=level;
}

void Errors::_setLogLevel(int level){ //throw (ESingletonInstance)
    return getInstance()->setLogLevel(level);
}    
// }}}

// {{{ destroyInstace
void Errors::destroyInstance(){ //throw (ESingletonInstance) 
   if (instance==NULL) 
       throw ESingletonInstance("Errors: destroying null class");
   delete instance;
   instance=NULL;
}
// }}}

// {{{ getInstance
Errors::Errors* Errors::getInstance(){ //throw (ESingletonInstance) 
    if (instance==NULL) {
        throw ESingletonInstance(std::string(
                    "Errors: no instance to get"));
    }
    return instance;
}
// }}}

// {{{ createInstance
void Errors::createInstance(std::string logFile){
   // throw (ESingletonInstance) 
    if (instance!=NULL) 
        throw ESingletonInstance("Errors: create instance called"
               " twice");

    instance=new Errors::Errors(logFile);
}
// }}}

