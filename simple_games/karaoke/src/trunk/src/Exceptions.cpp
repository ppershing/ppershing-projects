// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "Exceptions.h"
#include <cstring>
#include "Errors.h"
#include "StackTrace.h"

//{{{ Exception
Exception::Exception(const std::string& s) throw(): 
    message("General Exception: "+s){
}

Exception::Exception(const std::string& s,const std::exception& cause)
    throw(): 
    message("General Exception: "+s+" caused by:\n"+cause.what()){ 
}

Exception::~Exception() throw(){
}
// }}}

//{{{ EAssertionFailed
EAssertionFailed::EAssertionFailed(const std::string& s) throw():Exception(s){
}


EAssertionFailed::~EAssertionFailed() throw(){
}
// }}}

//{{{ ECantOpenFile
ECantOpenFile::ECantOpenFile(const std::string &s) throw():
    Exception("Can't open file: "+s){
}

ECantOpenFile::~ECantOpenFile() throw(){
}
// }}}

// {{{ EIllegalArgument
EIllegalArgument::EIllegalArgument(const std::string& s) throw():
    Exception("Illegal argument: "+s){

//        std::string message="Illegal argument: "+s+
//            " | stack: "+getStringStack();
//    Errors::_addError(message,Errors::ERROR);
}

EIllegalArgument::~EIllegalArgument() throw(){
}
// }}}

// {{{ EIOError
EIOError::EIOError(const std::string& s) throw():Exception("IO Error: "+s){
}

EIOError::~EIOError() throw(){

}
// }}}

// {{{ EAudioDecode
EAudioDecode::EAudioDecode(const std::string& s) throw():
    Exception("Audio Decode Error: "+s){
}

EAudioDecode::~EAudioDecode() throw(){
}
// }}}

// {{{ ESingletonInstance
ESingletonInstance::ESingletonInstance(const std::string& s) throw():
    Exception("Singleton instance error: "+s){
}

ESingletonInstance::~ESingletonInstance() throw(){
}
// }}}

//{{{ ENotImplemented
ENotImplemented::ENotImplemented(const std::string& s) throw():
    Exception("Called function is not implemented: "+s){
}

ENotImplemented::~ENotImplemented() throw(){
}
// }}}
