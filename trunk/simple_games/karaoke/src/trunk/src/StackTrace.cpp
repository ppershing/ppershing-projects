// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "StackTrace.h"


#define HELPER(i)  stackData[stackDataLength++] = \
                    ((pFrame = __builtin_frame_address(i)) != NULL) \
                    ? __builtin_return_address(i) : NULL; \
                if (pFrame==NULL) return; \
                if (stackDataLength == _MAX_STACK_DATA) return;

// {{{ StackTrace
StackTrace::StackTrace():stackDataLength(0){
    void *pFrame;
    HELPER(0);
    HELPER(1);
    HELPER(2);
    HELPER(3);
    HELPER(4);
    HELPER(5);
    HELPER(6);
    HELPER(7);
    HELPER(8);
    HELPER(9);
    HELPER(10);
    HELPER(11);
    HELPER(12);
    HELPER(13);
    HELPER(14);
    HELPER(15);
    HELPER(16);
    HELPER(17);
    HELPER(18);
    HELPER(19);
}
// }}}

// {{{ ~StackTrace
StackTrace::~StackTrace(){

}
// }}}

// {{{ getStringStackTrace
std::string StackTrace::getStringData() const {
    std::string s;
    char cstr[12];
    for (int q=_STRING_SKIP_FRAMES;q<stackDataLength;q++) {
            sprintf(cstr,"%x ",(unsigned int)stackData[q]);
            s=s+cstr;
        }
   return s;
}
// }}}

// {{{ getStackData
std::vector<unsigned int> StackTrace::getStackData() const {
    std::vector<unsigned int> result(stackDataLength);
    for (int i=0; i<stackDataLength; i++)
         result[i]=(unsigned int) stackData[i];
    return result;

}
// }}}
