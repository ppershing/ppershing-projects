// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "MemoryInfo.h"
#include <malloc.h>

// {{{ getOccupiedMemory
int MemoryInfo::getOccupiedMemory(){
    return mallinfo().uordblks; 
}
// }}}

// {{{ getMaxMemory 
int MemoryInfo::getMaxMemory(){
    return mallinfo().usmblks;
}
// }}}
