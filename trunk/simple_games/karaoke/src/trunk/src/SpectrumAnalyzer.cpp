// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "SpectrumAnalyzer.h"

// {{{ getSpectrumSize
int SpectrumAnalyzer::getSpectrumSize(){
    return getSpectrumMax()-getSpectrumMin();
}
// }}}
