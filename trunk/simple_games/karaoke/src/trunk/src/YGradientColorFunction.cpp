// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "YGradientColorFunction.h"
#include "ColorFunction.h"
//FIXME: checkovanie hodnot

// {{{ YGradientColorFunction
YGradientColorFunction::YGradientColorFunction(){
    R1=G1=B1=A1=R2=G2=B2=A2=255;
}
// }}}

// {{{ setGradient
void YGradientColorFunction::setGradient(const int R1, const int G1, const int
            B1, const int A1, const int R2, const int G2,
            const int B2, const int A2){
    this->R1=R1;
    this->G1=G1;
    this->B1=B1;
    this->A1=A1;

    this->R2=R2;
    this->G2=G2;
    this->B2=B2;
    this->A2=A2;
}
// }}}


// {{{ R
int YGradientColorFunction::R(const double x __attribute__ ((unused)) , const double y){
   return (int)((R2-R1)*y)+R1;
}
// }}}

// {{{ G
int YGradientColorFunction::G(const double x __attribute__ ((unused)) , const double y){
   return (int)((G2-G1)*y)+G1;
}
// }}}

// {{{ B
int YGradientColorFunction::B(const double x __attribute__ ((unused)) , const double y){
   return (int)((B2-B1)*y)+B1;
}
// }}}

// {{{ A
int YGradientColorFunction::A(const double x __attribute__ ((unused)) , const double y){
   return (int)((A2-A1)*y)+A1;
}
// }}}
