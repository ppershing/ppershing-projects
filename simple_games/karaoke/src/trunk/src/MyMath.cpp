// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "MyMath.h"
#include "Exceptions.h"

// {{{ log2 
int MyMath::log2(int x){
    // log(0)=-inf, log(1)==0, log(2)==1, log(3)==2, log(4)==2,
    // log(5)==3, ...
    if (x<=0) throw EIllegalArgument("Trying to take logarithm of"
            " non-positive integer");
    --x;
    int cnt=0;
    while (x>0) {x/=2; cnt++; }
    return cnt;
}
// }}}

// {{{ isPowerOf2
int MyMath::isPowerOf2(int x){
    if (x<=0) throw EIllegalArgument("Trying to take isPowerOf2 from"
            " non-positive integer");
    return (x&(x-1))==0;
}
// }}}

// {{{ mod
double MyMath::mod(const double x, const double y){
  if(y<=0) throw EIllegalArgument("Division by zero");
  int q = x/y;
  //printf("x=%f, y=%f, q=%f, mod=%f",x,y,q,x-(double)q);
  return x-(double)q*y;
}
// }}}
