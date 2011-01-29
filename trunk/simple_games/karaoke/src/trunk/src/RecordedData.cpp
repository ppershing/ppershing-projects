// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#include "RecordedData.h"

// {{{ constructor
RecordedData::RecordedData(){
}
// }}}

// {{{ constructor
RecordedData::RecordedData(double _pitch){
  pitch = _pitch;
}
// }}}

// {{{ destructor
RecordedData::~RecordedData(){
}
// }}}

// {{{ setColotByTarget
void RecordedData::setColorByTarget(double target){
  double dist = (target - pitch) * (target - pitch);
  R = G = B = 128+127.0/(dist+1.0);
  A = 255;
}
// }}}

