// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_Y_GRADIENT_COLOR_FUNCTION
#define H_Y_GRADIENT_COLOR_FUNCTION

#include "ColorFunction.h"

/**
  color function that makes gradient in vertical direction
  */
class YGradientColorFunction: public ColorFunction {
    public:
    /**
      returns R component of gradient
      */
    virtual int R(const double x, const double y);

    /**
      returns G component of gradient
      */
    virtual int G(const double x, const double y);

    /**
      returns B component of gradient
      */
    virtual int B(const double x, const double y);

    /**
      returns Alpha component of gradient
      */
    virtual int A(const double x, const double y);

    /**
      sets gradient, R1-A1 is color of bottom point (y=0),
      R2-A2 is gradient of top point (y=1)
      */
    void setGradient(const int R1, const int G1, const int
            B1, const int A1, const int R2, const int G2,
            const int B2, const int A2);

    /**
      constructor,
      default gradient is 255,255,255,255  255,255,255,255
      */
    YGradientColorFunction();
    private:
    /**
      color of bottom point
      */
    int R1,G1,B1,A1;

    /**
      color of top point
      */
    int R2,G2,B2,A2;

};

#endif
