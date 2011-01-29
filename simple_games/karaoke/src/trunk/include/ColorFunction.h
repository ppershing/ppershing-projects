// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_COLOR_FUNCTION
#define H_COLOR_FUNCTION

/**
  class ColorFunction is base class for all
  functions that colors something,
  class returns RGBA value according to
  point [x,y], 0<=x,y<=1
  ideal class for implementing configurable
  visual effects as gradient, fading, ...
  Note that it can't be used for obtaining
  enormous data like painting whole background,
  because is too slow (virtual + not inlined)
  */

class ColorFunction{
    public:
    /**
      return Red component of point [x,y], 0<=x,y<=1
      */
    virtual int R(const double x, const double y)=0;

    /**
      return Greeen component of point [x,y], 0<=x,y<=1
      */
    virtual int G(const double x, const double y)=0;

    /**
      return Blue component of point [x,y], 0<=x,y<=1
      */
    virtual int B(const double x, const double y)=0;

    /**
      return Alpha component of point [x,y], 0<=x,y<=1
      */
    virtual int A(const double x, const double y)=0;
};

#endif
