// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_MY_MATH
#define H_MY_MATH

#undef log2
#undef mod
class MyMath{
    public:
    static int log2(int x);
    static int isPowerOf2(int x);
    /**
      Returns remainder after modulus for doubles from interval <0,y).
      @author Miso
    */
    static double mod(const double x, const double y);

    static inline double sqr(const double x){
            return x*x;
        }
};

#endif 
