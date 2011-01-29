// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk
#ifndef H_RANGE_STATISTICS
#define H_RANGE_STATISTICS

/**
  class providing some templated statistics on
  iterator range
  */
class RangeStatistics{
    public:

        /**
          returns iterator to maximum element
          */
    template <class T>
   static T getRangeMax(T first, T last){
            if (first==last) throw EIllegalArgument(
                    "Statistics::getRangeMax over empty range");
            T result=first;
            T it=first;
            while (it!=last){
                if ((*it)>(*result)) {
                    result=it;
                }

            ++it;
            }
            return result;
        }

    /*
       returns iterator to minimum element
       */
    template <class T>
   static T getRangeMin(T first, T last){
            if (first==last) throw EIllegalArgument(
                    "Statistics::getRangeMin over empty range");
            T result=first;
            T it=first;
            while (it!=last){
                if ((*it)<(*result)) {
                    result=it;
                }

            ++it;
            }
            return result;
        }
};

#endif

