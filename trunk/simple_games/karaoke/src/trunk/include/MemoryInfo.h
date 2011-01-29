// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_MEMORY_INFO
#define H_MEMORY_INFO

/**
  class implementing basic info about memory consumption.
  Note that this not include memory allocated not via malloc/new
  (typical example is shared library loading and all SDL functions
  */
class MemoryInfo{
    public:
        /**
          return actual memory we are using
          */
        static int getOccupiedMemory(void);
        /**
          return maximum memory we have used
          */
        static int getMaxMemory(void);
};


#endif
