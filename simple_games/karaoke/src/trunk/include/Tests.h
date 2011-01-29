// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_TESTS
#define H_TESTS

#include <string>

class Tests{

    public:
        void _init();
        void init();
        void finalize();
        void _finalize();
        void do_tests();
        void do_benchmarks();

        void do_font_tests();
        void do_surface_tests();
        void do_ringbuffer_benchmarks();
        void do_ringbuffer_readWrite(int ringSize,int readSize,int times);

        static Tests instance;

    private:
        int mem;
};

class TestBenchmarkStatus{
    public:
    TestBenchmarkStatus(std::string name);
    ~TestBenchmarkStatus();
    void finalize();

    private:
        int memorySize;
        int startTime;
        std::string benchmark;
};

#endif
