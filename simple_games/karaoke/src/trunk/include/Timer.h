// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_TIMER
#define H_TIMER

class Timer{
    public:
      void start();
      double tick();
      double getTimeFromStart();
      double getFPS();

      static double getSystemTime();
      double getCalibratedSystemTime();
      /**
      @author Miso
      @param time when we want to get calibrated time at.
      */
      double getCalibratedSystemTimeAt(double time);
      /**
        @return callibration
      */
      double setCalibratedSystemTime(double time);
    private:
      double callibration;

      double startTime;
      double lastTime;
      double fpsStartTime;
      double lastFPS;
      int ticks;
      int fpsTicks;
      static double fpsTime;
};

#endif
