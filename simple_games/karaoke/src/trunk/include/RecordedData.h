// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_RECORDED_DATA
#define H_RECORDED_DATA

class RecordedData{
  public:
    /**
      constructor
    */
    RecordedData();
    /**
      constructor with parameters
    */
    RecordedData(double _pitch);
    /**
      destructor
    */
    ~RecordedData();
    /**
      Sets color of the drawn recorded data by distance from target pitch.
      @author Miso
      @param target Target pitch.
    */
    void setColorByTarget(double target);
    /**
      Recorded pitch.
    */
    double pitch;
    /**
      Color of the drawn recorded data.
    */
    int R,G,B,A;
};

#endif
