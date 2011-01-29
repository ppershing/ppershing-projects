#ifndef H_SFX
#define H_SFX

class SFX{
    public:
    SFX();
    ~SFX();
    void loadFromMP3(const std::string& fileName);
    private:
    AudioSamples data;
    int playPosition;
}

#endif
