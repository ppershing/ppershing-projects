// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk
#ifndef H_VERSION
#define H_VERSION

#include <string>

/**
  returns various information about version of product
  */
class Version{
    public:
    /**
      returns product name
      */
    static std::string getProductName();

    /**
      return build name
      */
    static std::string getBuildName();

    /**
      return state of build (stable/unstable/testing)
      */
    static std::string getBuildState();

    /**
      return date of this build
      */
    static std::string getBuildDate();

    /**
      return full product name, include all information
      */
    static std::string getFullProductName();

    /**
      return list of authors and contributors
      */
    static std::string getAuthors();

    /**
      return version number
      */
    static std::string getVersionString();

    /**
      major number of build
      */
    static int getMajorNumber();

    /**
      minor number of build
      */
    static int getMinorNumber();

    /**
      build number, increases by each build
      */
    static int getBuildNumber();
};

#endif
