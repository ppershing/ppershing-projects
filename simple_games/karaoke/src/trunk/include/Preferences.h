// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_PREFERENCES
#define H_PREFERENCES

#include <string>
#include <map>
#include "Exceptions.h"

/**
  Class for manipulating with ini files
  also provides one singleton instance and
  quickacces to this singleton instance with underscore
  */
class Preferences{
    public:
        /**
          constructor, creates empty registry set
          */
        Preferences();

        /**
          destructor
          */
        ~Preferences();

        /**
          load data from ini file
          on error throws
          a) ECantOpenFile on problem with opening file
          b) EIOError on other IO error
          c) Exception on any other problem
          */
        void loadFromFile(const std::string& fileName);
            //throws (ECantOpenFile,EIOError,Exception);
        /**
          shorthand
          */
        static void _loadFromFile(const std::string& fileName);
            //throws (ECantOpenFile,EIOError,Exception,ESingletonInstance);

        /**
          save data to init file, throws
          a) ECantOpenFile on priblem with opening files
          b) EIOError on other IO errir
          */
        void saveToFile(const std::string& fileName);
            //throws (ECantOpenFile,EIOError);

        /**
          shorthand
          */
        static void _saveToFile(const std::string& fileName);
           //throws (ECantOpenFile,EIOError,ESingletonInstance);
        
        /**
          get string value of key
          */
        std::string getString(const std::string& key);
         // throws (EIllegalArgument);

        /**
          shorthand for standard instance
          */
        static std::string _getString(const std::string& key);
        // throws (EIllegalArgument,ESingletonInstance);

        /**
          get int value of key,
          on non-int key value, return maximum parsed prefix
          */
        int getInt(const std::string& key);
         // throws (EIllegalArgument);

        /**
          shorthand for standard instance
          */
        static int _getInt(const std::string& key);
           // throws (EIllegalArgument,ESingletonInstance);


        /**
          get double value of key,
          on non-double key value, return maximum parsed prefix
          */
        double      getDouble(const std::string& key);
            // throws (EIllegalArgument);

        /**
          shorthand for standard instance
          */
        static double _getDouble(const std::string& key);
           // throws (EIllegalArgument,ESingletonInstance);
        
        /**
          get string value of key if exists
          */
        std::string getDefaultString(const std::string& key,const
                std::string& _default);
         // throws (EIllegalArgument);

        /**
          shorthand for standard instance
          */
        static std::string _getDefaultString(const std::string& key, const
                std::string& _default);
        // throws (EIllegalArgument,ESingletonInstance);

        /**
          get int value of key,
          on non-int key value, return maximum parsed prefix,
          if key do not exists, return default
          */
        int getDefaultInt(const std::string& key,const int _default);
         // throws (EIllegalArgument);

        /**
          shorthand for standard instance
          */
        static int _getDefaultInt(const std::string& key, const int _default);
           // throws (EIllegalArgument,ESingletonInstance);


        /**
          get double value of key,
          on non-double key value, return maximum parsed prefix
          if key do not exists, return default
          */
        double      getDefaultDouble(const std::string& key,const double
                _default);
            // throws (EIllegalArgument);

        /**
          shorthand for standard instance
          */
        static double _getDefaultDouble(const std::string& key,const double
                _default);
           // throws (EIllegalArgument,ESingletonInstance);
        /**
          retuns true, if there exists key with this name
          */
        int         haveKey(const std::string& key) const;

        /**
          shorthand for standard instance
          */
        static int _haveKey(const std::string& key);
            //throws (ESingletonInstance);

        /**
          delete key with this name.
          On non-existing key, throw EIllegalArgument
          */
        void deleteKey(const std::string& key);
        //throws(EIllegalArgument);

        /**
          shorthand for standard instance
          */
        static void _deleteKey(const std::string& key);
           // throws(EIllegalArgument, ESingletonInstance);

        /**
          set value of key,
          on illegal string, throw EIllegalArgument
          */
        void        setString(const std::string& key,
                              const std::string& value);
               // throws (EIllegalArgument);

        /**
          shorthand for standard instances
          */
        static void _setString(const std::string& key,
                              const std::string& value);
               // throws (EIllegalArgument, ESingletonInstance);

        /**
          set value of key
          */
        void        setInt(const std::string& key,const int value);

        /**
          shorthand for standard instance
          */
        static void _setInt(const std::string& key,const int value);
            //throws (ESingletonInstance);

        /**
          set value of key
          */
        void        setDouble(const std::string& key, const double value);

        /**
          shorthand for standard instance
          */
        static void _setDouble(const std::string& key, const double value);
            //throws (ESingletonInstance);

        /**
          create standard instance,
          throw ESingletonInstance if already instantiated
          */
        static void createInstance();
         //throws(ESingletonInstance);

        /**
          destroy standard instance,
          throw ESingletonInstance if destroying null
          */
        static void destroyInstance();
        //throws(ESingletonInstance);

        /**
          get standard instance
          */
        static Preferences* getInstance();
        //throws (ESingletonInstance);
        
    private:
        std::map<std::string,std::string> data; 
        static Preferences* instance;
        /**
          internal function for adding preference from load function
          */
        void addPreference(std::string s);
        //throws(Exception);
};

#endif

