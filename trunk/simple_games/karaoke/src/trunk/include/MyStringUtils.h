// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_MYSTRINGUTILS
#define H_MYSTRINGUTILS

#include <vector>
#include <string>


class MyStringUtils{
    public:
        /**
          convert int to string
          */
        static	std::string intToString(const int value);

        /**
          convert int to Informatic string, eg use Kb,Mb,Gb
          */
        static  std::string intToStringIT(const int value);

        /**
          convert int to SI string, eg use (u - micro) (m-mili)
          (-), (k - kilo), (M - mega) , (G-Giga)
          */
        static  std::string intToStringSI(const int value);

        /**
          convert double to SI string
          @see intToStringSI
          */
        static  std::string doubleToStringSI(const double value);

        /**
          convert double to string
          */
        static	std::string doubleToString(const double value);

        static  std::string doubleToString(const double value,const int
                decimalPlaces);

        /**
          convert string to int
          */
        static	int stringToInt(const std::string& value);

        /**
          convert string to double
          */
        static	double stringToDouble(const std::string& value);


        /**
          split strings
          @param text is string to split
          @param delimiters is string of all characters, that should
           be delimiting tokens
          @param strict if is zero, than splitting won't be strict
             and there may be empty strings in the output,
             if there are two adjacent delimiters 
          @return vector of strings - splited tokens
          */ 
        static	std::vector<std::string> splitString(
                const std::string& text, 
                const std::string& delimiters,
                const int strict);

        /**
          split strings,
          automatically call splitting strings with strict=1
          */
        static	std::vector<std::string> splitString(
                const std::string& text, 
                const std::string& delimiter);

        /**
          Replaces characters in given string by other characters.
          @author Miso
        */
        static std::string replace(std::string text,char c1,char c2);

        /**
          remove leading and trailing spaces from string
          */
        static	std::string trimString(const std::string& text);

        /**
          test if string is composed only from set of characters
          @param text text to test
          @param characters allowed characters
          @returns 1 if text is composed only from allowed characters
          */
        static	int containsOnly(const std::string& text, 
                const std::string& characters);

        /**
          basic set of characters, [a-z][A-Z][0-9].,
          */
        static const std::string basicCharacters;

        /**
          template to convert anything to string
          */
        template <typename T> static std::string toString(const T& i);

        /**
          template to convert string to anything
          */
        template <typename T> static T fromString(const std::string& s);
};
#endif
