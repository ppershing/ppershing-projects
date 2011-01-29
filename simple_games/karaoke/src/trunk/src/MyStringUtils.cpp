// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include <string>
#include <vector>
#include <iostream>
#include <sstream>

#include "NoDebug.h"
#include "MyStringUtils.h"

// {{{ basicCharacters
const std::string MyStringUtils::basicCharacters = std::string(
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXY"
        "0123456789.,-");
// }}}

// {{{ template toString
template <typename T> std::string MyStringUtils::toString(const T& i) {
    std::ostringstream ss;
    ss << i;
    return ss.str();
}
// }}}

// {{{ template fromString
template <typename T> T MyStringUtils::fromString(const std::string& s) {
    std::stringstream ss;
    T result;
    ss.str(s);
    ss>>result;
    return result;
}
// }}}

// {{{ intToString
std::string MyStringUtils::intToString(const int value){
    return toString(value);
}
// }}}

// {{{ intToStringIT 
std::string MyStringUtils::intToStringIT(const int value){
    const int powers[4]={1,1024,1024*1024,1024*1024*1024};
    const int decimalPart=50;
    const char* modifiers[4]={"","K","M","G"};
    const int maxModifier=4;
    int m=0;
    while (m<maxModifier-1 && value>powers[m+1]) m++;
    if (value/powers[m]>decimalPart || m==0)
        return intToString(value/powers[m])+modifiers[m]+'B';
    return doubleToString((double)value/powers[m],2)+modifiers[m]+'B';
}
// }}}

// {{{ intToStringSI
std::string MyStringUtils::intToStringSI(const int value){
    return doubleToStringSI((double) value);
}
// }}}

// {{{
std::string MyStringUtils::doubleToStringSI(const double value){
    const char* modifiers[6]={"u","m","","k","M","G"};
    const double k=1000; // kilo
    const int noModifier=2;
    const double maxValue=100;
    const int modifierCount=6;
    double v=value;
    for (int q=0;q<noModifier;q++) v*=k;
    int m=0;
    while (m<modifierCount-1 && v>maxValue){
        v/=k;
        m++;
    }
    if (v<0.0005) return std::string("~0");
     else
    if (v<1) return doubleToString(v,3)+modifiers[m];
    else if (v<10) return doubleToString(v,2)+modifiers[m];
    else return doubleToString(v,1)+modifiers[m];
}
// }}}

// {{{ doubleToString(value)
std::string MyStringUtils::doubleToString(const double value){
    char tmp[100];
    sprintf(tmp,"%f",value);
    return std::string(tmp);
}
// }}}

// {{{ doubleToString(value,precision)
std::string MyStringUtils::doubleToString(const double value,const int
    precision){
    char tmp[100];
    sprintf(tmp,"%.*f",precision,value);
    return std::string(tmp);
}
// }}}

// {{{stringToInt
int MyStringUtils::stringToInt(const std::string& value){
    return fromString<int>(value);
}
// }}}

// {{{ stringToDouble
double MyStringUtils::stringToDouble(const std::string& value){
    return fromString<double>(value);
}
// }}}

// {{{ splitString
std::vector<std::string> MyStringUtils::splitString(
  const std::string& text, const std::string& delimiters, int strict){

    std::vector<std::string> result;
    int split;
    std::string temp;

    for(int q=0;q<(int) text.size(); q++) {
        split=0; 
        for (int w=0;w<(int) delimiters.size();w++)
            if (text[q]==delimiters[w]) split=1;

        if (split) {
            if (strict || temp.size()>0) result.push_back(temp);
            temp=""; 
        } else {
            temp+=text[q];
        }
    }
    if (temp.size()>0u) result.push_back(temp);
    return result;
}


std::vector<std::string> MyStringUtils::splitString(
  const std::string& text, const std::string& delimiters){
    return splitString(text,delimiters,1); // default is strict (no
                                           // empty tokens )
}
// }}}

// {{{ replace
std::string MyStringUtils::replace(std::string text,const char c1,const char c2){
  std::string s=text;
  for(std::string::iterator it = s.begin();it != s.end();it++){
    if(*it==c1)
      *it=c2;
  }
  return s;
}
// }}}

// {{{ containsOnly
int MyStringUtils::containsOnly(
  const std::string& text, const std::string& characters){
        DEBUG("containsOnly '"+text+"' "+characters);

    for (int q=0;q<(int) text.size();q++){
        int ok=0;
        for (int w=0;w<(int) characters.size(); w++)
            if (text[q]==characters[w]) ok=1;
        
        if (!ok) {
            DEBUG("fail at "+text[q]);
            return 0;
        }
    }
    return 1;
}
// }}}

// {{{trimString
std::string MyStringUtils::trimString(const std::string& text){
    int first=0;
    while (first<(int) text.size() && text[first]==' ') first++;
    if (first==(int) text.size()-1) return std::string(""); // empty string
    int end=text.size()-1;
    while (text[end]==' ') end--;
    return text.substr(first,end-first);
}
// }}}
