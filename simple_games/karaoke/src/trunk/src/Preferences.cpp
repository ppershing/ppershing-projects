// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#include "Preferences.h"
#include "MyStringUtils.h"
#include "Exceptions.h"
#include "Debug.h"

#include <iostream>
#include <fstream>

Preferences::Preferences* Preferences::instance=NULL;

// {{{ createInstance
void Preferences::createInstance(){ //throw (ESingletonInstance)
    if (instance!=NULL) throw ESingletonInstance(
            "Preferences: create instance called twice");

    instance=new Preferences();
}
// }}}

// {{{ getInstance
Preferences::Preferences* Preferences::getInstance(){
  //throw (ESingletonInstance)
    if (instance==NULL) 
        throw ESingletonInstance(
                "Preferences: get null instance");
        
    return instance;
}
// }}}

// {{{ destroyInstance
void Preferences::destroyInstance(){ //throw (ESingletonInstance)
    if (instance==NULL) throw ESingletonInstance(
            "Preferences: destroy null instance");

    delete instance;
    instance=NULL;
}
// }}}

// {{{ Preferences
Preferences::Preferences(){

}
//}}}

// {{{ ~Preferences
Preferences::~Preferences(){
}
// }}}

// {{{ addPreference
void Preferences::addPreference(std::string s){ //throw (Exception)
    std::vector<std::string> v=MyStringUtils::splitString(s,":");
    if (v.size()!=2) throw Exception(std::string("Preferences:LoadFromFile "
            " file is corrupted at line ")+s);

    DEBUG("Loaded key "+v[0]+"   value "+v[1]);

    if (!MyStringUtils::containsOnly(v[0],
                MyStringUtils::basicCharacters+"/"))
        throw Exception("Preferences:LoadFromFile "
                " have bad characters(1) at line "+s);

    if (!MyStringUtils::containsOnly(v[1],
                MyStringUtils::basicCharacters+"/"))
        throw Exception("Preferences:LoadFromFile "
                " have bad characters(2) at line "+s);

    data[v[0]]=v[1]; // priradime kluc
}
// }}}

//FIXME: rethrow addPreference's exceptions

// {{{ loadFromFile
void Preferences::loadFromFile(const std::string& fileName){
 //throw(EIOError,ECantOpenFile,Exception) 

     DEBUG("Preferences:loadFromFile "+fileName);

    std::ifstream f;
    f.open(fileName.c_str());
    if (f.fail()) throw ECantOpenFile("Preferences:loadFromFile "
            "cant open file "+fileName);
    while (!f.eof()){
        std::string s;
        f>>s;
        if (s=="") continue;

        addPreference(s);

        if (f.fail() || f.bad()) throw EIOError("Preferences:loadFromFile "
                " caused IO error while loading "+fileName);
    }

    f.close();
    DEBUG("load sucessfull");
}

void Preferences::_loadFromFile(const std::string& fileName){
 //throw (EIOError,ECantOpenFile,Exception,ESingletonInstance)
    getInstance()->loadFromFile(fileName);
}
// }}}

// {{{ getInt
int Preferences::getInt(const std::string& key){
 //throw (EIllegalArgument)
     DEBUG("Preferences:getInt "+key);

    if (!haveKey(key)) throw EIllegalArgument(
            "Preferences::getInt without key "+key);
    return MyStringUtils::stringToInt(data[key]);
}

int Preferences::_getInt(const std::string& key){
 //throw (EIllegalArgument,ESingletonInstance)
    return getInstance()->getInt(key);
}
// }}}

// {{{ getDouble
double Preferences::getDouble(const std::string& key){
 //throw (EIllegalArgument)

     DEBUG("Preferences:getDouble "+key);

    if (!haveKey(key)) throw EIllegalArgument(
            "Preferences::getDouble without key "+key);

    return MyStringUtils::stringToDouble(data[key]);
}

double Preferences::_getDouble(const std::string& key){
 //throw (EIllegalArgument,ESingletonInstance)
    return getInstance()->getDouble(key);
}
// }}}

// {{{ getString
std::string Preferences::getString(const std::string& key){
 //throw (EIllegalArgument)
     DEBUG("Preferences:getString "+key);

    if (!haveKey(key)) throw EIllegalArgument(
            "Preferences::getString without key "+key);
    return data[key];
}

std::string Preferences::_getString(const std::string& key){
 //throw (EIllegalArgument,ESingletonInstance)
    return getInstance()->getString(key);
}
// }}}

// {{{ getDefaultInt
int Preferences::getDefaultInt(const std::string& key,const int
        _default){
 //throw (EIllegalArgument)
     DEBUG("Preferences:getDefaultInt "+key);

    if (!haveKey(key)) return _default;
    return MyStringUtils::stringToInt(data[key]);
}

int Preferences::_getDefaultInt(const std::string& key,const int
        _default){
 //throw (EIllegalArgument,ESingletonInstance)
    return getInstance()->getDefaultInt(key,_default);
}
// }}}

// {{{ getDefaultDouble
double Preferences::getDefaultDouble(const std::string& key,const
        double _default){
 //throw (EIllegalArgument)

     DEBUG("Preferences:getDefaultDouble "+key);

    if (!haveKey(key)) return _default;

    return MyStringUtils::stringToDouble(data[key]);
}

double Preferences::_getDefaultDouble(const std::string& key, const
        double _default){
 //throw (EIllegalArgument,ESingletonInstance)
    return getInstance()->getDefaultDouble(key,_default);
}
// }}}

// {{{ getDefaultString
std::string Preferences::getDefaultString(const std::string& key,
        const std::string& _default){
 //throw (EIllegalArgument)
     DEBUG("Preferences:getString "+key);

    if (!haveKey(key)) return _default;
    return data[key];
}

std::string Preferences::_getDefaultString(const std::string& key,
        const std::string& _default){
 //throw (EIllegalArgument,ESingletonInstance)
    return getInstance()->getDefaultString(key,_default);
}
// }}}

// {{{ haveKey
int Preferences::haveKey(const std::string& key) const{
    DEBUG("Preferences: haveKey "+key);
    if (data.find(key)!=data.end()) return 1;
    return 0;
}

int Preferences::_haveKey(const std::string& key){
  //throw (ESingletonInstance,ESingletonInstance)
    return getInstance()->haveKey(key);
}
// }}}

// {{{ setString
void Preferences::setString(const std::string& key,
  const std::string& value){ //throw (EIllegalArgument)
    DEBUG("Preferences: setString "+key+" "+value);
    if (!MyStringUtils::containsOnly(key,
                MyStringUtils::basicCharacters+"/"))
         throw EIllegalArgument("Preferences::setString key "
                 +key+" have bad characters");
    
    if (!MyStringUtils::containsOnly(value,
                MyStringUtils::basicCharacters)+"/")
        throw EIllegalArgument("Preferences::setString value "
                +value+" have bad characters");

    data[key]=value;
}

void Preferences::_setString(const std::string& key,
  const std::string& value){ //throw (EIllegalArgument,ESingletonInstance) 
    getInstance()->setString(key,value);
}

// }}}

// {{{ setInt
void Preferences::setInt(const std::string& key,const int value){
    setString(key,MyStringUtils::intToString(value));
}

void Preferences::_setInt(const std::string& key,const int value){
  // throw (ESingletonInstance)
    getInstance()->setInt(key,value);

}

// }}}

// {{{ setDouble
void Preferences::setDouble(const std::string& key,const double value){
    setString(key,MyStringUtils::doubleToString(value));
}

void Preferences::_setDouble(const std::string& key,
  const double value){ // throw (ESingletonInstance)
    getInstance()->setDouble(key,value);

}

// }}}

// {{{ deleteKey
void Preferences::deleteKey(const std::string& key){
 //throw(EIllegalArgument)

     DEBUG("Preferences:deleteKey "+key);

    if (!haveKey(key)) throw EIllegalArgument("Preferences:deleteKey "
            +key+" not found");
    data.erase(key);
}

void Preferences::_deleteKey(const std::string& key){
  //throw (EIllegalArgument,ESingletonInstance)
      getInstance()->deleteKey(key);
}
// }}}

// {{{ saveToFile

void Preferences::saveToFile(const std::string& fileName){
  //throw (EIOError,ECantOpenFile) 
      DEBUG("Preferences:saveToFile "+fileName);

    std::ofstream f;
    f.open(fileName.c_str());
    if (f.fail()) throw ECantOpenFile("Preferences:saveToFile "
            "cant open file "+fileName);

    std::map<std::string,std::string>::iterator i;
    std::vector<std::string> oldpath,newpath;

    for (i=data.begin();i!=data.end();i++) {
        // ok, first we want to separate different preference paths
        newpath=MyStringUtils::splitString(i->first,"/");
        if (newpath.size()>1) newpath.erase(newpath.end()-1);
        if (newpath!=oldpath) f<<std::endl; // separator
        oldpath=newpath;

        f<<i->first<<":"<<i->second<<std::endl;

        if (f.fail()) throw EIOError("Preferences:saveToFile "+
                fileName+" write error");
    }


    f.close();
    if (f.fail()) throw EIOError("Preferences:saveToFile "+
                fileName+" write error");
    DEBUG("save sucessfull");

}

void Preferences::_saveToFile(const std::string& fileName){
  //throw (EIOError,ECantOpenFile,ESingletonInstance)
        return getInstance()->saveToFile(fileName);

}
// }}}

