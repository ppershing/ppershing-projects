// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_FILE_UTILS
#define H_FILE_UTILS

class FileUtils;

#include "Directory.h"

class FileUtils{
public:
	static bool fileExists(const std::string filename);
	static int getSize(const std::string filename);
};

#endif
