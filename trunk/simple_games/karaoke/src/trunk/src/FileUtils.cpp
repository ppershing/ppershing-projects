// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk


#include "Directory.h"
#include "FileUtils.h"
#include "Exceptions.h"

#include<sys/stat.h>
#include<sys/types.h>

#define SUCCESS 0

// {{{  fileExists
bool FileUtils::fileExists(const std::string filename){
	#if defined(OSLINK_OSDIR_NOTSUPPORTED)
	return false;
	#elif defined(OSLINK_OSDIR_POSIX)
	struct stat buffer;
	return(stat(filename.c_str(),&buffer)==0);
	#elif defined(OSLINK_OSDIR_WINDOWS)
	return GetFileAttributes(filename.c_str())!=0xFFFFFFFF;
	#endif
}
// }}}

// {{{  getSize
int FileUtils::getSize(const std::string filename){
	if(!fileExists(filename))throw EIllegalArgument("File not exists");
	#if defined(OSLINK_OSDIR_NOTSUPPORTED)
	return 0;
	#elif defined(OSLINK_OSDIR_POSIX)
	struct stat buffer;
	stat(filename.c_str(),&buffer);
	return buffer.st_size;
	#elif defined(OSLINK_OSDIR_WINDOWS)
	
	#endif
}
// }}}
