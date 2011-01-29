// modified and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

/**
 * Copyright (C) 2002 Bart Vanhauwaert
 *
 * Permission to use, copy, modify, distribute and sell this software
 * for any purpose is hereby granted without fee. This license
 * includes (but is not limited to) standalone compilation or as part
 * of a larger project.
 *
 * This software is provided "as is" without express or implied warranty.
 *
 * For a full statement on warranty and terms and conditions for
 * copying, distribution and modification, please see the comment block
 * at the end of this file.
 *
 * Version 1
 *
 */

#ifndef H_DIRECTORY
#define H_DIRECTORY

#if defined(unix) || defined(__unix) || defined(__unix__)
#define OSLINK_OSDIR_POSIX
#elif defined(_WIN32)
#define OSLINK_OSDIR_WINDOWS
#else
#define OSLINK_OSDIR_NOTSUPPORTED
#endif

#include <string>

/**
	Class for iterating files within the directory. Works on Windows either on Unix. Uses Iterator Visitor Pattern.
	@author Miso
	@see FileUtils
*/	

#if defined(OSLINK_OSDIR_NOTSUPPORTED)

class Directory
{
public::
	/**
          Constructor that creates an instance from name of directory. If the directory does not exists, no file is contained within.
          */
	Directory(const std::string&);
	/**
          Destructor
          */
	bool hasNext();
	/**
          Checks if there exists at least one more file
          */
	std::string next();
};

#elif defined(OSLINK_OSDIR_POSIX)

#include <sys/types.h>
#include <dirent.h>

class Directory{
public:
	/**
          Constructor that creates an instance from name of directory.
          */	
	Directory(const std::string& aName);
	/**
          Destructor
          */	
	~Directory();
	/**
          Checks if there exists at least one more file
          */	
	bool hasNext();
	/**
          Returns the name of the next file in the directory
          */	
	std::string next();
private:
	DIR* handle;
	bool willfail;
	/**
          Name of the current file
          */	
	std::string current;
};

#elif defined(OSLINK_OSDIR_WINDOWS)

#include <windows.h>
#include <winbase.h>

class Directory{
public:
	/**
          Constructor that creates an instance from name of directory.
          */
	Directory(const std::string& aName);
	/**
          Destructor
          */
	~Directory();
	/**
          Checks if there exists at least one more file
          */
	bool hasNext();
	/**
          Returns the name of the next file in the directory
          */	  
	std::string next();
private:
	HANDLE	handle;
	bool willfail;
	/**
          Name of the current file
          */	
	std::string current;
};

#endif

#endif

/**
 *
 * The "library", below, refers to the collection of software functions
 * and/or data contained in this file, prepared so as to be conveniently
 * compiled and linked with application programs (which use some of those
 * functions and data) to form executables.
 *
 *                             NO WARRANTY
 *                              
 * 1. BECAUSE THE LIBRARY IS LICENSED FREE OF CHARGE, THERE IS NO
 * WARRANTY FOR THE LIBRARY, TO THE EXTENT PERMITTED BY APPLICABLE LAW.
 * EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR
 * OTHER PARTIES PROVIDE THE LIBRARY "AS IS" WITHOUT WARRANTY OF ANY
 * KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE
 * LIBRARY IS WITH YOU.  SHOULD THE LIBRARY PROVE DEFECTIVE, YOU ASSUME
 * THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.
 * 
 * 2. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN
 * WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY
 * AND/OR REDISTRIBUTE THE LIBRARY AS PERMITTED ABOVE, BE LIABLE TO YOU
 * FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR
 * CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE
 * LIBRARY (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
 * RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
 * FAILURE OF THE LIBRARY TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
 * SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGES.
 * 
 * END OF TERMS AND CONDITIONS
 *
 */
