// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk

#ifndef H_SONG_LIST_SCREEN
#define H_SONG_LIST_SCREEN


#include <stdlib.h>
#include "MyAssert.h"
#include <cassert>
#include "boost/filesystem/operations.hpp"
#include <vector>
#include "Song.h"
#include "SongComparator.h"
#define bfs boost::filesystem

/**
          Class holding basic data about songs. It loads songs at the start of the game.
	@author Miso
	@see Song
*/	
		  
class SongList{
public:
	/**
          Constructor
          */	
	SongList();
	/**
          Destructor
          */	
	~SongList();
        /**
	Loads songs from given directory using Directory.
	@author Miso
	@return 1 if success, -1 if failed.
	@param dirname Name of the directory songs are stored in.
	@see Song
	*/
	int loadFromDir(const std::string dirname);
        /**
        this functions guess font size so font
        will fit into dimenstions
        @author Miso
        @return maximal font size so all songs will fit into box
        @param fontName Name of the font title of the song will be written with.
        @see Font
        */ 
        int getMaxFontSize(const std::string& fontName,const int width, const int height);
        void compareBy(std::string attr);
        void compareByNext();
        /**
          Gets the attribute by which is song list sorted.
          @author Miso
          @return String label of that attribute.
        */
        std::string getSortAttribute();
        /**
          Sorts songlist by current song comparator.
          @author Miso
          @see SongComparator
        */
        void sort();
        /**
          @author Miso
          @param index Index of wanted song.
          @return Song at given index.
        */
        Song getSong(int index);
        /**
          @author Miso
          @param index Index of song after which is song we want.
          @return Song at index right after given index.
        */
        Song getNext(int index);
        /**
          @author Miso
          @param index Index of song after which is song index of we want.
          @return Index of the song right after song at given index.
        */
        int getNextIndex(int index);
        /**
          @author Miso
          @param index Index of song before which is song we want.
          @return Song at index right before given index.
        */
        Song getPrev(int index);
        /**
          @author Miso
          @param index Index of song before which is song index of we want.
          @return Index of the song right before song at given index.
        */
        int getPrevIndex(int index);
        /**
          Iterates through songs and finds one that has UID equal to given UID
          @author Miso
          @param UID UID of wanted song.
          @return Index of the song with given UID. -1 if none has.
        */
        int getIndexFromUID(int UID);
	/**
          List of the songs.
	  @see Song
          */
	std::vector<Song> song;
	/**
          Number of the songs failed to load.
          @author Miso
	  @see Song
          */
	int numSongsFailed;
        std::string familyDirname;
private:
        /**
          Loads songs from given directory path using boost filesystem.
          Recursively finds files with extension .txt, loads header and checks
          if song file is regular.
          @author Miso
          @return 1 if succeed, -1 if failed.
          @param dir_path Path of directory to recursively search.
          @see Song
        */
        int loadFromDirPath(const bfs::path & dir_path);
        /**
          Maximum of song's UIDs
        */
        int maxUID;
        /**
          Comparator for song sorting.
          @see SongComparator
          */
        SongComparator songComparator;
};

#endif
