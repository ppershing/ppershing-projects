// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk

#ifndef H_FONT
#define H_FONT


#include <string>
#include <SDL/SDL_ttf.h>

class Graphics;

class Font{
  public:
    friend class Graphics;
    /**
      constructor, creates null font
      */
    Font();

    /**
      constructor from name and size,
      throws EIllegalArgument on failure
      */
    Font(const std::string& fontName, const int pt);
    
    /**
      pointer-safe copy of font
      */
    Font& operator=(const Font& f);

    /**
      copy constructor
      */
    Font(const Font& f);

    /**
      copy destructor
      */
    ~Font();

    /**
      return optimal line spacing
      */
    int getLineHeight();

    /**
      return height of the font
      */
    int getHeight();

    /**
      calculates width of text,
      note that this may be variable for
      equal-length strings
      */
    int getWidth(const std::string& text);


    /**
      this functions guess font size so font
      will fit into dimenstions
      */
    static int getMaxFontSize(const std::string& fontName,
                const std::string& text,
                const int width, const int height);

    static int getMaxFontSize(const std::string& fontname,
                const std::string& text,
                const int width);

  private:
    /**
      return pointer to font, checks for null
      */
    TTF_Font* getFont();

    /**
      cleanup old data
      */
    void cleanup();

    /**
      assign new data
      */
    void assign(const Font& f);

    /**
      number of instances pointing to same TTF_Font
      */
    int* referenceCount;

    /**
      pointer to actual font
      */
    TTF_Font* font;
};


#endif
