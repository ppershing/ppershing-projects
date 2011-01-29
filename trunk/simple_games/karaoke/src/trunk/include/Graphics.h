// created and maintained by miso
// please report any bug or suggestion to miso<>ksp<>sk


#ifndef H_GRAPHICS
#define H_GRAPHICS

/**
*	Static class for safely drawing primitives on the screen.
*	@author Miso
*	@see Surface
*	@see Font
*/		

class Graphics; 
#include "Surface.h"
#include <SDL/SDL.h>
#include "MyStringUtils.h"
#include "Font.h"

class Graphics{
public:
	/**
          Checks whether the point is inside the surface.
	@author PP
	@return void
	@param screen Reference to the screen wanted to check on
	@param x x-coordinate of point
	@param y y-coordinate of point
          */
	static void checkPoint(Surface& screen, const int x,const int y);
	static Graphics* Instance();
	/**
          Draws the pixel on the surface, and checks it by checkPoint.
	@author Miso
	@return void
	@param screen Reference to the screen wanted to draw on
	@param x x-coordinate of point
	@param y y-coordinate of point
	@param R Red part of the color of the point
	@param G Green part of the color of the point
	@param B Blue part of the color of the point
	@param A Alpha of the point
	@see Surface
          */
	static void drawPixel(Surface& screen, int x, int y, 
                const int R, const int G, const int B, const int A);
	/**
          Draws text on the surface with desired font.
	@author Miso
	@return void
	@param screen Reference to the screen wanted to write on
	@param font Font to write the text in 
	@param text Text to write
	@param x x-coordinate of text
	@param y y-coordinate of point
	@param R Red part of the color of the text
	@param G Green part of the color of the text
	@param B Blue part of the color of the text
	@param A Alpha of the text
	@see Font
	@see Surface
          */
	static void drawText(Surface& screen, Font& font, const std::string text, 
                const int x,const int y, const int R, const int G, const int B, const int A);
	/**
          draws text on the surface with desired font and returns that surface
	@author Miso
	@return A pure new Surface with Text written on.
	@param screen Reference to the screen wanted to write on
	@param font Font to write the text in.
	@param text Text to write
	@param x x-coordinate of text
	@param y y-coordinate of point
	@param R Red part of the color of the text
	@param G Green part of the color of the text
	@param B Blue part of the color of the text
	@param A  Alpha of the text
	@see Surface
	@see Font
          */			
	static Surface textToSurface(Surface& screen, Font& font, const std::string text, 
                const int x,const int y, const int R, const int G, const int B, const int A);
	/**
          draws a filled rectangle on the surface
	@author PP
	@return void
	@param screen Reference to the screen wanted to draw on
	@param  x1 x-coordinate of the first corner of the rectangle
	@param  y1 y-coordinate of the first corner of the rectangle
	@param  x2 x-coordinate of the second corner of the rectangle
	@param  y2 y-coordinate of the second corner of the rectangle
	@param  R Red part of the color of the rectangle
	@param  G Green part of the color of the rectangle
	@param  B Blue part of the color of the rectangle
	@param  A  Alpha of the rectangle
	@see Surface
          */		
	static void filledRectangle(Surface& screen, const int x1, const int y1, 
                const int x2, const int y2, const int R, const int G, const int B, const int A);
	/**
          draws a filled rectangle on the surface with  costructed rectangle
	@author PP
	@return void
	@param screen Reference to the screen wanted to draw on
	@param *rect Pointer to rectangle wanted to draw
	@param R Red part of the color of the rectangle
	@param G Green part of the color of the rectangle
	@param B Blue part of the color of the rectangle
	@param A  Alpha of the rectangle
	@see Surface
          */	
	static void filledRectangle(Surface& screen, SDL_Rect *rect, 
                const int R, const int G, const int B, const int A);
	/**
          draws a empty rectangle on the surface
	@author Miso
	@return void
	@param screen [Reference to the screen wanted to draw on
	@param x1 x-coordinate of the first corner of the rectangle
	@param y1 y-coordinate of the first corner of the rectangle
	@param x2 x-coordinate of the second corner of the rectangle
	@param y2 y-coordinate of the second corner of the rectangle
	@param R Red part of the color of the rectangle
	@param G Green part of the color of the rectangle
	@param B Blue part of the color of the rectangle
	@param A Alpha of the rectangle
	@see Surface
          */	
	static void rectangle(Surface& screen, const int x1, const int y1, 
                const int x2, const int y2, const int R, const int G, const int B, const int A);
	/**
          draws a empty rectangle on the surface with  costructed rectangle
	@author Miso
	@return void
	@param screen Reference to the screen wanted to draw on
	@param *rect Pointer to rectangle wanted to draw
	@param R Red part of the color of the rectangle
	@param G Green part of the color of the rectangle
	@param B Blue part of the color of the rectangle
	@param A Alpha of the rectangle
	@see Surface
          */	
	static void rectangle(Surface& screen, SDL_Rect *rect, 
                const int R, const int G, const int B, const int A);
	/**
          draws a line on the surface
	@author Miso
	@return void
	@param screen Reference to the screen wanted to draw on
	@param x1 x-coordinate of the first end of the line
	@param y1 y-coordinate of the first end of the line
	@param x2 x-coordinate of the second end of the line
	@param y2 y-coordinate of the second end of the line
	@param R Red part of the line
	@param G Green part of the line
	@param B Blue part of the line
	@param A Alpha of the line
	@see Surface
          */	
	static void line(Surface& screen, const int x1, const int y1, 
                const int x2, const int y2, const int R, const int G, const int B, const int A);
        /**
          Draws a circle on the surface.
          @author Miso
          @return void
          @param screen Reference to the screen wanted to draw on.
          @param x x-coordinate of the center of the circle.
          @param y y-coordinate of the center of the circle.
          @param r radius of circle in pixels.
          @param R Red part of the color.
          @param G Green part of the color.
          @param B Blue part of the color.
          @param A Alpha of the color.
          @see Surface
        */
        static void circle(Surface& screen, const int x, const int y,
            const int r, const int R, const int G, const int B, const int A);
        /**
          Draws a filled circle on the surface.
          @author Miso
          @return void
          @param screen Reference to the screen wanted to draw on.
          @param x x-coordinate of the center of the circle.
          @param y y-coordinate of the center of the circle.
          @param r radius of circle in pixels.
          @param R Red part of the color.
          @param G Green part of the color.
          @param B Blue part of the color.
          @param A Alpha of the color.
          @see Surface

        */
        static void filledCircle(Surface& screen, const int x, const int y,
                const int r, const int R, const int G, const int B, const int A);
	/**
          dont really know what does this one do:)
	@author Miso
	@return void
	@param screen Reference to the screen wanted to draw on
	@see Surface
          */	
	static void flip(Surface& screen);
};



#endif
