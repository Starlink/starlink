// -*-c++-*-
#ifndef _ImageColor_h_
#define _ImageColor_h_
/*
 * E.S.O. - VLT project 
 * "@(#) $Id: ImageColor.h,v 1.8 1998/09/28 21:54:26 abrighto Exp $" 
 *
 * ImageColor.h - class definitions for class ImageColor
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 */

#include <sys/types.h>
#include <tk.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "ColorMapInfo.h"
#include "ITTInfo.h"


/* 
 * An instance of this class is used to manage colors and colormaps
 * for RtdImage and derived widgets
 */
class ImageColor {
private:
    Display* display_;		// X display info
    Visual* visual_;		// X visual
    int screen_;		// default screen
    int depth_;			// screen visual depth (8, 16, 24, ...)
    int readOnly_;		// flag: true if we are using read-only colors
    int cmapSize_;		// size of colormap

    Colormap defaultCmap_;	// default colormap
    Colormap colormap_;		// current colormap

    int colorCount_;		// count of allocated colors
    int freeCount_;		// number of colors to leave free

    // color pixel values
    unsigned long pixelval_[MAX_COLOR];

    // colormap cells
    XColor colorCells_[MAX_COLOR]; 
    XColor ittCells_[MAX_COLOR]; // cells after applying ITT 

    // list of colormap info read from files
    ColorMapInfo* cmaps_;

    // current colormap
    ColorMapInfo* cmap_;

    // list of ITT info read from files
    ITTInfo* itts_;

    // current ITT
    ITTInfo* itt_;

    // keep a list of windows, whose colormaps are managed
    Tk_Window windowList_[126];

    // status after constructor
    int status_;
    
    // -- private methods --
    
    // reload existing colormap/itt 
    int loadColorMap(ColorMapInfo*);
    int loadITT(ITTInfo*);
    int storeColors(XColor* colors);

public:
    // constructor
    ImageColor(Display*, Visual*, int depth, Colormap colormap, int numColors);
    
    // member functions
    int numFreeColors();
    int allocate(int numFreeColors);
    int reallocate(int numFreeColors);

    // load (reload) a color map from the given file
    int loadColorMap(char* filename);

    // load (reload) an ITT from the given file
    int loadITT(char* filename);

    // rotate the colormap by the given amount
    int rotateColorMap(int amount);

    // shift the colormap by the given amount
    int shiftColorMap(int amount);

    // scale the current colormap/ITT by the given amount
    int scaleITT(int amount);

    // reset colormap to original state
    int reset();

    // start using a private colormap
    int usePrivateCmap();

    // return true if we are using a private colormap
    int usingPrivateCmap() {return (colormap_ != defaultCmap_);}

    // if we are using a private colormap, set it for the given window
    int setColormap(Tk_Window);


    // member access
    int freeCount() const {return freeCount_;}
    int colorCount() const {return colorCount_;}
    unsigned long* pixelval() {return pixelval_;}
    unsigned long pixelval(int i) const {return pixelval_[i];}
    XColor* colorCells() {return itt_ ? ittCells_ : colorCells_;}
    int depth() const {return depth_;}
    const ColorMapInfo* cmap() const {return cmap_;}
    const ITTInfo* itt() const {return itt_;}
    int status() const {return status_;}
};


#endif /* _ImageColor_h_ */
