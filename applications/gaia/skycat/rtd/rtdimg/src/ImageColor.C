/*
 * E.S.O. - VLT project 
 * "@(#) $Id: ImageColor.C,v 1.10 1999/03/22 21:41:42 abrighto Exp $"
 *
 * ImageColor.C - member routines for class ImageColor
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * Peter W. Draper 05/03/98  Added full support for X visuals in addition
 *                           to pseudocolor (merged my changes from GAIA).
 */
static const char* const rcsId="@(#) $Id: ImageColor.C,v 1.10 1999/03/22 21:41:42 abrighto Exp $";


#include <string.h>
#include <math.h>
#include <iostream.h>
#include "ErrorHandler.h"
#include "ImageColor.h"
#include "error.h"
#include "define.h"


/*
 * constructor: passed the X Display handle and the number of
 * colors to allocate initially.
 */
ImageColor::ImageColor(Display* display, Visual* visual, 
                       int depth, int numColors)
: display_(display),
  visual_(visual),
  screen_(DefaultScreen(display_)),
  depth_(depth),
  cmapSize_(XCellsOfScreen(DefaultScreenOfDisplay(display_))),
  defaultCmap_(DefaultColormap(display_, DefaultScreen(display_))),
  colormap_(DefaultColormap(display_, DefaultScreen(display_))),
  colorCount_(0),
  freeCount_(0),
  cmap_(NULL),
  cmaps_(NULL),
  itt_(NULL),
  itts_(NULL),
  status_(0)
{

  //  Check if the visual we're dealing with is readonly, or if it
  //  can be modified (XXX need to deal with DirectColor).
  if ( visual_->c_class == PseudoColor || visual_->c_class == DirectColor ||
       visual_->c_class == GrayScale ) {
    readOnly_ = 0;
  } else {
    readOnly_ = 1;

    //  Assume all colours are available, and use them.
    cmapSize_ = min(MAX_COLOR,int(pow( 2.0, depth_)));
  }

  //  If default visual isn't the same then create a local colormap.
  Visual *defvis = DefaultVisual(display_, screen_);
  if ( defvis->c_class != visual_->c_class ) {
    colormap_ = XCreateColormap(display_,
                                XRootWindow(display_, screen_),
                                visual_,
                                AllocNone);
  }

  //  Initialisations.
  memset(colorCells_, '\0', sizeof(colorCells_));
  memset(windowList_, '\0', sizeof(windowList_));
  
  allocate(numColors);
}


/* 
 * return the number of free color cells available (up to MAX_COLOR) 
 */
int ImageColor::numFreeColors()
{
    ErrorHandler errorHandler(display_); // catch X errors
    if ( ! readOnly_ ) {
      int i;
      for (i = MAX_COLOR-1; i > 0; i--) {
	if (XAllocColorCells(display_, colormap_, False, 0, 0, pixelval_, i) != 0) {
          XFreeColors(display_, colormap_, pixelval_, i, 0);  // (note: here 0 return means error)
          return i;
	}
      }
    } else {

      //  Readonly so all colors are free.
      return min(MAX_COLOR,int(pow( 2.0, depth_)));
    }
    return 0;
}


/* 
 * free and then re-allocate at most numColors color cells
 */
int ImageColor::reallocate(int numColors)
{
    if (readOnly_) {
	colorCount_ = cmapSize_;
	return 0;
    }

    if (colorCount_) {
	XFreeColors(display_, colormap_, pixelval_, colorCount_, 0);
	colorCount_ = 0;
    }
    if (allocate(numColors) == 0) {
	if (cmap_)
	    if (loadColorMap(cmap_) != 0)
		return 1;
    }
    return 0;
}


/*
 * If we are using a 8-bit pseudocolor visual, this method allocates at most 
 * numColors read/write color cells in the colormap. 
 */
int ImageColor::allocate(int numColors)
{
    if (readOnly_) {
	colorCount_ = cmapSize_;
	return 0;
    }

    // for 8-bit color, allocate read-write color cells
    if (colorCount_) {
	// free colors allocated in the default colormap
	XFreeColors(display_, colormap_, pixelval_, colorCount_, 0);
	colorCount_ = 0;
    }
    freeCount_ =  numFreeColors();

    if (numColors < freeCount_)
	colorCount_ = numColors;
    else
	colorCount_ = freeCount_;
    freeCount_ -= colorCount_;
    if (freeCount_ < 0)
	freeCount_ = 0;
    
    if (colorCount_ <= 0) 
	return error("no more colors available");

    if (XAllocColorCells(display_, colormap_, False, 0, 0, pixelval_, colorCount_) == 0) {
	colormap_ = defaultCmap_;
	freeCount_ = 0;
	colorCount_ = 0;
	return error("error allocating colors for colormap");
    }
    
    for (int i=0; i<colorCount_; i++) {
	colorCells_[i].pixel = pixelval_[i];
	colorCells_[i].flags = DoRed | DoGreen | DoBlue;
    }

    storeColors(colorCells_);
    return 0;
}


/*
 * Start using a private colormap and make an attempt to keep the
 * color values in the original default colormap, to avoid color
 * flashing.
 */
int ImageColor::usePrivateCmap() 
{
    if (readOnly_) 
	return 0;

    // used to save and restore colormap values
    unsigned long pixelval[MAX_COLOR];
    XColor saved_colors[MAX_COLOR];

    ErrorHandler errorHandler(display_); // catch X errors

    if (colormap_ != defaultCmap_) 
	return 0;  // already using a private map

    // get a copy of the default colormap so we can restore most of the colors later
    for (int i = 0; i < cmapSize_; i++) {
	saved_colors[i].pixel = i;
	saved_colors[i].flags = DoRed | DoGreen | DoBlue;
    }
    XQueryColors(display_, colormap_, saved_colors, cmapSize_);
    if (errorHandler.errors()) // check for X errors
	return 1;
	
    if (colorCount_) {
	// free colors allocated in the default colormap
	XFreeColors(display_, colormap_, pixelval_, colorCount_, 0);
	colorCount_ = 0;
    }
    colormap_ = XCreateColormap(display_, XRootWindow(display_, screen_),
				visual_, AllocNone);
    if (errorHandler.errors()) // check for X errors
	return 1;
	
    // XCreateColormap might return the default colormap again...
    if (colormap_ == defaultCmap_) {
	return error("error creating private colormap");
    }

    if (XAllocColorCells(display_, colormap_, False, 0, 0, pixelval, cmapSize_) == 0) {
	colormap_ = defaultCmap_;
	freeCount_ = 0;
	return error("error allocating colors for colormap");
    }

    XStoreColors(display_, colormap_, saved_colors, cmapSize_);
    if (errorHandler.errors()) // check for X errors
	return 1;
	
#if 0
    // XXX note: this doesn't work well, since duplicate colors in the
    // default colormap that we copied cause XAllocColor to return a
    // different pixel value for the color, defeating the whole purpose
    // here, which is to keep the new colormap the same as the old one
    // for the first n colors...

    // now that we have set the color cell values, we can free the cells again.
    XFreeColors(display_, colormap_, pixelval, MAX_COLOR, 0);
    if (errorHandler.errors()) // check for X errors
	return ERROR;

    // reserve the first n colors for the GUI components 
    for (i = 0; i < n; i++)
	if (XAllocColor(display_, colormap_, saved_colors+i) == 0)
	    return error("error allocating read-only colors");
#else
    // XXX note: this version works a little bit better, but any new GUI colors
    // allocated using XAllocColor will not be taken from the first n colors,
    // since they are reserved. There doesn't seem to be anyway to avoid 
    // the color flashing, since even if we don't reserve the first n colors,
    // the pixel values returned from XAllocColor will still be different than
    // those in the original default colormap, due to duplicate pixels: i.e.:
    // in my tests, pixels 0 and 9 were both "white", so when you get to pixel
    // nine and request white, you will most likely get a pixel value of 0 returned,
    // causing flashing in other applications that use pixel 9 for white...

    // free the colors, but not the first n colors (to keep the GUI colors)
    int n = 128;
    XFreeColors(display_, colormap_, pixelval+n, cmapSize_-n, 0);
#endif
    
    if (errorHandler.errors()) // check for X errors
	return 1;

    return 0;
}


/* 
 * If we are using a read-write colormap, store the current colors in it,
 * otherwise allocate read-only colors for the current colors.
 * Returns 0 if all is OK.
 */
int ImageColor::storeColors(XColor* colors) 
{
    ErrorHandler errorHandler(display_); // catch X errors

    if (readOnly_) {
	for (int i = 0; i < colorCount_; i++) {
	    if (!XAllocColor(display_, colormap_, colors+i))
		return fmt_error("can't allocate %d read-only colors (only %d)", 
				 colorCount_, i);
	    pixelval_[i] = colors[i].pixel;
	}
    }
    else {
	XStoreColors(display_, colormap_, colors, colorCount_);
    }
    
    if (errorHandler.errors()) // check for X errors
	return 1;

    return 0;
}


/* 
 * load a color map from the given file
 * where file contains MAX_COLOR lines of (r g b) values
 */
int ImageColor::loadColorMap(char* filename)
{
    ColorMapInfo* m = ColorMapInfo::get(filename);
    if (!m)
	return 1;

    return loadColorMap(m);
}


/* 
 * load or re-load the given color map
 */
int ImageColor::loadColorMap(ColorMapInfo* m)
{
    cmap_ = m;
    
    // set the color values from the colormap file, but reserve the
    // first and last colors for special use
    int n1 = colorCount_-1, n2 = colorCount_-2;
    
    // set first color to black
    colorCells_[0].red = colorCells_[0].green = colorCells_[0].blue = 
	XBlackPixelOfScreen(DefaultScreenOfDisplay(display_));

    m->interpolate(colorCells_+1, n2);

    // set last color default to white
    colorCells_[n1].red =  colorCells_[n1].green = colorCells_[n1].blue = 
	XWhitePixelOfScreen(DefaultScreenOfDisplay(display_));

    // re-install the ITT if necessary
    if (itt_)
      return loadITT(itt_);

    return storeColors(colorCells_);
}


/* 
 * rotate the current colormap/ITT by the given amount
 */
int ImageColor::rotateColorMap(int amount)
{
    if (!cmap_)
	return 0;

    if (!itt_)
	memcpy(ittCells_, colorCells_, sizeof(ittCells_));
    
    // rotate, but reserve first and last cell
    cmap_->rotate(amount, ittCells_+1, colorCells_+1, colorCount_-2);

    if (itt_) 
	memcpy(ittCells_, colorCells_, sizeof(ittCells_));

    storeColors(colorCells_);

    return 0;
}


/* 
 * shift the current colormap/ITT by the given amount
 */
int ImageColor::shiftColorMap(int amount)
{
    if (!cmap_)
	return 0;

    // shift, but reserve first and last cell
    cmap_->shift(amount, colorCells_+1, ittCells_+1, colorCount_-2);
    storeColors(ittCells_);
    
    return 0;
}


/* 
 * load an intensity transfer table (ITT) from the given file
 * where file contains MAX_COLOR ITT values, one per line 
 */
int ImageColor::loadITT(char* filename)
{
    ITTInfo* m = ITTInfo::get(filename);
    if (!m)
	return 1;

    return loadITT(m);
}


/* 
 * load or re-load an intensity transfer table (ITT)
 */
int ImageColor::loadITT(ITTInfo* m)
{
    itt_ = m;
    memcpy(ittCells_, colorCells_, sizeof(ittCells_));
    
    // set the color values based on the itt map, but reserve
    // the first and last colors for special use
    m->interpolate(colorCells_+1, ittCells_+1, colorCount_-2);
    storeColors(ittCells_);
    return 0;
}


/* 
 * scale (squeeze or stretch) the current colormap/ITT by the given amount
 */
int ImageColor::scaleITT(int amount)
{
    if (!itt_)
	return 0;
	
    memcpy(ittCells_, colorCells_, sizeof(ittCells_));

    // scale, but reserve first and last colors
    itt_->scale(amount, colorCells_+1, ittCells_+1, colorCount_-2);

    storeColors(ittCells_);
    return 0;
}


/*
 * reset colormap to original state
 */
int ImageColor::reset()
{ 
    if (!cmap_)
	return 0;
   return loadColorMap(cmap_);
}


/*
 * If we are using a private colormap, set it for the given window.
 */
int ImageColor::setColormap(Tk_Window w)
{
    if (colormap_ != defaultCmap_) 
	Tk_SetWindowColormap(w, colormap_);
    return 0;
}





