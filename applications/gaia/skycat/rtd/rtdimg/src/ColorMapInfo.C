/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: ColorMapInfo.C,v 1.4 1997/11/19 21:48:44 abrighto Exp $"
 *
 * ColorMapInfo.C - member routines for class ColorMapInfo
 * 
 * See the man page for a complete description.
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 */
static const char* const rcsId="@(#) $Id: ColorMapInfo.C,v 1.4 1997/11/19 21:48:44 abrighto Exp $";


#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <fstream.h>
#include "ColorMapInfo.h"
#include "util.h"
#include "error.h"

// linked list, cache of colormap files
ColorMapInfo* cmaps_ = NULL;

/*
 * constructor - arguments are the name of the colormap
 * and an array of RGB color values. Both are assumed to
 * have been allocated.
 */
ColorMapInfo::ColorMapInfo(char* name, RGBColor* rgb)
    : rgb_(rgb),
      name_(name), 
      next_(cmaps_)
{
    // make this the head of the list of colormaps
    cmaps_ = this;
}


/*
 * destructor
 */
ColorMapInfo::~ColorMapInfo()
{
    // remove from list
    if (this == cmaps_) {
	cmaps_ = next_;
    } 
    else {
	for (ColorMapInfo* m = cmaps_; m; m = m->next_) {
	    if (m->next_ == this) {
		m->next_ = next_;
		break;
	    }
	}
    }
}


/* 
 * read a ColorMap from a file and return  a new instance for it
 */
ColorMapInfo* ColorMapInfo::get(char* filename) 
{
    // just use the basename to identify the colormap
    char* name = strdup(fileBasename(filename));
    
    // see if we read this one already
    ColorMapInfo* m;
    for (m = cmaps_; m; m = m->next()) 
	if (strcmp(m->name(), name) == 0)
	    break;

    if (m) 
	return m;

    // have to read in file
    ifstream f(filename);
    if (! f) {
	error("could not open colormap file: ", filename);
	return (ColorMapInfo*) NULL;
    }

    RGBColor* rgb = new RGBColor[MAX_COLOR];
    if (! rgb) {
	error("could not allocate colormap");
	return (ColorMapInfo*) NULL;
    }
    for (int i = 0; i < MAX_COLOR; i++) {
	f >> rgb[i];
    }
    if (! f) {
	error("error reading colormap file: ", filename);
	return (ColorMapInfo*) NULL;
    }

    m = new ColorMapInfo(name, rgb);
    if (! m) 
	error("could not create colormap");
    
    return m;
}


/* 
 * write a list of loaded colormap files to the given stream
 * separated by spaces
 */
void ColorMapInfo::list(ostream& os) 
{
    ColorMapInfo* m;
    for (m = cmaps_; m; m = m->next()) 
	os << m->name() << " ";
}


/* 
 * set the red, green and blue values for the the colormap
 * and interpolate based on the count of available colors
 */
void ColorMapInfo::interpolate(XColor* colorCells, int colorCount)
{
    int index;

    for (int i=0; i<colorCount; i++) {
	index = (i * (MAX_COLOR - 1))/colorCount; 
	colorCells[i].red = (ushort)(rgb_[index].red * 65535);
	colorCells[i].green = (ushort)(rgb_[index].green * 65535);
	colorCells[i].blue = (ushort)(rgb_[index].blue * 65535);
    }
}


/*
 * rotate the colormap given by src by the given amount, putting
 * the result in dest. colorCount is the number of colors in the
 * colormap.
 */
void ColorMapInfo::rotate(int amount, XColor* src, XColor* dest, int colorCount)
{
    int index;

    for (int i=0; i<colorCount; i++) {
	index = (i - amount) % colorCount;
	if (index < 0) 
	    index += colorCount;
	dest[i].red = src[index].red;
	dest[i].green = src[index].green;
	dest[i].blue = src[index].blue;
    }
}


/*
 * shift the colormap given by src by the given amount, putting
 * the result in dest. colorCount is the number of colors in the
 * colormap.
 */
void ColorMapInfo::shift(int amount, XColor* src, XColor* dest, int colorCount)
{
    int index;

    for (int i=0; i<colorCount; i++) {
	index = (i - amount);
	if (index < 0) 
	    index = 0;
	else if (index >= colorCount) 
	    index = colorCount - 1;
	dest[i].red = src[index].red;
	dest[i].green = src[index].green;
	dest[i].blue = src[index].blue;
    }
}
