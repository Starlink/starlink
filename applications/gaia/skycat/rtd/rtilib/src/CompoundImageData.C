/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id" 
 *
 * CompoundImageData.C - member functions for class CompoundImageData
 *
 * This class is used for images that are divided into multiple HDUs
 * (FITS header/data units, FITS image extensions). The idea here 
 * is to make it appear to the using class as if there is a single image.
 * The image extensions are combined based on the WCS information in
 * each header.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  14/02/00  Created
 */


#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <cstring>
#include <cassert>
#include <cmath>
#include "define.h"
#include "FitsIO.hxx"
#include "error.h"
#include "CompoundImageData.h"


/*
 * Constructor: Create an array of image objects, one for each 
 * image extension and initialize the image size to be the 
 * complete size of all of the images.
 *
 * name   -  the name of the image (for logging/debugging
 * imio   -  the object representing the FITS image file
 * hduList - the list of image HDU indexes to use
 * numHDUs - the number of image HDU indexes in hduList
 * biasInfo - used when calculating bias info
 * verbose - print diagnostic messages
 */
CompoundImageData::CompoundImageData(const char* name, const ImageIO& imio, 
				     int* hduList, int numHDUs,
				     biasINFO* biasInfo, int verbose)
    : ImageData(name, imio, verbose)
{
    numImages_ = numHDUs;
    images_ = new ImageData*[numImages_];
    minX_ = maxX_ = minY_ = maxY_ = 0.;

    // make sure it is a FITS file
    if (!imio.rep() || strcmp(imio.rep()->classname(), "FitsIO") != 0) {
        status_ = error("The \"hdu\" subcommand is only supported for FITS files");
	return;			// error
    }
    FitsIO* fits = (FitsIO*)imio.rep();

    // create images and note the min/max coordinates
    for(int i = 0; i < numImages_; i++) {
	// need a (reference counted) copy so we can change the current HDU
	FitsIO* fitsExt = fits->copy();
	if ((status_ = fitsExt->setHDU(hduList[i])) != 0) {
	    delete fitsExt;
	    return;		// error
	}
	
	images_[i] = ImageData::makeImage(name, fitsExt, biasInfo, verbose);

	double x0 = -images_[i]->crpix1_,
	    y0 = -images_[i]->crpix2_,
	    x1 = x0 + images_[i]->width_ - 1,
	    y1 = y0 + images_[i]->height_ - 1;
	
	if (i == 0) {
	    minX_ = min(x0, x1);
	    minY_ = min(y0, y1);
	    maxX_ = max(x0, x1);
	    maxY_ = max(y0, y1);
	}
	else {
	    minX_ = min(min(x0, x1), minX_);
	    minY_ = min(min(y0, y1), minY_);
	    maxX_ = max(max(x0, x1), maxX_);
	    maxY_ = max(max(y0, y1), maxY_);
	}
    }
    
    // reset image dimensions to imclude the entire collection of images
    dispWidth_ = width_ = int(maxX_ - minX_ + 1);
    dispHeight_ = height_ = int(maxY_ - minY_ + 1);
    area_ = width_*height_;

}


/*
 * copy constructor
 *
 * Note that the classes managing the image data (ImageIO, Mem, ...) use
 * reference counting, so that the data is not actually copied normally,
 * but shared as long as possible.
 */
CompoundImageData::CompoundImageData(const CompoundImageData& im)
    : ImageData(im),
      numImages_(im.numImages_),
      minX_(im.minX_),
      maxX_(im.maxX_),
      minY_(im.minY_),
      maxY_(im.maxY_)
{
    // copy the images_ array
    images_ = new ImageData*[numImages_];
    for(int i = 0; i < numImages_; i++) {
	images_[i] = im.images_[i]->copy();
    }
}


/*
 * Destructor: delete the image array.
 */
CompoundImageData::~CompoundImageData() {
    for(int i = 0; i < numImages_; i++) {
	delete images_[i];
    }
    delete images_;
}


/*
 * Set x0, y0, x1, y1 to the bounds of the given image data in FITS image coordinates.
 */
void CompoundImageData::getBounds(ImageData* imageData, double& x0, double& y0, double& x1, double& y1) 
{
    x0 = -imageData->crpix1_ - minX_;
    y0 = -imageData->crpix2_ - minY_;
    x1 = x0 + imageData->width_ - 1;
    y1 = y0 + imageData->height_ - 1;
}


/*
 * Specify a new lookup table to use for this image and its size.
 * Redefined from parent class to pass on to each image extension.
 */
int CompoundImageData::lookupTable(LookupTable lookup)
{
    if (ImageData::lookupTable(lookup) != 0)
	return 1;		// error

    for(int i = 0; i < numImages_; i++) {
	if (images_[i]->lookupTable(lookup) != 0)
	    return 1;		// error
    }

    return 0;
}


/*
 * This method is used to save color information for pos. later use
 *
 * Note that the first and last colors are reserved for special use:
 * The first color should be black and the last color can be set to
 * some saturation color.
 */
void CompoundImageData::setColors(int ncolors, unsigned long* colors) 
{
    ImageData::setColors(ncolors, colors);

    for(int i = 0; i < numImages_; i++) {
	images_[i]->setColors(ncolors, colors);
    }
}


/*
 * Set the destination XImage buffer and the dimensions of the XImage in
 * pixels. (This class copies the rawimage to xImage, doing any necessary
 * transformations along the way.)
 */
void CompoundImageData::setXImage(ImageDisplay* xImage)
{
    ImageData::setXImage(xImage);

    for(int i = 0; i < numImages_; i++) {
	images_[i]->setXImage(xImage);
    }
}


/*
 * Save the contents of the given region of this image to the given file.
 * The coordinates are expected in image pixel units.
 */
int CompoundImageData::write(const char* filename, double rx0, double ry0, double rx1, double ry1)
{
    // XXX to be implemented
    return error("Sorry: saving a region is not supported for compound images yet");
}


/*
 * Copy the cutlevels, scale, rotate and flip parameters from the
 * given struct to this image (see saveParams()).
 * If restoreCutLevels is non-zero (default), the saved cut-levels are restored
 * otherwise they are not and the cutlevels are left as they were 
 * initialized (to the approx. min/max pixel value).
 *
 * Redefined from parent class to propagate to the image extensions. 
 */
void CompoundImageData::restoreParams(ImageDataParams& p, int restoreCutLevels)
{
    if (p.status != 0) 
	return;			// don't use if status != 0

    ImageData::restoreParams(p, restoreCutLevels);

    for(int i = 0; i < numImages_; i++) {
	images_[i]->restoreParams(p, restoreCutLevels);
    }
}


/*
 * create the color scale lookup table for the image.
 * The arguments are:
 * 
 * ncolors -  the number of available colors 
 * colors -   an array of pixel values for the available colors
 *
 * The color scaling algorithm used is determined by the value of
 * colorScaleType_, which defaults to LINEAR_SCALE.
 */
void CompoundImageData::colorScale(int ncolors, unsigned long* colors)
{
    int i;
    for(i = 0; i < numImages_; i++) {
	if (i == 0) {
	    scaledLowCut_ = images_[i]->scaledLowCut_;
	    scaledHighCut_ = images_[i]->scaledHighCut_;
	}
	else {
	    scaledLowCut_ = min(images_[i]->scaledLowCut_, scaledLowCut_);
	    scaledHighCut_ = max(images_[i]->scaledHighCut_, scaledHighCut_);
	}
    }
    
    ImageData::colorScale(ncolors, colors);

    for(i = 0; i < numImages_; i++) {
	images_[i]->lookupTable(lookup_);
    }
}


/*
 * set the scaling (zoom) factor
 */
void CompoundImageData::setScale(int xScale, int yScale)
{
    ImageData::setScale(xScale, yScale);

    for(int i = 0; i < numImages_; i++) {
	images_[i]->setScale(xScale, yScale);
    }
}


/* 
 * rotate the image by the given angle  
 * (actually exchange the x/y axes if angle not 0)
 */
void CompoundImageData::rotate(int angle)
{
    ImageData::rotate(angle);

    for(int i = 0; i < numImages_; i++) {
	images_[i]->rotate(angle);
    }
}


/* 
 * flip the image X axis if b is non-zero 
 */
void CompoundImageData::flipX(int b) {
    ImageData::flipX(b);

    for(int i = 0; i < numImages_; i++) {
	images_[i]->flipX(b);
    }
}

/* 
 * flip the image Y axis if b is non-zero 
 */
void CompoundImageData::flipY(int b) {
    ImageData::flipY(b);

    for(int i = 0; i < numImages_; i++) {
	images_[i]->flipY(b);
    }
}


/*
 * set the cut levels to the given values.
 * If scaled is 1, the low and high values should be already "scaled" with 
 * bzero and bscale.
 */
void CompoundImageData::setCutLevels(double low, double high, int scaled)
{
    ImageData::setCutLevels(low, high, scaled);

    for(int i = 0; i < numImages_; i++) {
	images_[i]->setCutLevels(low, high, scaled);
    }
}


/* Just pass on to the parent class and image extensions */
void CompoundImageData::subsample(int b) {
    ImageData::subsample(b);

    for(int i = 0; i < numImages_; i++) {
	images_[i]->subsample(b);
    }
}


/* Just pass on to the parent class and image extensions */
void CompoundImageData::sampmethod(int b) {
    ImageData::sampmethod(b);

    for(int i = 0; i < numImages_; i++) {
	images_[i]->sampmethod(b);
    }
}


/* Just pass on to the parent class and image extensions */
void CompoundImageData::verbose(int b) {
    ImageData::verbose(b);

    for(int i = 0; i < numImages_; i++) {
	images_[i]->verbose(b);
    }
}


/* Just pass on to the parent class and image extensions */
void CompoundImageData::name(const char* name) {
    ImageData::name(name);

    for(int i = 0; i < numImages_; i++) {
	images_[i]->name(name);
    }
}

/* Just pass on to the parent class and image extensions */
void CompoundImageData::object(const char* object) {
    ImageData::object(object);

    for(int i = 0; i < numImages_; i++) {
	images_[i]->object(object);
    }
}


/*
 * Return non-zero if the rectangle given by (ax0,ay0 ax1,ay1) intersects
 * the rectangle given by (bx0,by0 bx1,by1).
 */
inline int intersects(double ax0, double ay0, double ax1, double ay1,
		      double bx0, double by0, double bx1, double by1) {
    
    return !((ax1 <= bx0) ||
	     (ay1 <= by0) ||
	     (ax0 >= bx1) ||
	     (ay0 >= by1));
}


/*
 * Return non-zero if the point given by (x,y) intersects
 * the rectangle given by (x0,y0 x1,y1).
 */
inline int intersects(double x, double y, double x0, double y0, double x1, double y1) {
    
    return !((x <= x0) ||
	     (y <= y0) ||
	     (x >= x1) ||
	     (y >= y1));
}


/*
 * Update the X image area starting at the given offset and continuing
 * to the end of the raw image or the end of the X image data, which
 * ever comes first.
 *
 * This method is used when the X Image is the same size as the visible
 * window (or image, if smaller) and displays the part of the image at
 * some x,y scroll offset.
 *
 * Redefined from the parent class to always clear teh XImage.  
 */
void CompoundImageData::updateOffset(double x, double y)
{
    if (!xImage_ || width_ <= 0 || height_ <= 0 
	|| (update_pending_ == 0 && x == prevX_ && y == prevY_))
	return;

    if (clear_) {		// temp clear image
	xImage_->clear(color0_);
	clear_ = 0;
	return;
    }

    prevX_ = x;
    prevY_ = y;

    int x0 = int(x), y0 = int(y), x1 = width_-1, y1 = height_-1, dest_x = 0, dest_y = 0;

    // handle case where x0,y0 are negative (i.e.: image starts in the 
    // middle of the window somewhere rather than the window starting at 
    // the middle of the image)
    if (x < 0) {
	dest_x = -x0 + 1;
	x0 = 0;
    }
    if (y < 0) {
	dest_y = -y0 + 1; 
	y0 = 0;
    }
   
    // always clear the XImage, since there may be different dest offsets
    xImage_->clear(color0_);

    // copy raw to X image while doing transformations
    toXImage(x0, y0, x1, y1, dest_x, dest_y);
}


/*
 * copy the raw image to the xImage, doing any transformations as
 * necessary.
 *
 * The arguments x0, y0, x1 and y1 are the bounding box of the region of
 * the raw image that needs to be copied (origin at (0,0)) 
 *
 * dest_x and dest_y give the coordinates in the XImage where copying
 * should start. These are normally either (-x0,-y0) or (0,0).
 *
 * This method adjust the coordinates, if necessary and then calls
 * the virtual methods in derived classes to do the real work.
 *
 * As a side effect, the member variables x0_, y0_, x1_, y1_ are set to
 * the bounding box of the visible area of the image in FITS image
 * coordinates.
 */
void CompoundImageData::toXImage(int x0, int y0, int x1, int y1, int dest_x, int dest_y)
{
    // set and clip the member variables x0_, y0_, x1_, y1_
    setBounds(x0, y0, x1, y1, dest_x, dest_y);

    // Draw each image extension as needed
    for(int i = 0; i < numImages_; i++) {
	// Get the offset of the image extension in the complete image (dx, dy).
	// Note: crpix is by convention negative, y-axis reversed (origin at (1,1) ???),
	// but here we need positive values, with origin at (0,0)
	//int dx = -int(images_[i]->crpix1_) + 1, 
	//    dy = -int(images_[i]->crpix2_) + 1; 
	int w = images_[i]->width_, 
	    h = images_[i]->height_;
	int dx =(int)(-images_[i]->crpix1_ - minX_), 
	    dy =(int)(-images_[i]->crpix2_ - minY_);

	if (! flipY_)
	    dy = height_ - dy - h; // y-axis reversed here
	if (flipX_)
	    dx = width_ - dx - w;

	if (intersects(x0_, y0_, x1_, y1_, dx, dy, dx+w-1, dy+h-1)) {
	    int idest_x = dest_x + max(dx - x0_, 0);
	    int idest_y = dest_y + max(dy - y0_, 0);

	    // translate to the coordinates of the image extension
	    x0 = max(x0_ - dx, 0);
	    y0 = max(y0_ - dy, 0);
	    x1 = w - 1; 
	    y1 = h - 1;

	    if (x1 > x0 && y1 > y0) {
		images_[i]->toXImage(x0, y0, x1, y1, idest_x, idest_y);
	    }
	}
    }

    // x0_, y0_, x1_ and y1_ are the coordinates of the visible part of 
    // the image and are needed later for setting cut levels and calculating 
    // the min/max pixel for the displayed image area. The display routines
    // called above (rawToXImage, grow, shrink) expect the coordinates to have
    // the origin at upper left (0,0) (XImage type coordinates), while the rest
    // of the code deals with FITS image coordinates (origin at lower left 
    // (1, 1) at mag 1).
    flip(x0_, y0_, x1_, y1_);
    
    update_pending_ = 0;
}


/*----------------------------------------------------------------------------
 * The methods below access the actual image data and are normally implemented
 * in ImageTemplates.C. Here we need to define the methods to access the
 * correct image extensions, based on the visible area, or requested area
 * of the image.
 *----------------------------------------------------------------------------*/


/*
 * Scan the image to find the min and max values and set the member
 * variables accordingly.  To save time, only the visible area of the
 * image is examined (x0_, y0_, x1_ and y1_ are set each time the
 * image is updated, however they are initially set to the entire
 * image).  
 * The result is that the member variables minValue_ and maxValue_ 
 * are set.
 */
void CompoundImageData::getMinMax()
{
    int n = 0;
    for(int i = 0; i < numImages_; i++) {
	double x0, y0, x1, y1;
	getBounds(images_[i], x0, y0, x1, y1);
	if (intersects(x0_, y0_, x1_, y1_, x0, y0, x1, y1)) {
	    images_[i]->getMinMax();
	    if (n++ == 0) {
		minValue_ = images_[i]->minValue_;
		maxValue_ = images_[i]->maxValue_;
	    }
	    else {
		minValue_ = min(images_[i]->minValue_, minValue_);
		maxValue_ = max(images_[i]->maxValue_, maxValue_);
	    }
	}
    }
}


/*
 * print the coordinates and raw data value at the given x,y image
 * coords to the buffer
 *
 * A "-" is printed if the x,y coords are out of range.
 * "blank" is printed if the pixel is blank.
 */
char* CompoundImageData::getValue(char* buf, double x, double y)
{
    for(int i = 0; i < numImages_; i++) {
	// get bounding box of this image extension
	double x0, y0, x1, y1;
	getBounds(images_[i], x0, y0, x1, y1);
	if (intersects(x, y, x0, y0, x1, y1)) {
	    return images_[i]->getValue(buf, x-x0, y-y0);
	}
    }
    sprintf(buf, "%.1f %.1f -", x, y);
    return buf;
}



/*
 * return the value at the x,y image coords as a double.
 *
 * The input x,y is assumed to be in image coords.
 * If the coords are out of range, 0.0 is returned.
 */
double CompoundImageData::getValue(double x, double y) 
{
    for(int i = 0; i < numImages_; i++) {
	// get bounding box of this image extension
	double x0, y0, x1, y1;
	getBounds(images_[i], x0, y0, x1, y1);
	if (intersects(x, y, x0, y0, x1, y1)) {
	    return images_[i]->getValue(x-x0, y-y0);
	}
    }
    return 0.0;
}


/*
 * print the X, Y coordinates, raw data value and World Coordinates
 * at the given x,y image coords to the given buffers. 
 *
 * rx and ry are the image coordinates to use to access the pixel value. This might
 * be different than x,y, since x,y are the logical image coordinates, assuming the
 * image starts at 1,1, which might not actually be the case.
 *
 * A blank value is printed if the rx,ry coords are out of range.
 *
 * note: x, y and rx,ry are expected in image coords, while in the result,
 * xStr and yStr are in "chip" coords, since they should  be displayed.
 */
void CompoundImageData::getValues(double x, double y, double rx, double ry, 
				  char* xStr, char* yStr, char* valueStr,
				  char* raStr, char* decStr, char* equinoxStr)
{
    *valueStr = '\0';
    *xStr = '\0';
    *yStr = '\0';
    *raStr = '\0';
    *decStr = '\0';
    *equinoxStr = '\0';
    
    for(int i = 0; i < numImages_; i++) {
	// get bounding box of this image extension
	double x0, y0, x1, y1;
	getBounds(images_[i], x0, y0, x1, y1);
	if (intersects(rx, ry, x0, y0, x1, y1)) {
	    images_[i]->getValues(x-x0, y-y0, rx-x0, ry-y0, 
				  xStr, yStr, valueStr,
				  raStr, decStr, equinoxStr);
	    // display global chip coords for x and y
	    double cx = x, cy = y;
	    imageToChipCoords(cx, cy);
	    sprintf(xStr, "%.1f", cx);
	    sprintf(yStr, "%.1f", cy);
	    return;
	}
    }
}


/*
 * Fill the given array with the pixel values surrounding the given point.
 * nrows and ncols give the dimensions of the array. Any values that are outside
 * of the image or are blank (BLANK keyword) are set to -HUGE_VAL. 
 *
 * Note: it is assumed that nrows and ncols are odd numbers and that the array
 * is one row and column larger (nrows+1 x ncols+1), so that it can hold the 
 * X and Y index headings.
 *
 * rx and ry are the image coordinates to use to access the pixel value. This might
 * be different than x,y, since x,y are the logical image coordinates, assuming the
 * image starts at 1,1, which might not actually be the case.
 *
 * note: x, y and rx,ry are expected in image coords, however the coordinates in the
 * result are "chip" coords, since they should  be displayed.
 */
void CompoundImageData::getValues(double x, double y, double rx, double ry, double* ar, 
				  int nrows, int ncols, int flag)
{
    // clear out array first
    int nn = (nrows+1)*(ncols+1);
    int i;
    for(i = 0; i < nn; i++)
	ar[i] = -HUGE_VAL;

    for(i = 0; i < numImages_; i++) {
	// get bounding box of this image extension
	double x0, y0, x1, y1;
	getBounds(images_[i], x0, y0, x1, y1);
	double m = ncols/2, n = nrows/2;
	if (intersects(rx-m, ry-n, rx+m, ry+n, x0, y0, x1, y1)) {
	    images_[i]->getValues(x-x0, y-y0, rx-x0, ry-y0, ar, nrows, ncols, 1);
	}
    }
}


/*
 * Fill the given array with the pixel values (converted to floats as needed)
 * at the given x,y image pos, width and height.
 *
 * The array should be large enough for at least (w x h) floats.
 *
 * Any values that are outside of the image are set to blank or 0, if there
 * is no blank pixel value defined (If "flag" is non-zero, values outside 
 * the image are not changed). 
 *
 * Note: x and y are expected in image coordinates
 */
void CompoundImageData::getValues(double x, double y, int w, int h, float* ar, int flag)
{
    // clear out array first
    int nn = w*h;
    int i;
    for(i = 0; i < nn; i++)
	ar[i] = 0;

    for(i = 0; i < numImages_; i++) {
	// get bounding box of this image extension
	double x0, y0, x1, y1;
	getBounds(images_[i], x0, y0, x1, y1);
	double m = w/2, n = h/2;
	if (intersects(x-m, y-n, x+m, y+n, x0, y0, x1, y1)) {
	    images_[i]->getValues(x-x0, y-y0, w, h, ar, 1);
	}
    }
}


/*
 * Copy raw image data from this image to the given image data area,
 * starting at the image coordinates (x, y) and with the dimentions (w,h) 
 * in pixels.  Since this is a copy from one raw image to another, no
 * data conversion is done.
 */
void CompoundImageData::copyImageArea(void* data, double x, double y, int w, int h)
{
    // XXX to be implemented
    error("Sorry: copying a region is not supported for compound images yet");
}


/*
 * copy the raw image to the xImage
 *
 * The arguments x0, y0, x1 and y1 are the bounding box of the region of
 * the raw image that needs to be copied.
 *
 * dest_x and dest_y give the coordinates in the XImage where copying
 * should start. These are normally either (x0,y0) or (0,0).
 */
void CompoundImageData::rawToXImage(int x0, int y0, int x1, int y1, 
				    int dest_x, int dest_y)
{
    return;  // see toXImage() above
}


/*
 * This method is called to magnify the image by factors >= 2
 * The arguments x0, y0, x1 and y1 are the bounding box of the region
 * that needs to be copied.
 *
 * dest_x and dest_y give the coordinates in the XImage where copying
 * should start. These are normally either (x0,y0) or (0,0).
 */
void CompoundImageData::grow(int x0, int y0, int x1, int y1, 
			     int dest_x, int dest_y)
{
    return;  // see toXImage() above
}



/*
 * This method is called to shrink the image. If subsample_ is 1, just
 * take every nth pixel as a sample, otherwise take the max of the
 * surrounding pixels
 *
 * dest_x and dest_y give the coordinates in the XImage where copying
 * should start. These are normally either (x0,y0) or (0,0).
 *
 * Note: there is a (non-fatal) bug here that shows up when dest_x,dest_y are non-zero,
 *       (the image display gets split or mixed up somehow...) - needs some
 *       testing to see what the cause is.
 */
void CompoundImageData::shrink(int x0, int y0, int x1, int y1, int dest_x, int dest_y)
{
    return;  // see toXImage() above
}


/*
 * Set the cut levels using a median filtering algorithm.
 * To save time, only the visible area of the image is examined
 * (x0_, y0_, x1_ and y1_ are set each time the image is updated).
 */
void CompoundImageData::medianFilter()
{
    int n = 0;
    for(int i = 0; i < numImages_; i++) {
	double x0, y0, x1, y1;
	getBounds(images_[i], x0, y0, x1, y1);
	if (intersects(x0_, y0_, x1_, y1_, x0, y0, x1, y1)) {
	    images_[i]->medianFilter();
	    if (n++ == 0) {
		lowCut_ = images_[i]->lowCut_;
		highCut_ = images_[i]->highCut_;
	    }
	    else {
		lowCut_ = min(images_[i]->lowCut_, lowCut_);
		highCut_ = max(images_[i]->highCut_, highCut_);
	    }
	}
    }
    setCutLevels(lowCut_, highCut_, 0);
}


/*
 * scan the image to find the distribution of pixel values
 * and set the cut levels so that the given percent of pixels
 * are within the low and high cut values.
 */
void CompoundImageData::autoSetCutLevels(double percent)
{
    int n = 0;
    for(int i = 0; i < numImages_; i++) {
	double x0, y0, x1, y1;
	getBounds(images_[i], x0, y0, x1, y1);
	if (intersects(x0_, y0_, x1_, y1_, x0, y0, x1, y1)) {
	    images_[i]->autoSetCutLevels(percent);
	    if (n++ == 0) {
		lowCut_ = images_[i]->lowCut_;
		highCut_ = images_[i]->highCut_;
	    }
	    else {
		lowCut_ = min(images_[i]->lowCut_, lowCut_);
		highCut_ = max(images_[i]->highCut_, highCut_);
	    }
	}
    }
    
    setCutLevels(lowCut_, highCut_, 0);
}


/*
 * Fill the given array (xyvalues) with statistics about the distribution
 * of pixels in the visible image area (given by x0_, y0_, x1_, y1_).
 * xyvalues[n*2+1] is set to the number of pixels with value at or near n.
 * The factor is used to fit the information in the given size aray.
 */	
void CompoundImageData::getPixDist(int numValues, double* xyvalues, double factor)
{
    for(int i = 0; i < numImages_; i++) {
	double x0, y0, x1, y1;
	getBounds(images_[i], x0, y0, x1, y1);
	if (intersects(x0_, y0_, x1_, y1_, x0, y0, x1, y1)) {
	    images_[i]->getPixDist(numValues, xyvalues, factor);
	}
    }
}



/*
 * Fill the given histogram with the distribution of pixels in the
 * visible image area (given by x0_, y0_, x1_, y1_).  h.histogram[n] is
 * set to the number of pixels with a value of n (after conversion to
 * short if needed).
 */	
void CompoundImageData::getHistogram(ImageDataHistogram& h)
{
    for(int i = 0; i < numImages_; i++) {
	double x0, y0, x1, y1;
	getBounds(images_[i], x0, y0, x1, y1);
	if (intersects(x0_, y0_, x1_, y1_, x0, y0, x1, y1)) {
	    images_[i]->getHistogram(h);
	}
    }
}


/*
 * If there is a special value for blank pixels, get it.
 *
 * Note that the blank pixel value is not scaled by bscale, since we
 * compare pixels values with this value before scaling.
 */
void CompoundImageData::initBlankPixel()
{
    for(int i = 0; i < numImages_; i++) {
	images_[i]->initBlankPixel();
    }
}

/*
 * initialize conversion from base type to short (trivial in this case)
 * and scale the low and high cut levels to short range
 */
void CompoundImageData::initShortConversion() 
{
    for(int i = 0; i < numImages_; i++) {
	images_[i]->initShortConversion();
	scaledLowCut_ = images_[i]->scaledLowCut_;
	scaledHighCut_ = images_[i]->scaledHighCut_;
    }
}


/*
 * return the data type of the raw data
 */
int CompoundImageData::dataType() 
{
    return images_[0]->dataType();
}


/*
 * return true if the data type is signed
 */
int CompoundImageData::isSigned() 
{
    return images_[0]->isSigned();
}


/*
 * return a copy of this object
 */
ImageData* CompoundImageData::copy() 
{
    return new CompoundImageData(*this);
}
