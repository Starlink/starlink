/*
 * E.S.O. - VLT project 
 * "@(#) $Id: ImageData.C,v 1.36 1999/03/22 21:42:02 abrighto Exp $" 
 *
 * ImageData.C - member functions for class ImageData
 *
 * See the man page RTI(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 *
 * Peter W. Draper 02/07/96  Corrected autoSetCutLevels to set cutoff
 *                           according to number of pixels actually counted
 * Peter W. Draper 17/12/97  Modified setScale so that dispWidth and
 *                           dispHeight cannot be zero when using very
 *                           large negative scales (this happens for
 *                           extremely thin images, i.e. 5000x37).
 *
 * Peter W. Draper 03/02/97  Modified flip(int,int,int,int) to remove
 *                           -1 introduced by array coordinates.
 *                           Modified autoSetCutLevels to take account
 *                           of blank pixels.
 *
 * Allan Brighton 12/03/98   Removed ImageData::read, use makeImage instead,
 *                           since it allows different image types through
 *                           subclassing.
 *                           Moved WSC object (wcs_) to class ImageIO (does not
 *                           change the public interface). This makes it easier 
 *                           to derive new image types or replace the WCS
 *                           implementation in a derived class of ImageIORep.
 *
 * Peter W. Draper 23/02/98  Added code to interpolate between the
 *                           autoSetCutLevels bins, increased the
 *                           number of bins to 2048 (from 256).
 *
 * Peter W. Draper 04/03/98  Added changes to support multiple display
 *                           visuals. Renamed setXImageData to setXImage.
 *
 * Peter W. Draper 13/01/99  Merged my changes into SkyCat 2.2.
 *
 * Peter W. Draper 19/03/01  Stopped use of DATAMIN and DATAMAX
 *                           keywords, these are often incorrect, a
 *                           fact which cannot be worked around. 
 */
static const char* const rcsId="@(#) $Id: ImageData.C,v 1.36 1999/03/22 21:42:02 abrighto Exp $";


#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <iostream.h>
#include "error.h"
#include "define.h"
#include "FitsIO.h"
#include "fitshead.h"
#include "NativeImageData.h"
#include "ByteImageData.h"
#include "XImageData.h"
#include "ShortImageData.h"
#include "UShortImageData.h"
#include "LongImageData.h"
#include "FloatImageData.h"
#include "ImageDisplay.h"

// initialize static member variables
// Note: these are static since there is only one static color map for all images
// of this class
int ImageData::ncolors_ = 0;	         // number of available colors
unsigned long* ImageData::colors_ = NULL; // array of color values
unsigned long ImageData::color0_ = 0;     // reserved color for black pixels
unsigned long ImageData::colorn_ = 0;     // reserved color for saturated pixels

// C routine used to get image statistics
extern "C" {
    iqe(float *pfm, float *pwm, int mx, int my, float *parm, float *sdev);
}


/*
 * constructor: initialize the image with the given name from the ImageIO
 * object holding the image data and description.
 *
 * If verbose is true, print debugging messages.
 *
 * lookup_size is optional and defaults to LOOKUP_SIZE (short range). It
 * can be specified as 256 for Byte images, for example.
 */
ImageData::ImageData(const char* imageName, const ImageIO& image, 
		     int verbose, int lookup_size)
: image_(image),
  width_(image.width()),
  height_(image.height()),
  prevX_(0.0),
  prevY_(0.0),
  x0_(0), y0_(0), x1_(width_-1), y1_(height_-1),
  dispWidth_(width_),
  dispHeight_(height_),
  area_(width_*height_),
  xImage_(NULL),
  xImageData_(NULL),
  xImageBytesPerLine_(0),
  xImageSize_(0),
  xImageBytesPerPixel_(1),
  xImageMaxX_(width_-1),
  xImageMaxY_(height_-1),
  colorScaleType_(LINEAR_SCALE),
  haveBlank_(0),
  lowCut_(0.0),
  highCut_(0.0),
  scaledLowCut_(0),
  scaledHighCut_(0),
  scaledBlankPixelValue_(0),
  minValue_(0.0),
  maxValue_(0.0),
  expo_(10.0),
  xScale_(1),
  yScale_(1),
  rotate_(0),
  flipX_(0),
  flipY_(0),
  startX_(0),
  startY_(0),
  binX_(1),
  binY_(1),
  subsample_(1),
  update_pending_(1),
  verbose_(verbose),
  lookup_(lookup_size),
  clear_(0)
{
    name(imageName);
    object("");
}


/*
 * copy constructor: copy the image
 *
 * Note that the classes managing the image data (ImageIO, Mem, ...) use
 * reference counting, so that the data is not actually copied normally,
 * but shared as long as possible.
 */
ImageData::ImageData(const ImageData& im)
: image_(im.image_),
  width_(im.width_),
  height_(im.height_),
  prevX_(0.0),
  prevY_(0.0),
  x0_(0), y0_(0), x1_(width_-1), y1_(height_-1),
  dispWidth_(im.dispWidth_),
  dispHeight_(im.dispHeight_),
  area_(im.area_),
  xImage_(NULL),		// will be set later
  xImageData_(NULL),
  xImageBytesPerLine_(0),
  xImageSize_(0),
  xImageMaxX_(0),
  xImageMaxY_(0),
  xImageBytesPerPixel_(1),
  colorScaleType_(im.colorScaleType_),
  haveBlank_(im.haveBlank_),
  lowCut_(im.lowCut_),
  highCut_(im.highCut_),
  scaledLowCut_(im.scaledLowCut_),
  scaledHighCut_(im.scaledHighCut_),
  scaledBlankPixelValue_(im.scaledBlankPixelValue_),
  minValue_(im.minValue_),
  maxValue_(im.maxValue_),
  expo_(im.expo_),
  xScale_(im.xScale_),
  yScale_(im.yScale_),
  rotate_(im.rotate_),
  flipX_(im.flipX_),
  flipY_(im.flipY_),
  startX_(im.startX_),
  startY_(im.startY_),
  binX_(im.binX_),
  binY_(im.binY_),
  subsample_(im.subsample_),
  update_pending_(1),
  verbose_(im.verbose_),
  lookup_(im.lookup_),
  clear_(0)
{
    name("");  // new name should be set from outside
    object(im.object_);
}


/*
 * Specify a new lookup table to use for this image and its size.
 * Note: only allowed if this is a copy of another image.
 */
int ImageData::lookup(LookupTable lookup)
{
    if (lookup.size() != lookup_.size())
	return error("warning: tried to copy lookup table with wrong size");

    lookup_ = lookup; 
    update_pending_++;
    return 0;
}


/*
 * This method is used to save color information for pos. later use
 *
 * Note that the first and last colors are reserved for special use:
 * The first color should be black and the last color can be set to
 * some saturation color.
 */
void ImageData::setColors(int ncolors, unsigned long* colors) 
{
    ncolors_ = ncolors - 2;
    colors_ = colors + 1;
    color0_ = colors[0];
    colorn_ = colors[ncolors-1];
}


/*
 * Set the destination XImage buffer and the dimensions of the XImage in
 * pixels. (This class copies the rawimage to xImage, doing any necessary
 * transformations along the way.)
 */
void ImageData::setXImage(ImageDisplay* xImage)
{
    // save XImage info
    xImage_ = xImage;
    if ( xImage == NULL ) return;
    xImageData_ = xImage_->data();
    xImageBytesPerPixel_ = xImage_->depth()/8;
    xImageBytesPerLine_ = xImage_->bytesPerLine();

    // get XImage size in bytes
    xImageSize_ = xImageBytesPerLine_ * xImage_->height() * xImageBytesPerPixel_;

    // get the highest x,y indexes in the XImage
    double x = xImage_->width();
    double y = xImage_->height();
    undoTrans(x, y, 1);
    xImageMaxX_ = int(x)-1;
    xImageMaxY_ = int(y)-1;

    update_pending_++;
}


/* 
 * Replace the current image data with new data of the same size and type
 */
void ImageData::data(const Mem& data) 
{
    image_.data(data);
   
    // make sure image is regenerated
    update_pending_++;
}


/* 
 * Replace the current header with a new header
 */
void ImageData::header(const Mem& header) 
{
    // XXX reinitialize wcs() here ?
    image_.header(header);
}


/*
 * save the contents of this image to the given file
 */
int ImageData::write(const char* filename)
{
    return image_.write(filename);
}


/*
 * Save the contents of the given region of this image to the given file.
 * The coordinates are expected in image pixel units.
 */
int ImageData::write(const char* filename, double rx0, double ry0, double rx1, double ry1)
{
    double x0 = min(rx0,rx1), y0 = min(ry0,ry1);
    double x1 = max(rx0,rx1), y1 = max(ry0,ry1);
    int ix0, iy0, ix1, iy1;
    getIndex(x0, y0, ix0, iy0);
    getIndex(x1, y1, ix1, iy1);
    int w = ix1-ix0, h = iy1-iy0;

    // copy the original header and reset the relevant keywords
    const Mem& origHeader = image_.header();
    int origHeaderSize = origHeader.length();
    char* origHeaderPtr = (char*)origHeader.ptr();
    Mem header(origHeaderSize, 0);
    if (header.status() != 0)
	return 1;		// error
    char* head = (char*)header.ptr();
    memcpy(head, origHeaderPtr, origHeaderSize);

    // Update the FITS keywords for the new image size.
    // Note: we can't use FitsIO for this, since we already have the header,
    // but the NAXIS keyword are wrong still, so fall back on wcssubs hget()
    hlength(head, origHeaderSize);
    hputi4(head, "NAXIS1", w);
    hputcom(head, "NAXIS1", "Length X axis");
    hputi4(head, "NAXIS2", h);
    hputcom(head, "NAXIS2", "Length Y axis");

    // update WCS keywords if needed
    if (wcs().isWcs()) {
	double cx = w/2.0, cy = h/2.0;
	hputr8(head, "CRPIX1", cx);
	hputcom(head, "CRPIX1", "Refpix of first axis");
	hputr8(head, "CRPIX2", cy);
	hputcom(head, "CRPIX2", "Refpix of second axis");

	double ra, dec;
	if (wcs().pix2wcs(ix0+cx, iy0+cy, ra, dec) != 0)
	    return 1;

	hputr8(head, "CRVAL1", ra);
	hputcom(head, "CRVAL1", "RA at Ref pix in decimal degrees");
	hputr8(head, "CRVAL2", dec);
	hputcom(head, "CRVAL2", "DEC at Ref pix in decimal degrees");
    }
    
    // get and copy data for the specified image area
    int newDataSize = w*h*image_.pixelSize();
    Mem data(newDataSize, 0);
    if (data.status() != 0)
	return 1;		// error

    // copy the selected area
    copyImageArea(data.ptr(), x0, y0, w, h);

    // write the file so that FitsIO can edit it (keywords are incorrect still)
    // XXX Note: this is a bit tricky since class FitsIO is using the cfitsio
    // library and will complain if not editing a FITS file.
    FitsIO fits(w, h, image_.bitpix(), image_.bzero(), image_.bscale(), 
		header, data);
    if (fits.status() != 0 || fits.write(filename) != 0)
	return 1;		// error

    return 0;
}


/*
 * Make a new image from the given ImageIO object and return a pointer to
 * a derived class of this class specialized in that type of file/image.
 *
 * Note that pointers to classes such as FitsIO are automatically converted 
 * to an ImageIO object through a special constructor. In this way, you can
 * add new image types by deriving a new classes in the same way as the
 * FitsIO class (from ImageIORep).
 *
 * name - is an arbitrary name for the image.
 * imio - is a reference to an ImageIO object for the image (or ptr, see above).
 * verbose - is a flag, if true, print out diagnostic messages.
 */
ImageData* ImageData::makeImage(const char* name, const ImageIO& imio, int verbose)
{
    if (imio.status() != 0)
	return NULL;

    ImageData* image = NULL;

    // if this flag is true, use native byte order. Note that this has
    // no effect on machines that use network byte order already (sun, hp,...),
    // see NativeImageData.[Ch].
    int native = imio.nativeByteOrder();

    switch (imio.bitpix()) {
    case BYTE_IMAGE:
	image = new ByteImageData(name, imio, verbose);
	break;
    case X_IMAGE:
	image = new XImageData(name, imio, verbose);
	break;
    case USHORT_IMAGE:
	if (native)
	    image = new NativeUShortImageData(name, imio, verbose);
	else
	    image = new UShortImageData(name, imio, verbose);
	break;
    case SHORT_IMAGE:
	if (native)
	    image = new NativeShortImageData(name, imio, verbose);
	else
	    image = new ShortImageData(name, imio, verbose);
	break;
    case LONG_IMAGE:
	if (native)
	    image = new NativeLongImageData(name, imio, verbose);
	else
	    image = new LongImageData(name, imio, verbose);
	break;
    case FLOAT_IMAGE:
	if (native)
	    image = new NativeFloatImageData(name, imio, verbose);
	else
	    image = new FloatImageData(name, imio, verbose);
	break;
    default:
	char buf[32];
	sprintf(buf, "%d", imio.bitpix());
	error("unsupported image BITPIX value: ", buf);
    }

    if (image) 
	return image->initImage();
    
    return image;
}


/* 
 * Initialize a new image. Determine the min and max pixel values,
 * set the default cut levels and get some keyword values that we
 * will need later. Retruns a pointer to this image.
 */
ImageData* ImageData::initImage()
{
    // See if there is a special value for blank pixels.
    // Note that the blank pixel value is not scaled by bscale, since
    // we compare pixels values with this value before scaling.
    initBlankPixel();

    // get the values of the image keywords we need here
    char* s = image_.get("OBJECT");
    if (s) {
	char* p = strchr(s, '\'');
	if (p) *p = '\0'; // make up for minor bug in (XXX old) wcslib - remove quotes
	object(s); // save object name
    }
    
    // save the values of DET.WIN.STRX and DET.WIN.STRY, needed to calculate
    // the chip coords
    if (image_.get("HIERARCH ESO DET WIN1 STRX", startX_) != 0 
	|| image_.get("HIERARCH ESO DET WIN1 STRY", startY_) != 0) {
	startX_ = startY_ = 0;
    } 
    else {
	// assume STRX and STRY start at (1,1), we want just the offset
	startX_--;
	startY_--;
	if (startX_ < 0 || startY_ < 0)
	    startX_ = startY_ = 0;
    }

    // save the values of DET.WIN.BINX and DET.WIN.BINY for detector binning
    // settings for calculating the chip coordinates
    if (image_.get("HIERARCH ESO DET WIN1 BINX", binX_) != 0 
	|| image_.get("HIERARCH ESO DET WIN1 BINY", binY_) != 0) {
	binX_ = binY_ = 1;
    }
    else {
	// assume BINX=1 and BINY=1 means no binning 
	if (binX_ < 1 || binY_ < 1)
	    binX_ = binY_ = 1;
    }
    
    // get min/max pixel and use to set default cut levels
    setDefaultCutLevels();

    // initialize world coordinates, if the caller did not already
    if (! wcs().initialized()) 
	image_.wcsinit();

    return this;
}


/*
 * reinitialize the image after a change (such as moving to a new HDU)
 */
int ImageData::reinit()
{
    initImage();
    return 0;
}


/*
 * Copy the cutlevels, scale, rotate and flip parameters from the
 * this image to the given struct.
 */
void ImageData::saveParams(ImageDataParams& p)
{
    p.status = 0;
    p.flipX = flipX_;
    p.flipY = flipY_;
    p.rotate = rotate_;
    p.xScale = xScale_;
    p.yScale = yScale_;
    
    p.dataType = dataType();
    p.lowCut = lowCut_;
    p.highCut = highCut_;
    p.colorScale = colorScaleType_;
    
}


/*
 * Copy the cutlevels, scale, rotate and flip parameters from the
 * given struct to this image (see above).
 * If restoreCutLevels is non-zero (default), the saved cut-levels are restored
 * otherwise they are not and the cutlevels are left as they were 
 * initialized (to the approx. min/max pixel value).
 */
void ImageData::restoreParams(ImageDataParams& p, int restoreCutLevels)
{
    if (p.status != 0) 
	return;			// don't use if status != 0

    flipX(p.flipX);
    flipY(p.flipY);
    rotate(p.rotate);
    setScale(p.xScale, p.yScale);
    if (restoreCutLevels && p.lowCut != p.highCut)
	setCutLevels(p.lowCut, p.highCut, 0);
    colorScaleType_ = p.colorScale;
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
void ImageData::colorScale(int ncolors, unsigned long* colors)
{
    // save the color info for pos. later use
    setColors(ncolors, colors);
    
    // reset the lookup table
    lookup_.reset(colors_[0]);

    // call a method to create a lookup table for speedy access
    switch(colorScaleType_) {
    case LINEAR_SCALE:
	lookup_.linearScale(scaledLowCut_, scaledHighCut_, isSigned(), ncolors_, colors_);
	break;
    case LOG_SCALE: 
	lookup_.logScale(scaledLowCut_, scaledHighCut_, isSigned(), ncolors_, colors_, expo_);
	break;
    case SQRT_SCALE:
	lookup_.sqrtScale(scaledLowCut_, scaledHighCut_, isSigned(), ncolors_, colors_, expo_);
	break;
    case HISTEQ_SCALE:
	ImageDataHistogram h;
	getHistogram(h);
	lookup_.histeqScale(scaledLowCut_, scaledHighCut_, isSigned(), 
			    ncolors_, colors_, h.histogram, h.area);
	break;
    }
    
    // set value for blank pixel
    if (haveBlank_)
	lookup_.setPixelColor(scaledBlankPixelValue_, color0_);
    
    // make sure image is regenerated
    update_pending_++;
}


/*
 * set the scaling (zoom) factor
 */
void ImageData::setScale(int xScale, int yScale)
{
    if (xScale == xScale_ && yScale == yScale_)
	return;
    xScale_ = xScale;
    yScale_ = yScale;
    
    if (xScale > 0) {  // note: both x and y scale are pos. or neg.
	dispWidth_ = width_ * xScale_;
	dispHeight_ = height_ * yScale_;
    } 
    else if (xScale < 0) {
	dispWidth_ = width_/-xScale_;
        if (dispWidth_ == 0) 
	    dispWidth_ = 1;
	dispHeight_ = height_/-yScale_;
        if (dispHeight_ == 0) 
	    dispHeight_ = 1;
    }
    area_ = width_*height_;
    if (rotate_) 
	swap(dispWidth_, dispHeight_);
    update_pending_++;
    
}


/* 
 * rotate the image by the given angle  
 * (only 0 and 90 degrees supported now)
 */
void ImageData::rotate(int angle)
{
    angle = (angle != 0); // make boolean, since there is only one angle...
    if (rotate_ != angle) {
	rotate_ = angle;
	swap(dispWidth_, dispHeight_);
	swap(xImageMaxX_, xImageMaxY_);
	update_pending_++;
    }
}


/* 
 * Flip the x,y coordinates according to the current transformations.
 * If width and height are given, they are used for flipping as needed.
 */
void ImageData::flip(double& x, double& y, int width, int height)
{
    int c = (xScale_ > 1) ? 0 : 1; 
    
    if (!flipY_) 		// raw image has y axis reversed
	y = ((height ? height : height_) - c) - y;

    if (flipX_) 
	x = ((width ? width : width_) - c) - x;
}


/* 
 * Flip both pairs of x,y coordinates according to the current transformations
 * and swap x0,x1 and y0,y1 resp. if flipped.
 */
void ImageData::flip(int& x0, int& y0, int& x1, int& y1)
{
    int c = (xScale_ > 1) ? 0 : 1; 

    if (!flipY_) {		// raw image has y axis reversed
	int y = y0, h = height_ - c;
	y0 = h - y1;
	y1 = h - y;
    }
    if (flipX_) {
	int x = x0, w = width_ - c;
	x0 = w - x1;
	x1 = w - x;
    }
}


/*
 * apply the current transformations to the given coordinates
 * If distFlag is 1, x and y are treated as a distance, otherwise
 * they are treated as a point and flipped as needed.
 * If x and y offsets are specified, they are subtracted from x and y.
 * If width and height are given, they are used for flipping as needed.
 */
void ImageData::doTrans(double& x, double& y, int distFlag, 
			double xOffset, double yOffset, 
			int width, int height)
{
    if (! distFlag) {
	// fits starts at 1,1 (0.5 is left side of pixel when zoomed)
	double f = (xScale_ > 1) ? 0.5 : 1.0;
	x -= f;	
	y -= f;

	flip(x, y, width, height);

	x -= xOffset;
	y -= yOffset;
    }

    if (rotate_) 
	swap(x, y);
    
    if (xScale_ > 1) {		// then yScale must also be >= 1
	x *= xScale_; 
	y *= yScale_; 
    } 
    else if (xScale_ < 0) {
	x /= -xScale_; 
	y /= -yScale_; 
    }
}


/* 
 * undo the current transformations on the given coordinates.
 * If distFlag is 1, x and y are treated as a distance, otherwise
 * they are treated as a point and flipped as needed.
 * If x and y offsets are specified, they are add to x and y.
 * If width and height are given, they are used for flipping as needed.
 */
void ImageData::undoTrans(double& x, double& y, int distFlag, 
			  double xOffset, double yOffset, 
			  int width, int height)
{
    if (xScale_ > 1) { // then yScale must also be > 1
	x /= xScale_; 
	y /= yScale_; 
    } 
    else if (xScale_ < 0) {
	x *= -xScale_; 
	y *= -yScale_; 
    }
    if (rotate_) 
	swap(x, y);

    if (! distFlag) {
	x += xOffset;
	y += yOffset;
	
	flip(x, y, width, height);

	// fits starts at 1,1 (0.5 is left side of pixel when zoomed)
	double f = (xScale_ > 1) ? 0.5 : 1.0;
	x += f;	
	y += f;
    }
}


/*
 * convert x,y image coords to a distance (from the origin) by flipping
 * where needed If width and height are given, they are used for flipping
 * as needed.
 */
void ImageData::coordsToDist(double& x, double& y, int width, int height)
{
    // fits starts at 1,1 (0.5 is left side of pixel when zoomed)
    double f = (xScale_ > 1) ? 0.5 : 1.0;
    x -= f;	
    y -= f;

    flip(x, y, width, height);
}


/*
 * convert an x,y distance (from the origin) to x,y image coords by
 * flipping where needed If width and height are given, they are used for
 * flipping as needed.
 */
void ImageData::distToCoords(double& x, double& y, int width, int height)
{
    flip(x, y, width, height);

    // fits starts at 1,1 (0.5 is left side of pixel when zoomed)
    double f = (xScale_ > 1) ? 0.5 : 1.0;
    x += f;	
    y += f;
}


/*
 * Convert the given image coordinates to detector chip/CCD coordinates.
 * While image coordinates start at 1,1 (.5,.5), chip coordinates might
 * have a different origin and/or binning. The the member variables
 * startX_ and startY_ give the offsets of the image origin. These are
 * set from the FITS keywords HIERARCH ESO DET WIN STRX and STRY, if
 * found, but may also be set via member methods.
 */
void ImageData::imageToChipCoords(double& x, double& y)
{
    x = x * binX_ + startX_;
    y = y * binY_ + startY_;
}


/*
 * Convert the given detector chip/CCD coordinates to image coordinates.
 * (see comments above for imageToChipCoords).
 */
void ImageData::chipToImageCoords(double& x, double& y)
{
    x = (x - startX_) / binX_;
    y = (y - startY_) / binY_;
}


/*
 * single argument versions of the above methods
 */
void ImageData::imageToChipCoords(double& x)
{
    x += startX_;
}
void ImageData::chipToImageCoords(double& x)
{
    x -= startX_;
}


/*
 * Convert floating point image coords to integer image array index.
 * (Image coords start at 1,1 at mag 1, array index is 0,0...)
 *
 * Note: added "switch" hack to fix a probable "off-by-one" bug somewhere
 * in the coordinate handling - not sure where. It adjusts the x,y index
 * into the raw image by 1 where needed.
 *
 * It is interesting to note that this is only needed when the image
 * scale is 1.  This might have something to do with the fact that we
 * always add or subtract 1 to images coords to make FITS coordinates
 * start at 1 at mag level 1, while at mag 2 they start at 0.5,0.5...
 *
 * Return 0 if the index is in range, 1 otherwise.
 */
int ImageData::getIndex(double x, double y, int& ix, int& iy)
{
    // get integer index in raw image for pixel value
    if (xScale_ > 1) {
	ix = int(x-0.5);
	iy = int(y-0.5);
    } 
    else {
	ix = int(x-1.0);
	iy = int(y-1.0);
    }

    // return 0 if in range, otherwise 1
    return (ix < 0 || iy < 0 || ix >= width_ || iy >= height_);
}


/*
 * set the scaling factor so that the image will fit in the given box
 */
void ImageData::shrinkToFit(int width, int height)
{
    int factor = -max((width_-1)/width+1, (height_-1)/height+1);
    if (factor >= -1)
	factor = 1;
    setScale(factor, factor);
}


/*
 * set the default cut levels for the image to the min/max pixel
 * value. If DATAMIN and DATAMAX are defined in the FITS header,
 * use them, otherwise scan the image for the approx. min/max
 * values. PWD: remove DATAMIN and DATAMAX dependence. When these are
 * incorrect there's no way around it.
 */
void ImageData::setDefaultCutLevels()
{
//    double d1, d2;
//    if (image_.get("DATAMIN", d1) == 0 && image_.get("DATAMAX", d2) == 0 && d1 < d2) {
//	// note that DATAMIN and MAX are AFTER adding bzero and multiplying 
//	// by bscale. We only use these values to display to the user, but
//	// not internally, since we are going to scale everything to bytes
//	// in the end anyway.
//	minValue_ = unScaleValue(d1);
//	maxValue_ = unScaleValue(d2);
//    }
//    else {

    // scan image for min/max pixel value. Note that at this point,
    // we don't know how much of the image we will actually display,
    // so if the image is really huge, just take a 1k x 1k area in
    // the center.
    int xc = width_/2, yc = height_/2;
    if (xc > 512) {
        x0_ = xc-512;
        x1_ = xc+512;
    }
    if (yc > 512) {
        y0_ = yc-512;
        y1_ = yc+512;
    }
    getMinMax();
//    }
    // set default cut levels 
    setCutLevels(minValue_, maxValue_, 0);
}


/*
 * set the cut levels to the given values.
 * If scaled is 1, the low and high values should be already "scaled" with 
 * bzero and bscale.
 */
void ImageData::setCutLevels(double low, double high, int scaled)
{
    if (scaled) {
	highCut_ = unScaleValue(high);
	lowCut_ = unScaleValue(low);
    }
    else {
	highCut_ = high;
	lowCut_ = low;
    }
    
    // initialize conversion from base type to short,
    // used by color scaling algorithms as index in lookup table 
    initShortConversion();

    // make sure image is re-made
    update_pending_++;
}


/*
 * scan the image to find the distribution of pixel values
 * and set the cut levels so that the given percent of pixels
 * are within the low and high cut values.
 */
void ImageData::autoSetCutLevels(double percent)
{
    // get dimensions of visible image area (minus 10 pixels on each side)
    int w = x1_ - x0_ + 1, h = y1_ - y0_ + 1;
    // change to  percent to cut off and split between low and high
    int cutoff = (int)((w*h * (100.0 - percent)/100.0)/2);

    // set initial default values
    getMinMax();		// get min/max pixel estimate for visible area
    double low = minValue_;
    double high = maxValue_;

    // xyvalues is an array of X,Y pairs where:
    // the X values are the pixel values (rounded to nearest factor)
    // the Y values are the number of pixels in a given range
    int numValues = 2048;
    double xyvalues[2048*2];

    // get a rough distribution of the data
    getDist(numValues, xyvalues);
    
    // find out how many pixel we actually counted (may be significant
    // numbers of blanks)
    int npixels = 0;
    int i;
    for (i=0 ; i<numValues; i++) {
        npixels += (int)(xyvalues[i*2+1]);
    }
    if ( npixels > 0 ) {

      // change to  percent to cut off and split between low and high
      int cutoff = int((double(npixels)*(100.0-percent)/100.0)/2.0);
      
      // set low cut value
      npixels = 0;
      int nprev = 0;
      for (i=0 ; i<numValues; i++) {
        nprev = npixels;
        npixels += (int)(xyvalues[i*2+1]);
	if (npixels >= cutoff) {
          low = xyvalues[i*2];
          if ( i != 0 ) {
            // Interpolate between the relevant bins.
            double interp = (double(cutoff)-double(nprev))/(double(npixels)-double(nprev));
            low = xyvalues[(i-1)*2] + (low-xyvalues[(i-1)*2])*interp;
          }
          break;
	}
      }
      
      // set high cut value
      npixels = 0;
      nprev = 0;
      for (i=numValues-1 ; i>0; i--) {
        nprev = npixels;
	npixels += (int)(xyvalues[i*2+1]);
	if (npixels >= cutoff) {
          high = xyvalues[i*2];
          if ( i != numValues-1 ) {
            // Interpolate between the relevant bins.
            double interp = (double(cutoff)-double(nprev))/(double(npixels)-double(nprev));
            high = xyvalues[(i+1)*2] + (xyvalues[(i+1)*2]-high)*interp;
          }
          break;
	}
      }
    }

    if (high > low)
	setCutLevels(low, high, 1);
}


/*
 * Update the entire X image data, if necessary, from the raw data,
 * with transformations
 */
void ImageData::update()
{
    if (xImage_ && update_pending_ && width_ > 0 && height_ > 0) {
	toXImage(0, 0, width_-1, height_-1, 0, 0);
    } 
}


/*
 * update the image area starting at the given offset (image coordinate
 * distance from the upper left corner of the window)
 * and continuing to the end of the raw image or the end of the X image
 * data, which ever comes first. The raw data starting at the offset
 * (x0,y0) is copied to the X image starting at (0,0) or at the correct
 * offset (see code below).
 *
 * (This method is used when the X Image is the same size as the visible
 * window (or image, if smaller) and displays the part of the image at
 * some x,y scroll offset.)
 */
void ImageData::updateOffset(double x, double y)
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
   
    // we have to clear out the XImage if the new image doesn't cover it
    // up completely
    if (dest_x || dest_y || x1-x0 < xImageMaxX_ || y1-y0 < xImageMaxY_) {
	// if (verbose_)
	//    printf("%s: clear ximage before update\n", name_);
	xImage_->clear(color0_);
    }

    // copy raw to X image while doing transformations
    toXImage(x0, y0, x1, y1, dest_x, dest_y);
}


/*
 * copy the raw image to the xImage, doing any transformations as
 * necessary.
 *
 * The arguments x0, y0, x1 and y1 are the bounding box of the region of
 * the raw image (image coordinates) that needs to be copied.
 *
 * dest_x and dest_y give the coordinates in the XImage where copying
 * should start. These are normally either (-x0,-y0) or (0,0).
 *
 * This method adjust the coordinates, if necessary and then calls
 * the virtual methods in derived classes to do the real work.
 */
void ImageData::toXImage(int x0, int y0, int x1, int y1, int dest_x, int dest_y)
{
    // add any requested x,y raw data offset and check range
    int maxx = width_ - 1, maxy = height_ - 1;

    // if (verbose_)
    //	printf("toXImage: %d,%d, %d,%d\n", x0, y0, x1, y1);

    x0_ = min(max(x0, 0), maxx);
    y0_ = min(max(y0, 0), maxy);
    x1_ = min(min(x1, maxx-dest_x), x0_+xImageMaxX_-dest_x);
    y1_ = min(min(y1, maxy-dest_y), y0_+xImageMaxY_-dest_y);

    // copy the relevant area of the raw image to the X image
    if (xScale_ > 1) 
	grow(x0_, y0_, x1_, y1_, dest_x, dest_y); 
    else if (xScale_ < 0) 
	shrink(x0_, y0_, x1_, y1_, dest_x, dest_y);
    else 
	rawToXImage(x0_, y0_, x1_, y1_, dest_x, dest_y);

    // x0_, y0_, x1_ and y1_ are the coordinates of the visible part of 
    // the image and are needed later for setting cut levels and calculating 
    // the min/max pixel for the displayed image area. The display routines
    // above (rawToXImage, grow, shrink) use the "unflipped" coords 
    // (because they were originally written that way...)
    flip(x0_, y0_, x1_, y1_);
    
    update_pending_ = 0;
}


/*
 * scan the raw image along the line (x0,y0) to (x1,y1) (image coords)
 * and generate an array with (index, pixel value) information.
 *
 * Return the number of (index,value) pairs generated for the line.
 *
 */
int ImageData::getSpectrum(double* xyvalues, int x0, int y0, int x1, int y1)
{
    int i = 0;

    if (y1 == y0) {
	// horizontal line
	int startx = min(x0, x1);
	int endx = max(x0, x1);
	for (int x = startx; x <= endx; x++) {
	    xyvalues[i*2] = i;
	    xyvalues[i*2+1] = getValue(x, y0);
	    i++;
	}
	return i;
    }

    if (x1 == x0) {
	// vertical line
	int starty = min(y0, y1);
	int endy = max(y0, y1);
	for (int y = starty; y <= endy; y++) {
	    xyvalues[i*2] = i;
	    xyvalues[i*2+1] = getValue(x0, y);
	    i++;
	}
	return i;
    }

    // sloped line

    // use Bresenham midpoint line scan-conversion algorithm
    // see: Computer Graphics Princ. a. Pract., 2nd Ed., p. 78
    // also see x11r5/mit/server/ddx/cfb/cfbline.c, cfbbres.c

    int x = x0;
    int y = y0;
    int e, e1, e2, e3;		// bresenham error and increments
    int len;			// length of segment 

    int adx = x1 - x0;		// abs values of dx and dy
    int ady = y1 - y0;
    int signdx = 1;		// sign of dx and dy
    int signdy = 1;

    if (adx < 0) {
	adx = -adx;
	signdx = -1;
    }
    if (ady < 0) {
	ady = -ady;
	signdy = -1;
    }

    // start pixel
    xyvalues[i*2] = i;
    xyvalues[i*2+1] = getValue(x, y); 
    i++;

    if (adx > ady) {
	// X major axis;
	e1 = ady << 1;
	e2 = e1 - (adx << 1);
	e3 = e2 - e1;
	e = -adx;
	len = adx;
	while (len--) { 
	    e += e1;
	    x += signdx;
	    if (e >= 0) {
		y += signdy;
		e += e3;
	    }
	    xyvalues[i*2] = i;
	    xyvalues[i*2+1] = getValue(x, y); 
	    i++;
	}
    }
    else {
	// Y major axis
	e1 = adx << 1;
	e2 = e1 - (ady << 1);
	e3 = e2 - e1;
	e = -ady;
	len = ady;
	while(len--) {
	    e += e1;
	    y += signdy;
	    if (e >= 0) {
		x += signdx;
		e += e3;
	    }
	    xyvalues[i*2] = i;
	    xyvalues[i*2+1] = getValue(x, y); 
	    i++;
	}
    }

    // return the number of (index,value) pairs generated
    return i;
}


/*
 * get statistics on specified area of image by calling the function
 * "iqe" (Image Quality Estimate) and passing it the requested part 
 * of the image as an array of doubles.
 *
 * x,y   - are the x and y offsets in image coords in the image
 * w,h   - indicate the size of the image square to examine
 *
 * The rest of the values are return parameters passed back from 'iqe':
 *
 * meanX          = mean X position within array, first pixel = 0
 * meanY          = mean Y position within array, first pixel = 0
 * fwhmX          = FWHM in X
 * fwhmY          = FWHM in Y
 * symetryAngle   = angle of major axis, degrees, along X = 0
 * objectPeak     = peak value of object above background
 * meanBackground = mean background level
 *
 * The return value is 0 if all is OK.
 */
int ImageData::getStatistics(double x, double y, int w, int h, 
		  double& meanX, double& meanY,
		  double& fwhmX, double& fwhmY,
		  double& symetryAngle, 
		  double& objectPeak, double& meanBackground)
{
    // Get the image data for the area into an array.
    // Use floats because the midas C routines use them...
    float* ar = new float[w*h];
    getValues(x, y, w, h, ar);

    float parm[8], sdev[8];
    int status = (iqe(ar, NULL, w, h, parm, sdev) != 0);

    delete [] ar;

    meanX = parm[0]; 
    meanY = parm[2]; 
    fwhmX = parm[1]; 
    fwhmY = parm[3]; 
    symetryAngle = parm[4];  
    objectPeak = parm[5];  
    meanBackground = parm[6];

    if (status != 0)
	error("Could not calculate statistics on specified area of image. Please make another selection.");

    return status;
}


/* 
 * scan the image and generate X,Y values to show the distribution of
 * pixel values in the image. "numValues" is the max number of X,Y pairs
 * to put in xyvalues (if there are not enough values, numValues is modified
 * to reflect the actual, smaller number of values)
 */
void ImageData::getDist(int& numValues, double* xyvalues)
{
    double n = maxValue_ - minValue_;
    if (n <= 0) {
        numValues = 0;
	return;
    }
    
    if (n < numValues && dataType() != FLOAT_IMAGE) 
        numValues = int(n);

    register float m = minValue_;
    double factor = n/numValues;

    // the X values are the pixel values 
    for (int i=0; i<numValues; i++, m+=factor) {
	xyvalues[i*2] = scaleValue(m);
	xyvalues[i*2+1] = 0;
    }

    // the Y values are the number of pixels in a given range
    if (factor >= 0.0)
	getPixDist(numValues, xyvalues, factor);
}

