/*
 * E.S.O. - VLT project
 *
 * "@(#) $Id: ImageTemplates.C,v 1.4 2005/02/02 01:43:02 brighton Exp $"
 *
 * ImageTemplates.C - template member functions for classes derived from
 *                    class ImageData (not C++ templates, uses cpp macros)
 *
 * See the man page ImageData(3) for a complete description of this class
 * hierarchy.
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * Peter W. Draper 29/01/97  Added FITS_LONG changes (for alpha 64 longs)
 * P.Biereichel    30/06/97  Changed parameters in getValues() for pixel table
 * Peter W. Draper 23/02/98  Changed min/max calculations to use more
 *                           pixels (too many occurences of "strange" limits).
 *                 06/03/98  Added changes to support XImage depths
 *                           greater than one byte.
 * pbiereic        22/03/99  Added on-the-fly subtraction.
 * Peter W. Draper 17/08/00  Changed getMinMax slightly. This now
 *                           checks if the total height of the image
 *                           is shown, rather than just testing
 *                           y0_. This could be wrong for zoomed
 * pbiereic        21/06/00  Fixed "array out of bounds" in getMinMax()
 * Peter W. Draper 19/03/01  Made sure that x1 and y1 in getMinMax,
 *                           never run off the edges of image.
 * pbiereic        17/02/03  Native byte order routines revised
 * Peter W. Draper 30/05/03  Skip runs of blank pixels in median estimate
 * pbiereic        18/06/04  Added experimental sampling methods
 * Peter W. Draper 01/11/06  Changed getMinMax to deal with images that
 *                           have a dimension of 1 (but not both).
 *
 * This file is included in the .C files for classes derived from class
 * ImageData. The file defines a number of member functions that are
 * always the same, except for the underlying raw image data type. The
 * member functions are implemented in the derived classes for speed
 * (otherwise we would have to call virtual functions on each pixel...)
 *
 * Before including, the following #defines need to be made:
 *
 * #define CLASS_NAME <...> - as the name of the derived class
 * #define DATA_TYPE as the raw image data type (short, long, float,...)
 * #define NTOH(x)   to be just (x) if you're sure your type never requires
 *                   byte-swapping.  Otherwise leave it undefined and the
 *                   code below should do the right thing.
 * #define ISNAN(x)  Only for floating point types, returns true if x is a NAN
 */

/*
 * For all member functions within this file use the inline function getVal()
 * instead of referencing the raw image directly. getVal() returns the raw
 * image pixel value or the subtracted value if a bias frame was selected.
 * Before using getVal() initialize this function with initGetVal().
 */

#include <iostream>
#include <cassert>
#include "define.h"

// This is a no-op for non-floating point data types. This macro is
// defined as isnan(x) for floating point types.
#ifndef ISNAN
#define ISNAN(x) 0
#endif

/*
 * getVal() is an inline function which subtracts on-the-fly a
 * bias pixel if a bias frame is selected.
 * p    - is a pointer to the raw image
 * idx  - is an index to the image
 *
 * looks "big" but it isn't (gcc assembler code was checked).
 */
inline DATA_TYPE CLASS_NAME::getVal(DATA_TYPE* p, int idx)
{
    // return pixel value if bias subtraction is off
    if (! ImageData::biasInfo_->on)
        return (DATA_TYPE)NTOH(*(p + idx));

    if ( ! bias_swap_bytes_) {

	// return faster if image dimensions and types are the same
	if (ImageData::biasInfo_->sameTypeAndDims)
	    return ((DATA_TYPE)NTOH(*(p + idx)) -
		    (DATA_TYPE)((DATA_TYPE)(*((DATA_TYPE*)ImageData::biasInfo_->ptr + idx))));

	register biasINFO* bias = ImageData::biasInfo_;
	register int x = idx % width_ + startX_;
	register int y = idx / width_ + startY_;

	// if pixel is not within the boundary of the bias image return pixel value
	if (x < 0 || x >= bias->width || y < 0 || y >= bias->height)
	    return NTOH(*(p + idx));

	register int biasIdx = y * bias->width + x;

	switch (bias->type) {
	case BYTE_IMAGE:
	case X_IMAGE:
	    return (DATA_TYPE)NTOH(*(p + idx)) - (DATA_TYPE)((byte)(*((byte *)bias->ptr     + biasIdx)));
	case USHORT_IMAGE:
	    return (DATA_TYPE)NTOH(*(p + idx)) - (DATA_TYPE)((ushort)(*((ushort *)bias->ptr + biasIdx)));
	case SHORT_IMAGE:
	    return (DATA_TYPE)NTOH(*(p + idx)) - (DATA_TYPE)((short)(*((short *)bias->ptr   + biasIdx)));
	case LONG_IMAGE:
	    return (DATA_TYPE)NTOH(*(p + idx)) - (DATA_TYPE)((int)(*((int *)bias->ptr       + biasIdx)));
	case FLOAT_IMAGE:
	    return (DATA_TYPE)NTOH(*(p + idx)) - (DATA_TYPE)((float)(*((float *)bias->ptr   + biasIdx)));
	case DOUBLE_IMAGE:
	    return (DATA_TYPE)NTOH(*(p + idx)) - (DATA_TYPE)((double)(*((double *)bias->ptr + biasIdx)));
	default:
	    return (DATA_TYPE)NTOH(*(p + idx));
	}
    }

    else {

	register biasINFO* bias = ImageData::biasInfo_;
	register int x = idx % width_ + startX_;
	register int y = idx / width_ + startY_;

	// if pixel is not within the boundary of the bias image return pixel value
	if (x < 0 || x >= bias->width || y < 0 || y >= bias->height)
	    return NTOH(*(p + idx));

	register int biasIdx = y * bias->width + x;

	switch (bias->type) {
	case BYTE_IMAGE:
	case X_IMAGE:
	    return (DATA_TYPE)NTOH(*(p + idx)) - (DATA_TYPE)((byte)(*((byte *)bias->ptr             + biasIdx)));
	case USHORT_IMAGE:
	    return (DATA_TYPE)NTOH(*(p + idx)) - (DATA_TYPE)((ushort)SWAP16(*((ushort *)bias->ptr   + biasIdx)));
	case SHORT_IMAGE:
	    return (DATA_TYPE)NTOH(*(p + idx)) - (DATA_TYPE)((short)SWAP16(*((short *)bias->ptr     + biasIdx)));
	case LONG_IMAGE:
	    return (DATA_TYPE)NTOH(*(p + idx)) - (DATA_TYPE)((int)SWAP32(*((int *)bias->ptr         + biasIdx)));
	case FLOAT_IMAGE:
	    return (DATA_TYPE)NTOH(*(p + idx)) - (DATA_TYPE)((float)SWAP_FLOAT(*((float *)bias->ptr + biasIdx)));
	case DOUBLE_IMAGE:
	    return (DATA_TYPE)NTOH(*(p + idx)) - (DATA_TYPE)((double)SWAP_DOUBLE(*((double *)bias->ptr + biasIdx)));
	default:
	    return (DATA_TYPE)NTOH(*(p + idx));
	}
    }
}

/*
 * Scan the image to find the min and max values and set the member
 * variables accordingly.  To save time, only the visible area of the
 * image is examined (x0_, y0_, x1_ and y1_ are set each time the
 * image is updated, however they are initially set to the entire
 * image).
 * The result is that the member variables minValue_ and maxValue_
 * are set.
 */
void CLASS_NAME::getMinMax()
{
    DATA_TYPE* rawImage = (DATA_TYPE*)image_.dataPtr();
    int p;
    register DATA_TYPE  value;

    initGetVal();  // init flag for speeding up bias subtraction

    // use area of image that is visible.
    // if we are looking at the whole image, ignore the margin
    int w = x1_ - x0_ + 1, h = y1_ - y0_ + 1;
    int xmargin = 0, ymargin = 0;
    if (w == width_)
	xmargin = int(w * 0.02);
    if ( h == height_ )
	ymargin = int(h * 0.02);  // PWD: change here, test was y0_ == 0

    int x0 = x0_ + xmargin;
    int y0 = y0_ + ymargin;
    int x1 = min( x1_ - xmargin, width_ - 1 ); // PWD: stop running
    int y1 = min( y1_ - ymargin, height_ - 1 );// off edges
    w = x1 - x0 + 1;
    h = y1 - y0 + 1;

    if (w < 1 || h < 1) {
	if (area_ > 0) {
	    minValue_ = maxValue_ = getVal(rawImage, 0);
        } else {
	    minValue_ = maxValue_ = 0;
        }
	return;
    }

    // set the x, y increments so that not every pixel or line is
    // examined on large images
    int xincr = w/256;
    if (xincr == 0)
	xincr++;
    int yincr = h/256;
    if (yincr == 0)
	yincr++;

    if (x1 >= x1_ - xincr) {
	x1 = x1_ - xincr;
        if ( x1 < 0 ) {
            x1 = 1;
        }
   }

    if (y1 >= y1_ - yincr) {
	y1 = y1_ - yincr;
        if ( y1 < 0 ) {
            y1 = 1;
        }
    }

    // try to speed things up a bit on large images:
    // don't examine every pixel, just look at a few sample lines
    p = y0*width_+x0; // first pixel in region to examine
    value = getVal(rawImage, p);

    // ignore BLANK pixels. For efficiency, we only check the flag once
    int end = area_;
    if (haveBlank_) {
	// make sure starting min/max values are not the BLANK pixel
	while(value == blank_ || ISNAN(value)) {
	    p += 10;              // check another pixel
	    if (p >= end)
		break;
	    value = getVal(rawImage, p);
	}
	if (ISNAN(value))
	    value = 0;
	minValue_ = maxValue_ = value;
	for (int y = y0; y <= y1; y+=yincr) {
            p = y*width_+x0;
	    if (p >= end)
		break;
	    for (int x = x0; x <= x1; x+=xincr, p+=xincr) {
                if ((value = getVal(rawImage, p)) == blank_ || ISNAN(value))
		    continue;
		if (value < minValue_)
		    minValue_ = value;
		else if (value > maxValue_)
		    maxValue_ = value;
	    }
	}
    }
    else {
	// note that for non-float types, the ISNAN call is optimized away
	while(ISNAN(value)) {
	    p += 10;              // check another pixel
	    if (p >= end)
		break;
	    value = getVal(rawImage, p);
	}
	if (ISNAN(value))
	    value = 0;
	minValue_ = maxValue_ = value;
	for (int y = y0; y <= y1; y+=yincr) {
            p = y*width_+x0;
	    if (p >= end)
		break;
	    for (int x = x0; x <= x1; x+=xincr, p+=xincr) {
                value = getVal(rawImage, p);
		if (ISNAN(value))
		    continue;
		if (value < minValue_)
		    minValue_ = value;
		else if (value > maxValue_)
		    maxValue_ = value;
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
char* CLASS_NAME::getValue(char* buf, double x, double y)
{
    DATA_TYPE* rawImage = (DATA_TYPE*)image_.dataPtr();
    DATA_TYPE value;

    initGetVal();  // init flag for bias subtraction

    int ix, iy;
    if (getIndex(x, y, ix, iy) != 0)
	sprintf(buf, "%.1f %.1f -", x, y);
    else {
	value = getVal(rawImage, iy*width_+ix);
	if (haveBlank_ && value == blank_)
	    sprintf(buf, "%.1f %.1f blank", x, y);
	else
	    sprintf(buf, "%.1f %.1f %g", x, y, scaleValue(value));
    }
    return buf;
}


/*
 * return the value at the x,y image coords as a double.
 *
 * The input x,y is assumed to be in image coords.
 * If the coords are out of range, 0.0 is returned.
 */
double CLASS_NAME::getValue(double x, double y)
{
    DATA_TYPE* rawImage = (DATA_TYPE*)image_.dataPtr();
    int ix, iy;

    initGetVal();  // init flag for bias subtraction

    if (getIndex(x, y, ix, iy) != 0)
	return 0.0;
    return scaleValue(getVal(rawImage, iy*width_+ix));
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
void CLASS_NAME::getValues(double x, double y, double rx, double ry,
			   char* xStr, char* yStr, char* valueStr,
			   char* raStr, char* decStr, char* equinoxStr)
{
    initGetVal();  // init flag for speeding up bias subtraction

    // display chip coords for x and y
    double cx = x, cy = y;
    imageToChipCoords(cx, cy);
    sprintf(xStr, "%.1f", cx);
    sprintf(yStr, "%.1f", cy);

    *raStr = '\0';
    *decStr = '\0';
    *equinoxStr = '\0';
    if (wcs().isWcs()) {
	char buf[80];
	wcs().pix2wcs(x, y, buf, sizeof(buf), 1);
	sscanf(buf, "%s %s %s", raStr, decStr, equinoxStr);
    }

    // get integer index in raw image for pixel value
    int ix, iy;
    *valueStr = '\0';
    if (getIndex(rx, ry, ix, iy) == 0) {
	DATA_TYPE* rawImage = (DATA_TYPE*)image_.dataPtr();
	DATA_TYPE value = getVal(rawImage, iy*width_+ix);

	if (haveBlank_ && value == blank_)
	    strcpy(valueStr, "blank");
	else
	    sprintf(valueStr, "%g", scaleValue(value));
    }
}


/*
 * Fill the given array with the pixel values surrounding the given point.
 * nrows and ncols give the dimensions of the array. Any values that are outside
 * of the image or are blank (BLANK keyword) are set to -HUGE_VAL (If "flag"
 * is non-zero, values outside the image are not changed).
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
 *
 */
void CLASS_NAME::getValues(double x, double y, double rx, double ry, double* ar,
			   int nrows, int ncols, int flag)
{
    DATA_TYPE* rawImage = (DATA_TYPE*)image_.dataPtr();

    initGetVal();  // init flag for speeding up bias subtraction

    int m = ncols/2;
    int n = nrows/2;
    int w = ncols+1;
    int i, j;

    // get pixel index
    int ix, iy;

    // insert the x coord headings
    for (i = 0; i < ncols; i++) {
	double cx = x+(i-m);
	imageToChipCoords(cx);
	ar[i+1] = cx;	// X coord top heading
    }

    for (j = 0; j < nrows; j++) {
	double cy = y+(j-n);
	imageToChipCoords(cy);
	ar[(j+1)*w] = cy; // Y coord left heading
	for (i = 0; i < ncols; i++) {
	    if (getIndex(rx+(i-m), ry+(j-n), ix, iy) == 0) {
		DATA_TYPE value = getVal(rawImage, iy*width_+ix);
		if (haveBlank_ && value == blank_)
		    ar[(j+1)*w+i+1] = -HUGE_VAL;
		else
		    ar[(j+1)*w+i+1] = scaleValue(value);
	    }
	    else if (! flag) {
		ar[(j+1)*w+i+1] = -HUGE_VAL;
	    }
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
void CLASS_NAME::getValues(double x, double y, int w, int h, float* ar, int flag)
{
    DATA_TYPE* rawImage = (DATA_TYPE*)image_.dataPtr();

    int i, j;
    int ix, iy;
    DATA_TYPE value;

    initGetVal();  // init flag for speeding up bias subtraction

    getIndex(x, y, ix, iy);

    for (j = 0; j < h; j++) {
	for (i = 0; i < w; i++) {
	    int rx = ix+i, ry = iy+j;
	    if (rx >= 0 && ry >= 0 && rx < width_ && ry < height_) {
		value = getVal(rawImage, ry*width_+rx);
		if (haveBlank_ && value == blank_) {
		    // ar[j*w+i] = 0;
		    ar[j*w+i] = blank_;
		}
		else {
		    ar[j*w+i] = scaleValue(value);
		}
	    }
	    else if (!flag) {
		ar[j*w+i] = blank_;
	    }
	}
    }
}


/*
 * Copy raw image data from this image to the given image data area,
 * starting at the image coordinates (x, y) and with the dimentions (w,h)
 * in pixels.  Since this is a copy from one raw image to another, no
 * data conversion is done.
 */
void CLASS_NAME::copyImageArea(void* data, double x, double y, int w, int h)
{
    DATA_TYPE* fromImage = (DATA_TYPE*)image_.dataPtr();
    DATA_TYPE* toImage = (DATA_TYPE*)data;

    int i, j;
    int ix, iy;

    getIndex(x, y, ix, iy);

    for (j = 0; j < h; j++) {
	for (i = 0; i < w; i++) {
	    int rx = ix+i, ry = iy+j;
	    if (rx >= 0 && ry >= 0 && rx < width_ && ry < height_) {
		toImage[j*w+i] = fromImage[ry*width_+rx];
	    }
	    else {
		toImage[j*w+i] = blank_;
	    }
	}
    }
}


/*
 * copy the raw image to the xImage
 *
 * The arguments x0, y0, x1 and y1 are the bounding box of the region of
 * the raw image that needs to be copied (origin at upper left (0,0)).
 *
 * dest_x and dest_y give the coordinates in the XImage where copying
 * should start. These are normally either (x0,y0) or (0,0).
 */
void CLASS_NAME::rawToXImage(int x0, int y0, int x1, int y1,
			     int dest_x, int dest_y)
{
    // if (verbose_)
    //	printf("%s: rawToXImage: %d,%d  %d,%d +(%d,%d), flip: %d,%d, rotate: %d\n",
    //	       name_, x0, y0, x1, y1, dest_x, dest_y, flipX_, flipY_, rotate_);

    register int i, j;

    // row/col increments
    register int src_x_inc, src_y_inc;
    register int dest_x_inc, dest_y_inc;

    // source/dest images
    register DATA_TYPE* rawImage = (DATA_TYPE*)image_.dataPtr();
    register int src;
    register byte* dest = xImageData_;

    initGetVal();  // init flag for speeding up bias subtraction

    // width of image area to update
    int w = x1 - x0 + 1;

    // set loop increments for based on current transformations
    switch (flipX_<<1|flipY_) {
    case 0: // none
	src =  (height_ - 1 - y0) * width_ + x0;
	src_x_inc = 1;
	src_y_inc = -width_ - w;
	break;
    case 1: // flipY
	src =  y0 * width_ + x0;
	src_x_inc = 1;
	src_y_inc = width_ - w;
	break;
    case 2: // flipX
	src =  (height_ - 1 - y0) * width_ + (width_ - 1 - x0);
	src_x_inc = -1;
	src_y_inc = -(width_ - w);
	break;
    case 3: // flipX and flipY
	src =  y0 * width_ + (width_ - 1 - x0);
	src_x_inc = -1;
	src_y_inc = width_ + w;
	break;
    }

    // need to take care with non-byte depths, so branch according to
    // this
    if ( xImageBytesPerPixel_ == 1 ) {

	// set args for rotate in dest image
	if (rotate_) {
	    dest_x_inc = xImageBytesPerLine_;
	    dest_y_inc = -(w * xImageBytesPerLine_ - 1);
	    dest += xImageBytesPerLine_ * dest_x  + dest_y;
	}
	else {
	    dest_x_inc = 1;
	    dest_y_inc = xImageBytesPerLine_ - w;
	    dest += xImageBytesPerLine_ * dest_y  + dest_x;
	}

	// copy the raw data to the X image...
	for (i=y0; i<=y1; i++) {
	    for (j=x0; j<=x1; j++) {
		*dest = lookup(getVal(rawImage, src));
		dest += dest_x_inc;
		src += src_x_inc;
	    }
	    src += src_y_inc;
	    dest += dest_y_inc;
	}
    }
    else {
	//  XImage has depth greater than a byte, need to take care with
	//  these (byte swapping etc. to server format)
	register int k = dest_x;
	register int l = dest_y;
	for (i=y0; i<=y1; i++) {
	    for (j=x0; j<=x1; j++) {
		if ( rotate_ ) {
		    xImage_->putpixel( l, k, llookup(getVal(rawImage, src)));
		}
		else {
		    xImage_->putpixel( k, l, llookup(getVal(rawImage, src)));
		}
		src += src_x_inc;
		k++;
	    }
	    src += src_y_inc;
	    l++;
	    k = dest_x;
	}
    }
}

/*
 * This method is called to magnify the image by factors >= 2.
 *
 * The arguments x0, y0, x1 and y1 are the bounding box of the region
 * of the raw image that needs to be copied (origin at upper left (0,0)).
 *
 * dest_x and dest_y give the coordinates in the XImage where copying
 * should start. These are normally either (x0,y0) or (0,0).
 */
void CLASS_NAME::grow(int x0, int y0, int x1, int y1,
		      int dest_x, int dest_y)
{
    register byte *p, *q;
    register byte c;
    register int i, j, n, m;
    register int xs = xScale_, ys = yScale_;

    // row/col increments
    register int src_x_inc, src_y_inc;
    register int dest_x_inc, dest_y_inc;

    // source/dest images
    register DATA_TYPE* rawImage = (DATA_TYPE*)image_.dataPtr();
    register int src;
    register byte* dest = xImageData_;
    register byte* end = xImageData_ + xImageSize_;

    initGetVal();  // init flag for speeding up bias subtraction

    // width of image area to update
    int w = x1 - x0 + 1;

    // set loop increments based on current transformations
    switch (flipX_<<1|flipY_) {
    case 0: // none
	src =  (height_ - 1 - y0) * width_ + x0;
	src_x_inc = 1;
	src_y_inc = -width_ - w;
	break;
    case 1: // flipY
	src =  y0 * width_ + x0;
	src_x_inc = 1;
	src_y_inc = width_ - w;
	break;
    case 2: // flipX
	src =  (height_ - 1 - y0) * width_ + (width_ - 1 - x0);
	src_x_inc = -1;
	src_y_inc = -(width_ - w);
	break;
    case 3: // flipX and flipY
	src =  y0 * width_ + (width_ - 1 - x0);
	src_x_inc = -1;
	src_y_inc = width_ + w;
	break;
    }

    // need to take care with non-byte depths, so branch according to
    // this
    if ( xImageBytesPerPixel_ == 1 ) {

	// set args for rotate in dest image
	if (rotate_) {
	    dest_x_inc = xImageBytesPerLine_ * xs;
	    dest_y_inc = -(w * xs * xImageBytesPerLine_ - ys);
	    dest += xImageBytesPerLine_ * xs * dest_x + dest_y * ys;
	}
	else {
	    dest_x_inc = xs;
	    dest_y_inc = xImageBytesPerLine_ * ys  -  w * xs;
	    dest += xImageBytesPerLine_ * ys * dest_y + dest_x * xs;

	}

	// copy the raw data to the X image...
	for (i=y0; i<=y1; i++) {
	    for (j=x0; j<=x1; j++) {
		c = lookup(getVal(rawImage, src));
		q = p = dest;
		src += src_x_inc;
		dest += dest_x_inc;
		// replicate the source pixel to an xs x ys box in the dest
		for (n=0; n<ys; n++) {
		    for (m=0; m<xs; m++) {
			if (p >= end) {
			    break;
			}
			*p++ = c;
		    }
		    p = q += xImageBytesPerLine_;
		}
	    }
	    src += src_y_inc;
	    dest += dest_y_inc;
	}
	return;
    }


    //  XImage has depth greater than a byte. Need to take care with
    //  these (byte swapping etc. to server format, if really
    //  pushed for performance could use ImageByteOrder() and wing it).
    int k = dest_x * xs;
    int l = dest_y * ys;
    unsigned long cl;
    int width;
    int height;
    if ( rotate_ ) {
	height = xImage_->width();
	width = xImage_->height();
    }
    else {
	width = xImage_->width();
	height = xImage_->height();
    }
    for ( i = y0; i <= y1; i++ ) {
	for ( j = x0; j <= x1; j++ ) {
	    cl = llookup(getVal(rawImage, src));
	    // replicate the source pixel to an xs x ys box
	    int maxN = min(l+ys,height), maxM = min(k+xs,width);
	    for ( n = l; n < maxN; n++ ) {
		for ( m = k; m < maxM; m++ ) {
		    if ( rotate_ ) {
			xImage_->putpixel( n, m, cl );
		    }
		    else {
			xImage_->putpixel( m, n, cl );
		    }
		}
	    }
	    k += xs;
	    src += src_x_inc;
	}
	k = dest_x * xs;
	l += ys;
	src += src_y_inc;
    }
}

/*
 * get samples of a square box
 *    rawImage : input array
 *    idx      : offset in array
 *    wbox     : width of box
 *    samples  : output vector
 *    returns the number of pixels in the output array samples
 */

inline int CLASS_NAME::getBsamples(DATA_TYPE *rawImage, int idx, int wbox, DATA_TYPE *samples)
{
    int i, k, src;

    for (k = 0; k < wbox; k++) {
	src = idx + k * width_;
	for (i = 0; i < wbox; i++) {
	    *samples++ = getVal(rawImage, src++);
	}
    }
    return (wbox * wbox);
}

/*
 * get samples of a square box (only "black" values on chess-board)
 *    rawImage : input array
 *    idx      : offset in array
 *    wbox     : width of box
 *    samples  : output vector
 *    returns the number of pixels in the output array samples
 */

inline int CLASS_NAME::getCsamples(DATA_TYPE *rawImage, int idx, int wbox, DATA_TYPE *samples)
{
    int i, k, src;

    for (k = 0; k < wbox; k++) {
	src = idx + k * width_ + k%2;
	for (i = 0; i < wbox; i += 2) {
	    *samples++ = getVal(rawImage, src+i);
	}
    }
    return ((wbox * wbox) / 2 + wbox%2);
}

/*
 * get samples on a diagonal cross of a square box
 *    rawImage : input array
 *    idx      : offset in array
 *    wbox     : width of box
 *    samples  : output vector
 *    returns the number of pixels in the output array samples
 */

inline int CLASS_NAME::getXsamples(DATA_TYPE *rawImage, int idx, int wbox, DATA_TYPE *samples)
{
    int i, n = 0, m = wbox/2;
    int offs = wbox -1;
    int idxo = idx + offs;
    int woff = width_ * offs;

    if (wbox %2 != 0) {
	n++;
	*samples++ = getVal(rawImage, idx + width_ * m + m); /* center pixel of cross */
    }

    for (i = 0; i < m; i++) {
	*samples++ = getVal(rawImage, idx);
	*samples++ = getVal(rawImage, idxo);
	*samples++ = getVal(rawImage, idx + woff);
	*samples++ = getVal(rawImage, idxo + woff);
	idx += width_ + 1;
	offs -= 2;
	idxo = idx + offs;
	woff = width_ * offs;
    }
    return (n + m * 4);
}

/*
 * get median value of array samples
 *    samples  : data vector
 *    n        : number of pixels in vector samples
 *    returns the median value of vector samples
 */

inline DATA_TYPE CLASS_NAME::getMedian(DATA_TYPE *samples, int n)
{
    int i, k;
    DATA_TYPE *pi, *pk = samples, tmp;

    for (k = 0; k < n; k++, pk++) {
	pi = samples + k+1;
	for (i = k+1; i < n; i++, pi++) {
	    if (*pk < *pi) {
		tmp = *pk;
		*pk = *pi;
		*pi = tmp;
	    }
	}
    }
    return (*(samples + n/2));
}

/*
 * get RMS value of array samples
 *    samples  : data vector
 *    n        : number of pixels in vector samples
 *    returns the RMS value of vector samples
 */

inline DATA_TYPE CLASS_NAME::getRMS(DATA_TYPE *samples, int n)
{
    int i, cnt = 0;
    double sum = 0.0, sumsq = 0.0;
    DATA_TYPE value;

    for (i = 0; i < n; i++) {
        value = *samples++;
        cnt++;
        sumsq += value * value;
        sum += value;
    }
    if (cnt < 2)
        return ((DATA_TYPE) 0);
    value = (DATA_TYPE)sqrt((sumsq - ((sum * sum) / cnt)) / (cnt -1));
    return (value);
}

/*
 * get value of a shrunk image which is not subsampled
 *    rawImage : input array
 *    idx      : offset in array
 *    wbox     : width of box
 *    samples  : allocated temporary array
 *    xs       : scale factor
 *    returns the value according to the sampling method
 */

inline DATA_TYPE CLASS_NAME::getBoxVal(DATA_TYPE *rawImage, int idx, int wbox, DATA_TYPE *samples, int xs)
{
    DATA_TYPE value;
    DATA_TYPE *psamples = samples;
    int n, m;
    double sum;

    switch (sampmethod_) {

    case SAMP_METHOD_MIN:
        m = getBsamples(rawImage, idx, wbox, samples);
        for (n = 1, value = *psamples++; n < m; n++, psamples++) {
            if (*psamples < value)
                value = *psamples;
        }
        return (value);

    case SAMP_METHOD_MEAN:
        m = getBsamples(rawImage, idx, wbox, samples);
        for (n = 0, sum = 0.0; n < m; n++) {
            sum += *psamples++;
        }
        return ((DATA_TYPE) (sum / m));

    case SAMP_METHOD_MEDIAN:
        m = getBsamples(rawImage, idx, wbox, samples);
        return (getMedian(samples, m));

    case SAMP_METHOD_RMS:
        m = getBsamples(rawImage, idx, wbox, samples);
        return ((DATA_TYPE)getRMS(samples, m));

    case SAMP_METHOD_MAX_CROSS:
        m = getXsamples(rawImage, idx, wbox, samples);
        for (n = 1, value = *psamples++; n < m; n++, psamples++) {
            if (*psamples > value)
                value = *psamples;
        }
        return (value);

    case SAMP_METHOD_MIN_CROSS:
        m = getXsamples(rawImage, idx, wbox, samples);
        for (n = 1, value = *psamples++; n < m; n++, psamples++) {
            if (*psamples < value)
                value = *psamples;
        }
        return (value);

    case SAMP_METHOD_MEAN_CROSS:
        m = getXsamples(rawImage, idx, wbox, samples);
        for (n = 0, sum = 0.0; n < m; n++) {
            sum += *psamples++;
        }
        return ((DATA_TYPE) (sum / m));

    case SAMP_METHOD_MEDIAN_CROSS:
        m = getXsamples(rawImage, idx, wbox, samples);
        return (getMedian(samples, m));

    case SAMP_METHOD_MEDIAN_CHESS:
        m = getCsamples(rawImage, idx, wbox, samples);
        return (getMedian(samples, m));

    case SAMP_METHOD_MEDIAN_9:
        wbox = (xs < 3) ? 1 : 3;
        m = getBsamples(rawImage, idx, wbox, samples);
        return (getMedian(samples, m));

    default:   /* SAMP_METHOD_MAX */
        m = getBsamples(rawImage, idx, wbox, samples);
        for (n = 1, value = *psamples++; n < m; n++, psamples++) {
            if (*psamples > value)
                value = *psamples;
        }
        return (value);
    }
    return (0);
}


/*
 * This method is called to shrink the image. If subsample_ is 1, just
 * take every nth pixel as a sample, otherwise take the max of the
 * surrounding pixels
 *
 * The arguments x0, y0, x1 and y1 are the bounding box of the region
 * of the raw image that needs to be copied (origin at upper left (0,0)).
 *
 * dest_x and dest_y give the coordinates in the XImage where copying
 * should start. These are normally either (x0,y0) or (0,0).
 *
 * Note: there is a (non-fatal) bug here that shows up when dest_x,dest_y are non-zero,
 *       (the image display gets split or mixed up somehow...) - needs some
 *       testing to see what the cause is.
 */
void CLASS_NAME::shrink(int x0, int y0, int x1, int y1, int dest_x, int dest_y)
{
    int p=0, q=0;
    int i, j, n, m;
    int xs = -xScale_, ys = -yScale_;

    initGetVal();  // init flag for speeding up bias subtraction

    // for odd shrink factors, we have to be carefull about the end conditions
    x1 -= (x1-x0+1)%xs;
    y1 -= (y1-y0+1)%ys;
    int w = x1-x0+1;

    // row/col increments
    int src_x_inc, src_y_inc;
    int dest_x_inc, dest_y_inc;

    // source/dest images
    DATA_TYPE* rawImage = (DATA_TYPE*)image_.dataPtr();
    int src;
    byte* dest = xImageData_;
    byte* end = xImageData_ + xImageSize_ - 1;

    DATA_TYPE maxval = 0;

    // set loop increments based on current transformations
    switch (flipX_<<1|flipY_) {
    case 0: // none
	src =  (height_ - ys - y0) * width_ + x0;
	src_x_inc = xs;
	src_y_inc = -width_ * ys - w;
	break;
    case 1: // flipY
	src =  y0 * width_ + x0;
	src_x_inc = xs;
	src_y_inc = width_ * ys - w;
	break;
    case 2: // flipX
	src =  (height_ - ys - y0) * width_ + (width_ - xs - x0);
	src_x_inc = -xs;
	src_y_inc = -(width_ * ys - w);
	break;
    case 3: // flipX and flipY
	src =  y0 * width_ + (width_ - xs - x0);
	src_x_inc = -xs;
	src_y_inc = width_ * ys + w;
	break;
    }

    if ( xImageBytesPerPixel_ == 1 ) {
	//  use faster methods for byte xImages

	// set args for rotate in dest image
	if (rotate_) {
	    dest_x_inc = xImageBytesPerLine_;
	    dest_y_inc = -(w/xs * xImageBytesPerLine_ - 1);
	    dest += xImageBytesPerLine_ * (dest_x/xs) + (dest_y/ys);
	}
	else {
	    dest_x_inc = 1;
	    dest_y_inc = xImageBytesPerLine_ - w/xs;
	    dest += xImageBytesPerLine_ * (dest_y/ys) + (dest_x/xs);
	}

	// copy the raw data to the X image...
	if (subsample_) {
	    // use faster "subsample" algorithm
	    for (i=y0; i<=y1; i+=ys) {
		for (j=x0; j<=x1; j+=xs) {
		    if (dest > end) {
			break;
		    }
		    *dest = lookup(getVal(rawImage, src));
		    dest += dest_x_inc;
		    src += src_x_inc;
		}
		src += src_y_inc;
		dest += dest_y_inc;
	    }
	}
	else {
	    // don't subsample: take max pixel
	    for (i=y0; i<=y1; i+=ys) {
		for (j=x0; j<=x1; j+=xs) {
		    if (dest > end) {
			break;
		    }
		    q = p = src;
		    for (n=0; n<ys; n++) {
			for (m=0; m<xs; m++, p++) {
			    if (getVal(rawImage, p) > maxval)
				maxval = getVal(rawImage, p);
			}
			p = q += width_;
		    }
		    *dest = lookup(maxval);
		    maxval = 0;
		    dest += dest_x_inc;
		    src += src_x_inc;
		}
		src += src_y_inc;
		dest += dest_y_inc;
	    }
	}
	return;
    }


    //  XImage depth greater than a byte. Use careful methods to
    //  keep byte order etc. correct for server.
    int k = dest_x / xs;
    int l = dest_y / ys;

    if (subsample_ || xs < 2 || ys < 2) {
	// use faster "subsample" algorithm
	for (i=y0; i<=y1; i+=ys) {
	    for (j=x0; j<=x1; j+=xs) {
		if ( rotate_ ) {
		    xImage_->putpixel( l, k, llookup(getVal(rawImage, src)));
		}
		else {
		    xImage_->putpixel( k, l, llookup(getVal(rawImage, src)));
		}
		k++;
		src += src_x_inc;
	    }
	    src += src_y_inc;
	    k = dest_x / xs;
	    l++;
	}
	return;
    }

    // don't subsample: take pixel defined by sampling method

    DATA_TYPE *samples = new DATA_TYPE[xs * ys];
    DATA_TYPE vsamp;
    int wbox = xs < ys ? xs : ys;

    for (i = y0; i < y1; i += ys, src += src_y_inc, k = dest_x / xs, l++) {
        for (j = x0; j < x1; j += xs, src += src_x_inc) {
            vsamp = getBoxVal(rawImage, src, wbox, samples, xs);

            if ( rotate_ )
                xImage_->putpixel(l, k++, llookup(vsamp));
            else
                xImage_->putpixel(k++, l, llookup(vsamp));
        }
    }
    delete[] samples;
}


/*
 * Set the cut levels using a median filtering algorithm.
 * To save time, only a center area of the visible image is examined.
 */
void CLASS_NAME::medianFilter()
{
    getMinMax();		// get min/max pixel estimate for visible area
    DATA_TYPE *rawImage = (DATA_TYPE*)image_.dataPtr(); // image data
    const int nmed = 7;		   // length of median filter
    int xskip = nmed*3, yskip = 3; // skip pixels for speed
    int x0 = x0_ + 10;		   // ignore outside areas
    int y0 = y0_ + 10;
    int x1 = x1_ - 10;
    int y1 = y1_ - 10;
    int i, j, k, l;
    int p=0;
    DATA_TYPE tmp;
    DATA_TYPE val, lcut, hcut, medary[nmed];

    // use this value as a default in place of bad pixels (blank, NAN)
    DATA_TYPE mval = (DATA_TYPE)((minValue_ + maxValue_)/2);

    initGetVal();  // init flag for speeding up bias subtraction

    if (x1-x0 <= nmed || y1-y0 <= nmed)
	return;
    for (i=y0; i<=y1; i+=yskip) {
	for (j=x0; j<=x1; j+=xskip) {
	    p = i*width_ + j;

	    // get array for finding meadian
	    for (k=0; k < nmed; k++) {
		medary[k] = getVal(rawImage, p++);
		// ignore blank pixels and NANs
		if (ISNAN(medary[k]) || (haveBlank_ && medary[k] == blank_)) {
		    medary[k] = mval;
		}
	    }

	    // get median value
	    for (k=0; k < nmed; k++) {
		for (l=k; l < nmed; l++) {
		    if (medary[k] < medary[l]) {
			tmp = medary[l];
			medary[l] = medary[k];
			medary[k] = tmp;
		    }
		}
	    }
	    val = medary[nmed/2];

            if ( val == mval ) {
                if ( i == y0 ) {
                    lcut = hcut = 0; // To be safe.
                }
                continue; // PWD: Skip runs of blank pixels.
            }

	    if (i == y0)
		// set initial low and high cut values
		lcut = hcut = val;
	    else {
		// compare meadian with lcut, hcut
		if (val < lcut)
		    lcut = val;
		if (val > hcut)
		    hcut = val;
	    }
	}
    }

    setCutLevels(lcut, hcut, 0);
}


/*
 * Fill the given array (xyvalues) with statistics about the distribution
 * of pixels in the visible image area (given by x0_, y0_, x1_, y1_).
 * xyvalues[n*2+1] is set to the number of pixels with value at or near n.
 * The factor is used to fit the information in the given size aray.
 */
void CLASS_NAME::getPixDist(int numValues, double* xyvalues, double factor)
{
    DATA_TYPE* rawImage = (DATA_TYPE*)image_.dataPtr();
    DATA_TYPE mv = (DATA_TYPE)minValue_;
    DATA_TYPE value;

    initGetVal();  // init flag for speeding up bias subtraction

    // the Y values are the number of pixels in a given range
    // note: to save time, don't check all the pixels, just a
    // number of sample lines in the visible area of the image.
    if (x1_ <= x0_ || y1_ <= y0_)
	return;

    for (int i=y0_; i<y1_; i++) {
	for (int j=x0_; j<x1_; j++) {
	    // the code below converts an image pixel value to an index
	    // in the xyvalues array, so that we can increment the count for
	    // that pixel value range. The array is numValues*2 long, since
	    // for each pixel value, the value and the count are stored.
	    value = getVal(rawImage, i*width_+j);
	    if (ISNAN(value) || (haveBlank_ && value == blank_))
		continue;
	    int idx = int((value-mv)/factor);
	    if (idx >= 0 && idx < numValues)
		xyvalues[idx*2+1]++;
	}
    }
}


/*
 * Fill the given histogram with the distribution of pixels in the
 * visible image area (given by x0_, y0_, x1_, y1_).  h.histogram[n] is
 * set to the number of pixels with a value of n (after conversion to
 * short if needed).
 */
void CLASS_NAME::getHistogram(ImageDataHistogram& hist)
{
    DATA_TYPE* rawImage = (DATA_TYPE*)image_.dataPtr();
    DATA_TYPE value;

    initGetVal();  // init flag for speeding up bias subtraction

    // if we are looking at the whole image, ignore the margin
    int w = x1_ - x0_ + 1, h = y1_ - y0_ + 1;
    int xmargin = 0, ymargin = 0;
    if (w == width_)
	xmargin = int(w * 0.2);
    if (y0_ == 0)
	ymargin = int(h * 0.2);

    int x0 = x0_ + xmargin;
    int y0 = y0_ + ymargin;
    int x1 = x1_ - xmargin;
    int y1 = y1_ - ymargin;

    // the array values are the number of pixels in a given range
    // note: to save time, don't check all the pixels, just a
    // number of sample lines in the visible area of the image.
    if (x1 <= x0 || y1 <= y0) {
	hist.area = 0;
	return;
    }
    hist.area = (x1 - x0) * (y1 - y0);

    for (int i=y0; i<y1; i++) {
	for (int j=x0; j<x1; j++) {
	    // the code below converts an image pixel value to short and then
	    // uses it to index in the histogram array, so that we can increment the
	    // count for that pixel value.
	    value = getVal(rawImage, i*width_+j);
	    if (ISNAN(value) || (haveBlank_ && value == blank_))
		continue;
	    hist.histogram[convertToUshort(value)]++;
	}
    }
}


/*
 * If there is a special value for blank pixels, get it.
 *
 * Note that the blank pixel value is not scaled by bscale, since we
 * compare pixels values with this value before scaling.
 */
void CLASS_NAME::initBlankPixel()
{
    haveBlank_ = (image_.get("BLANK", blank_) == 0);
    if (!haveBlank_)
	haveBlank_ = (image_.get("BADPIXEL", blank_) == 0);
}

#undef ISNAN
