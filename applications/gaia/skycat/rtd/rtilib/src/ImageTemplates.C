/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: ImageTemplates.C,v 1.26 1999/03/22 21:42:02 abrighto Exp $" 
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
 *                 17/08/00  Changed getMinMax slightly. This now
 *                           checks if the total height of the image
 *                           is shown, rather than just testing
 *                           y0_. This could be wrong for zoomed
 *                           images that do not fill the display area
 *                           making the region used overrun. 
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

#include <iostream.h>
#include <strstream.h>
#include <assert.h>
#include "define.h"
#include "LongImageData.h"   // For definition of FITS_LONG

#if !defined(NTOH) && \
    (defined(i386) || defined(__i386__) || defined(__alpha) || \
     defined(vax) || defined(__vax__) || (defined(mips) && defined(MIPSEL)))
  /*
   * If DATA_TYPE might require byte swapping, and if we are on a
   * PC or DEC machine, then declare NTOH() as an inline function.
   */
#include <netinet/in.h>

/*
 * NTOH() - convert a basic type of any size from network-to-host byte order
 *        This code is never compiled on Suns and HP's but on DECs and PCs,
 *        you should probably use an optimizing compiler like gcc -O for this.
 *
 * The following function looks "big", but it really isn't so bad since the
 * compiler evaluates sizeof(DATA_TYPE) at _compile-time_ and throws away
 * everything except the applicable segment of code.  I tested this to be
 * true with gcc, which is currently the only compiler that's ever going
 * to see this code unless someone compiles on an alpha with cc.
 *
 */
inline DATA_TYPE NTOH(DATA_TYPE x)
{
    if (sizeof(DATA_TYPE)==1) {
        /*
         * Yes, if no one ever bothers to define DATA_BYTE_SIZED, then this
	 * will just happen instead.  I just don't trust the compiler to
	 * optimize this away 100% percent though, so that's why it's better
	 * to define DATA_BYTE_SIZED when you know this is going to happen,
	 * to avoid confusing the compiler at all.  (We're already putting
	 * a lot of faith in the compiler with this function to begin with!)
	 */
        return (x);
    }
    else if (sizeof(DATA_TYPE)==2) {
        /*
	 * If ntohs turns out to be slow on some compiler, try something like
	 *
         *   unsigned short ret;
         *   ret  = (x & 0xff00) >> 8;
         *   ret |= (unsigned short) (x & 0x00ff) << 8;
	 *   return ret;
	 */
        return (DATA_TYPE)ntohs((unsigned short)x);
    }
    else if (sizeof(DATA_TYPE)==4) {
        /*
         * Prevent auto conversions in case of floats
	 * If "DATA_TYPE" happens to be "unsigned long" or even "signed long",
	 * this union is really useless, but again, all of this gets reduced
	 * away at compile-time.  I tested it with "gcc -O". No speed hit here.
         */
        union { unsigned FITS_LONG raw; DATA_TYPE typed; } ret;

        ret.typed = x;
        ret.raw = ntohl(ret.raw);
        return ret.typed;
    }
    else if (sizeof(DATA_TYPE)==8) {
        /*
	 * Handle doubles, or any arbitrary type that is 8 bytes.
	 * Uses same trick to avoid automatic type conversions.
	 */
        union { unsigned long raw[2]; DATA_TYPE typed; } ret;
        unsigned long tmp;

        ret.typed = x;
        tmp = ret.raw[0];
        ret.raw[0] = ntohl(ret.raw[1]);
        ret.raw[1] = ntohl(tmp);
        return ret.typed;
    }
    else // if you ever get an image full of zeros... add more sizes here!
        return 0;
}
#else
#ifndef NTOH
#define NTOH(x) (x)
#endif
#endif

// This is a no-op for non-floating point data types. This macro is
// defined as isnan(x) for floating point types.
#ifndef ISNAN
#define ISNAN(x) 0
#endif

/*
 * scan the image to find the min and max values
 * and set the member variables accordingly.
 * To save time, only the visible area of the image is examined
 * (x0_, y0_, x1_ and y1_ are set each time the image is updated, however
 * they are initially set to the entire image).
 */
void CLASS_NAME::getMinMax()
{
    DATA_TYPE* rawImage = (DATA_TYPE*)image_.dataPtr();
    DATA_TYPE* p;
    DATA_TYPE  value;
    
    // use area of image that is visible.
    // if we are looking at the whole image, ignore the margin
    int w = x1_ - x0_ + 1, h = y1_ - y0_ + 1;
    int xmargin = 0, ymargin = 0;
    if (w == width_)
	xmargin = int(w * 0.02);
    if (h == height_)
	ymargin = int(h * 0.02);  // PWD: change here, test was y0_ == 0

    int x0 = x0_ + xmargin;
    int y0 = y0_ + ymargin;
    int x1 = x1_ - xmargin;
    int y1 = y1_ - ymargin;
    w = x1 - x0 + 1;
    h = y1 - y0 + 1;

    if (w <= 1 || h <= 1) {
	if (area_ > 0)
	    minValue_ = maxValue_ = NTOH(rawImage[0]);
	else 
	    minValue_ = maxValue_ = 0;
	return;
    }

    // set the x, y increments so that not every pixel or line is
    // examined on large images
    int xincr = w/2048;
    if (xincr == 0)
	xincr++;
    int yincr = h/256;
    if (yincr == 0)
	yincr++;


    // try to speed things up a bit on large images:
    // don't examine every pixel, just look at a few sample lines
    p = rawImage + y0*width_+x0; // first pixel in region to examine
    value = NTOH(*p);

    // ignore BLANK pixels. For efficiency, we only check the flag once
    DATA_TYPE* end = rawImage+area_;
    if (haveBlank_) {
	// make sure starting min/max values are not the BLANK pixel
	while(value == blank_ || ISNAN(value)) {
	    p += 10;              // check another pixel
	    if (p >= end)
		break;
	    value = NTOH(*p);
	}
	if (ISNAN(value))
	    value = 0;
	minValue_ = maxValue_ = value;
	for (int y = y0; y <= y1; y+=yincr) {
	    p = rawImage + y*width_+x0; 
	    for (int x = x0; x <= x1; x+=xincr, p+=xincr) {
		if ((value = NTOH(*p)) == blank_ || ISNAN(value))
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
	    value = NTOH(*p);
	}
	if (ISNAN(value))
	    value = 0;
	minValue_ = maxValue_ = value;
	for (int y = y0; y <= y1; y+=yincr) {
	    p = rawImage + y*width_+x0; 
	    for (int x = x0; x <= x1; x+=xincr, p+=xincr) {
		register DATA_TYPE value = NTOH(*p);
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

    int ix, iy;
    if (getIndex(x, y, ix, iy) != 0)
	sprintf(buf, "%.1f %.1f -", x, y);
    else {
	DATA_TYPE value = NTOH(rawImage[iy*width_+ix]);
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
    if (getIndex(x, y, ix, iy) != 0) 
	return 0.0;
    return scaleValue(NTOH(rawImage[iy*width_+ix]));
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
    // display chip coords for x and y
    double cx = x, cy = y;
    imageToChipCoords(cx, cy);
    sprintf(xStr, "%.1f", cx);
    sprintf(yStr, "%.1f", cy);

    *valueStr = '\0';
    *raStr = '\0';
    *decStr = '\0';
    *equinoxStr = '\0';

    // get integer index in raw image for pixel value
    int ix, iy;
    if (getIndex(rx, ry, ix, iy) == 0) {
	DATA_TYPE* rawImage = (DATA_TYPE*)image_.dataPtr();
	DATA_TYPE value = NTOH(rawImage[iy*width_+ix]);

	if (haveBlank_ && value == blank_) 
	    strcpy(valueStr, "blank");
	else
	    sprintf(valueStr, "%g", scaleValue(value));

	if (wcs().isWcs()) {
	    char buf[80];
	    wcs().pix2wcs(x, y, buf, sizeof(buf), 1);  
	    sscanf(buf, "%s %s %s", raStr, decStr, equinoxStr);
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
void CLASS_NAME::getValues(double x, double y, double rx, double ry, double* ar, 
			   int nrows, int ncols)
{
    DATA_TYPE* rawImage = (DATA_TYPE*)image_.dataPtr();

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
		DATA_TYPE value = NTOH(rawImage[iy*width_+ix]);
		if (haveBlank_ && value == blank_)
		    ar[(j+1)*w+i+1] = -HUGE_VAL;
		else
		    ar[(j+1)*w+i+1] = scaleValue(value);
	    }
	    else {
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
 * is no blank pixel value defined. 
 *
 * Note: x and y are expected in image coordinates
 */
void CLASS_NAME::getValues(double x, double y, int w, int h, float* ar)
{
    DATA_TYPE* rawImage = (DATA_TYPE*)image_.dataPtr();

    int i, j;
    int ix, iy;

    getIndex(x, y, ix, iy);
    
    for (j = 0; j < h; j++) {
	for (i = 0; i < w; i++) {
	    int rx = ix+i, ry = iy+j;
	    if (rx >= 0 && ry >= 0 && rx < width_ && ry < height_) {
		DATA_TYPE value = NTOH(rawImage[ry*width_+rx]);
		if (haveBlank_ && value == blank_) {
		    // ar[j*w+i] = 0;
		    ar[j*w+i] = blank_;
		}
		else {
		    ar[j*w+i] = scaleValue(value);
		}
	    }
	    else {
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
 * the raw image that needs to be copied.
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

    // source/dest images
    register DATA_TYPE* src = (DATA_TYPE*)image_.dataPtr();

    // width of image area to update
    int w = x1 - x0 + 1;

    // set loop increments for based on current transformations
    switch (flipX_<<1|flipY_) {
    case 0: // none
	src +=  (height_ - 1 - y0) * width_ + x0;
	src_x_inc = 1;
	src_y_inc = -width_ - w;
	break;
    case 1: // flipY
	src +=  y0 * width_ + x0;
	src_x_inc = 1;
	src_y_inc = width_ - w;
	break;
    case 2: // flipX
	src +=  (height_ - 1 - y0) * width_ + (width_ - 1 - x0);
	src_x_inc = -1;
	src_y_inc = -(width_ - w);
	break;
    case 3: // flipX and flipY
	src +=  y0 * width_ + (width_ - 1 - x0);
	src_x_inc = -1;
	src_y_inc = width_ + w;
	break;
    }

    // need to take care with non-byte depths, so branch according to
    // this
    if ( xImageBytesPerPixel_ == 1 ) {

	register byte* dest = xImageData_;
	register int dest_x_inc, dest_y_inc;

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
          *dest = lookup(NTOH(*src));
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
      int k = dest_x;
      int l = dest_y;
      for (i=y0; i<=y1; i++) {
        for (j=x0; j<=x1; j++) {
          if ( rotate_ ) {
            xImage_->putpixel( l, k, llookup(NTOH(*src)));
          } else {
            xImage_->putpixel( k, l, llookup(NTOH(*src)));
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
 * This method is called to magnify the image by factors >= 2
 * The arguments x0, y0, x1 and y1 are the bounding box of the region
 * that needs to be copied.
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

    // source/dest images
    register DATA_TYPE* src = (DATA_TYPE*)image_.dataPtr();

    // width of image area to update
    int w = x1 - x0 + 1;

    // set loop increments based on current transformations
    switch (flipX_<<1|flipY_) {
    case 0: // none
	src +=  (height_ - 1 - y0) * width_ + x0;
	src_x_inc = 1;
	src_y_inc = -width_ - w;
	break;
    case 1: // flipY
	src +=  y0 * width_ + x0;
	src_x_inc = 1;
	src_y_inc = width_ - w;
	break;
    case 2: // flipX
	src +=  (height_ - 1 - y0) * width_ + (width_ - 1 - x0);
	src_x_inc = -1;
	src_y_inc = -(width_ - w);
	break;
    case 3: // flipX and flipY
	src +=  y0 * width_ + (width_ - 1 - x0);
	src_x_inc = -1;
	src_y_inc = width_ + w;
	break;
    }

    // need to take care with non-byte depths, so branch according to
    // this
    if ( xImageBytesPerPixel_ == 1 ) {

	register byte* dest = xImageData_;
	register byte* end = xImageData_ + xImageSize_;
	register int dest_x_inc, dest_y_inc;

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
		c = lookup(NTOH(*src)); 
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
    } 
    else {

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
		cl = llookup(NTOH(*src));
		// replicate the source pixel to an xs x ys box
		for ( n = l; n < min(l+ys,height); n++ ) {
		    for ( m = k; m < min(k+xs,width); m++ ) {
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
void CLASS_NAME::shrink(int x0, int y0, int x1, int y1, int dest_x, int dest_y)
{
    register DATA_TYPE *p, *q;
    register int i, j, n, m;
    register int xs = -xScale_, ys = -yScale_;

    // for odd shrink factors, we have to be carefull about the end conditions
    x1 -= (x1-x0+1)%xs;
    y1 -= (y1-y0+1)%ys;
    int w = x1-x0+1;

    // row/col increments
    register int src_x_inc, src_y_inc;

    // source/dest images
    register DATA_TYPE* src = (DATA_TYPE*)image_.dataPtr();
    register DATA_TYPE maxval = 0;
   
    // set loop increments based on current transformations
    switch (flipX_<<1|flipY_) {
    case 0: // none
	src +=  (height_ - ys - y0) * width_ + x0; 
	src_x_inc = xs;
	src_y_inc = -width_ * ys - w;
	break;
    case 1: // flipY
	src +=  y0 * width_ + x0;
	src_x_inc = xs;
	src_y_inc = width_ * ys - w;
	break;
    case 2: // flipX
	src +=  (height_ - ys - y0) * width_ + (width_ - xs - x0); 
	src_x_inc = -xs;
	src_y_inc = -(width_ * ys - w);
	break;
    case 3: // flipX and flipY
	src +=  y0 * width_ + (width_ - xs - x0);
	src_x_inc = -xs;
	src_y_inc = width_ * ys + w;
	break;
    }

    if ( xImageBytesPerPixel_ == 1 ) {
	//  use faster methods for byte xImages

	register byte* dest = xImageData_;
	register byte* end = xImageData_ + xImageSize_ - 1;
	register int dest_x_inc, dest_y_inc;

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
		    *dest = lookup(NTOH(*src));
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
			    if (NTOH(*p) > maxval)
				maxval = NTOH(*p);
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
    } 
    else {
	//  XImage depth greater than a byte. Use careful methods to 
	//  keep byte order etc. correct for server.
	int k = dest_x / xs;
	int l = dest_y / ys;
	unsigned long cl;
	if (subsample_) {
	    // use faster "subsample" algorithm
	    for (i=y0; i<=y1; i+=ys) {
		for (j=x0; j<=x1; j+=xs) {
		    if ( rotate_ ) {
			xImage_->putpixel( l, k, llookup(NTOH(*src)));
		    } else {
			xImage_->putpixel( k, l, llookup(NTOH(*src)));
		    }
		    k++; 
		    src += src_x_inc;
		}
		src += src_y_inc;
		k = dest_x / xs;
		l++;
	    }
	}
	else {
	    // don't subsample: take max pixel
	    for (i=y0; i<=y1; i+=ys) {
		for (j=x0; j<=x1; j+=xs) {
		    q = p = src;
		    maxval = NTOH(*p);
		    for (n=0; n<ys; n++) {
			for (m=0; m<xs; m++, p++) {
			    if (NTOH(*p) > maxval)
				maxval = NTOH(*p);
			}
			p = q += width_;
		    }
		    if ( rotate_ ) {
			xImage_->putpixel( l, k, llookup(maxval));
		    } else {
			xImage_->putpixel( k, l, llookup(maxval));
		    }
		    k++; 
		    src += src_x_inc;
		}
		src += src_y_inc;
		k = dest_x / xs;
		l++;
	    }
	}
    }
}


/*
 * Set the cut levels using a median filtering algorithm.
 * To save time, only the visible area of the image is examined
 * (x0_, y0_, x1_ and y1_ are set each time the image is updated).
 */
void CLASS_NAME::medianFilter()
{
    getMinMax();		// get min/max pixel estimate for visible area
    DATA_TYPE *rawImage = (DATA_TYPE*)image_.dataPtr(); // image data
    DATA_TYPE *end = rawImage + area_; // end of image data
    const int nmed = 7;		   // length of median filter
    int xskip = nmed*3, yskip = 3; // skip pixels for speed 
    int x0 = x0_ + 10;		   // ignore outside areas
    int y0 = y0_ + 10;
    int x1 = x1_ - 10;
    int y1 = y1_ - 10;
    register int i, j, k, l;
    register DATA_TYPE* p;
    register DATA_TYPE tmp;
    DATA_TYPE val, lcut, hcut, medary[nmed];
    
    // use this value as a default in place of bad pixels (blank, NAN)
    DATA_TYPE mval = (DATA_TYPE)((minValue_ + maxValue_)/2);

    if (x1-x0 <= nmed || y1-y0 <= nmed)
	return;	

    for (i=y0; i<=y1; i+=yskip) {
	for (j=x0; j<=x1; j+=xskip) {
	    p = rawImage + i*width_ + j;

	    // get array for finding meadian
	    for (k=0; k < nmed; k++) {
		medary[k] = NTOH(*p++);
		// ignore blank pixels and NANs
		if (ISNAN(medary[k]) || (haveBlank_ && medary[k] == blank_)) {
		    medary[k] = mval;  
		}
	    }

	    // get meadian value 
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
	    DATA_TYPE value = NTOH(rawImage[i*width_+j]);
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
void CLASS_NAME::getHistogram(ImageDataHistogram& h)
{
    DATA_TYPE* rawImage = (DATA_TYPE*)image_.dataPtr();

    // the array values are the number of pixels in a given range
    // note: to save time, don't check all the pixels, just a 
    // number of sample lines in the visible area of the image.
    if (x1_ <= x0_ || y1_ <= y0_) {
	h.area = 0;
	return;
    }
    h.area = (x1_ - x0_) * (y1_ - y0_);

    for (int i=y0_; i<y1_; i++) {
	for (int j=x0_; j<x1_; j++) {
	    // the code below converts an image pixel value to short and then
	    // uses it to index in the histogram array, so that we can increment the 
	    // count for that pixel value. 
	    DATA_TYPE value = NTOH(rawImage[i*width_+j]);
	    if (ISNAN(value) || (haveBlank_ && value == blank_))
		continue;
	    h.histogram[convertToUshort(value)]++;
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
