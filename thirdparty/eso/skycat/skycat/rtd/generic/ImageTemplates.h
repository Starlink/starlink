// -*-c++-*-
/*
 * E.S.O. - VLT project 
 * #
 * # "@(#) $Id: ImageTemplates.h,v 1.1.1.1 2006/01/12 16:39:03 abrighto Exp $" 
 *
 * ImageTemplates.h - template class method definitions for classes derived   
 *                    from class ImageData and defined in ImageTemplates.C
 *
 * This file is included in the header files for the different image data
 * types. ImageTemplates.C is included in the body files to define some
 * methods that differ only in the raw image data type.
 *
 * See the man page ImageData(3) and the RTD User's Guide for a complete
 * description of this class hierarchy.
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * P.Biereichel    30/06/97  Changed parameters in getValues() for pixel table
 * Peter W. Draper 12/07/99  Added getBlank();
 */

#define SAMP_METHOD_MAX              0  /* max value of all pixels in a NxN box (default) */
#define SAMP_METHOD_MIN              1  /* min value of all pixels in a NxN box */
#define SAMP_METHOD_MEAN             2  /* mean value of all pixels in a NxN box */
#define SAMP_METHOD_MEDIAN           3  /* median value of all pixels in a NxN box */
#define SAMP_METHOD_MAX_CROSS        4  /* max value of pixels on a diagonal cross in a NxN box */
#define SAMP_METHOD_MIN_CROSS        5  /* min value of pixels on a diagonal cross in a NxN box */
#define SAMP_METHOD_MEAN_CROSS       6  /* mean value of pixels on a diagonal cross in a NxN box */
#define SAMP_METHOD_MEDIAN_CROSS     7  /* median value of pixels on a diagonal cross in a NxN box */
#define SAMP_METHOD_MEDIAN_CHESS     8  /* median value of pixels in a chess-board like box */
#define SAMP_METHOD_MEDIAN_9         9  /* median value of a 3x3 box */
#define SAMP_METHOD_RMS              10 /* RMS value of all pixels in a NxN box */


protected:


// calculate min and max pixel values
void getMinMax();

// print raw data value at x,y coords to buffer
char* getValue(char* buf, double x, double y);

// return the value at the x,y coords as a double
double getValue(double x, double y);

// print the values at the given x,y coords to the buffers for display
void getValues(double x, double y, double rx, double ry, 
	       char* xStr, char* yStr, char* valueStr,
	       char* raStr, char* decStr, char* equinoxStr);

// get array of image pixel values and x,y coords around a point
void getValues(double x, double y, double rx, double ry,
	       double* ar, int nrows, int ncols, int flag = 0);

// get array of image pixel values at a given offset with given dimensions
void getValues(double x, double y, int w, int h, float* ar, int flag = 0);

// Copy raw image data from this image to the given image data area,
// starting at the image coordinates (x, y) and with the dimentions (w, h) 
// in pixels.  Since this is a copy from one raw image to another, no
// data conversion is done.
void copyImageArea(void* data, double x, double y, int w, int h);

// copy raw data to xImage, pos. with transformations (defined in derived class)
void rawToXImage(int x0, int y0, int x1, int y1, int dest_x, int dest_y);
void grow(int x0, int y0, int x1, int y1, int dest_x, int dest_y);
void shrink(int x0, int y0, int x1, int y1, int dest_x, int dest_y);

// automatically set the cut levels using median filtering
void medianFilter();

// get array with information about the pixel value distribution
void getPixDist(int numValues, double* xyvalues, double factor);

// If there is a special value for blank pixels, get it and set the
// values of haveBlankPixel_ and scaledBlankPixelValue_.
void initBlankPixel();

// Fill the given histogram with the distribution of pixels in the
// visible image area 
void getHistogram(ImageDataHistogram&);

// return the blank value.
int haveBlank() {return haveBlank_;}
double getBlank() {return (double) blank_;}
