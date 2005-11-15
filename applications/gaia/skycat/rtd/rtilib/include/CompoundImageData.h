// -*-c++-*-
#ifndef _CompoundImageData_h_
#define _CompoundImageData_h_
/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: CompoundImageData.h,v 1.3 2005/02/02 01:43:03 brighton Exp $" 
 *
 * CompoundImageData.h - class definitions for class CompoundImageData
 *
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  14/02/00  Created
 * Peter W. Draper 15/11/05  Added getBlank() and haveBlank().
 */

#include "ImageData.h"


/*
 * This class is used for images that are divided into multiple HDUs
 * (FITS header/data units, FITS image extensions). The idea here 
 * is to make it appear to the using class as if there is a single image.
 * The image extensions are combined based on the WCS information in
 * each header.
 */
class CompoundImageData : public ImageData {
protected:
    // Number of images in the images_ array
    int numImages_;

    // Array of image extensions to be combined to one image for display
    ImageData** images_;
    
    // bounds of the compound image
    double minX_, minY_, maxX_, maxY_;

    // value of blank pixel, if known (if haveBlankPixel_ is nonzero)
    float blank_;

protected:

    // initialize conversion from base type
    void initShortConversion();

    // check args and call virtual methods to copy raw data to xImage
    virtual void toXImage(int x0, int y0, int x1, int y1, int dest_x, int dest_y);

    // copy raw data to xImage, pos. with transformations (defined in derived class)
    void rawToXImage(int x0, int y0, int x1, int y1, int dest_x, int dest_y);
    void grow(int x0, int y0, int x1, int y1, int dest_x, int dest_y);
    void shrink(int x0, int y0, int x1, int y1, int dest_x, int dest_y);

    // Set x0, y0, x1, y1 to the bounds of the given image data in FITS image coordinates.
    void getBounds(ImageData* imageData, double& x0, double& y0, double& x1, double& y1);

public:

    // constructor
    CompoundImageData(const char* name, const ImageIO& imio, 
		      int* hduList, int numHDUs,
		      biasINFO* biasInfo, int verbose);

    // copy constructor (called by copy() member function)
    CompoundImageData(const CompoundImageData&);

    // destructor
    ~CompoundImageData();

    // return a copy of this object
    ImageData* copy();

    // return the data type of the raw data
    int dataType();

    // return true if the data type is signed
    int isSigned();

    // set the image color lookup table
    int lookupTable(LookupTable);
    
    // set the image colormap
    void setColors(int ncolors, unsigned long* colors);

    // set the destination X image buffer, dimensions and raw image offset
    void setXImage(ImageDisplay* xImage);

    // Save the contents of the given region of this image to the given file.
    // The coordinates are expected in image pixel units.
    int write(const char* filename, double x0, double y0, double x1, double y1);

    // restore saved transformation parameters
    void restoreParams(ImageDataParams&, int restoreCutLevels = 1);

    // set the colors to use for the image
    void colorScale(int ncolors, unsigned long* colors);

    // set the scaling factor
    void setScale(int xScale, int yScale);

    // rotate the image (actually exchange the x/y axes)
    void rotate(int);

    // flip the image X axis if b is non-zero
    void flipX(int b);

    // flip the image Y axis if b is non-zero
    void flipY(int b);

    // manually set the cut levels (if scaled is true, min and max are "bscaled")
    void setCutLevels(double min, double max, int scaled);

    // just pass these on to the parent class and image extensions
    void subsample(int b);
    void sampmethod(int b);
    void verbose(int b);
    void name(const char* name);
    void object(const char *object);

    int getNumImages() {return numImages_;}


    // The methods below access the actual image data and are normally implemented
    // in ImageTemplates.C. Here we need to define the methods to access the
    // correct image extensions, based on the visible area, or requested area
    // of the image.

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

    // update image from raw data starting at the given x,y offset
    void updateOffset(double x, double y);

    // automatically set the cut levels using median filtering
    void medianFilter();

    // automatically set the cut levels to leave percent visible
    void autoSetCutLevels(double percent = 98.0);

    // get array with information about the pixel value distribution
    void getPixDist(int numValues, double* xyvalues, double factor);

    // If there is a special value for blank pixels, get it and set the
    // values of haveBlankPixel_ and scaledBlankPixelValue_.
    void initBlankPixel();

    // Fill the given histogram with the distribution of pixels in the
    // visible image area 
    void getHistogram(ImageDataHistogram&);

    // These need to be redefined to use the first image extension, 
    // since the primary doesn't normally define BZERO and BSCALE..
    double highCut() {return images_[0]->scaleValue(highCut_);}
    double lowCut() {return images_[0]->scaleValue(lowCut_);}
    double minValue() {return images_[0]->scaleValue(minValue_);}
    double maxValue() {return images_[0]->scaleValue(maxValue_);}

    // return the blank value.
    int haveBlank() {return haveBlank_;}
    double getBlank();
};


#endif /* _CompoundImageData_h_ */
