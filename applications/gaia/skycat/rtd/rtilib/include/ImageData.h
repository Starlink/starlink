// -*-c++-*-
#ifndef _ImageData_h_
#define _ImageData_h_

/*
 * E.S.O. - VLT project 
 * "@(#) $Id: ImageData.h,v 1.24 1999/03/22 21:41:53 abrighto Exp $"
 *
 * Image.h - class definitions for drawing images in Tk/Tcl
 *
 * See the man page RTI(3) for a complete description of this class
 * library.
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 *
 * T. Herlin       06/12/95  Added set_object for camera objects
 *
 * Allan Brighton 12/03/98   Removed ImageData::read, use makeImage instead,
 *                           since it allows different image types through
 *                           subclassing.
 *                           Moved WSC object (wcs_) to class ImageIO (does not
 *                           change the public interface). This makes it easier 
 *                           to derive new image types or replace the WCS
 *                           implementation in a derived class of ImageIORep.
 * P.W. Draper     03/03/98  Added changes for display depths > 8 bits.
 *                 14/07/98  Added LOOKUP_BLANK to use last bin for
 *                           blank pixel (otherwise comes out at
 *                           scaled colour). 
 *                 12/07/99  Added haveBlank() and getBlank() members
 *                           to allow return of blank_ value as a double.
 */


#include <sys/types.h>
#include "WCS.hxx"
#include "LookupTable.h"
#include "ImageIO.h"
#include "ImageDisplay.h"

typedef unsigned char byte;	// type of XImage data (no longer ...)
struct ImageDataParams;		// forward ref
struct ImageDataHistogram;      // forward ref

// class for managing image data

class ImageData {
public:

    // types of color scaling
    enum ImageColorScaleType {
	LINEAR_SCALE,		// linear scale
	LOG_SCALE,		// logarithmic or exponential scale
	SQRT_SCALE,		// square root scale
	HISTEQ_SCALE		// histogram equalization
    };

    // values for color lookup table (from saoimage)
    enum {
	LOOKUP_SIZE = 65536,	// default size of buffer (full range of short 0x10000) 
	LOOKUP_WIDTH = 65534,	// range of image values allowed in histogram
	LOOKUP_MIN = -32767,	// minimum image value allowed
	LOOKUP_MAX = 32767,	// maximum image value allowed
	LOOKUP_OFF = 32768,	// offset from allocated array to zero (0x8000)
        LOOKUP_BLANK = 32768    // end bin for blank pixel
    };
    
protected:
    // arbitrary name of this image, used to identify the image in messages
    char name_[32];

    // pointers to the caller's XImage and data, which this class writes to
    ImageDisplay* xImage_;
    byte* xImageData_;

    // this represents the contents of the image file or other source
    // (uses reference counting so we can share this with other views)
    ImageIO image_;

    // dimensions of image in pixels
    int width_, height_;

    // These correspond to the FITS image keywords with the same name.
    // The image value is calculated as: bzero + bscale*filevalue,
    double bzero_, bscale_;

    // value in "OBJECT" header field: name of astronomical object
    char object_[80];

    // saved x, y values from last call to updateOffset(x, y)
    double prevX_, prevY_;
    
    // saved bounding box in raw image array of last image update
    // (used later for calculating  cut levels)
    int x0_, y0_, x1_, y1_;
    
    // XImage info
    int xImageBytesPerLine_;
    int xImageSize_;		// size in bytes = width*height*bytesPerPixel
    int xImageBytesPerPixel_;	// Number of bytes per pixel in xImage (1 for 8
				// bit displays).

    // max x image x,y coords scaled to image coords
    int xImageMaxX_, xImageMaxY_;

    // lookup table mapping unsigned short to byte
    LookupTable lookup_;
    
    // type of color scaling to do
    ImageColorScaleType colorScaleType_; 

    // these can be static for now, since all images share a static colormap
    static int ncolors_;	   // number of available colors
    static unsigned long* colors_; // array of color values
    static unsigned long color0_;  // reserved color for black pixels
    static unsigned long colorn_;  // reserved color for saturated pixels

    int clear_;			   // flag: if true, clear out the image once

    // minimum and maximum pixel values
    double minValue_;
    double maxValue_;

    // color cut values and blank pixel value scaled to short (or ushort)
    // range. Used in color scaling algorithms to generate lookup table
    int scaledHighCut_;
    int scaledLowCut_;
    int scaledBlankPixelValue_;
    
    int haveBlank_;		   // flag: true if the BLANK keyword was found
   
    // color cut values
    double highCut_;
    double lowCut_;
    
    // min image value scaled to short or ushort range
    // (used in color scaling algorithms to generate lookup table)
    int scaledMinValue_;
    
    // optional exponent for LOGARITHMIC and SQRT color scale (def: 10.0)
    double expo_;

    // X,Y amount image should be scaled
    int xScale_, yScale_;

    // flag: true if x and y coords should be swapped (rotate 90 deg.)
    int rotate_;
    
    // flag: true if image should be flipped in the X or Y direction
    int flipX_, flipY_;
   
    // values from ESO/VLT FITS keywords used to calculate detector coordinates.
    int startX_, startY_;
    int binX_, binY_;
   
    // display width and height (after scaling)
    int dispWidth_, dispHeight_;

    // area of image = width_ * height_ 
    int area_;

    // flag: true when the image needs updating 
    int update_pending_;

    // flag: if true, do a quick dirty job when shrinking image
    int subsample_;
    
    // flag: if true, print diagnostic messages
    int verbose_;


protected:
    // check args and call virtual methods to copy raw data to xImage
    virtual void toXImage(int x0, int y0, int x1, int y1, int dest_x, int dest_y);

    // copy raw data to xImage, pos. with transformations (defined in derived class)
    virtual void rawToXImage(int x0, int y0, int x1, int y1, 
			     int dest_x, int dest_y) = 0;

    virtual void shrink(int x0, int y0, int x1, int y1, 
			int dest_x, int dest_y) = 0;

    virtual void grow(int x0, int y0, int x1, int y1, 
		      int dest_x, int dest_y) = 0;

    // initialize conversion from base type to short,
    // used by color scaling algorithms as index in lookup table 
    // (defined in a derived class, not needed for byte images)
    virtual void initShortConversion() = 0;

    // If there is a special value for blank pixels, get it and set the
    // values of haveBlankPixel_ and scaledBlankPixelValue_.
    virtual void initBlankPixel() = 0;

    // scan the image for the min and max values
    virtual void getMinMax() = 0;

    // flip x,y coords about default or specified image width,height
    void flip(double& x, double& y, int width = 0, int height = 0);
    void flip(int& x0, int& y0, int& x1, int& y1);

    // apply bzero and bscale to the value
    double scaleValue(double d) {return image_.scaleValue(d);}
    
    // reverse the effect of bzero and bscale
    double unScaleValue(double d) {return image_.unScaleValue(d);}
    
    // get pixel value distribution info (internal version)
    virtual void getPixDist(int numValues, double* xyvalues, double factor) = 0;

    // Fill the given histogram with the distribution of pixels in the
    // visible image area 
    virtual void getHistogram(ImageDataHistogram&) = 0;

    // set default cut levels to min/max pixel values
    void setDefaultCutLevels();

    // initialize a new image, set default cut levels, etc.
    ImageData* initImage();

    // constructor (only called by derived clases)
    ImageData(const char* name, const ImageIO&, int verbose, 
	      int lookup_size = LOOKUP_SIZE);

    // copy constructor (called by copy() member function)
    ImageData(const ImageData&);

public:

    // destructor
    virtual ~ImageData() {}

    // save image to a file
    int write(const char* filename);
 
    // Save the contents of the given region of this image to the given file.
    // The coordinates are expected in image pixel units.
    int write(const char* filename, double x0, double y0, double x1, double y1);

    // return a pointer to a derived class of this class specialized in the given 
    // type of image, as given by the ImageIO reference.
    // Note: ImageIO is a reference counted class. New image types can be added
    // by subclassing the internal class ImageIORep.
    static ImageData* makeImage(const char* name, const ImageIO&, int verbose = 0);

    // reinitialize the image after a change (such as moving to a new HDU)
    int reinit();

    // apply/reverse transformations on coordinates (scale, rotate, flip, etc...)
    void doTrans(double& x, double& y, int distFlag = 0, 
		 double xOffset = 0.0, double yOffset = 0.0, 
		 int width = 0, int height = 0);
    void undoTrans(double& x, double& y, int distFlag = 0, 
		   double xOffset = 0.0, double yOffset = 0.0, 
		   int width = 0, int height = 0);
    
    // convert x,y coords to a distance by flipping where needed
    void coordsToDist(double& x, double& y, int width = 0, int height = 0);

    // convert distance to coords (reverse of above)
    void distToCoords(double& x, double& y, int width = 0, int height = 0);

    // Convert the given image coordinates to detector chip/CCD coordinates.
    void imageToChipCoords(double& x, double& y);

    // Convert the given detector chip/CCD coordinates to image coordinates.
    void chipToImageCoords(double& x, double& y);

    // single arg versions of the above methods
    void imageToChipCoords(double& x);
    void chipToImageCoords(double& x);

    // get pixel index from image coords
    int getIndex(double x, double y, int& ix, int& iy);

    // set the destination X image buffer, dimensions and raw image offset
    void setXImage(ImageDisplay* xImage);

    // set the scaling factor
    void setScale(int xScale, int yScale);
    
    // set the scaling factor so that the image will fit in the given box
    void shrinkToFit(int width, int height);

    // update the entire image from the raw image if necessary
    void update();

    // update image from raw data starting at the given x,y offset
    void updateOffset(double x, double y);

    // get array with information about the pixel value distribution
    void getDist(int& numValues, double* xyvalues);

    // scan the image along the given line and generate an array
    // with pixel value information
    int getSpectrum(double* xyvalues, int x0, int y0, int x1, int y1);

    // manually set the cut levels (if scaled is true, min and max are "bscaled")
    virtual void setCutLevels(double min, double max, int scaled);

    // automatically set the cut levels to leave percent visible
    virtual void autoSetCutLevels(double percent = 98.0);

    // automatically set the cut levels using median filtering
    virtual void medianFilter() {}

    // set the colors to use for the image
    virtual void colorScale(int ncolors, unsigned long* colors);

    // return the type of the raw data
    virtual int dataType() = 0;

    // return true if the data type is signed
    virtual int isSigned() = 0;

    // return a copy of the image
    virtual ImageData* copy() = 0;

    // save/restore transformation parameters
    void saveParams(ImageDataParams&);
    void restoreParams(ImageDataParams&, int restoreCutLevels = 1);

    // print x,y coords and raw data value at x,y coords to buffer
    virtual char* getValue(char* buf, double x, double y) = 0;

    // print the values at the given x,y coords to the buffers for display
    virtual void getValues(double x, double y, double rx, double ry, 
			   char* xStr, char* yStr, char* valueStr,
			   char* raStr, char* decStr, char* equinoxStr) = 0;

    // get array of image pixel values and x,y coords around a point
    virtual void getValues(double x, double y, double rx, double ry, double* ar, int nrows, int ncols) = 0;

    // get array of image pixel values at a given offset with given dimensions
    virtual void getValues(double x, double y, int w, int h, float* ar) = 0;

    // return the value at the x,y coords as a double
    virtual double getValue(double x, double y) = 0;

    // Copy raw image data from this image to the given image data area,
    // starting at the image coordinates (x, y) and with the dimentions (w, h) 
    // in pixels.  Since this is a copy from one raw image to another, no
    // data conversion is done.
    virtual void copyImageArea(void* data, double x, double y, int w, int h) = 0;

    // get statistics on specified area of image
    virtual int getStatistics(double x, double y, int w, int h, 
			      double& meanX, double& meanY,
			      double& fwhmX, double& fwhmY,
			      double& symetryAngle, 
			      double& objectPeak, double& meanBackground);

    // member access

    // get the image data or header
    Mem& data() {return (Mem&)image_.data();}
    Mem& header() {return (Mem&)image_.header();}

    // write a (ASCII formatted) copy of the FITS header to the given stream
    int getFitsHeader(ostream& os) {return image_.getFitsHeader(os); }

    // set new data or header
    void data(const Mem& data);
    void header(const Mem& header);
    
    // access the world coordinate info object for the image
    WCS& wcs() {return image_.wcs();}

    ImageDisplay* xImage() {return xImage_;}
    const ImageIO& image() {return image_;}
    void colorScaleType(ImageColorScaleType t) {colorScaleType_ = t;}
    ImageColorScaleType colorScaleType() {return colorScaleType_;}
    int ncolors() {return ncolors_;}
    unsigned long* colors() {return colors_;}
    unsigned long color0() {return color0_;}
    unsigned long colorn() {return colorn_;}
    void setColors(int ncolors, unsigned long* colors);
    void expo(double e) {expo_ = e;}
    double expo() {return expo_;}
    int width() {return width_;}
    int height() {return height_;}
    int dispWidth() {return dispWidth_;}
    int dispHeight() {return dispHeight_;}
    int xScale() {return xScale_;}
    int yScale() {return yScale_;}
    int flipX() {return flipX_;}
    void flipX(int b) {flipX_ = (b != 0); update_pending_++;}
    int flipY() {return flipY_;}
    void flipY(int b) {flipY_ = (b != 0); update_pending_++;}
    int rotate() {return rotate_;}
    void rotate(int);
    int startX() {return startX_;}
    int startY() {return startY_;}
    void startX(int x) {startX_ = x;}
    void startY(int y) {startY_ = y;}
    int binX() {return binX_;}
    int binY() {return binY_;}
    void binX(int x) {binX_ = x;}
    void binY(int y) {binY_ = y;}
    double highCut() {return scaleValue(highCut_);}
    double lowCut() {return scaleValue(lowCut_);}
    double minValue() {return scaleValue(minValue_);}
    double maxValue() {return scaleValue(maxValue_);}
    void subsample(int b) {subsample_ = b;}
    void verbose(int b) {verbose_ = b;}
    void name(const char* name) {strncpy(name_, name, sizeof(name_)-1);}
    char* name() {return name_;}
    void object(const char *object) {strncpy(object_, object, sizeof(object_)-1);}
    char* object() {return object_;}
    int update_pending() {return update_pending_;}
    void update_pending(int b) {update_pending_ = b;}
    LookupTable lookup() {return lookup_;}
    int lookup(LookupTable);
    void clear() {clear_ = 1; update_pending_++;}

    // return the blank value as a double
    virtual double getBlank() = 0;
    virtual int haveBlank() = 0;
};

// struct used to save transformation parameters of image for use in new image
struct ImageDataParams {
    int status;			// only restore if status is 0
    int flipX, flipY;		// true if image is flipped
    int rotate;			// true if x,y axis swapped
    int xScale, yScale;		// scale factors

    int dataType;		// data type of image data
    double lowCut, highCut;	// cut levels

    ImageData::ImageColorScaleType colorScale;  // color scale type

    // constructor
    ImageDataParams() : status(1) {}
};


// struct used to generate a histogram of the image data, where the pixels
// are first converted to shorts. 
struct ImageDataHistogram {
    int histogram[ImageData::LOOKUP_SIZE]; // count of pixels having this value 
                                // after conversion to short
    int area;			// area of image used to create histogram, 

    ImageDataHistogram() {
	memset(histogram, '\0', sizeof(histogram));
    }
    
    int size() {return sizeof(histogram);}
};


#endif _ImageData_h_
