// -*-c++-*-
#ifndef _ImageIO_h_
#define _ImageIO_h_
/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id: ImageIO.h,v 1.8 1998/05/18 21:22:15 abrighto Exp $" 
 *
 * ImageIO.h - declarations for class ImageIO, an abstract base class 
 *             representing the contents of an image file (or other
 *             image source) and managing the reading and writing of 
 *             image files. The image data is (optionally) kept in shared
 *             memory so that it can be easily accessed by external
 *             applications.
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 *
 *                 12/03/98  Remove dependency on FitsIO (delegated to
 *                           class FitsIO or other class derived from
 *                           ImageIORep.
 *                           Added WCS class, for optional World Coords
 *                           support.
 * Peter W. Draper 04/02/00  Changed constness of write so that
 *                           non-const member can be used within this
 *                           member. 
 */


#include <iostream.h>
#include <math.h>
#include "WCS.h"
#include "Mem.h"


// types of image data (these mostly correspond to the FITS BITPIX values)
enum ImageDataType {
    UNKNOWN_IMAGE  = -1,	// unknown type
    BYTE_IMAGE  = 8,		// 8 bit images                
    X_IMAGE  = -8,		// special, already color scaled, X image data               
    SHORT_IMAGE = 16,		// 16 bit signed                
    USHORT_IMAGE = -16,		// 16 bit unsigned              
    LONG_IMAGE = 32,		// 32 bit integer               
    FLOAT_IMAGE = -32		// 32 bit floating point        
};


/* 
 * This class is used internally for reference counting and subclassing.
 * The public interface is through the ImageIO class.
 */
class ImageIORep {
friend class ImageIO;
protected:
    int width_, height_;	// image dimensions
    int bitpix_;		// image data type 
    double bzero_, bscale_;	// image value = bzero + bscale * val

    Mem header_, data_;		// header and data are kept in shared memory,
				// Format of header is FITS.

    WCS wcs_;			// object used to manage World Coordinate conversion
                                // (You can initialize wcs by calling the virtual method "wcsinit()".)

    int refcnt_;		// reference count
    int status_;		// status after constructor

     // constructor (derived classes call this)
    ImageIORep(int width, int height, int bitpix, 
	       double bzero, double bscale, 
	       const Mem& header, const Mem& data)
	: width_(width), height_(height), bitpix_(bitpix), 
	  bzero_(bzero), bscale_(bscale), 
	  header_(header), data_(data),
	  refcnt_(1), status_(0) {}

public:
    // destructor
    virtual ~ImageIORep() {}

    // the following methods must be defined in a derived class

    // initialize world coordinates (based on the image header)
    virtual int wcsinit() = 0;

    // return true if this class uses native byte ordering
    // (FITS uses network byte order, so FitsIO would return 0 here).
    virtual int nativeByteOrder() const = 0;

    // If byte swapping is needed for this machine and image, make a byte
    // swapped copy of the image data, otherwise, do nothing.
    int byteSwapData();
 
    // return the class name as a string
    virtual const char* classname() const = 0;

    // get the value for the given image header keyword and return 0 if found
    virtual int get(const char* keyword, double& val) const = 0;
    virtual int get(const char* keyword, float& val) const = 0;
    virtual int get(const char* keyword, int& val) const = 0;
    virtual int get(const char* keyword, long& val) const = 0;
    virtual int get(const char* keyword, unsigned char& val) const = 0;
    virtual int get(const char* keyword, unsigned short& val) const = 0;
    virtual int get(const char* keyword, short& val) const = 0;

    // find and return the value for the given FITS keyword, or NULL if not found
    virtual char* get(const char* keyword) const = 0;

    //  write a (ASCII formatted) copy of the FITS header to the given stream.
    virtual int getFitsHeader(ostream& os) const = 0;

    // write the data to an image file 
    virtual int write(const char *filename) = 0;

    // apply bzero and bscale to the value
    double scaleValue(double d) const {return bzero_+d*bscale_;}
    
    // reverse the effect of bzero and bscale
    double unScaleValue(double d) const {return (d-bzero_)/bscale_;}

    // return the size in bytes of a raw image pixel 
    int pixelSize() const {return abs(bitpix_)/8;}

    // member access
    int width() const {return width_;}
    int height() const {return height_;}
    int bitpix() const {return bitpix_;}
    double bscale() const {return bscale_;}
    double bzero() const {return bzero_;}
    Mem& header() {return header_;}
    Mem& data() {return data_;}
    int status() const {return status_;}
    void status(int s) {status_ = s;}

    // return the object used to manage world coordinates
    WCS& wcs() const {return (WCS&)wcs_;}

    // set the object used to manage world coordinates
    void wcs(const WCS& newwcs) {wcs_ = newwcs;}

    // replace header
    int header(const Mem&);
    
    // replace data with data of same size
    int data(const Mem&);

    // Return true if no image is loaded (a 2x2 pixel or smaller
    // image is considered blank).
    virtual int isclear() const {return width_ <= 2 && height_ <= 2;}
};


/* 
 * This class defines the public interface for image I/O. It uses reference
 * counting with the above class to make it easier to implement different
 * views of the same image data.
 */
class ImageIO {
private:
    ImageIORep* rep_;		// internal representation for reference counting

public:
    // constructor, to create a null object (use assignment operator to set later)
    ImageIO() : rep_((ImageIORep*)NULL) {}

    // constructor, from a pointer to a subclass of ImageIORep (FitsIO, etc...).
    // rep should be allocated with new and will be deleted by this class when
    // there are no more references to it.
    ImageIO(ImageIORep* rep) : rep_(rep) {}

    // copy constructor
    ImageIO(const ImageIO&);

    // destructor
    ~ImageIO();

    // assignment
    ImageIO& operator=(const ImageIO&);

    // return true if the ImageIORep subclass uses native byte ordering
    int nativeByteOrder() const {
	return rep_->nativeByteOrder();
    }

    // write the data to an image file 
    int write(const char *filename) const {
	return rep_->write(filename);
    }

    // initialize world coordinates (based on the image header)
    int wcsinit() {
	return rep_->wcsinit();
    }

    // get the value for the given image header keyword and return 0 if found
    int get(const char* keyword, double& val) const {
	return rep_->get(keyword, val);
    }
    int get(const char* keyword, float& val) const {
	return rep_->get(keyword, val);
    }
    int get(const char* keyword, int& val) const {
	return rep_->get(keyword, val);
    }
    int get(const char* keyword, long& val) const {
	return rep_->get(keyword, val);
    }
    int get(const char* keyword, unsigned char& val) const {
	return rep_->get(keyword, val);
    }
    int get(const char* keyword, unsigned short& val) const {
	return rep_->get(keyword, val);
    }
    int get(const char* keyword, short& val) const {
	return rep_->get(keyword, val);
    }

    // find and return the value for the given FITS keyword, or NULL if not found
    char* get(const char* keyword) const {
	return rep_->get(keyword);
    }

    //  write a (ASCII formatted) copy of the FITS header to the given stream.
    int getFitsHeader(ostream& os) const {
	return rep_->getFitsHeader(os);
    }

    // apply bzero and bscale to the value
    double scaleValue(double d) const {return rep_->scaleValue(d);}
    
    // reverse the effect of bzero and bscale
    double unScaleValue(double d) const {return rep_->unScaleValue(d);}

    // return the size in bytes of a raw image pixel 
    int pixelSize() const {return rep_->pixelSize();}

    // member access
    int width() const {return rep_->width();}
    int height() const {return rep_->height();}
    int bitpix() const {return rep_->bitpix();}
    double bscale() const {return rep_->bscale();}
    double bzero() const {return rep_->bzero();}
    const Mem& header() const {return rep_->header();}
    const Mem& data() const {return rep_->data();}

    // return a reference to the object used to manage world coordinates
    WCS& wcs() const {return rep_->wcs();}

    // set the object used to manage world coordinates
    void wcs(const WCS& newwcs) {rep_->wcs(newwcs);}

    // replace header
    int header(const Mem& m) {return rep_->header(m);}
    
    // replace data with data of same size
    int data(const Mem& m) {return rep_->data(m);}

    // short cuts
    const char* headerPtr() const {return (const char*)rep_->header().ptr();}
    const void* dataPtr() const {return rep_->data().ptr();}

    // note: if status is non-zero, the other methods are undefined
    int status() const {return rep_ ? rep_->status() : 1;}

    // Return true if no image is loaded.
    int isclear() const {return rep_->isclear();}

    // return a pointer to the internal class
    ImageIORep* rep() const {return rep_;}
};

#endif /* _ImageIO_h_ */

