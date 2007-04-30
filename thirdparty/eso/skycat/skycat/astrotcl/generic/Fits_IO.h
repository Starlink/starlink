// -*-c++-*-
#ifndef _FitsIO_h_
#define _FitsIO_h_
/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id$" 
 *
 * Fits_IO.h - declarations for class FitsIO, a class representing the
 *            contents of a FITS image file (or other image source)
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * Peter W. Draper 10/01/00  Added getFitsFile member to allow access
 *                           to fitsio file handle (used to access
 *                           HDUs in derived/related classes). 
 * Peter W. Draper 04/02/00  Changed constness of write so that
 *                           non-const member can be used within this
 *                           member. 
 *                 15/08/00  Made write virtual so it can be overriden.
 * pbiereic        17/02/03  Revised byte-order issues
 * Peter W. Draper 13/06/05  Made setHDU virtual so it can be overriden.
 *                 28/11/05  Made copy virtual so it can be overridden.
 *                 14/12/05  Moved setHDU and copy to the ImageIORep base
 *                           class so that CompoundImageData does not
 *                           need knowledge of this class (so that other
 *                           ImageIORep implementations can be used in
 *                           CompoundImages).
 *                 01/03/07  Added putcard member to write a header card
 *                           without decomposition to kvc.
 * pbiereic        17/02/03  Revised byte-order issues
 * abrighto        02/01/05  Renamed .h file to avoid conflict with cfitsio's 
 *                           "fitsio.h" on case-ignoring file systems, such as 
 *                           Mac OSX.
 */

#include <cstdio>
#include <iostream>
#include "ImageIO.h"
#include "fitsio.h"

/*
 * This class manages reading and writing FITS files and storing the
 * image data. It is derived from ImageIORep rather than class ImageIO
 * for the sake of reference counting.
 */
class FitsIO : public ImageIORep {
private:
   
    // set wcslib header length for searching
    static void set_header_length(const Mem& header);
    void set_header_length() const;

    // make sure there is enough space in the header to insert/update a keyword
    int checkKeywordSpace(const char* keyword);

    // extend the size of the FITS header by one header block 
    int extendHeader();

    static void* reallocFile(void* p, size_t newsize);

protected:   
    //  PWD: Move here so that derived classes can manipulate (needed to get
    //  at HDU functions from ther  
    fitsfile* fitsio_;		// handle to use for cfitsio C library routines
    static FitsIO* fits_;	// current class ptr for reallocFile callback

    Mem primaryHeader_;		// the primary header, if there is more than one HDU

    Mem mergedHeader_;		// the primary header merged with the current extension
                                // header, if applicable (The primary header is appended
                                // after the extension header).
 
    // Check that this object represents a FITS file (and not just some kind of memory)
    // and return 0 if it does. If not, return an error message.
    int checkFitsFile();

    // return 0 if this object represents a FITS file that was mapped for read/write
    int checkWritable();

    // write the keyword/value pair to the given open file descriptor.
    static int put_keyword(FILE* f, const char* keyword, int value);
    static int put_keyword(FILE* f, const char* keyword, double value);
    static int put_keyword(FILE* f, const char* keyword, const char* value);
    static int put_keyword(FILE* f, const char* keyword, char value);
    
    // write keyword/value pair to the given stream.
    static int put_keyword(ostream& os, const char* keyword, int value);
    static int put_keyword(ostream& os, const char* keyword, double value);
    static int put_keyword(ostream& os, const char* keyword, char* value);
    static int put_keyword(ostream& os, const char* keyword, char value);

    // round off file size
    static void padFile(FILE* f, int size);
    
    // Report a cfitsio error
    static int cfitsio_error();

    // return a cfitsio handle, given the Mem object for the FITS header.
    static fitsfile* openFitsMem(Mem& header);

    // flush any memory changes to the file
    int flush();

    // Return an allocated FitsIO object, given the Mem objects for the header and data
    // and the cfitsio handle to use to access the file.
    static FitsIO* initialize(Mem& header, Mem& data, fitsfile* fitsio);

public:
    // constructor: init from given header and data and optional
    // cfitsio handle.
    FitsIO(int width, int height, int bitpix, double bzero, 
	   double bscale, const Mem& header, const Mem& data,
	   fitsfile* fitsio = NULL);

    // destructor
    ~FitsIO();

    // Return a copy of this object that shares the data, but can have a different current HDU
    virtual FitsIO* copy();

    // initialize world coordinates (based on the image header)
    int wcsinit();

    // return the class name as a string
    const char* classname() const {return "FitsIO";}

    // read a FITS file and return a pointer to an allocated FitsIO object
    // NULL if an error occurred
    static FitsIO* read(const char* filename, int memOptions = 0);

    // write the data to a FITS file 
    int write(const char *filename);

    // compress or decompress the given file and return the new filename
    // see comments in source file for details.
    static const char* check_compress(const char* filename, char* buf, int bufsz, 
				      int& istemp, int decompress_flag = 1, 
				      int bitpix = 0);

    // Return an allocated FitsIO object, given the Mem object for the file header
    // (header.ptr() should point to the entire FITS file contents.)
    static FitsIO* initialize(Mem& header);

    // Return an allocated FitsIO object, given the Mem objects for the header and data.
    static FitsIO* initialize(Mem& header, Mem& data);

    // generate a blank image with a FITS header based on the given fields
    static FitsIO* blankImage(double ra, double dec, double equinox, 
			      double radius, int width, int height, 
			      unsigned long color0);

    // find and set value for the given FITS keyword and return 0 if OK (found)
    int get(const char* keyword, double& val) const;
    int get(const char* keyword, float& val) const;
    int get(const char* keyword, int& val) const;
    int get(const char* keyword, long& val) const;
    int get(const char* keyword, unsigned char& val) const;
    int get(const char* keyword, unsigned short& val) const;
    int get(const char* keyword, short& val) const;

    // find and return the value for the given FITS keyword, or NULL if not found
    char* get(const char* keyword) const;

    // same as get(const char*), but you supply the buffer to hold the result
    char* get(const char* keyword, char* buf, int bufsz) const;

    // these are static versions of the above that require the cfitsio handle
    static int get(fitsfile*, const char* keyword, double& val);
    static int get(fitsfile*, const char* keyword, float& val);
    static int get(fitsfile*, const char* keyword, int& val);
    static int get(fitsfile*, const char* keyword, long& val);
    static int get(fitsfile*, const char* keyword, unsigned char& val);
    static int get(fitsfile*, const char* keyword, unsigned short& val);
    static int get(fitsfile*, const char* keyword, short& val);

    // find and return the value for the given FITS keyword, or NULL if not found
    static char* get(fitsfile*, const char* keyword);

    //  write a (ASCII formatted) copy of the FITS header to the given stream.
    int getFitsHeader(ostream& os) const;

    // write data to disk (network byte ordered, NBO)
    int fwriteNBO(char *data, int tsize, int size, FILE *f) const;

    // Insert the given FITS keyword and value and return 0 if OK
    // If there is not enough space in the header, the file size is
    // automatically increased.
    int put(const char* keyword, double val, const char* comment = NULL);
    int put(const char* keyword, float val, const char* comment = NULL);
    int put(const char* keyword, int val, const char* comment = NULL);
    int put(const char* keyword, const char* val, const char* comment = NULL);

    //  Insert a formatted header card.
    int putcard(const char* card);
    
    // -- HDU access --
    
    // Return the total number of HDUs
    int getNumHDUs();

    // Return the type of the current HDU as a string: "image", "ascii", 
    // or "binary" or NULL if there was as error.
    const char* getHDUType();

    // Return the index of the current HDU 
    int getHDUNum();

    // Move to the specified HDU and make it the current one
    virtual int setHDU(int num);

    // Delete the given HDU
    int deleteHDU(int num);

    
    // -- Read Fits Tables --

    // get the dimensions of the current FITS table
    int getTableDims(long& rows, int& cols);

    // return the table heading for the given column
    char* getTableHead(int col);

    // Return the value in the current FITS table at the given row and column
    char* getTableValue(long row, int col);

    // get the contents of the given column as an array of doubles
    int getTableColumn(int col, double* values, int numValues);


    // -- Write Fits Tables --

    // Create a FITS table and make it the current HDU
    int createTable(const char* extname, long rows, int cols,
		    char** headings, char** tform, int asciiFlag = 0);

    // Set the value in the current FITS table at the given row and column
    // (For now, all data types are treated as strings)
    int setTableValue(long row, int col, const char* value);
};

#endif /* _FitsIO_h_ */
