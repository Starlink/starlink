//+
//   Name:
//      NDFIO.C

//  Purpose:
//     Defines the NDF class member functions for reading and
//     writing NDFs.

//  Language:
//     C++

//
//  Notes:
//     May want to create an NDFIO::initialize member if supporting NDF
//     reading and writing from shared memory.
//

//  Copyright:
//     Copyright (C) 1997 Central Laboratory of the Research Councils

//  Authors:
//     Peter W. Draper (PDRAPER):
//     Allan Brighton, ESO (ALLAN):
//     {enter_new_authors_here}

//  History:
//     28-JUN-1996 (PDRAPER):
//        Started again for version 2.3.
//     22-NOV-1996 (PDRAPER):
//        Converted to accept memory header (rather than use a fixed
//        size local array).
//     16-JAN-1997 (PDRAPER):
//        Changed to create the memory copy of the image data before
//        copying. This is intended to reduce the overall footprint of
//        the application in memory when the NDF data is released. The
//        data is now also copied by chunking so that the total amount of
//        memory required is now smaller for very large images.
//     16-Mar-1998 (ALLAN)
//        Updated for Skycat/Gaia plugin (get() methods)
//        Removed static put_keyword, blankImage methods (not used)
//        Changed constructor to initialize WCS object.
//     {enter_changes_here}

//-

#include <string.h>
#include <strstream.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include "error.h"
#include "StarWCS.h"
#include "NDFIO.h"


/*
 * constructor 
 */
NDFIO::NDFIO(int width, int height, int bitpix, double bzero, 
	     double bscale, const Mem& header, const Mem& data, 
	     const char *component )
    : ImageIORep(width, height, bitpix, bzero, bscale, header, data)
{
    strncpy( component_, (char *) component, 20 );
}



//+
// Initialize world coordinates (based on the image header)
//-
int NDFIO::wcsinit()
{
    wcs_ = WCS(new StarWCS((const char*)header_.ptr()));
    return wcs_.status();
}

//
//  Read an NDF and convert it into an ImageIO compatible object
//  (FITS-like). The 2 flag arguments indicate whether the data should
//  be placed into shared memory or not. The filename is the fully
//  specified name of the file containing the NDF.
//

NDFIO *NDFIO::read( const char *filename, const char *component, int
                    mem_options ) 
{
  const int header_length = 80;      //  Length of FITS header.
  int header_records = 0;
  double bzero = 0.0, bscale = 1.0;
  int bitpix = 0, width = 0, height = 0;
  int ndfid = 0;
  char *inheader;
  Mem header, data;
  void *indata;
  char *error_mess;

  //  Try to read the NDF data array.
  //  Access the NDF getting its size, data type and pseudo FITS header.
  if ( rtdAccessNDF( filename, &bitpix, &width, &height, &inheader,
                     &header_records, &ndfid, &error_mess ) ) {

    // Create the Mem object to hold the image data.
    int tsize = width * height * abs( bitpix ) / 8;
    data = Mem( tsize, mem_options );
    indata = data.ptr();

    // Now copy the image into it.
    if ( rtdCopyNDF( ndfid, &indata, component, &error_mess ) ) { 

      // Copy the header.
      header = Mem( header_length * header_records + 1, mem_options );
      memcpy( (void *) header.ptr(), inheader, header_length * header_records + 1);
      
      free(inheader);  /*  Memory allocated by PSX_ routines */

      // Create NDFIO object with data and size.
      return new NDFIO( width, height, bitpix, bzero, bscale, header,
                        data, component );
    } else { 

      // Failed to copy the image, must have ran out of memory or some
      // such.
      error( error_mess );
      free( error_mess );
      return NULL;
    }
  } else {

    // Failed to open NDF so issue the error message we have been
    // given (this is a dynamic C string so free it).
    error( error_mess );
    free( error_mess );
    return NULL;
  }
}

//
//  NDF destructor.
//
NDFIO::~NDFIO() {
  int ndfid = 0;
  (void) hgeti4( (char *)header_.ptr(), "NDFID", &ndfid );
  rtdFreeNDF( ndfid );
}

//
//  Create a new NDF with a copy of the current data.
//
int NDFIO::write( const char *filename ) const {
  char *error_mess;

  //  Extract the NDF identifier from the fits headers).
  int ndfid = 0;
  (void) hgeti4( (char *)header_.ptr(), "NDFID", &ndfid );
  if ( rtdWriteNDF( filename, bitpix_, width_, height_, data_.ptr(),
                    ndfid, component(), (char *) header_.ptr(),
                    header_.size(), &error_mess ) ) {  
    return OK;
  } else { 
    
    // Failed to write NDF so issue the error message we have been
    // given (this is a dynamic C string so free it).
    error( error_mess );
    free( error_mess );
    return ERROR;
  }
}


//
//  write a (ASCII formatted) copy of the FITS header to the given stream.
// (format it in 80 char lines and replace any NULL chars with blanks)
//
int NDFIO::getFitsHeader(ostream& os) const
{
    istrstream is((char*)header_.ptr(), header_.size());
    char buf[81];
    while(is.read(buf, 80)) {
	for (int i = 0; i < 79; i++)
	    if (!isascii(buf[i]))
		buf[i] = ' ';
	buf[79] = '\n';
	os.write(buf, 80);
	if (strncmp(buf, "END     ", 8) == 0)
	    break;
    }
    os << ends;
    return 0;
}



/*
 * set wcslib header length for searching based on the Mem size/offset
 * (The hget routines depend on a static variable being set by hlength() - 
 * not very safe...)
 * Assumes that either header and data are pointing to the same memory
 * with data having an offset, or they are 2 different memory areas.
 */
void NDFIO::set_header_length(const Mem& header) 
{
    if (header.length() > 0)
	hlength((char*)header.ptr(), header.length());
}

/*
 * member version of above
 */
void NDFIO::set_header_length() const 
{
    set_header_length(header_);
}

//
// get value for the given FITS keyword and return 0 if OK (found)
// (allan)
//
int NDFIO::get(const char* keyword, double& val) const {
    int length = header_.length();
    char* ptr = (char*)header_.ptr();
    set_header_length();
    return length ? ! hgetr8(ptr, keyword, &val) : 1;
}
int NDFIO::get(const char* keyword, float& val) const {
    int length = header_.length();
    char* ptr = (char*)header_.ptr();
    set_header_length();
    return length ? ! hgetr4(ptr, keyword, &val) : 1;
}
int NDFIO::get(const char* keyword, int& val) const {
    int length = header_.length();
    char* ptr = (char*)header_.ptr();
    set_header_length();
    return length ? ! hgeti4(ptr, keyword, &val) : 1;
}
int NDFIO::get(const char* keyword, long& val) const {
    if (sizeof(int) != sizeof(long))
	return error("NDFIO: long int size not supported");
    int length = header_.length();
    char* ptr = (char*)header_.ptr();
    set_header_length();
    return length ? ! hgeti4(ptr, keyword, (int*)&val) : 1;
}
int NDFIO::get(const char* keyword, unsigned char& val) const {
    int length = header_.length();
    char* ptr = (char*)header_.ptr();
    set_header_length();
    if (length == 0) 
	return 1;
    short tmp;
    if (! hgeti2(ptr, keyword, &tmp))
	return 1;
    val = (unsigned char) tmp;
    return 0;
}
int NDFIO::get(const char* keyword, short& val) const {
    int length = header_.length();
    char* ptr = (char*)header_.ptr();
    set_header_length();
    return length ? ! hgeti2(ptr, keyword, &val) : 1;
}
int NDFIO::get(const char* keyword, unsigned short& val) const {
    int length = header_.length();
    char* ptr = (char*)header_.ptr();
    set_header_length();
    return length ? ! hgeti2(ptr, keyword, (short*)&val) : 1;
}

/*
 * find and return the value for the given FITS keyword, or NULL if not found
 */
char* NDFIO::get(const char* keyword) const {
    int length = header_.length();
    char* ptr = (char*)header_.ptr();
    set_header_length();
    return length ? hgetc(ptr, keyword) : 0;
}

