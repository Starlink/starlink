#ifndef _NDFIO_h_
#define _NDFIO_h_
//+
//   Name:
//      NDFIO.h

//  Purpose:
//     Defines the NDF class.

//  Language:
//     C++

//  Description:
//     This routine defines a simple class for reading and writing
//     NDF image data arrays.

//  Copyright:
//    Copyright (C) 1998 Central Laboratory of the Research Councils

//  History:
//     28-JUN-1996 (PDRAPER):
//        Start again for RTD version 2.3.
//     16-Mar-1998 (Allan Brighton, ESO)
//        Updated for Skycat/Gaia plugin (get() methods)
//        Removed static put_keyword, blankImage methods (not used)
//        Changed constructor to initialize WCS object.

//-

#include <stdio.h>
#include <iostream.h>

// allan: changed from wcslib.h to avoid name conflicts after upgrade to wcssubs-2.3
#include "fitshead.h"   

#include "ImageIO.h"
#include "rtdNDF.h"

class NDFIO : public ImageIORep {
private:
    // set wcslib header length for searching
    static void set_header_length(const Mem& header);
    void set_header_length() const;

protected:   

  //  Type of data component we have mapped.
  char component_[20];
  
public:

  //  Constructor.
  NDFIO( int width, int height, int bitpix, double bzero, 
         double bscale, const Mem &header, const Mem &data, 
         const char *component );

  //  Destructor (frees the NDF).
  ~NDFIO();

  // initialize world coordinates (based on the image header)
  int wcsinit();

  //  Read an NDF and return a pointer to an allocated NDFIO object
  //  NULL if an error occurred.
  static NDFIO *read( const char *filename, const char *component, int memOptions = 0 );

  // return true if this class uses native byte ordering
  // (The ndf library does the swapping already, so return 1 here).
  int nativeByteOrder() const {return 1;}

  // return the class name as a string
  const char* classname() const {return "NDFIO";}

  //  Write the data to a new NDF.
  int write( const char *filename ) const;

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

  //  Write a (ASCII formatted) copy of the FITS header to the given stream.
  int getFitsHeader(ostream& os) const;

  // Return the NDF component that is mapped.
  char *component() const { return (char *) component_; }

};

#endif // _NDFIO_h_
