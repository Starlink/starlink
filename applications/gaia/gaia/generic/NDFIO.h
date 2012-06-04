#ifndef _NDFIO_h_
#define _NDFIO_h_

/*+
 *  Name:
 *     NDFIO.h

 *  Purpose:
 *     Defines the NDF class.

 *  Language:
 *     C++

 *  Description:
 *     This routine defines a simple class for reading and writing
 *     NDF image data arrays.

 *  Copyright:
 *     Copyright (C) 1996-2005 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     Copyright (C) 2009 Science and Technology Facilities Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  History:
 *     28-JUN-1996 (PWD):
 *        Start again for RTD version 2.3.
 *     16-Mar-1998 (Allan Brighton, ESO)
 *        Updated for Skycat/Gaia plugin (get() methods)
 *        Removed static put_keyword, blankImage methods (not used)
 *        Changed constructor to initialize WCS object.
 *     18-JAN-2000 (PWD):
 *        Add changes to support a "hdu" command for NDFs.
 *     14-DEC-2005 (PWD):
 *        Implement copy and setHDU members of ImageIORep.
 *-
 */

#include <cstdio>

// ALLAN: changed from wcslib.h to avoid name conflicts after upgrade
// to wcssubs-2.3.
#include "fitshead.h"

#include "ImageIO.h"
#include "gaiaNDF.h"

// Length of COMPONENT name.
static const int COMPONENT_LENGTH = 64;

class NDFIO : public ImageIORep {

private:

   //  Set wcslib header length for searching.
   static void set_header_length(const Mem& header);
   void set_header_length() const;

protected:

   //  Current NDF identifier.
   int ndfid_;

   //  Current NDF data component.
   char component_[COMPONENT_LENGTH];

   //  Information structure for NDF displayables.
   void *NDFinfo_;

   //  Current NDF index.
   int curd_;

public:

   //  Constructor.
   NDFIO( void *NDFinfo, int curd, const char *component,
          int ndfid, int width, int height, int bitpix, double bzero,
          double bscale, const Mem& header, const Mem& data );

   //  Destructor (frees the NDF).
   ~NDFIO();

   //  Initialize world coordinates (based on the image header).
   int wcsinit();

   //  Read an NDF and return a pointer to an allocated NDFIO object
   //  NULL if an error occurred.
   static NDFIO *read( const char *filename, const char *component,
                       int deep_search );

   //  Return the class name as a string.
   const char* classname() const { return "NDFIO"; }

   //  Write the data to a new NDF.
   int write( const char *filename );

   //  Find and set value for the given FITS keyword and return 0 if
   //  OK (found).
   int get( const char* keyword, double& val ) const;
   int get( const char* keyword, float& val ) const;
   int get( const char* keyword, int& val ) const;
   int get( const char* keyword, long& val ) const;
   int get( const char* keyword, long long& val ) const;
   int get( const char* keyword, unsigned char& val ) const;
   int get( const char* keyword, unsigned short& val ) const;
   int get( const char* keyword, short& val ) const;

   //  Find and return the value for the given FITS keyword, or NULL
   //  if not found.
   char* get( const char* keyword ) const;

   //  Write a (ASCII formatted) copy of the FITS header to the given
   //  stream.
   int getFitsHeader( ostream& os ) const;

   //  Return the NDF component that is mapped.
   char *component() const { return (char *) component_; }

   //  Return the NDF identifier.
   int getNDFID() { return ndfid_; }

   //  Return number of NDFs that can current be displayed.
   int getNumNDFs();

   //  Index of NDF current available for display.
   int getNDFNum() { return curd_; }

   //  Check in NDF has the specified component.
   int checkComponent( int index, const char *component );

   //  Set the current NDF and component (not if current already)
   int setDisplayable( int index, const char *component );

   //  Set the current NDF and component.
   int makeDisplayable( int index, const char *component );

   //  Reset the current NDF and component.
   int resetDisplayable();

   //  Get printable NDF information.
   void getNDFInfo( int index, char *name, char *naxis1, char *naxis2,
                    char *hasvar, char *hasqual );

   //  Get readonly status of current displayable.
   int getReadonly();

   //  Set readonly status of current displayable.
   void setReadonly( int status );

   //  Create a copy.
   virtual NDFIO* copy();

   //  Switch to the i'th data component.
   virtual int setHDU(int hdu) {
      return ( setDisplayable( hdu, "DATA" ) == 0 );
   }

};

#endif // _NDFIO_h_
