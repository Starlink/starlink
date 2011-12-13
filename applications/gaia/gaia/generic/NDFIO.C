/*+
 *  Name:
 *     NDFIO.C

 *  Purpose:
 *    Defines the NDF class member functions for reading and
 *    writing NDFs.

 *  Language:
 *    C++

 *  Notes:
 *    May want to create an NDFIO::initialize member if supporting NDF
 *    reading and writing from shared memory.
 *

 *  Copyright:
 *     Copyright (C) 1997-2005 Central Laboratory of the Research Councils
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

 *  Authors:
 *     Peter W. Draper (PWD):
 *     Allan Brighton, ESO (ALLAN):
 *     {enter_new_authors_here}

 *  History:
 *     28-JUN-1996 (PWD):
 *        Started again for version 2.3.
 *     22-NOV-1996 (PWD):
 *        Converted to accept memory header (rather than use a fixed
 *        size local array).
 *     16-JAN-1997 (PWD):
 *        Changed to create the memory copy of the image data before
 *        copying. This is intended to reduce the overall footprint of
 *        the application in memory when the NDF data is released. The
 *        data is now also copied by chunking so that the total amount of
 *        memory required is now smaller for very large images.
 *     16-Mar-1998 (ALLAN)
 *        Updated for Skycat/Gaia plugin (get() methods)
 *        Removed static put_keyword, blankImage methods (not used)
 *        Changed constructor to initialize WCS object.
 *     29-OCT-1999 (PWD):
 *        Now use hgeti to access unsigned short value. The hgeti2
 *        routine is changed to truncate values into the range of
 *        signed word.
 *     12-JAN-2000 (PWD):
 *        Added changes to support NDF "HDUs". This is just a way of
 *        accessing NDF data components and NDFs within container
 *        files and is provided to match the FITS HDU concept.
 *     07-FEB-2000 (PWD):
 *        Added changes to get back writable data components when
 *        needed (for image patching).
 *     10-APR-2000 (PWD):
 *        Changed to use bitpix=8 explicitly when accessing quality
 *        component. Note still assumes variance is same data type as
 *        main array.
 *     05-SEP-2004 (PWD):
 *        Modify so that all memory is allocated by HDS or CNF so that we can
 *        safely pass this into Fortran, even on 64 bit machines.
 *     22-NOV-2005 (PWD):
 *        Switch to new byte swapping in Skycat 2.7.4.
 *     14-DEC-2005 (PWD):
 *        Implemented copy.
 *     {enter_changes_here}

 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <define.h>  //  From skycat util

#include <cstring>
#include <iostream>
#include <sstream>
#include <cstdlib>
#include <cctype>
#include <cmath>
#include "error.h"
#include "StarWCS.h"
#include "NDFIO.h"
extern "C" {
#include "cnf.h"
}

//  Length of a FITS header card.
static const int FITSCARD = 80;

//+
//  Constructor.
//-
NDFIO::NDFIO( void *NDFinfo, int curd, const char *component,
              int ndfid, int width, int height, int bitpix, double bzero,
              double bscale, const Mem& header, const Mem& data )
   : ImageIORep( width, height, bitpix, bzero, bscale, header, data ),
     ndfid_( ndfid ),
     NDFinfo_( NDFinfo ),
     curd_( curd )
{
   //  Record the component type.
   strncpy( component_, (char *) component, COMPONENT_LENGTH );

   //  We don't byte swap, so usingNetBO_ value only depends on BIGENDIAN.
   usingNetBO( BIGENDIAN );
}

//+
//  NDF destructor.
//-
NDFIO::~NDFIO() {

   //  Release all resources accessed in current NDF and related.
   if ( NDFinfo_ ) {
      gaiaReleaseMNDF( NDFinfo_ );
   }

   //  Release the header, this has been allocated by CNF.
   if ( header_.ptr() != NULL && header_.refcnt() <= 1 ) {
       cnfFree( header_.ptr() );
   }
}

//+
//  Initialize world coordinates (based on the image header)
//-
int NDFIO::wcsinit()
{
   wcs_ = WCS( new StarWCS( (const char *) header_.ptr(), header_.size() ) );
   return wcs_.status();
}

//+
//  Read an NDF and convert a displayable data array into an ImageIO
//  compatible object (FITS-like). The filename is the fully specified name of
//  the file containing the NDF.
//-
NDFIO *NDFIO::read( const char *filename, const char *component,
                    int deepsearch )
{
    Mem data;
    Mem header;
    char *error_mess;
    char *inheader;
    char *name;
    const int header_length = FITSCARD;      //  Length of FITS header.
    double bscale = 1.0;
    double bzero = 0.0;
    int bitpix = 0;
    int header_records = 0;
    size_t hsize = 0;
    int height = 0;
    int ndfid = 0;
    int width = 0;
    int hasv;
    int hasq;
    void *NDFinfo;
    void *indata;
    void *hdata;

    //  Parse the input filename. This becomes an information structure
    //  that describes the NDFs (of a list of NDFs if more than one are
    //  available at the given HDS path). Displayable array components
    //  of each NDF are known as "displayables".
    if ( gaiaInitMNDF( filename, deepsearch, &NDFinfo, &error_mess ) ) {

        //  Now check that the first NDF contains a displayable that
        //  corresponds to the given component.
        if ( gaiaCheckMNDF( NDFinfo, 1, component ) ) {

            //  NDF we can display something so get the NDF information
            //  we need.
            gaiaGetInfoMNDF( NDFinfo, 1, &name, &bitpix, &width, &height,
                             &inheader, &header_records, &ndfid,
                             &hasv, &hasq );

            //  If accessing quality, then the data type is unsigned byte
            //  not the current bitpix.
            if ( strncasecmp( "qua", component, 3 ) == 0 ) {
                bitpix = 8;
            }

            //  Create a Mem object to hold the displayable data.
            size_t tsize = (size_t) width * (size_t) height *
                           (size_t) ( abs( bitpix ) / 8 );

            // Now map the data and initialise with this pointer.
            if ( gaiaGetMNDF( NDFinfo, 1, component, &indata, &error_mess ) ) {

                // Mem just accepts pointer to data.
                data = Mem( indata, tsize, 0 );

                // Copy the header. Use CNF memory so it can be passed to
                // Fortran.
                hsize = (size_t)header_length * (size_t)header_records;
                hdata = cnfMalloc( hsize );
                memcpy( hdata, inheader, hsize );
                header = Mem( hdata, hsize, 0 );

                // Create NDFIO object with data and size.
                return new NDFIO( NDFinfo, 1, component, ndfid,
                                  width, height, bitpix, bzero, bscale,
                                  header, data );
            }
            else {
                // Failed to copy the image, must have ran out of memory or
                // some such.
                error( error_mess );
                free( error_mess );
                return NULL;
            }
        }
        else {
            // Failed to get the component.
            error( "Image does not contain the component:", component );
            return NULL;
        }
    }
    else {
        // Failed to open NDF so issue the error message we have been
        // given (this is a dynamic C string so free it).
        error( error_mess );
        free( error_mess );
        return NULL;
    }
}

//+
//  Create a new NDF with a copy of the current data.
//-
int NDFIO::write( const char *pathname )
{
   char *error_mess;
   if ( gaiaWriteNDF( pathname, bitpix_, width_, height_, data_.ptr(),
                      ndfid_, component(), (char *) header_.ptr(),
                      header_.size(), &error_mess ) ) {
      return OK;
   }
   else {
      // Failed to write NDF so issue the error message we have been
      // given (this is a dynamic C string so free it).
      error( error_mess );
      free( error_mess );
      return ERROR;
   }
}

//+
//  Write a (ASCII formatted) copy of the FITS header to the given
//  stream. (format it in 80 char lines and replace any NULL chars
//  with blanks).
//-
int NDFIO::getFitsHeader(ostream& os) const
{
    string istr( (char*)header_.ptr(), header_.length() );
    istringstream is( istr );
    char buf[FITSCARD+1];
    while( is.read( buf, FITSCARD ) ) {
        for ( int i = 0; i < FITSCARD; i++ ) {
            if ( ! isascii( buf[i] ) ) {
                buf[i] = ' ';
            }
        }
        buf[FITSCARD] = '\n';
        os.write( buf, FITSCARD+1 );
        if ( strncmp( buf, "END     ", 8 ) == 0 ) {
            break;
        }
    }
    os << ends;
    return 0;
}

//
//  Set wcslib header length for searching based on the Mem
//  size/offset (The hget routines depend on a static variable being
//  set by hlength() - not very safe...)  Assumes that either header
//  and data are pointing to the same memory with data having an
//  offset, or they are 2 different memory areas.
//
void NDFIO::set_header_length(const Mem& header)
{
   if (header.length() > 0) {
      hlength((char*)header.ptr(), header.length());
   }
}

//
//  Member version of above.
//
void NDFIO::set_header_length() const
{
   set_header_length(header_);
}

//
//  Get value for the given FITS keyword and return 0 if OK (found)
//  (ALLAN).
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
   if (sizeof(int) != sizeof(long)) {
      return error("NDFIO: long int size not supported");
   }
   int length = header_.length();
   char* ptr = (char*)header_.ptr();
   set_header_length();
   return length ? ! hgeti4(ptr, keyword, (int*)&val) : 1;
}

int NDFIO::get(const char* keyword, unsigned char& val) const {
   int length = header_.length();
   char* ptr = (char*)header_.ptr();
   set_header_length();
   if (length == 0) {
      return 1;
   }
   short tmp;
   if (! hgeti2(ptr, keyword, &tmp)) {
      return 1;
   }
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
   if ( length ) {
      int ival;
      int status = hgeti4(ptr, keyword, &ival);
      val = (unsigned short) ival;
      if ( status == 0 ) {
         return 1;
      } else {
         return 0;
      }
   } else {
      return 1;
   }
}

//
//  Find and return the value for the given FITS keyword, or NULL if
//  not found
//
char* NDFIO::get(const char* keyword) const {
   int length = header_.length();
   char* ptr = (char*)header_.ptr();
   set_header_length();
   return length ? hgetc(ptr, keyword) : 0;
}

//
//  --- MULTIPLE NDF AND COMPONENT ACCESS ---
//
//  Routine that parallel the FITS HDU concept. NDFs can be store more
//  than one per container file, plus each NDF may have more than just
//  its data array for display.
//
//  A "displayable" is reference by its NDF number (== HDU number) and
//  the component that is required.

//
//  Return the number of NDFs.
//
int NDFIO::getNumNDFs()
{
   return gaiaCountMNDFs( NDFinfo_ );
}

//
//  Existence of NDF component.
//
int NDFIO::checkComponent( int index, const char *component )
{
   return gaiaCheckMNDF( NDFinfo_, index, component );
}

//
//  Set the current displayable. Do nothing if already current.
//
int NDFIO::setDisplayable( int index, const char *component )
{
   //  If this is the current displayable then do nothing.
   if ( index == curd_ && component[0] == component_[0] ) {
      return 1;
   }
   return makeDisplayable( index, component );
}

//
//  Reset the displayable NDF component.
//
int NDFIO::resetDisplayable()
{
   return makeDisplayable( curd_, component_ );
}

//
//  Make given displayable the current image.
//
int NDFIO::makeDisplayable( int index, const char *component )
{
   //  Check component and NDF are available.
   if ( gaiaCheckMNDF( NDFinfo_, index, component ) ) {

      //  Ok, get NDF information and displayable data.
      Mem data;
      Mem header;
      char *error_mess;
      char *inheader;
      char *name;
      const int header_length = FITSCARD;
      int bitpix = 0;
      int header_records = 0;
      int height = 0;
      size_t hsize = 0;
      int ndfid = 0;
      int width = 0;
      int hasv;
      int hasq;
      void *indata;
      void *hdata;

      gaiaGetInfoMNDF( NDFinfo_, index, &name, &bitpix, &width,
                       &height, &inheader, &header_records, &ndfid,
                       &hasv, &hasq );

      //  If accessing quality, then the data type is unsigned byte
      //  not the current bitpix.
      if ( strncasecmp( "qua", component, 3 ) == 0 ) {
         bitpix = 8;
      }

      //  Create a Mem object to hold the displayable data.
      size_t tsize = (size_t)width * (size_t)height *
                     (size_t)( abs( bitpix ) / 8 );

      //  Release existing displayable.
      if ( curd_ != 0 ) {
         gaiaFreeMNDF( NDFinfo_, curd_ );
         curd_ = 0;
      }

      //  If NDF is marked writable then we need to allocate some
      //  memory to store the image.
      int readonly = gaiaGetReadMNDF( NDFinfo_, index );
      if ( ! readonly ) {

          //  Use CNF managed memory so that it is registered. Note this
          //  memory currently leaks.
          indata = cnfMalloc( tsize );
      }

      // Now copy the data into it.
      if ( gaiaGetMNDF( NDFinfo_, index, component, &indata, &error_mess ) ) {

         //  Mem object just accepts pointer to mapped memory.
         data = Mem( indata, tsize, 0 );

         // Copy the header.
         hsize = (size_t)header_length * (size_t)header_records;
         hdata = cnfMalloc( hsize );
         memcpy( hdata, inheader, hsize );
         header = Mem( hdata, hsize, 0 );

         //  Replace image and header and update related members.
         width_ = width;
         height_ = height;
         bitpix_ = bitpix;
         data_ = data;
         if ( header_.ptr() != NULL && header_.refcnt() <= 1 ) {
             //  Last reference to this memory about to go (refcnt is
             //  decremented by the assignment operator to follow), so OK
             //  to free.
             cnfFree( header_.ptr() );
         }
         header_ = header;

         //  This is now the current NDF and component.
         curd_ = index;
         int l = strlen( component );
         if ( l > COMPONENT_LENGTH ) l = COMPONENT_LENGTH - 1;
         memmove( component_, component, l );
         component_[COMPONENT_LENGTH-1] = '\0';
         ndfid_ = ndfid;
      }
      else {
         // Failed to copy the image, must have ran out of memory or some
         // such.
         if ( ! readonly ) {
             cnfFree( indata );
         }
         error( error_mess );
         free( error_mess );
         return 0;
      }
   }
   else {
      error( "Requested NDF component does not exist" );
      return 0;
   }
   return 1;
}

//
//  Get information about an NDF. All returned as strings.
//
//  Note that path should have a length equal to MAXNDFNAME (defined
//  in gaiaNDF.h) and will be returned as "." for the main NDF
//  (previously this was the full NDF specification for such objects).
//
void NDFIO::getNDFInfo( int index, char *path, char *naxis1, char *naxis2,
                        char *hasvar, char *hasqual )
{
   int bitpix;
   int width;
   int height;
   char *header;
   char *pathd;
   int hlen;
   int ndfid;
   int hasq;
   int hasv;

   gaiaGetInfoMNDF( NDFinfo_, index, &pathd, &bitpix, &width, &height,
                    &header, &hlen, &ndfid, &hasv, &hasq );

   strcpy( path, pathd );
   sprintf( naxis1, "%d", width );
   sprintf( naxis2, "%d", height );
   if ( hasv ) {
      strcpy( hasvar, "true" );
   }
   else {
      strcpy( hasvar, "false" );
   }
   if ( hasq ) {
      strcpy( hasqual, "true" );
   }
   else {
      strcpy( hasqual, "false" );
   }
}

//
//  Return if the current NDF has readonly status (i.e. it's data
//  shouldn't be modified).
//
int NDFIO::getReadonly()
{
   return gaiaGetReadMNDF( NDFinfo_, curd_ );
}

//
//  Set the readonly status of the current NDF. To get data in new
//  mode do resetDisplayable.
//
void NDFIO::setReadonly( int status )
{
   gaiaSetReadMNDF( NDFinfo_, curd_, status );
}

//
//  Create a copy. Wraps a simple NDF clone.
//
NDFIO* NDFIO::copy()
{
    void *newInfo = gaiaCloneMNDF( NDFinfo_ );
    int newId = gaiaGetIdMNDF( newInfo, curd_ );

    return new NDFIO( newInfo, curd_, component_, newId,
                      width_, height_, bitpix_, bzero_, bscale_,
                      header_, data_ );
}
