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
//     Peter W. Draper (PWD):
//     Allan Brighton, ESO (ALLAN):
//     {enter_new_authors_here}

//  History:
//     28-JUN-1996 (PWD):
//        Started again for version 2.3.
//     22-NOV-1996 (PWD):
//        Converted to accept memory header (rather than use a fixed
//        size local array).
//     16-JAN-1997 (PWD):
//        Changed to create the memory copy of the image data before
//        copying. This is intended to reduce the overall footprint of
//        the application in memory when the NDF data is released. The
//        data is now also copied by chunking so that the total amount of
//        memory required is now smaller for very large images.
//     16-Mar-1998 (ALLAN)
//        Updated for Skycat/Gaia plugin (get() methods)
//        Removed static put_keyword, blankImage methods (not used)
//        Changed constructor to initialize WCS object.
//     29-OCT-1999 (PWD):
//        Now use hgeti to access unsigned short value. The hgeti2
//        routine is changed to truncate values into the range of
//        signed word.
//     12-JAN-2000 (PWD):
//        Added changes to support NDF "HDUs". This is just a way of
//        accessing NDF data components and NDFs within container
//        files and is provided to match the FITS HDU concept.
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
extern "C" {
#include "cnf.h"
}


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
   strncpy( component_, (char *) component, 20 );
}

//+
//  NDF destructor.
//-
NDFIO::~NDFIO() {

   //  Release all resources accessed in current NDF and related.
   if ( NDFinfo_ ) {
      rtdReleaseNDF( NDFinfo_ );
   }
}

//+
//  Initialize world coordinates (based on the image header)
//-
int NDFIO::wcsinit()
{
   wcs_ = WCS( new StarWCS( (const char *) header_.ptr() ) );
   return wcs_.status();
}

//+
//  Read an NDF and convert a displayable data array into an ImageIO
//  compatible object (FITS-like). The 2 flag arguments indicate
//  whether the data should be placed into shared memory or not. The
//  filename is the fully specified name of the file containing the
//  NDF.
//-
NDFIO *NDFIO::read( const char *filename, const char *component,
                    int useShm )
{
   Mem data;
   Mem header;
   char *error_mess;
   char *inheader;
   char *name;
   const int header_length = 80;      //  Length of FITS header.
   double bscale = 1.0;
   double bzero = 0.0;
   int bitpix = 0;
   int header_records = 0;
   int height = 0;
   int ndfid = 0;
   int width = 0;
   int hasv;
   int hasq;
   void *NDFinfo;
   void *indata;

   //  Parse the input filename. This becomes an information structure
   //  that describes the NDFs (of a list of NDFs if more than one are
   //  available at the given HDS path). Displayable array components
   //  of each NDF are known as "displayables".
   if ( rtdInitNDF( filename, &NDFinfo, &error_mess ) ) {

      //  Now check that the first NDF contains a displayable that
      //  corresponds to the given component.
      if ( rtdCheckDisplayable( NDFinfo, 1, component ) ) {

         //  NDF we can display something so get the NDF information
         //  we need.
         rtdGetNDFInfo( NDFinfo, 1, &name, &bitpix, &width, &height,
                        &inheader, &header_records, &ndfid, &hasv, &hasq );

         //  Create a Mem object to hold the displayable data.
         int tsize = width * height * ( abs( bitpix ) / 8 );
         //data = Mem( tsize, useShm );
         //indata = data.ptr();
         void *indata;

         // Now copy the data into it.
         if ( rtdCopyDisplayable( NDFinfo, 1, component, &indata,
                                  &error_mess ) ) {

            data = Mem( indata, tsize, 0 );

            // Copy the header.
            header = Mem( header_length * header_records + 1, useShm );
            memcpy( (void *) header.ptr(), inheader, header_length * header_records + 1);

            // Create NDFIO object with data and size.
            return new NDFIO( NDFinfo, 1, component, ndfid,
                              width, height, bitpix, bzero, bscale,
                              header, data );
         } else {

            // Failed to copy the image, must have ran out of memory or some
            // such.
            error( error_mess );
            free( error_mess );
            return NULL;
         }
      } else {

         // Failed to get the component.
         error( "Image does not contain the component:", component );
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

//+
//  Create a new NDF with a copy of the current data.
//-
int NDFIO::write( const char *pathname ) const {
   char *error_mess;
   if ( rtdWriteNDF( pathname, bitpix_, width_, height_, data_.ptr(),
                     ndfid_, component(), (char *) header_.ptr(),
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

//+
//  Write a (ASCII formatted) copy of the FITS header to the given
//  stream. (format it in 80 char lines and replace any NULL chars
//  with blanks).
//-
int NDFIO::getFitsHeader(ostream& os) const
{
   istrstream is((char*)header_.ptr(), header_.size());
   char buf[81];
   while(is.read(buf, 80)) {
      for (int i = 0; i < 79; i++) {
         if (!isascii(buf[i])) {
            buf[i] = ' ';
         }
      }
      buf[79] = '\n';
      os.write(buf, 80);
      if (strncmp(buf, "END     ", 8) == 0) {
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
   return rtdCountNDFs( NDFinfo_ );
}

//
//  Existence of NDF component.
//
int NDFIO::checkComponent( int index, const char *component )
{
   return rtdCheckDisplayable( NDFinfo_, index, component );
}

//
//  Set the current displayable.
//
int NDFIO::setDisplayable( int index, const char *component )
{
   //  If this is the current displayable then do nothing.
   if ( index == curd_ && component[0] == component_[0] ) {
      return 1;
   }

   //  Check component and NDF are available.
   if ( rtdCheckDisplayable( NDFinfo_, index, component ) ) {

      //  Ok, get NDF information and displayable data.
      Mem data;
      Mem header;
      char *error_mess;
      char *inheader;
      char *name;
      const int header_length = 80;
      double bscale = 1.0;
      double bzero = 0.0;
      int bitpix = 0;
      int header_records = 0;
      int height = 0;
      int ndfid = 0;
      int width = 0;
      int hasv;
      int hasq;
      void *indata;
      rtdGetNDFInfo( NDFinfo_, index, &name, &bitpix, &width, &height,
                     &inheader, &header_records, &ndfid, &hasv, &hasq );

      //  Create a Mem object to hold the displayable data.
      int tsize = width * height * ( abs( bitpix ) / 8 );
      // data = Mem( tsize, data_.shared() );
      //indata = data.ptr();

      //  Release existing displayable.
      if ( curd_ != 0 ) {
         rtdFreeDisplayable( NDFinfo_, curd_ );
      }

      // Now copy the data into it.
      if ( rtdCopyDisplayable( NDFinfo_, index, component, &indata,
                               &error_mess ) ) {

         data = Mem( indata, tsize, 0 );

         // Copy the header.
         header = Mem( header_length * header_records + 1, header_.shared() );
         memcpy( (void *) header.ptr(), inheader, header_length *
                 header_records + 1);

         //  Replace image and header and update related members.
         width_ = width;
         height_ = height;
         bitpix_ = bitpix;
         data_ = data;
         header_ = header;

         //  This is now the current NDF and component.
         curd_ = index;
         strncpy( component_, (char *) component, 20 );
         ndfid_ = ndfid;
      } else {

         // Failed to copy the image, must have ran out of memory or some
         // such.
         error( error_mess );
         free( error_mess );
         return 0;
      }

   } else {
      error( "Requested NDF component does not exist" );
      return 0;
   }
   return 1;
}

//
//  Get information about an NDF. All returned as strings.
//
void NDFIO::getNDFInfo( int index, char *name, char *naxis1, char *naxis2,
                        char *hasvar, char *hasqual )
{
   int bitpix;
   int width;
   int height;
   char *header;
   char *named;
   int hlen;
   int ndfid;
   int hasq;
   int hasv;

   rtdGetNDFInfo( NDFinfo_, index, &named, &bitpix, &width, &height,
                  &header, &hlen, &ndfid, &hasv, &hasq );

   strcpy( name, named );
   sprintf( naxis1, "%d", width );
   sprintf( naxis2, "%d", height );
   if ( hasv ) {
      strcpy( hasvar, "true" );
   } else {
      strcpy( hasvar, "false" );
   }
   if ( hasq ) {
      strcpy( hasqual, "true" );
   } else {
      strcpy( hasqual, "false" );
   }
}
