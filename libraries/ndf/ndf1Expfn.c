#define _POSIX_SOURCE 1            /* Declare POSIX source */

#include <errno.h>                 /* For errno */
#include <limits.h>                /* System limits (for PATH_MAX) */
#include <stdio.h>                 /* For FILENAME_MAX */
#include <stdlib.h>                /* For malloc, free, etc. */
#include <string.h>                /* String functions */

#include <sys/types.h>             /* For struct stat */
#include <sys/stat.h>              /* For stat function */
#include <unistd.h>                /* For getcwd, etc. */

#if !defined( FILENAME_MAX )       /* Overcome gcc compiler problems on SUNs */
#if defined( PATH_MAX )            /* Use POSIX definition instead of ANSI C */
#define FILENAME_MAX PATH_MAX
#else
#define FILENAME_MAX _POSIX_PATH_MAX
#endif
#endif

#include "sae_par.h"               /* Standard SAE constants */
#include "ems.h"                   /* EMS_ error reporting routines */
#include "f77.h"                   /* Fortran 77 <=> C interface macros */
#include "ndf1.h"                  /* Internal NDF definitions */
#include "ndf_err.h"               /* NDF error definitions */

void ndf1Expfn( const char *in, int getfid, char *out, size_t out_length,
                char *fid, size_t fid_length, int *status ){
/*
*+
*  Name:
*     ndf1Expfn

*  Purpose:
*     Expand a file name.

*  Synopsis:
*     void ndf1Expfn( const char *in, int getfid, char *out, size_t out_length,
*                     char *fid, size_t fid_length, int *status )

*  Description:
*     This function expands a foreign file name into its full (absolute)
*     form and optionally returns a file identification code which
*     uniquely identifies the file.

*  Arguments:
*     in
*        The initial file name, which is to be expanded. Leading and
*        trailing blanks are ignored.
*     getfid
*        Whether a file identification code is to be returned.
*     out
*        Pointer to an array in which to return the fully expanded file name.
*     out_length
*        The length of the supplied "out" array.
*     fid
*        Pointer to an array in which to return the file identification code
*        (not used unless "getfid" is non-zero).
*     fid_length
*        The length of the supplied "fid" array.
*     *status
*        The global status.

*  Notes:
*     -  The file name and type fields are not interpreted by this routine
*     and are returned as supplied. Only directory information is added.
*     -  A blank value for OUT will be returned without error if the
*     input file name is blank (no fid value will be returned in this
*     case).
*     -  An error will be reported if the OUT buffer is too short to
*     accommodate the returned file name.
*     -  The file itself need not exist unless a file identification
*     code is requested.
*     -  The length of the file identification code returned will be
*     system-dependent (a suitably long character string should be
*     provided). An error will result if the string supplied is too
*     short to accommodate it.
*     -  If it is not possible to obtain file identification
*     information (for instance, this may occur when accessing some
*     remote files) then fid will be returned set to all blanks. No
*     error will result unless the file also appears not to exist.

*  Implementation Deficiencies:
*     Full shell expansion of the file name is not performed. Hence,
*     environment variable substitution will not occur, although use
*     of file names such as ~user/file is supported.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK. RAL)

*  History:
*     10-MAR-1994 (RFWS):
*        Original version.
*     16-MAR-1994 (RFWS):
*        Added the GETfid and fid arguments.
*     12-MAY-1994 (RFWS):
*        Report an error if the returned file name is truncated.
*     12-MAY-1994 (RFWS):
*        Allow version numbers to be returned (if originally specified) on
*        VMS.
*     20-MAY-1994 (RFWS):
*        Also return version numbers (to uniquely identify the file) on
*        VMS if GETfid is .TRUE..
*     19-JUL-2018 (DSB):
*        Modified to be called from C and renamed to ndf1Expfn. VMS
*        support removed.

*-
*/

/* Local Variables: */
   char *result;              /* Pointer to result string */
   char *tmp;                 /* Temporary variable */
   char cwd[ FILENAME_MAX + 1 ];/* Current working directory buffer */
   size_t len;                /* Length of file name */
   size_t size;               /* Amount of memory to allocate */
   size_t szname;             /* Size of name string */
   struct stat statbuf;       /* File status information buffer */

/* Initialise the returned strings to be null. */
   *out = 0;
   if( getfid ) *fid = 0;

/* Check the inherited global status. */
   if ( *status != SAI__OK ) return;

/* Get a copy of the supplied file name excluding any leading or trailing
   spaces. */
   result = ndf1Strip( NULL, in, 1, 0, &len, NULL, status );

/* If the name is blank, then return a blank result. */
   if( len > 0 ) {

/* If the input file name begins with ~, then expand this into the
   appropriate user's initial working directory name and free the original
   string. */
      if ( result[ 0 ] == '~' ) {
         tmp = ndf1Tilde( result, status );
         (void) astFree( result );
         result = tmp;
      }

/* If the input name is not an absolute file name, then attempt to obtain
   the name of the current working directory to prepend to it.  Check for
   errors. */
      if ( *status == SAI__OK ) {
         if ( result[ 0 ] != '/' ) {
            if( !getcwd( cwd, sizeof( cwd ) ) ) {
               *status = NDF__FATIN;
               emsErrno( "MESSAGE", errno );
               emsRep( " ", "Unable to determine the path name of the "
                       "current working directory - ^MESSAGE", status );

/* Loop to repeatedly remove any redundant occurrences of ./ at the start
   of the relative path name. */
            } else {
               for( szname = strlen( result ); ( szname >= 2 ) &&
                     ( result[ 0 ] == '.' ) && ( result[ 1 ] == '/' );
                     szname -= (size_t) 2 ) {
                  (void) memmove( (void *) result, (void *) ( result + 2 ),
                                  szname - (size_t) 1 );
               }

/* Allocate space to hold the full file name, checking for errors. */
               size = strlen( cwd ) + 1 + szname + 1;
               tmp = (char *) astMalloc( size );
               if( tmp == NULL ) {
                  *status = NDF__NOMEM;
                  emsSeti( "NBYTES", (int) size );
                  emsErrno( "MESSAGE", errno );
                  emsRep( " ", "Unable to allocate a block of ^NBYTES "
                          "bytes of memory - ^MESSAGE", status );

/* If OK, construct the full file name and free the space holding the
   original version. */
               } else {
                  (void) strcpy( tmp, cwd );
                  (void) strcat( tmp, "/" );
                  (void) strcat( tmp, result );
                  result = astFree( result );
                  result = tmp;
               }
            }
         }
      }

/* If OK and a file identification code is required, then obtain file
   status information and check for errors. Zero the file status buffer
   first to avoid any possible problems with junk which may exist within
   structure padding bytes. */
      if( *status == SAI__OK && getfid ) {
         (void) memset( (void *) &statbuf, 0, sizeof( statbuf ) );
         if ( stat( result, &statbuf ) ) {
            *status = NDF__FILNF;
            emsSetnc( "FILE", result, EMS__SZTOK );
            emsErrno( "MESSAGE", errno );
            emsRep( " ", "Unable to obtain file status information for "
                    "file \'^FILE\' - ^MESSAGE", status );

/* Check that the fid string is long enough to accommodate the result.
   Report an error if it is not (demand one extra character so that an
   all-blank result can be distinguished from all possible valid results,
   where trailing characters will be set to zero). */
         } else if ( fid_length < sizeof( statbuf.st_ino ) +
                                  sizeof( statbuf.st_dev ) + 1 ) {
            *status = NDF__TRUNC;
            emsSetnc( "ROUTINE", "NDF1_EXPFN", EMS__SZTOK );
            emsSeti( "LEN", fid_length );
            emsSeti( "RET", (int) ( sizeof( statbuf.st_ino ) +
                                       sizeof( statbuf.st_dev ) +
                                       1 ) );
            emsRep( " ", "Routine ^ROUTINE called with a fid argument "
                    "which is too short (^LEN characters) to accommodate "
                    "the returned file identification (^RET characters) "
                    "- internal programming error.", status );

/* If OK, zero the fid string and copy the file status fields that identify
   the file into the start of it. */
         } else {
            (void) memset( (void *) fid, 0, (size_t) fid_length );
            (void) memcpy( (void *) fid,
                           (const void *) &statbuf.st_ino,
                           sizeof( statbuf.st_ino ) );
            (void) memcpy( (void *) ( fid + sizeof( statbuf.st_ino ) ),
                           (const void *) &statbuf.st_dev,
                           sizeof( statbuf.st_dev ) );
         }
      }

/* If there has been no error, then copy the expanded file name into the
   output buffer, truncating if necessary. */
      ndf1Ccpy( result, out, out_length, status );
   }

/* Free temporary workspace. */
   result = astFree( result );

/* If necessary, call the error tracing function. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Expfn", status );

   return;
}
