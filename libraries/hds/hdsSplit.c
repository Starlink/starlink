#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "star/mem.h"
#include "star/util.h"
#include "ems.h"
#include "hds.h"
#include "sae_par.h"
#include "dat_par.h"
#include "dat_err.h"
#include "chr.h"

void hdsSplit( const char *name, size_t *f1, size_t *f2, size_t *p1,
               size_t *p2, int *status ){
/*
*+
*  Name:
*     hdsSplit

*  Purpose:
*     Split an HDS object name into a file name and a path name.

*  Synopsis:
*     void hdsSplit( const char *name, size_t *f1, size_t *f2, size_t *p1,
*                    size_t *p2, int *status )

*  Description:
*     This function analyses a general HDS object name and locates the
*     substrings which specify the container file name and the path name of
*     the object within the file.

*  Parameters:
*     name
*        Pointer to a null terminated string holding the HDS object name to
*        be analysed.
*     *f1
*        Returned holding the zero-based character position of the start of
*        the file name.
*     *f2
*        Returned holding the zero-based character position of the end of
*        the file name.
*     *p1
*        Returned holding the zero-based character position of the start of
*        the path name.
*     *p2
*        Returned holding the zero-based character position of the end of
*        the path name.
*     *status
*        The global status.

*  Notes:
*     -  If the function succeeds, then "f1" and "f2" will always identify
*     the container file name.
*     -  If the object describes a top-level object, then there will be no
*     path name. In this case, "p2" will be returned greater than "p1".
*     Otherwise, "p1" and "p2" will identify the path name.
*     -  This function performs some checks on the validity of the object
*     name supplied, but these are not comprehensive. Only an attempt to
*     locate the object will fully validate the name.
*     -  Any blank characters which surround the file or path names will be
*     excluded from the returned character string positions.
*     - If the string begins with a quote, it is assumed that the name is
*     quoted and the component part follows. eg "test.sdfx".MORE will
*     result in a filename called test.sdfx and a .MORE component.
*     - If the first component after the root is ".sdf" this will be
*     absorbed into the filename (which HDS can open without problem)
*     unless there is a component at the top level of the HDS file called
*     "SDF". If the file can not be opened by HDS the ".sdf" will be
*     assumed to be part of the filename. This approach is not full proof
*     since "hdsSplit" is not always called with a full path to a valid
*     file. In generaly the best place for disambiguating would be the
*     caller but this function is used in places other than "hdsFind" so it
*     is better to absorb the overhead. The HDS open will only occur for
*     the .sdf case. The earlier note comments that some validation occurs
*     but not all, this is probably at odds with that sentiment.

*  Machine-specific features used:
*     This function unavoidably has to make assumptions about the format of
*     VAX/VMS and POSIX file names.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful, but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     21-MAY-2018 (DSB):
*        Original version, based on equivalent Fortran function by RFWS
*        et al.
*     20-JAN-2019 (DSB):
*        - Initialise returned values before checking inherited status.
*        - Return immediately if the supplied string pointer is NULL.
*-
*/

/* Local Variables: */
   HDSLoc *testloc = NULL;/* Test locator for .sdf disambiguation */
   char *lname;          /* Supplied name without trailing or leading spaces */
   char *p;              /* Pointer to matching character */
   char flext[ DAT__SZFLX + 1 ];/* Local copy of DAT__FLEXT */
   char tc;              /* Terminating character */
   hdsbool_t there;      /* Did we have a SDF component? */
   int delta;            /* .SDF correction */
   int i1;               /* First INDEX result */
   int i2;               /* Second INDEX result */
   int iend;             /* Position of first char following file name */
   int lstat;            /* Local status */
   size_t f;             /* Position of first non-blank character */
   size_t istart;        /* Position of start of path name */
   size_t l;             /* Position of last non-blank character */
   size_t nc;            /* Number of characters to copy */
   size_t offset;        /* Offset into string */

/* Initialise */
   *p1 = 1;
   *p2 = 0;
   *f1 = 1;
   *f2 = 0;

/* Check inherited global status and supplied string. */
   if( *status != SAI__OK || !name ) return;

/* Create a null-terminated copy of the object name that excludes any
   leading or trailing spaces, reporting an error if the name is blank. */
   chrFandl( name, &f, &l );
   if( f > l ) {
      *status = DAT__NAMIN;
      emsRep( " ", "Blank HDS name supplied.", status );
   } else {
      nc = l - f + 1;
      lname = starMalloc( nc + 1 );
      if( lname ) {
         memcpy( lname, name + f, nc );
         lname[ nc ] = 0;
      } else if( *status == SAI__OK ) {
         emsRepf( " ", "Failed to allocate %zu bytes of memory", status, nc + 1 );
      }
   }

/* If the first non-blank character is a quote.
   =========================================== */
   if( *status == SAI__OK ) {

/* If the name starts with a quote, search for the closing quote. */
      if( lname[ 0 ] == '"' ) {
         iend = 0;
         if( nc > 1 ) {
            p = strchr( lname + 1, '"' );
            if( p ) iend = p - lname;
         }

/* If the closing quote is missing, then report an error. */
         if( iend == 0 ) {
            *status = DAT__NAMIN;
            emsRepf( "", "Missing quote in the HDS name '%s'.", status, name );

/* If the quotes are consecutive, then report an error. */
         } else if( iend == 1 ) {
            *status = DAT__NAMIN;
            emsRepf( "", "File name absent in the HDS name '%s'.", status, name );

/* Otherwise, find the first and last non-blank characters in the
   filename which appears between the quotes. */
         } else {
            lname[ 0 ] = ' ';
            lname[ iend ] = 0;
            chrFandl( lname, f1, f2 );

/* If the file name is blank, then report an error. */
            if( *f1 > *f2 ) {
               lname[ 0 ] = '"';
               lname[ iend ] = '"';
               *status = DAT__NAMIN;
               emsRepf( "", "Quoted filename is blank in the HDS "
                        "name '\"%s\"'.", status, name );

/* Otherwise, derive the position of the file name string. */
            } else {
               *f1 += f;
               *f2 += f;
            }
         }

/* If there have been no errors, then attempt to find the position of
   the path name string. */
         if( *status == SAI__OK ) {

/* If the file name extends to the end of the object name string, then
   there is no path name (i.e. it is a top-level object), so return
   a "null" position. */
            if( iend >= l ) {
               *p1 = 1;
               *p2 = 0;

/* Otherwise, find the first and last non-blank character positions in
   whatever follows the file name. Terminate the string to exclude any
   trailing spaces. */
            } else {
               chrFandl( lname + iend + 1, &istart, &l );
               istart += iend + 1;
               l += iend + 1;
               lname[ l + 1 ] = 0;

/* If this candidate path name does not start with a "." or a "(", then
   there is a top-level name present. This must be ignored (the actual
   file name fills this role). */
               if( lname[ istart ] != '.' && lname[ istart ] != '(' ) {

/* Find the first following occurrence of a "." or "(". */
                  p = strchr( lname + istart, '.' );
                  i1 = p ? ( p - lname ) : 0;
                  p = strchr( lname + istart, '(' );
                  i2 = p ? ( p - lname ) : 0;
                  if( i1 == 0 || ( i2 != 0 && i2 < i1 ) ) i1 = i2;

/* Move the starting position to the character found, or beyond the
   last non-blank character if a "." or "(" was not found. */
                  if( i1 != 0 ) {
                     istart = i1;
                  } else {
                     istart = l + 1;
                  }
               }

/* If we are beyond the end of the input name string, then only a
   top-level object name is present. Ignore it (i.e. return a "null"
   path name position). */
               if( istart > l ) {
                  *p1 = 1;
                  *p2 = 0;

/* Otherwise return the path name limits. */
               } else {
                  *p1 = istart + f;
                  *p2 = l + f;
               }
            }
         }

/* If the file name is not quoted.
   ============================== */

/* Its end must be detected. First determine which operating system is
   in use, as this determines the file name format. */
      } else {

/* If we are on VMS, then search for a "]" or ">" character, which may
   mark the end of an explicit directory specification (note the latter
   may result if a file name is entered on a DCL command line). */
#if defined( vms )
         p = strchr( lname, ']' );
         iend = p ? p - lname : -1;
         if( iend == -1 ) {
            p = strchr( lname, '>' );
            iend = p ? p - lname : -1;
         }

/* If there is no explicit directory reference, then search for a ":"
   which may mark the end of a logical name representing the directory.
   Also search for a "(". The ":" can be used only if it occurs before
   the "(", otherwise it is part of a subscript expression. */
         if( iend == -1 ) {
            p = strchr( lname, ':' );
            i1 = p ? p - lname : -1;
            p = strchr( lname, '(' );
            i2 = p ? p - lname : -1;
            if( ( i2 == -1 ) || ( i1 < i2 ) ) iend = i1;
         }

/* If we are not on VMS, then assume POSIX file name format. Search for
   the last "/" character which delimits the final field of the file's
   path name. */
#else
         p = strrchr( lname, '/' );
         iend = p ? p - lname: -1;
#endif

/* Having located the end of the directory specification (if any), now
   search for a "." or a "(" which marks the end of the first field in
   the object path name. */
         p = strchr( lname + iend + 1, '.' );
         i1 = p ? p - lname : -1;
         p = strchr( lname + iend + 1, '(' );
         i2 = p ? p - lname : -1;
         if( i1 == -1 || ( i2 != -1 && i2 < i1 ) ) i1 = i2;

/* If an ending character was found above, replace it with a null
   character in order to terminate the file name. Otherwise the whole
   string is the file name and so we leave it unchanged. */
         if( i1 < 0 ) {
            iend = nc;
         } else {
            iend = i1;
            tc = lname[ iend ];
            lname[ iend ] = 0;
         }

/* If a "." occurs in the first character position, then report an
   error. */
         if( iend == 0 ) {
            *status = DAT__NAMIN;
            emsRepf( " ", "Missing field in the HDS name '%s'.", status, name );

/* Otherwise, note the file name position, omitting any trailing blanks. */
         } else {
            *f1 = f;
            *f2 = f + chrLen( lname ) - 1;
         }

/* Re-instate the character that was replaced by a null above. */
         if( i1 >= 0 ) lname[ iend ] = tc;

/* Attempt to remove .sdf from the path and into the filename. Need to look
   for 4 characters after the end of the filename. The main problem here is
   that we have to *assume* that there is no SDF component OR we have to
   actually look... Have to watch out for .sdfx components. */
         offset = iend + DAT__SZFLX;
         if( offset <= nc && chrSimlrN( lname + iend, DAT__FLEXT, DAT__SZFLX ) ) {

/* The path starts with ".sdf". This can only be part of the file name if
   ".sdf" is followed by a character that marks the end of the file name
   (' ', '.', '(' or end-of-string). For instance, if the path starts with
   ".sdfgroup" then we cannot use the leading ".sdf" as part of the file
   name. */
            delta = 0;
            if( offset < nc ) {
               if( lname[ offset ] == ' ' ||
                   lname[ offset ] == '.' ||
                   lname[ offset ] == '(' ) {

/* Next character is space, . or paren so we are safe calling this .sdf */
                  delta = DAT__SZFLX;
               }

/* ".sdf" is at end of string so absorb it */
            } else {
               delta = DAT__SZFLX;
            }

/* This is where we can really tell the difference with the .SDF component
   vs .sdf file extension if we want to be rock solid. We assume that if
   the file can not be opened, that the .sdf is part of the file name. Move
   this IF to hdsFind if we are uncomfortable with the assumptions here. */
            if( delta != 0 ) {
               emsMark();
               lstat = SAI__OK;

               if( i1 >= 0 ) lname[ iend ] = 0;
               hdsOpen( lname, "READ", &testloc, &lstat );
               if( i1 >= 0 ) lname[ iend ] = tc;

               if( lstat == SAI__OK ) {

/* File opened okay, so now disambiguate (taking substring requires local
   buffer). */
                  star_strlcpy( flext, DAT__FLEXT, sizeof( flext ) );
                  datThere( testloc, flext + 1, &there, &lstat );
                  if( there ) delta = 0;
                  datAnnul( &testloc, &lstat );
                  if( lstat != SAI__OK ) emsAnnul( &lstat );

/* Could not open file (assume that .sdf is part of the filename rather than
   a component */
               } else {
                  emsAnnul( &lstat );
               }
               emsRlse();
            }

            *f2 += delta;
            iend += delta;
         }

/* If no errors have occurred, then attempt to locate the path name
   string. */
         if( *status == SAI__OK ) {

/* If the file name extends to the end of the object name string, then
   there is no path name (i.e. it is a top-level object), so return
   a "null" position. */
            if( iend >= nc ) {
               *p1 = 1;
               *p2 = 0;

/* Otherwise, eliminate any leading blanks from the candidate path name
   which follows the file name and return the path name limits. */
            } else {
               chrFandl( lname + iend, &istart, &l );
               *p1 = istart + iend + f;
               *p2 = l + iend + f;
            }
         }
      }

/* Free the local copy of the supplied name. */
      starFree( lname );
   }
}

