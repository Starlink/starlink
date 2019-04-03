#include "dat_par.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/cmp.h"
#include "star/util.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void ndf1Hwrt( NdfDCB *dcb, const char *appn, int nlines,
               int linelen, const char *text, int *status ){
/*
*+
*  Name:
*     ndf1Hwrt

*  Purpose:
*     Write text lines to the history component of an NDF.

*  Synopsis:
*     void ndf1Hwrt( NdfDCB *dcb, const char *appn, int nlines,
*                    int linelen, const char *text, int *status )

*  Description:
*     This function writes lines of text to the history component of an
*     NDF. If the history has not yet been modified by the current
*     application (and "appn" is not "<"append">"), it creates a new
*     history record, initialises it, and inserts the text suppled. If the
*     history has already been modified (or if "appn" is "<"append">"),
*     then the new text is simply appended to any already present. The
*     function returns without action if the NDF does not have a history
*     component.

*  Parameters:
*     dcb
*        Pointer to the NDF whose history is to be modified.
*     appn
*        Pointer to a null terminated string holding the name of the
*        application. This is only used (to initialise the new history
*        record) if the history has not yet been modified by the current
*        application, otherwise it is ignored. If a blank value is given,
*        then a suitable default will be used instead. The special value
*        "<"append">" may be supplied in order to append the text lines to
*        the current history text even if the  history has not yet been
*        modified by the current application.
*     nlines
*        Number of new lines of text to be added to the history record
*        (must be at least 1).
*     linelen
*        The length of each spaced-padded fixed-length string in text.
*     text
*        Pointer to a character array holding the input text lines to be
*        copied. It's length should be at least "nlines*linelen". Each line
*        of text should be space-padded to occupy a length of "linelen"
*        characters. Note, there should be no terminating null characters
*        in this array.
*     *status
*        The global status.

*  Notes:
*     -  If a new history record is being initialised, then the text length
*     (characters per line) of the new record is determined by the length
*     of the elements of the "text" array. If the history record has
*     already been written to, then the existing text length is not altered
*     and assignment of new values takes place using Fortran character
*     assignment rules. No error occurs if text is truncated.
*     -  This function does not perform any formatting or translation
*     operations on the text supplied.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   HDSLoc *cell = NULL;  /* Locator for array sell */
   HDSLoc *loc = NULL;   /* Temporary locator */
   HDSLoc *tloc = NULL;  /* Locator for new text lines */
   NdfUNAME info;        /* System info */
   char *lappn;          /* Pointer to local application name */
   char *pntr;           /* Pointer to mapped text lines */
   char lappn_buf[ NDF__SZAPP + 1 ];/* Local application name */
   char ref[ NDF__SZREF + 1 ];     /* Dataset reference name */
   char time[ NDF__SZHDT + 1 ];    /* Formatted creation time string */
   char user[ NDF__SZUSR + 1 ];    /* User name */
   float sec;            /* Seconds field value */
   hdsdim dim;           /* Object dimension size */
   hdsdim sub;           /* Array subscript */
   int append;           /* Append text to current record? */
   int init;             /* Initialise new history record? */
   int ymdhm[ 5 ];       /* Integer date/time field values */
   size_t clen;          /* Length of character object */
   size_t el;            /* Number of array elements mapped */
   size_t l;             /* Last significant character position */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that history information is available in the DCB. */
   ndf1Dh( dcb, status );
   if( *status == SAI__OK ) {

/* Check if a history component is present, otherwise there is nothing
   more to do. */
      if( dcb->hloc ) {

/* See if we should append the text to the current history record. */
         append = ( !strcmp( appn, "<APPEND>" ) && dcb->hnrec > 0 );

/* Note if a new history record must be initialised. This will be so if
   history has not yet been modified by the current application so that
   the current record's text length is still zero, and we are not
   appending to the current history record. */
         init = ( ( dcb->htlen == 0 ) && !append );

/* Obtain a new (empty) history record structure if necessary. */
         if( init ) {
            ndf1Hincr( dcb, status );

/* If OK, record the text length associated with the new history
   record. */
            if( *status == SAI__OK ) dcb->htlen = linelen;
         }

/* IF OK, obtain a locator to the current element of the history record
   structure array. */
         if( *status == SAI__OK ) {
            sub = dcb->hnrec;
            datCell( dcb->hrloc, 1, &sub, &cell, status );

/* If this is a new history record structure, then it must be
   initialised. */
            if( init ) {

/* Ensure that it is empty (we may be re-using a structure which
   previously held information which was not properly deleted). */
               ndf1Hrst( cell, status );

/* If a date/time for the history record has been specified using
   ndfHdate then convert it to separate fields, as needed by ndf1Fmhdt. */
               if( dcb->htime > 0.0 ) {
                  ndf1Mjd2t( dcb->htime, ymdhm, &sec, status );

/* The supplied date may not be in chronological order, so indicate that
   we need to sort the records before closing the NDF. */
                  dcb->hsort = 1;

/* Otherwise get the current UTC time fields. */
               } else {
                  ndf1Time( ymdhm, &sec, status );
               }

/* Convert these fields to standard history format. */
               ndf1Fmhdt( ymdhm, sec, time, sizeof( time ), status );

/* Create a DATE component in the history record and write the date/time
   string to it. */
               datNew0C( cell, "DATE", NDF__SZHDT, status );
               cmpPut0C( cell, "DATE", time, status );

/* Determine the significant length of the application name. Use the
   name supplied if it is not blank, otherwise use the default stored
   in the DCB. If this is also blank, then determine the default
   application name locally and use that. */
               lappn = ndf1Strip( NULL, appn, 1, 0, &l, NULL, status );
               if( l > 0 ) {
                  /* Use lappn as-is */

               } else if( Ndf_DCB_happn[0] != 0 ) {
                  lappn = ndf1Strip( lappn, Ndf_DCB_happn, 1, 0, &l,
                                     NULL, status );
               } else {
                  ndf1Getap( lappn_buf, sizeof( lappn_buf ), status );
                  (void) astFree( lappn );
                  lappn = lappn_buf;
                  l = astChrLen( lappn );
               }

/* Create a COMMAND component in the history record with the required
   length and write the application name to it. */
               l = NDF_MAX( 1, l );
               datNew0C( cell, "COMMAND", l, status );
               cmpPut0C( cell, "COMMAND", lappn, status );

/* Obtain the user name and determine its length. Create a "user"
   component in the history record and write the name to it. */
               ndf1Cuserid( user, sizeof(user), status );
               l = strlen( user );
               datNew0C( cell, "USER", l, status );
               cmpPut0C( cell, "USER", user, status );

/* Obtain the host machine node name and determine its length. Create a
   HOST component in the history record and write the name to it. */
               ndf1Uname( &info, status );
               l =  NDF_MAX( 1, strlen( info.nodename ) );
               datNew0C( cell, "HOST", l, status );
               cmpPut0C( cell, "HOST", info.nodename, status );

/* Obtain a reference name for the dataset being processed and determine
   its length. Use the name of the foreign format file if there is one
   associated with the NDF. */
               if( !dcb->fcb ) {
                  datRef( dcb->loc, ref, sizeof(ref), status );
               } else {
                  star_strlcpy( ref, dcb->forfl, sizeof( ref ) );
               }
               l = NDF_MAX( 1, astChrLen( ref ) );

/* Create a DATASET component in the history record and write the
   dataset name to it. */
               datNew0C( cell, "DATASET", l, status );
               cmpPut0C( cell, "DATASET", ref, status );

/* Create a new "text" array in the history record with the required
   number of lines and characters per line. */
               clen = linelen;
               datNew1C( cell, "TEXT", clen, nlines, status );
               datFind( cell, "TEXT", &tloc, status );

/* If we are appending to an existing history record, then obtain a
   locator for the "text" array and determine its size. */
            } else {
               datFind( cell, "TEXT", &loc, status );
               datSize( loc, &el, status );
               dim = el;

/* Increase its size by the required number of lines. */
               dim += nlines;
               datAlter( loc, 1, &dim, status );

/* Obtain a locator to a slice of the "text" array composed of the new
   lines just added and annul the locator to the whole array. */
               sub = dim - nlines + 1;
               datSlice( loc, 1, &sub, &dim, &tloc, status );
               datAnnul( &loc, status );

/* If we are appending to the current history record, the length recorded
   in the DCB may still be zero. If this is the case, record the length of
   the text already in the history record. */
               datClen( tloc, &clen, status );
               if( dcb->htlen == 0 ) dcb->htlen = clen;
            }

/* Map the new lines in the "text" array for writing and copy the history
   text into them. */
            datMapV( tloc, "_CHAR", "WRITE", (void **) &pntr, &el, status );
            if( *status == SAI__OK ) ndf1Hcpy( nlines, dcb->htlen, pntr,
                                               text, status );

/* Annul the "text" array locator, thus unmapping the new lines. */
            datAnnul( &tloc, status );

/* Annul the locator for the current history record. */
            datAnnul( &cell, status );
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Hwrt", status );

}
