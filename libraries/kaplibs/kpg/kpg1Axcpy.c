#include "sae_par.h"
#include "prm_par.h"
#include "kaplibs.h"
#include "mers.h"
#include <string.h>

/* Use 8-byte NDF interface */
#define NDF_I8 1
#include "ndf.h"

void kpg1Axcpy( int indf1, int indf2, int ax1, int ax2, int *status ){
/*
*+
*  Name:
*     kpg1Axcpy

*  Purpose:
*     Copies an NDF AXIS structure from one NDF to another.

*  Language:
*     C.

*  Invocation:
*     void kpg1Axcpy( int indf1, int indf2, int ax1, int ax2, int *status )

*  Description:
*     This function copies all AXIS information describing axis "ax1" in
*     "indf1", into "indf2" axis "ax2".

*  Arguments:
*     indf1
*        The source NDF.
*     indf2
*        The destination NDF.
*     ax1
*        The one-based index of the axis to be copied within "ndf1".
*        Must be less than or equal to the number of pixel axes in "ndf1".
*     ax2
*        The one-based index of the new axis to create within "ndf2".
*        Must be less than or equal to the number of pixel axes in "ndf2".
*     status
*        The inherited status.

*  Copyright:
*     Copyright (C) 2010, 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     MJC: Malcolm J. Currie (JAC, Hawaii)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     19-AUG-2010 (DSB):
*        Original version.
*     2012 April 18 (MJC):
*        Test for character-component existence.
*     2012-05-09 (TIMJ):
*        Add _INT64
*     4-OCT-2019 (DSB):
*        Use 8-byte NDF interface.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Constants: */
#define CHARSIZE 200   /* Maximum length of an AXIS character component */
#define NACOMP 3       /* Number of AXIS array components */
#define NCCOMP 2       /* Number of AXIS character components */

/* Local Variables: */
   char cval[ CHARSIZE ];
   char type[ NDF__SZTYP + 1 ];
   const char *acomp[ NACOMP ] = { "Centre", "Variance", "Width" };
   const char *ccomp[ NCCOMP ] = { "Label", "Unit" };
   hdsdim el;
   hdsdim lbnd1[ NDF__MXDIM ];
   hdsdim lbnd2[ NDF__MXDIM ];
   hdsdim ubnd1[ NDF__MXDIM ];
   hdsdim ubnd2[ NDF__MXDIM ];
   int icomp;
   int indf3;
   int ndim;
   int there;
   size_t size;
   void *pntr1;
   void *pntr2;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* First copy all the character components. */
   cval[ 0 ] = 0;
   for( icomp = 0; icomp < NCCOMP; icomp++ ) {
      ndfAstat( indf1, ccomp[ icomp ], ax1, &there, status );
      if ( there == 1 ) {
         ndfAcget( indf1, ccomp[ icomp ], ax1, cval, CHARSIZE, status );
         ndfAcput( cval, indf2, ccomp[ icomp ], ax2, status );
      }
   }

/* Get the output NDF pixel bounds. */
   ndfBound( indf2, NDF__MXDIM, lbnd2, ubnd2, &ndim, status );

/* Get the input NDF pixel bounds. */
   ndfBound( indf1, NDF__MXDIM, lbnd1, ubnd1, &ndim, status );

/* Change the bounds on the required input axis to match those of the
   output axis. */
   lbnd1[ ax1 ] = lbnd2[ ax2 ];
   ubnd1[ ax1 ] = ubnd2[ ax2 ];

/* Get a section of the input NDF with these bounds. */
   ndfSect( indf1, ndim, lbnd1, ubnd1, &indf3, status );

/* Now copy all the array components. */
   for( icomp = 0; icomp < NACOMP; icomp++ ) {

/* Get the array data type. */
      ndfAtype( indf3, acomp[ icomp ], ax1, type, NDF__SZTYP + 1, status );

/* Map the input and output arrays. */
      ndfAmap( indf3, acomp[ icomp ], ax1, type, "Read", &pntr1, &el, status );
      ndfAmap( indf2, acomp[ icomp ], ax2, type, "Write", &pntr2, &el, status );

/* Get the number of bytes in a single element of the array. */
      if( !strcmp( type, "_DOUBLE" ) ) {
         size = VAL__NBD;
      } else if( !strcmp( type, "_REAL" ) ) {
         size = VAL__NBR;
      } else if( !strcmp( type, "_INTEGER" ) ) {
         size = VAL__NBI;
      } else if( !strcmp( type, "_INT64" ) ) {
         size = VAL__NBK;
      } else if( !strcmp( type, "_WORD" ) ) {
         size = VAL__NBW;
      } else if( !strcmp( type, "_UWORD" ) ) {
         size = VAL__NBUW;
      } else if( !strcmp( type, "_BYTE" ) ) {
         size = VAL__NBB;
      } else if( !strcmp( type, "_UBYTE" ) ) {
         size = VAL__NBUB;
      } else if( *status == SAI__OK ) {
         msgSetc( "T", type );
         errRep( "", "kpg1Axcpy: Unknown NDF data type '^T' - programming "
                 "error.", status );
      }

/* If no error has occurred, copy the axis data from input NDF (section)
   to output NDF. */
      if( *status == SAI__OK ) {
         memcpy( pntr2, pntr1, el*size );
      } else {
         break;
      }

/* Unmap the input and output arrays. */
      ndfAunmp( indf3, acomp[ icomp ], ax1, status );
      ndfAunmp( indf2, acomp[ icomp ], ax2, status );
   }

/* Annul the input section NDF. */
   ndfAnnul( &indf3, status );
}
