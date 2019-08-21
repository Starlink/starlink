#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "prm_par.h"
#include "ndf_ast.h"
#include <string.h>

#define PIXFMT "%3.1f" /* Default format for pixel cordinates */
#define FRAFMT "%5.4f" /* Default format for FRACTION cordinates */
#define SZFMT 2*VAL__SZD /* Max. characters in formatted value */

void ndf1Inifr( NdfACB *acb, AstFrameSet *iwcs, int *status ){
/*
*+
*  Name:
*     ndf1Inifr

*  Purpose:
*     Initialise standard Frames in an NDF's WCS information.

*  Synopsis:
*     void ndf1Inifr( NdfACB *acb, AstFrameSet *iwcs, int *status )

*  Description:
*     This function inspects the Domain attribute of the current Frame of
*     an AST_ FrameSet, a pointer to which is supplied. If the domain
*     identifies the Frame as representing one of the standard coordinate
*     systems associated with an NDF, the Frame's other attributes are
*     initialised so as to apply to that coordinate system.

*  Parameters:
*     acb
*        Pointer to the NDF entry in the ACB.
*     iwcs
*        An AST_ pointer to a FrameSet containing WCS information
*        associated with the NDF.
*     *status
*        The global status.

*  Notes:
*     - No modification occurs if the current Frame is not recognised as
*     corresponding to one of the standard NDF coordinate systems.
*     - It is assumed that the base Frame of the FrameSet corresponds with
*     the NDF's data grid coordinate system, and that the Mapping between
*     that Frame and the current Frame is correctly set up. The values of
*     coordinates transformed from the base to the current Frame may appear
*     in values set for the current Frame"s attributes.

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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfDCB *dcb;          /* Pointer to NDF entry in the DCB */
   char *costr;          /*  Formatted coordinate string */
   char *pntr;           /* Pointer to mapped string */
   char attr[ 20 ];      /* AST attribute name */
   const char *domain;   /* Current Frame domain */
   const char *fmtval;   /* Buffer for formatted value */
   double coord[ NDF__MXDIM ];     /* Coordinates of 1st pixel */
   double ind[ NDF__MXDIM ];/* Indices of 1st pixel */
   hdsdim lbnd[ NDF__MXDIM ];      /* Lower pixel-index bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* Upper pixel-index bounds */
   int iaxis;            /* Loop counter for axes */
   int naxes;            /* Number of current Frame axes */
   int nc;               /* Number of characters in buffer */
   int ndim;             /* Number of NDF dimensions */
   int nin;              /* Number of input coordinates */
   int there;            /* Component present? */


/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Store the indices of the first pixel */
   for( iaxis = 0; iaxis < NDF__MXDIM; iaxis++ ) ind[ iaxis ] = 1.0;

/* Obtain the number of input coordinates and number of current Frame
   axes for the FrameSet supplied. */
   nin = astGetI( iwcs, "Nin" );
   naxes = astGetI( iwcs, "Naxes" );

/* Transform the indices of the first pixel in the NDF into the current
   coordinate system to give the coordinates of the pixel"s
   centre. Then normalise the resulting coordinates. */
   astTranN( iwcs, 1, nin, 1, ind, 1, naxes, 1, coord );
   astNorm( iwcs, coord );

/* Now convert these coordinates into a character string. Start with an
   opening "(" and then loop through each axis of the current Frame. */
   nc = 0;
   costr = astAppendString( NULL, &nc, "(" );
   for( iaxis = 0; iaxis < naxes; iaxis++ ){

/* Format each coordinate value and concatenate the resulting strings,
   separated by commas. */
      if( iaxis > 0 ) costr = astAppendString( costr, &nc, "," );
      fmtval = astFormat( iwcs, iaxis + 1, coord[ iaxis ] );
      if( astChrLen( fmtval ) > 0 ) {
         costr = astAppendString( costr, &nc, fmtval );
      }
   }

/* Append a closing ")". */
   costr = astAppendString( costr, &nc, ")" );

/* Obtain the Domain attribute value for the current Frame. Then
   initialise the Frame's attributes according to this domain. */
   domain = astGetC( iwcs, "Domain" );

/* Grid index coordinates.
   ----------------------- */
   if( *status == SAI__OK && !strcmp( domain, "GRID" ) ) {

/* Set up a suitable Frame title. */
      if( naxes == 1 ) {
         astSet( iwcs, "Title=Data grid index; first pixel at %s", costr );
      } else {
         astSet( iwcs, "Title=Data grid indices; first pixel at %s", costr );
      }

/* For each axis, set up a format, label, symbol and unit value. */
      for( iaxis = 0; iaxis < naxes; iaxis++ ){
         astSet( iwcs, "Format(%d)=%s", iaxis + 1, PIXFMT );
         astSet( iwcs, "Label(%d)=Data grid index %d", iaxis + 1, iaxis + 1 );
         astSet( iwcs, "Symbol(%d)=g%d", iaxis + 1, iaxis + 1 );
         astSet( iwcs, "Unit(%d)=pixel", iaxis + 1 );
      }

/* Pixel coordinate system.
   ------------------------ */
   } else if( *status == SAI__OK && !strcmp( domain, "PIXEL" ) ) {

/* Set up a suitable Frame title. */
      if( naxes == 1 ) {
         astSet( iwcs, "Title=Pixel coordinate; first pixel at %s", costr );
      } else {
         astSet( iwcs, "Title=Pixel coordinates; first pixel at %s", costr );
      }

/* For each axis, set up a format, label, symbol and unit value. */
      for( iaxis = 0; iaxis < naxes; iaxis++ ){
         astSet( iwcs, "Format(%d)=%s", iaxis + 1, PIXFMT );
         astSet( iwcs, "Label(%d)=Pixel coordinate %d", iaxis + 1, iaxis + 1 );
         astSet( iwcs, "Symbol(%d)=p%d", iaxis + 1, iaxis + 1 );
         astSet( iwcs, "Unit(%d)=pixel", iaxis + 1 );
      }

/* Axis coordinate system.
   ----------------------- */
   } else if( *status == SAI__OK && !strcmp( domain, "AXIS" ) ) {

/* Set up a suitable Frame title. */
      if( naxes == 1 ) {
         astSet( iwcs, "Title=Axis coordinate; first pixel at %s", costr );
      } else {
         astSet( iwcs, "Title=Axis coordinates; first pixel at %s", costr );
      }

/* To obtain label and unit strings, we must access the NDF's "axis"
   component. Obtain an index to the NDF entry in the DCB and determine
   how many dimensions it has. */
      dcb = acb->dcb;
      aryBound( dcb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* Loop through all the current Frame axes. */
      for( iaxis = 0; iaxis < naxes; iaxis++ ){

/* Determine if there is a corresponding dimension in the NDF data
   object. */
         there = ( iaxis < ndim );

/* If so, then ensure that DCB information is available for the axis
   label component. Then test the component's locator to see if the
   component exists. */
         if( there ) {
            ndf1Dac( iaxis, NDF__ALAB, dcb, status );
            if( *status == SAI__OK ) there = ( dcb->acloc[ iaxis ][ NDF__ALAB ]
                                               != NULL );
         }

/* If there is no axis label information available, then use a default
   label string. */
         if( *status == SAI__OK ) {
            if( !there ) {
               astSet( iwcs, "Label(%d)=Axis %d", iaxis + 1, iaxis + 1 );

/* Otherwise, determine the component's length and map it for reading. */
            } else {
               pntr = ndf1Hmp0C( dcb->acloc[ iaxis ][ NDF__ALAB ],
                                 status );

/* Set the Frame's axis label to the value of the mapped string. Then
   un-map the label component. */
               if( pntr ) {
                  astSet( iwcs, "Label(%d)=%s", iaxis + 1, pntr );
                  datUnmap( dcb->acloc[ iaxis ][ NDF__ALAB ], status );
                  pntr = astFree( pntr );
               }
            }
         }

/* Repeat the process above to set a value for the axis unit
   attribute... */

/* Check if units available. */
         there = ( iaxis < ndim );

         if( there ) {
            ndf1Dac( iaxis, NDF__AUNI, dcb, status );
            if( *status == SAI__OK ) there = ( dcb->acloc[ iaxis ][ NDF__AUNI ]
                                               != NULL );
         }

/* Supply a suitable default format and a default unit of "pixel", but
   only if the axis component is absent. If it is present, clear these
   attributes, since the physical units are unknown (and may not be
   pixels). */
         if( *status == SAI__OK ) {
            if( !there ) {
               if( iaxis >= ndim ) {
                  astSet( iwcs, "Format(%d)=%s", iaxis + 1, PIXFMT );
                  astSet( iwcs, "Unit(%d)=pixel", iaxis + 1 );
               } else if( !dcb->aloc[ iaxis ] ) {
                  astSet( iwcs, "Format(%d)=%s", iaxis + 1, PIXFMT );
                  astSet( iwcs, "Unit(%d)=pixel", iaxis + 1 );
               } else {
                  sprintf( attr, "Format(%d)", iaxis + 1 );
                  astClear( iwcs, attr );
                  sprintf( attr, "Unit(%d)", iaxis + 1 );
                  astClear( iwcs, attr );
               }

/* Map the units string. */
            } else {
               pntr = ndf1Hmp0C( dcb->acloc[ iaxis ][ NDF__AUNI ],
                                 status );

/* Use the string and unmap it. */
               if( pntr ) {
                  astSet( iwcs, "Unit(%d)=%s", iaxis + 1, pntr );
                  datUnmap( dcb->acloc[ iaxis ][ NDF__AUNI ], status );
                  pntr = astFree( pntr );
               }
            }
         }

/* Set an axis symbol value for the Frame. */
         if( *status == SAI__OK ) {
            astSet( iwcs, "Symbol(%d)=a%d", iaxis + 1, iaxis + 1 );
         }
      }

/* Normalised pixel coordinate system.
   ----------------------------------- */
   } else if( *status == SAI__OK && !strcmp( domain, "FRACTION" ) ) {

/* Set up a suitable Frame title. */
      if( naxes == 1 ) {
         astSet( iwcs, "Title=Normalised pixel coordinate; first "
                  "pixel at %s", costr );
      } else {
         astSet( iwcs, "Title=Normalised pixel coordinates; first "
                  "pixel at %s", costr );
      }

/* For each axis, set up a format, label, symbol and unit value. */
      for( iaxis = 0; iaxis < naxes; iaxis++ ){
         astSet( iwcs, "Format(%d)=%s", iaxis + 1, FRAFMT );
         astSet( iwcs, "Label(%d)=Normalised pixel coordinate %d", iaxis + 1, iaxis + 1 );
         astSet( iwcs, "Symbol(%d)=f%d", iaxis + 1, iaxis + 1 );
         astSet( iwcs, "Unit(%d)= ", iaxis + 1 );
      }
   }

/* Free dynamically allocated strings. */
   costr = astFree( costr );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Inifr", status );

}

