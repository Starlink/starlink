#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Ncut( NdfACB *acb1, const char *str, NdfACB **acb2, int *status ){
/*
*+
*  Name:
*     ndf1Ncut

*  Purpose:
*     Cut a section specified by a character string from an NDF.

*  Synopsis:
*     void ndf1Ncut( NdfACB *acb1, const char *str, NdfACB **acb2, int *status )

*  Description:
*     This function creates a section from an NDF, generating a new ACB
*     entry describing the new NDF.  The dimension bounds defining the
*     section are supplied as a parenthesised character string via the
*     "str" parameter (e.g.  "(256,256)", "(,,~3)", "(3.5:5,8:)" or
*     "(,7.0~100,,:6)", etc.  If this string is blank, then the function
*     returns an ACB entry describing the original NDF by cloning the ACB
*     entry given.

*  Parameters:
*     acb1
*        Pointer to the input ACB entry.
*     str
*        Pointer to a null terminated string holding the section bounds
*        expression.
*     *acb2
*        Pointer to the output ACB entry.
*     *status
*        The global status.

*  Notes:
*     -  The syntax of the "str" string will be fully validated by this
*     function. It must contain enclosing parentheses unless it is
*     completely blank.
*     -  If this function is called with "status" set, then a value of zero
*     will be returned for "acb2". The same value will also be returned if
*     the function should fail for any reason.

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
   AstFrameSet *iwcs;    /* AST pointer to WCS FrameSet */
   AstMapping *wcsmap;   /* AST pointer to WCS Mapping */
   char *lstr;           /* Local copy of dimension bounds string */
   const char *dom;      /* Pointer to Domain string */
   double dflbnd[ NDF__MXDIM ];    /* Default lower bounds */
   double dfubnd[ NDF__MXDIM ];    /* Default upper bounds */
   double glbnd[ NDF__MXDIM ];     /* Lower GRID bound */
   double gubnd[ NDF__MXDIM ];     /* Upper GRID bound */
   double value1[ NDF__MXDIM ];    /* First bound specifier */
   double value2[ NDF__MXDIM ];    /* Second bound specifier */
   double xl[ NDF__MXDIM ];        /* GRID coords at min WCS axis value */
   double xu[ NDF__MXDIM ];        /* GRID coords at max WCS axis value */
   hdsdim lbnd[ DAT__MXDIM ];      /* Lower dimension bounds */
   hdsdim lbndd[ NDF__MXDIM ];     /* NDF lower pixel bounds */
   hdsdim ubnd[ DAT__MXDIM ];      /* Upper dimension bounds */
   hdsdim ubndd[ NDF__MXDIM ];     /* NDF upper pixel bounds */
   int frame1[ NDF__MXDIM ];       /* Frame used by VALUE1 */
   int frame2[ NDF__MXDIM ];       /* Frame used by VALUE2 */
   int i;                /* Loop counter for dimensions */
   int isbnd[ NDF__MXDIM ];        /* Are VALUEs explicit bounds? */
   int isdef1[ NDF__MXDIM ];       /* Is VALUE1 a defalut value? */
   int isdef2[ NDF__MXDIM ];       /* Is VALUE2 a defalut value? */
   int nax;              /* No. of axes in chosen coord system */
   int ndim;             /* Number of section dimensions */
   int ndimd;            /* Input NDF number of dimensions */
   int usewcs;           /* Use WCS instead of AXIS? */
   int wcssec;           /* Use WCS section syntax? */
   size_t slen;          /* Length of dimensions bounds string */

/* Initialise the returned ACB index. */
   *acb2 = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Create a copy of the dimension bounds string excluding any leading
   or trailing spaces. */
   lstr = ndf1Strip( NULL, str, 1, 0, NULL, NULL, status );
   if( *status == SAI__OK ) {

/* If the string is blank, then simply clone the ACB entry. */
      slen = strlen( lstr );
      if( slen == 0 ) {
         ndf1Cln( acb1, acb2, status );

/* Otherwise, check that the string has enclosing parentheses and report
   an error if it does not. */
      } else if( lstr[ 0 ] != '(' || lstr[ slen - 1 ] != ')' ) {
         *status = NDF__BNDIN;
         ndf1Amsg( "NDF", acb1 );
         errRepf( " ", "Invalid section '%s' specified for the NDF ^NDF "
                  "-- enclosing parenthesis missing.", status, lstr );

/* Otherwise, decide if the section is given in terms of pixel/axis
   coordinates or WCS coordinates. */
      } else {

/* If the opening parenthesis is followed by an asterisk  "*" the section
   refers purely to WCS coords (in which remove the asterisk). Otherwise,
   it may contain a mix of axis/wcs coords and pixel indices. */
         if( lstr[1] == '*' ) {
            wcssec = 1;
            lstr[1] = ' ';
            ndf1Rmblk( lstr );
         } else {
            wcssec = 0;
         }

/* Get the pixel bounds of the NDF. */
         aryBound( acb1->did, NDF__MXDIM, lbndd, ubndd, &ndimd, status );

/* Get the WCS FrameSet. */
         ndf1Rdwcs( acb1, &iwcs, status );

/* If we are using WCS syntax, the default bounds are the WCS values at
   the edges of the bounding box containing the whole NDF. */
         if( wcssec ) {

/* Store the bounds of the NDF in double precision GRID coords. */
            for( i = 0; i < ndimd; i++ ){
               glbnd[ i ] = 0.5;
               gubnd[ i ] = (double)( ubndd[ i ] - lbndd[ i ] ) + 1.5;
            }

/* For efficiency, extract the Mapping from the WCS FrameSet and pass it
   to "astMapBox". */
            wcsmap = astGetMapping( iwcs, AST__BASE, AST__CURRENT );

/* Get the corresponding bounds on each WCS axis. These are used as the
   defaults for any bounds that are not specified in the supplied string. */
            nax = astGetI( iwcs, "Nout" );
            for( i = 0; i < nax; i++ ){
               astMapBox( wcsmap, glbnd, gubnd, 1, i + 1, dflbnd + i,
                          dfubnd + i, xl, xu );
            }

/* Annul the WCS Mapping */
            wcsmap = astAnnul( wcsmap );

/* If we are not using WCS syntax, the default bounds are the pixel
   indices at the edges of the NDF. */
         } else {
            nax = ndimd;
            for( i = 0; i < nax; i++ ){
               dflbnd[ i ] = (double)( lbndd[ i ] );
               dfubnd[ i ] = (double)( ubndd[ i ] );
            }

/* If the current WCS Frame is the AXIS Frame, we interpret non-integer
   values using the AXIS-based code written by RFWS. Otherwise we use new
   WCS-based code. */
            dom = astGetC( iwcs, "Domain" );
            if( dom ) usewcs = strcmp( dom, "AXIS" ) ? 1 : 0;
         }

         if( *status == SAI__OK ) {

/* Remove the enclosing parentheses (supply a blank bounds expression
   if "()" was specified) and parse the dimension bounds expression. */
            if( ( !wcssec && !strcmp( lstr, "()" ) ) ||
                ( wcssec && !strcmp( lstr, "(*)" ) ) ) {
               ndf1Psnde( " ", nax, dflbnd, dfubnd, iwcs, wcssec, value1,
                          value2, &ndim, frame1, frame2, isbnd, isdef1,
                          isdef2, status );
            } else {
               lstr[ slen - 1 ] = 0;
               ndf1Psnde( lstr + 1, nax, dflbnd, dfubnd, iwcs, wcssec, value1,
                          value2, &ndim, frame1, frame2, isbnd, isdef1, isdef2,
                          status );
               lstr[ slen - 1 ] = ')';
            }

/* If an error occurs, then report contextual information. */
            if( *status != SAI__OK ) {
               ndf1Amsg( "NDF", acb1 );
               errRep( " ", "Unable to select the specified section of the "
                       "NDF ^NDF", status );

/* Otherwise, calculate the actual lower and upper bounds of each
   dimension in pixels. */
            } else {

/* Convert any FRACTION values into corresponding pixel indices. */
               ndf1Fr2px( ndim, ndimd, lbndd, ubndd, isbnd, value1, value2,
                          frame1, frame2, status );

/* If we are using WCS syntax, we do all axes together, */
               if( wcssec ) {
                  ndf1Wclim( iwcs, ndim, ndimd, lbndd, ubndd, isdef1, isdef2,
                             value1, value2, isbnd, lbnd, ubnd, status );

/* For the old pixel/axis syntax, do each axis independently unless
   floating point values are being interpreted as WCS values */
               } else if( !usewcs ) {

                  for( i = 0; i < ndim; i++ ){
                     ndf1Axlim( i + 1, acb1, value1[ i ], value2[ i ],
                                frame1[ i ], frame2[ i ], isbnd[ i ],
                                lbnd + i, ubnd + i, status );

/* If an error occurs, then report context information and quit. */
                     if( *status != SAI__OK ) {
                        msgSeti( "DIM", i + 1 );
                        msgSetc( "SECTION", lstr );
                        errRep( " ", "Error in dimension ^DIM of the NDF "
                                "section specification '^SECTION'.", status );
                        break;
                     }
                  }

/* If we are using the old syntax, but non-integer values are being
   interpretde as WCS values... */
               } else {
                  ndf1Wplim( iwcs, ndim, lbndd, ubndd, value1, value2, frame1,
                             frame2, isbnd, isdef1, isdef2, lbnd, ubnd, status );
               }

/* Select the section from the NDF. */
               ndf1Cut( acb1, ndim, lbnd, ubnd, acb2, status );

/* Report further context information if needed. */
               if( *status != SAI__OK ) {
                  ndf1Amsg( "NDF", acb1 );
                  errRep( " ", "Unable to select the specified section of the "
                          "NDF ^NDF", status );
               }
            }
         }

/* Free the WCS FrameSet pointer. */
         if( iwcs ) iwcs = astAnnul( iwcs );

      }
   }

/* Free the local copy of the dimensions bounds string. */
   lstr = astFree( lstr );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Ncut", status );

}

