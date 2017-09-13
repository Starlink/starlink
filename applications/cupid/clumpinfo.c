#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/ndg.h"
#include "ast.h"
#include "star/kaplibs.h"
#include "star/grp.h"
#include "star/hds.h"
#include "par.h"
#include "prm_par.h"
#include "cupid.h"
#include <math.h>
#include <string.h>
#include <stdio.h>


void clumpinfo( int *status ) {
/*
*+
*  Name:
*     CLUMPINFO

*  Purpose:
*     Obtain information about one or more previously identified clumps.

*  Language:
*     C

*  Type of Module:
*     ADAM A-task

*  Description:
*     This application returns various items of information about a
*     single clump, or a collection of clumps, previously identified
*     using FINDCLUMPS or EXTRACTCLUMPS.

*  Usage:
*     clumpinfo ndf clumps quiet

*  ADAM Parameters:
*     CLUMPS = LITERAL (Read)
*        Specifies the indices of the clumps to be included in the
*        returned information. It can take any of the following values:
*
*        - "ALL" or "*" --  All clumps.
*
*        - "xx,yy,zz" -- A list of clump indices.
*
*        - "xx:yy" --  Clump indices between xx and yy inclusively.  When
*        xx is omitted the range begins from one; when yy is omitted the
*        range ends with the final clump index.
*
*        - Any reasonable combination of above values separated by
*        commas.
*     FLBND( ) = _DOUBLE (Write)
*          The lower bounds of the bounding box enclosing the selected
*          clumps in the current WCS Frame of the input NDF. Celestial axis
*          values are always in units of radians, but spectral axis units
*          will be in the spectral units used by the current WCS Frame.
*     FUBND( ) = _DOUBLE (Write)
*          The upper bounds of the bounding box enclosing the selected
*          clumps. See parameter FLBND for more details.
*     LBOUND( ) = _INTEGER (Write)
*          The lower pixel bounds of bounding box enclosing the selected
*          clumps.
*     NCLUMPS = _INTEGER (Write)
*        The total number of clumps descrriptions stored within the supplied
*        NDF.
*     NDF = NDF (Read)
*        The NDF defining the previously identified clumps. This
*        should contain a CUPID extension describing all the identified
*        clumps, in the format produced by FINDCLUMPS or EXTRACTCLUMPS.
*     QUIET = _LOGICAL (Read)
*        If TRUE, then no information is written out to the screen,
*        although the output parameters are still assigned values. [FALSE]
*     UBOUND( ) = _INTEGER (Write)
*          The upper pixel bounds of bounding box enclosing the selected
*          clumps.

*  Notes:
*     - It is hoped to extend the range of information reported by this
*     application as new requirements arise.

*  Synopsis:
*     void clumpinfo( int *status );

*  Copyright:
*     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     22-MAR-2007 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   AstFrame *cfrm;      /* Pointer to current WCS Frame */
   AstMapping *cmap;    /* Pointer to PIXEL->current Frame Mapping */
   CupidClumpInfo info; /* Structure holding returned information */
   Grp *grp = NULL;     /* GRP group holding input NDF name */
   HDSLoc *aloc = NULL; /* Locator for CLUMPS array in CUPID extension */
   HDSLoc *cloc = NULL; /* Locator for a single CLUMP structure */
   HDSLoc *xloc = NULL; /* Locator for CUPID extension */
   char *p;             /* Pointer into tmpstr string */
   char tmpstr[ 100 ];  /* Buffer for temporary strings */
   const char *dom;     /* Pointer to axis Domain name */
   double flbnd[ NDF__MXDIM ]; /* Lower bounds of WCS bounding box */
   double fubnd[ NDF__MXDIM ]; /* Upper bounds of WCS bounding box */
   double plbnd[ NDF__MXDIM ]; /* Lower bounds of PIXEL bounding box */
   double pubnd[ NDF__MXDIM ]; /* Upper bounds of PIXEL bounding box */
   int *clump_flags = NULL;  /* Flags indicating if each clump is to be used */
   int *clump_indices = NULL;/* List of indices of clumps to be used */
   int i;               /* Loop count */
   hdsdim iclump;       /* One-based clump index */
   int indf;            /* NDF identifier for input NDF */
   int ipix;            /* Index of PIXEL Frame */
   size_t nclumps;      /* No. of clump descriptions within the supplied NDF */
   int nuse;            /* Number of clumps to be used */
   int primary;         /* Value for locator primary flag */
   int quiet;           /* Supress screen output? */
   size_t size;         /* Number of values in group "*grp" */
   int there;           /* Does the enquired object exist? */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Begin an AST context */
   astBegin;

/* Start an NDF context */
   ndfBegin();



/* Obtain the input NDF and get a locator for the array of clump
   descriptions within it.
   -----------------------------------------------------------------  */

/* Get an identifier for the input NDF. We use NDG (via kpg1_Rgndf)
   instead of calling ndfAssoc directly since NDF/HDS has problems with
   file names containing spaces, which NDG does not have. */
   kpg1Rgndf( "NDF", 1, 1, "", &grp, &size, status );
   ndgNdfas( grp, 1, "READ", &indf, status );
   grpDelet( &grp, status );

/* Check the NDF has a suitable CUPID extension containing an array of
   clump cut-outs. Get an HDS locator for the array. */
   ndfXstat( indf, "CUPID", &there, status );
   if( !there ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         ndfMsg( "NDF", indf );
         errRep( "", "The NDF \"^NDF\" does not contain a CUPID extension "
                 "such as created by FINDCLUMPS or EXTRACTCLUMPS.", status );
      }

   } else {
      ndfXloc( indf, "CUPID", "READ", &xloc, status );
      datThere( xloc, "CLUMPS", &there, status );
      if( !there ) {
         if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            ndfMsg( "NDF", indf );
            errRep( "", "The CUPID extension within NDF \"^NDF\" does not "
                    "contain an array of clumps such as created by "
                    "FINDCLUMPS or EXTRACTCLUMPS.", status );
         }

      } else {
         datFind( xloc, "CLUMPS", &aloc, status );
         primary = 1;
         datPrmry( 1, &aloc, &primary, status );

      }
      datAnnul( &xloc, status );
   }

/* Abort if we have no clumps array locator, or if an error occurred. */
   if( !aloc || *status != SAI__OK ) goto L999;



/* Calculate the required clump information, and store it in the "info"
   structure.
   -----------------------------------------------------------------  */

/* Indicate that the structure holding the returned information has not
   yet been initialised. */
   info.init = 0;

/* Get the WCS FrameSet from the input NDF, and store a pointer to it in
   the "info" structure. */
   kpg1Gtwcs( indf, &(info.iwcs), status );

/* Get the number of clumps defined within the input NDF. */
   datSize( aloc, &nclumps, status );

/* Get the list of clump indices to iclude in the returned information. */
   clump_flags = astMalloc( sizeof( int )*nclumps );
   clump_indices = astMalloc( sizeof( int )*nclumps );
   kpg1Gilst( 1, (int) nclumps, (int) nclumps, "CLUMPS", clump_flags, clump_indices,
              &nuse, status );

/* Loop round all clumps that are to be used. */
   for( i = 0; i < nuse && *status == SAI__OK; i++ ) {
      iclump = clump_indices[ i ];

/* Get a locator for this clump. */
      datCell( aloc, 1, &iclump, &cloc, status );

/* Update the returned information to include this clump. */
      cupidClumpInfo1( cloc, &info, status );

/* Annul the clump structure locator. */
      datAnnul( &cloc, status );

   }



/* Write out the information to the screen and to appropriate output
   parameters.
   -----------------------------------------------------------------  */

/* See if screen output is required. */
   parGet0l( "QUIET", &quiet, status );
   if( !quiet ) msgBlank( status );

/* The number of clumps defined within the input NDF... */
   parPut0i( "NCLUMPS", (int) nclumps, status );
   if( ! quiet ) {
      msgSeti( "NCLUMPS", (int) nclumps );
      msgOut( "", "   Total no. of clumps: ^NCLUMPS", status );
   }

/* Pixel index bounding box... */
   parPut1i( "LBOUND", info.npix, info.lbnd, status );
   parPut1i( "UBOUND", info.npix, info.ubnd, status );

   if( !quiet ) {
      p = tmpstr + sprintf( tmpstr, "( " );
      for( i = 0; i < info.npix; i++) {
         p += sprintf( p, "%d:%d", info.lbnd[ i ], info.ubnd[ i ] );
         if( i < info.npix - 1 ) p += sprintf( p, ", " );
      }
      p += sprintf( p, " )" );

      msgSetc( "SECTION", tmpstr );
      msgOut( "", "   Pixel index bounding box: ^SECTION", status );
   }

/* WCS bounding box (first convert the pixel index bounding box into WCS
   coords)... */
   cfrm = astGetFrame( info.iwcs, AST__CURRENT );

   kpg1Asffr( info.iwcs, "PIXEL", &ipix, status );
   cmap = astGetMapping( info.iwcs, ipix, AST__CURRENT );

   for( i = 0; i < info.npix; i++ ) {
      plbnd[ i ] = info.lbnd[ i ] - 1.0;
      pubnd[ i ] = info.ubnd[ i ];
   }

   for( i = 0; i < info.nwcs; i++ ) {
     astMapBox( cmap, plbnd, pubnd, 1, i + 1, flbnd + i, fubnd + i,
                NULL, NULL);
   }

   astNorm( cfrm, flbnd );
   astNorm( cfrm, fubnd );

   parPut1d( "FLBND", info.nwcs,  flbnd, status );
   parPut1d( "FUBND", info.nwcs,  fubnd, status );

   if( !quiet ) {
      msgOut( "", "   WCS bounding box:", status );

      for( i = 0; i < info.nwcs; i++) {
         msgSetc( "L", astFormat( cfrm, i + 1, flbnd[ i ] ) );
         msgSetc( "U", astFormat( cfrm, i + 1, fubnd[ i ] ) );

         sprintf( tmpstr, "Domain(%d)", i + 1 );
         dom = astGetC( cfrm, tmpstr );
         if( dom && strcmp( dom, "SKY" ) ) {
            sprintf( tmpstr, "Unit(%d)", i + 1 );
            msgSetc( "UNT", astGetC( cfrm, tmpstr ) );
         } else {
            msgSetc( "UNT", "" );
         }

         sprintf( tmpstr, "Label(%d)", i + 1 );
         msgSetc( "LAB", astGetC( cfrm, tmpstr ) );

         msgOut( "", "        ^LAB: ^L -> ^U ^UNT", status );
      }
   }

   if( !quiet ) msgBlank( status );



/* Tidy up.
   --------      */
L999:;

/* Free resources. */
   clump_flags = astFree( clump_flags );
   clump_indices = astFree( clump_indices );
   if( aloc ) datAnnul( &aloc, status );

/* End the NDF context */
   ndfEnd( status );

/* End the AST context */
   astEnd;

/* If an error has occurred, issue another error report identifying the
   program which has failed (i.e. this one). */
   if( *status != SAI__OK ) {
      errRep( "CLUMPINFO_ERR", "CLUMPINFO: Failed to obtain information "
              "about one or more previously identified clumps.", status );
   }

}
