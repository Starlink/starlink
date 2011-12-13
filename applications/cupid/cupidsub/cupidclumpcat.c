#include "sae_par.h"
#include "cupid.h"
#include "mers.h"
#include "star/hds.h"
#include "ast.h"
#include "star/kaplibs.h"

void cupidClumpCat( const char *param, double *tab, int size,
                    int i, int ndim, const char *ttl, int *status ){
/*
*+
*  Name:
*     cupidClumpCat

*  Purpose:
*     Add a clump into a catalogue, and optionally write the catlogue to
*     disk.

*  Language:
*     Starlink C

*  Synopsis:
*     void cupidClumpCat( const char *param, char *loc, double *tab,
*                         int size, int i, int ndim, const char *ttl,
*                         int *status )

*  Description:
*     The clump parameters contained within the HDS object located by "loc"
*     are copied into the "tab" array. If "param" is not NULL, the contents
*     of the "tab" array are then written out to a catalogue.

*  Parameters:
*     param
*        Name of the ADAM parameter to associated with the output
*        catalogue.
*     tab
*        Pointer to an array holding clump parameters.
*     size
*        The maximum number of clumps which can be stored in "tab".
*     i
*        The number of clumps contained in "tab" once any clump described by
*        "cloc" has been addded.
*     ndim
*        The number of pixel axes in the data.
*     ttl
*        A title for the output catalogue (if any).
*     status
*        Pointer to the inherited status value.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     11-NOV-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */

   AstFrame *frm1;               /* Frame describing clump parameters */
   AstFrame *frm2;               /* Frame describing clump centres */
   AstFrameSet *iwcs;            /* FrameSet to be stored in output catalogue */
   AstMapping *map;              /* Mapping from "frm1" to "frm2" */
   char dloc[ DAT__SZLOC + 1 ];  /* Component locator */
   const char *vg1;              /* First velocity gradient column name */
   const char *vg2;              /* Second velocity gradient column name */
   double *t;                    /* Pointer to next table value */
   int inperm[ 3 ];              /* Input axis permutation array */
   int j;                        /* Loop index */
   int ncol;                     /* Number of columns in table */
   int outperm[ 3 ];             /* Output axis permutation array */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Check the parameters. */
   if( i > size ) {
      *status = SAI__ERROR;
      msgSeti( "I", i );
      msgSeti( "SIZE", size );
      errRep( "CUPIDCLUMP_CAT_ERR1", "cupidClumpCat: Table size (^SIZE) "
              "is to small to accomodate ^I clumps (internal CUPID "
              "programming error).", status );
   }

/* Indicate we do not yet know the names of the two velocity gradient
   columns. */
   vg1 = NULL;
   vg2 = NULL;

/* Determine the number of columns in the catalogue (this equals the
   number of rows in the table). */
   ncol = ( ( ndim == 1 ) ? CUPID__GCNP1 : (
              (ndim ==2 ) ? CUPID__GCNP2 : CUPID__GCNP3 ) ) + 1;

/* If a clump has been supplied, add it to the table */
   if( cloc && *status == SAI__OK ) {

/* Get a pointer to the first element of the next free column in "tab". */
      t = tab + i - 1;

/* Extract each value from the Clump object and store it in the table. */
      datFind( cloc, "SUM", dloc, status );
      datGetD( dloc, 0, NULL, t, status );
      t += size;
      datAnnul( dloc, status );

      datFind( cloc, "PEAK", dloc, status );
      datGetD( dloc, 0, NULL, t, status );
      t += size;
      datAnnul( dloc, status );

      datFind( cloc, "OFFSET", dloc, status );
      datGetD( dloc, 0, NULL, t, status );
      t += size;
      datAnnul( dloc, status );

      datFind( cloc, "CENTRE1", dloc, status );
      datGetD( dloc, 0, NULL, t, status );
      t += size;
      datAnnul( dloc, status );

      datFind( cloc, "FWHM1", dloc, status );
      datGetD( dloc, 0, NULL, t, status );
      t += size;
      datAnnul( dloc, status );

      if( ndim > 1 ) {
         datFind( cloc, "CENTRE2", dloc, status );
         datGetD( dloc, 0, NULL, t, status );
         t += size;
         datAnnul( dloc, status );

         datFind( cloc, "FWHM2", dloc, status );
         datGetD( dloc, 0, NULL, t, status );
         t += size;
         datAnnul( dloc, status );

         datFind( cloc, "ANGLE", dloc, status );
         datGetD( dloc, 0, NULL,  t, status );
         t += size;
         datAnnul( dloc, status );

         if( ndim > 2 ) {

            datFind( cloc, "CENTRE3", dloc, status );
            datGetD( dloc, 0, NULL, t, status );
            t += size;
            datAnnul( dloc, status );

            datFind( cloc, "FWHM3", dloc, status );
            datGetD( dloc, 0, NULL, t, status );
            t += size;
            datAnnul( dloc, status );

            if( *status == SAI__OK ) {
               datFind( cloc, "VGRAD1", dloc, status );
               if( *status == SAI__OK ) {
                  datGetD( dloc, 0, NULL, t, status );
                  t += size;
                  datAnnul( dloc, status );
                  vg1 = "VGRAD1";
               } else {
                  errAnnul( status );
               }
            }

            if( *status == SAI__OK ) {
               datFind( cloc, "VGRAD2", dloc, status );
               if( *status == SAI__OK ) {
                  datGetD( dloc, 0, NULL, t, status );
                  t += size;
                  datAnnul( dloc, status );
                  if( !vg1 ) {
                     vg1 = "VGRAD2";
                  } else {
                     vg2 = "VGRAD2";
                  }
               } else {
                  errAnnul( status );
               }
            }

            if( *status == SAI__OK ) {
               datFind( cloc, "VGRAD3", dloc, status );
               if( *status == SAI__OK ) {
                  datGetD( dloc, 0, NULL, t, status );
                  t += size;
                  datAnnul( dloc, status );
                  vg2 = "VGRAD3";
               } else {
                  errAnnul( status );
               }
            }
         }
      }
   }

/* If required, write the table to disk. */
   if( param && *status == SAI__OK ) {

/* Start an AST context. */
      astBegin;

/* Create a Frame with "ncol" axes describing the table columns. */
      frm1 = astFrame( ncol, "Domain=PARAMETERS,Title=Clump parameters" );

/* Create a Frame with "ndim" axes describing the pixel coords at the
   clump centre. */
      frm2 = astFrame( ndim, "Domain=PIXEL,Title=Pixel coordinates" );

/* Set the attributes of these two Frames */
      astSetC( frm1, "Symbol(1)", "SUM" );
      astSetC( frm1, "Symbol(2)", "PEAK" );
      astSetC( frm1, "Symbol(3)", "OFFSET" );
      astSetC( frm1, "Symbol(4)", "CENTRE1" );
      astSetC( frm1, "Symbol(5)", "FWHM1" );
      astSetC( frm2, "Symbol(1)", "P1" );

      if( ndim > 1 ) {
         astSetC( frm1, "Symbol(6)", "CENTRE2" );
         astSetC( frm1, "Symbol(7)", "FWHM2" );
         astSetC( frm1, "Symbol(8)", "ANGLE" );
         astSetC( frm2, "Symbol(2)", "P2" );

         if( ndim > 2 ) {
            astSetC( frm1, "Symbol(9)",  "CENTRE3" );
            astSetC( frm1, "Symbol(10)", "FWHM3" );
            astSetC( frm1, "Symbol(11)", vg1 );
            astSetC( frm1, "Symbol(12)", vg2 );
            astSetC( frm2, "Symbol(3)", "P3" );
         }
      }

/* Create a Mapping (a PermMap) from the Frame representing the "ncol" clump
   parameters, to the "ndim" Frame representing clump centre positions. The
   inverse transformation supplies bad values for the other parameters. */
      for( j = 0; j < ncol; j++ ) inperm[ j ] = 0;

      inperm[ 4 ] = 0;
      inperm[ 6 ] = 1;
      inperm[ 9 ] = 2;

      outperm[ 0 ] = 4;
      outperm[ 1 ] = 6;
      outperm[ 2 ] = 9;

      map = (AstMapping *) astPermMap( ncol, inperm, ndim, outperm, NULL, " " );

/* Create a FrameSet to store in the output catalogue. It has two Frames,
   the base Frame has "ncol" axes - each axis describes one of the table
   columns. The current Frame has 2 axes and describes the clump (x,y)
   position. */
      iwcs = astFrameSet( frm1, "ID=FIXED_BASE" );
      astAddFrame( iwcs, AST__BASE, map, frm2 );
      astSetI( iwcs, "CURRENT", 1 );

/* Create the output catalogue */
      kpg1Wrlst( param, size, i, ncol, tab, AST__BASE, iwcs,
                 ttl, 1, NULL, 1, status );

/* End the AST context. */
      astEnd;

   }
}
