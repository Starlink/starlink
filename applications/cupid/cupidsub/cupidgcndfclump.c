#include "sae_par.h"
#include "cupid.h"
#include "mers.h"
#include "ndf.h"
#include "ast.h"
#include "star/hds.h"
#include "star/ndg.h"
#include <stdio.h>


void cupidGCNdfClump( HDSLoc **obj, double sum, double *par, double rms,
                    int ndim, int *lbox, int *ubox, int list_size,
                    double *mlist, int *plist, int *lbnd, int iclump,
                    int *dax, AstKeyMap *extra, int bad, int *status ){
/*
*+
*  Name:
*     cupidGCNdfClump

*  Purpose:
*     Create an NDF containing a description of a single clump.

*  Language:
*     Starlink C

*  Synopsis:
*     void cupidGCNdfClump( HDSLoc **obj, double sum, double *par, double rms,
*                         int ndim, int *lbox, int *ubox, int list_size,
*                         double *mlist, int *plist, int *lbnd, int iclump,
*                         int *dax, AstKeyMap *extra, int bad,
*                         int *status )

*  Description:
*     This function creates a temporary NDF and stores the integrated
*     intensity of the clump in its Data component. The bounds of the NDF
*     will be the smallest possible which still encompass the clump. In
*     addition, if required it will create a Cupid extension in the NDF
*     containing
*
*     - The parameters of a Gaussian approximation to the clump (if supplied).
*     - Any supplied extra information.

*  Parameters:
*     obj
*        A pointer to a locator for an HDS array of NDF objects. The array
*        will be extended to accomodate the new NDF. If NULL is supplied a
*        new temporary HDS object is created, and the locator stored at the
*        given address.
*     sum
*        The integrated intensity in the clump. Note, unlike par[0] and
*        par[1], this value should not be normalised to the RMS noise.
*     par
*        Pointer to an array holding the parameters of a Gaussian
*        approximation to the clump, or NULL. How many of these are used
*        depends on the value of "ndim": if "ndim" is 1 only elements 0 to
*        3 are used, if "ndim" is 2 only elements 0 to 6 are used, if "ndim"
*        is 3 all elements are used. All axis values are represented in GRID
*        pixels:
*
*           par[0]: Peak intensity of clump (in units of the RMS noise level).
*           par[1]: Constant intensity offset (in units of the RMS noise level).
*           par[2]: Model centre on internal axis 0 (in pixels)
*           par[3]: Intrinsic FWHM on axis 0 (in pixels)
*           par[4]: Model centre on internal axis 1 (in pixels)
*           par[5]: Intrinsic FWHM on axis 1 (in pixels)
*           par[6]: Spatial orientation angle (in radians, positive from
*                   +ve GRID1 axis to +ve GRID2 axis).
*           par[7]: Model centre on internal axis 3 (velocity) (in pixels)
*           par[8]: Intrinsic FWHM on velocity axis (in pixels)
*           par[9]: Axis 0 of internal velocity gradient vector (in velocity
*                   pixels per spatial pixel).
*           par[10]: Axis 1 of internal velocity gradient vector (in
*                   velocity pixels per spatial pixel).
*     rms
*        The RMS noise level.
*     ndim
*        The number of pixel axes in the array.
*     lbox
*        The lower grid index bounds of the area containing the clump
*        (using internal axis numbering).
*     ubox
*        The upper grid index bounds of the area containing the clump
*        (using internal axis numbering).
*     list_size
*        The number of values supplied in mlist and plist.
*     mlist
*        An array of "list_size" elements containing the clump values at
*        each pixel.
*     plist
*        An array of "ndim*list_size" elements in which each group of
*        "ndim" adjacent values forms the grid indices of the corresponding
*        value in "mlist". This uses external axis ordering.
*     lbnd
*        Pointer to array holding the pixel indices of the first pixel in
*        the user-supplied NDF (using external axis numbering).
*     iclump
*        The index of the current clump.
*     dax
*        Array holding external axis number indexed by internal axis number.
*     extra
*        An AstKeyMap holding extra diagnositic information to add to the
*        clump structure.
*     bad
*        Set the Unit component of the NDF to "BAD". This is used as a
*        flag to indicate that the clump touches too many areas of bad
*        pixels.
*     status
*        Pointer to the inherited status value.

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
*     Copyright (C) 2005,2007 Particle Physics & Astronomy Research Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-NOV-2005 (DSB):
*        Original version.
*     5-MAR-2007 (DSB):
*        Initiaslise "exloc" locator to NULL before calling datFind.
*     13-JAN-2009 (TIMJ):
*        DO NOT CAST int* to size_t* since that is not going to work for long.
*     26-JAN-2011 (DSB):
*        Ensure the "m" pointer is incremented even if the currrent model pixel
*        is off the edge of the NDF. Previously, this bug caused a stripey
*        "aliasing" type effect if the supplied model data extends outside the
*        bounds of the NDF.
*     25-MAY-2017 (DSB):
*        Switch off group history and provenance recording during this
*        function. This is because it can inflate the time taken to run
*        findclumps enormously if there are many thousands of clumps.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */

   const char *cen[] = { "CENTRE1", "CENTRE2", "CENTRE3" };
   const char *fwhm[] = { "FWHM1", "FWHM2", "FWHM3" };
   const char *vgrad[] = { "VGRAD1", "VGRAD2", "VGRAD3" };

   HDSLoc *cloc;                /* Cell locator */
   HDSLoc *dloc;                /* Component locator */
   HDSLoc *xloc;                /* Extension locator */
   HDSLoc *exloc;               /* Locator for structure holding extra info */
   const char *key;             /* KeyMap key name */
   double *ipd;                 /* Pointer to Data array */
   double *m;                   /* Pointer to next data value */
   double dval;                 /* Double value to store */
   int *p;                      /* Pointer to next grid axis value */
   int el;                      /* Number of elements mapped */
   int elb[ 3 ];                /* The lower NDF limit on each external axis */
   int elbox[ 3 ];              /* The lower box limit on each external axis */
   int estep[ 3 ];              /* The step size on each external axis */
   int eub[ 3 ];                /* The upper NDF limit on each external axis */
   int eubox[ 3 ];              /* The upper box limit on each external axis */
   int i;                       /* Point index */
   int indf;                    /* NDF identifier */
   int iv;                      /* 1D vector index for current data value */
   int j;                       /* Axis index */
   int lb[ 3 ];                 /* Lower pixel index bounds of NDF */
   int nex;                     /* No. of extra items of information */
   int ok;                      /* Pixel within clump NDF? */
   int old_ghstate;             /* Non-zero if group history recording is switched on */
   int old_pvstate;             /* Non-zero if provenance recording is switched on */
   int place;                   /* NDF place holder */
   int pv;                      /* Pixel offset */
   size_t oldsize;              /* Size of original NDF */
   hdsdim size[1];              /* No of elements in NDF array */
   int step[ 3 ];               /* The step size on each internal axis */
   int ub[ 3 ];                 /* Upper pixel index bounds of NDF */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* If no array was supplied create one of length 1. */
   if( !(*obj) ) {
      size[0] = 1;
      datTemp( "NDF", 1, size, obj, status );

/* Otherwise, get the number of NDFs already in the supplied array of
   NDFs, and increase it by 1. */
   } else {
      datSize( *obj, &oldsize, status );
      size[0] = oldsize + 1;
      datAlter( *obj, 1, size, status );
   }

/* Get a locator for the new cell. */
   cloc = NULL;
   datCell( *obj, 1, size, &cloc, status );

/* Begin an NDF context */
   ndfBegin();

/* Temporaily switch off group history and provenance recording since there
   can be thousands of clump NDFs. */
   ndgHltgh( 0, &old_ghstate, status );
   ndgHltpv( 0, &old_pvstate, status );

/* Find the pixel index bounds of the NDF and the step between adjacent
   pixels on each axis. */
   lb[ 0 ] = lbox[ 0 ] - 1 + lbnd[ dax[ 0 ] ];
   ub[ 0 ] = ubox[ 0 ] - 1 + lbnd[ dax[ 0 ] ];
   step[ 0 ] = 1;

   for( i = 1; i < ndim; i++ ) {
      lb[ i ] = lbox[ i ] - 1 + lbnd[ dax[ i ] ];
      ub[ i ] = ubox[ i ] - 1 + lbnd[ dax[ i ] ];
      step[ i ] = ( ub[ i - 1 ] - lb[ i - 1 ] + 1 )*step[ i - 1 ];
   }

/* Convert lbox, lb, ub and step from internal axis numbering to external axis
   numbering. */
   for( i = 0; i < ndim; i++ ) {
     elbox[ dax[ i ] ] = lbox[ i ];
     eubox[ dax[ i ] ] = ubox[ i ];
     elb[ dax[ i ] ] = lb[ i ];
     eub[ dax[ i ] ] = ub[ i ];
     estep[ dax[ i ] ] = step[ i ];
   }

/* Create a place holder for the new NDF within the new cell. The NDF will be
   copied to its final resting place before the program exits. */
   ndfPlace( cloc, " ", &place, status );

/* Create the NDF to receive the clump values. The size of this NDF is the
   minimum needed to contain the clump. */
   ndfNew( "_DOUBLE", ndim, elb, eub, &place, &indf, status );

/* Map the NDFs Data array, filling it with bad values. */
   ndfMap( indf, "DATA", "_DOUBLE", "WRITE/BAD", (void *) &ipd, &el, status );
   if( ipd ) {

/* Store every supplied model value in the NDF data array. */
      m = mlist;
      p = plist;
      for( i = 0; i < list_size; i++,m++ ) {

/* Find the 1D vector index into the NDF data array corresponding to the
   grid indices (within the user supplied NDF) of the current point.*/
         pv = *(p++);
         iv = pv - elbox[ 0 ];
         ok = ( pv >= elbox[ 0 ] && pv <= eubox[ 0 ] );
         for( j = 1; j < ndim; j++ ) {
            pv = *(p++);
            iv += ( pv - elbox[ j ] )*estep[ j ];
            if( pv < elbox[ j ] || pv > eubox[ j ] ) ok = 0;
         }

/* Store the value. */
         if( ok ) ipd[ iv ] = *m;
      }

/* Unmap the NDFs Data array. */
      ndfUnmap( indf, "DATA", status );
   }

/* If required, create a Cupid extension in the NDF. */
   if( par || extra ) {
      xloc = NULL;
      ndfXnew( indf, "CUPID", "CUPID_EXT", 0, NULL, &xloc, status );

/* First do Gaussian parameters. */
      if( par ) {

/* Store the integrated intensity in the clump. */
         dloc = NULL;
         datNew( xloc, "SUM", "_DOUBLE", 0, NULL, status );
         datFind( xloc, "SUM", &dloc, status );
         datPutD( dloc, 0, NULL, &sum, status );
         datAnnul( &dloc, status );

/* Store the parameters of the Gaussian approximation, taking out the
   normalisation by the RMS noise level.*/
         datNew( xloc, "PEAK", "_DOUBLE", 0, NULL, status );
         datFind( xloc, "PEAK", &dloc, status );
         dval = rms*par[ 0 ];
         datPutD( dloc, 0, NULL, &dval, status );
         datAnnul( &dloc, status );

         datNew( xloc, "OFFSET", "_DOUBLE", 0, NULL, status );
         datFind( xloc, "OFFSET", &dloc, status );
         dval = rms*par[ 1 ];
         datPutD( dloc, 0, NULL, &dval, status );
         datAnnul( &dloc, status );

         datNew( xloc, cen[ dax[ 0 ] ], "_DOUBLE", 0, NULL, status );
         datFind( xloc, cen[ dax[ 0 ] ], &dloc, status );
         dval = par[ 2 ] + lbnd[ dax[ 0 ] ] - 1.5;
         datPutD( dloc, 0, NULL, &dval, status );
         datAnnul( &dloc, status );

         datNew( xloc, fwhm[ dax[ 0 ] ], "_DOUBLE", 0, NULL, status );
         datFind( xloc, fwhm[ dax[ 0 ] ], &dloc, status );
         datPutD( dloc, 0, NULL, par + 3, status );
         datAnnul( &dloc, status );

         if( ndim > 1 ) {
            datNew( xloc, cen[ dax[ 1 ] ], "_DOUBLE", 0, NULL, status );
            datFind( xloc, cen[ dax[ 1 ] ], &dloc, status );
            dval = par[ 4 ] + lbnd[ dax[ 1 ] ] - 1.5;
            datPutD( dloc, 0, NULL, &dval, status );
            datAnnul( &dloc, status );

            datNew( xloc, fwhm[ dax[ 1 ] ], "_DOUBLE", 0, NULL, status );
            datFind( xloc, fwhm[ dax[ 1 ] ], &dloc, status );
            datPutD( dloc, 0, NULL, par + 5, status );
            datAnnul( &dloc, status );

            datNew( xloc, "ANGLE", "_DOUBLE", 0, NULL, status );
            datFind( xloc, "ANGLE", &dloc, status );
            dval = par[ 6 ]*AST__DR2D;
            datPutD( dloc, 0, NULL,  &dval, status );
            datAnnul( &dloc, status );

            if( ndim > 2 ) {

               datNew( xloc, cen[ dax[ 2 ] ], "_DOUBLE", 0, NULL, status );
               datFind( xloc, cen[ dax[ 2 ] ], &dloc, status );
               dval = par[ 7 ] + lbnd[ dax[ 2 ] ] - 1.5;
               datPutD( dloc, 0, NULL, &dval, status );
               datAnnul( &dloc, status );

               datNew( xloc, fwhm[ dax[ 2 ] ], "_DOUBLE", 0, NULL, status );
               datFind( xloc, fwhm[ dax[ 2 ] ], &dloc, status );
               datPutD( dloc, 0, NULL, par + 8, status );
               datAnnul( &dloc, status );

               datNew( xloc, vgrad[ dax[ 0 ] ], "_DOUBLE", 0, NULL, status );
               datFind( xloc, vgrad[ dax[ 0 ] ], &dloc, status );
               datPutD( dloc, 0, NULL, par + 9, status );
               datAnnul( &dloc, status );

               datNew( xloc, vgrad[ dax[ 1 ] ], "_DOUBLE", 0, NULL, status );
               datFind( xloc, vgrad[ dax[ 1 ] ], &dloc, status );
               datPutD( dloc, 0, NULL, par + 10, status );
               datAnnul( &dloc, status );

            }
         }
      }

/* Now store any extra diagnostic information. */
      if( extra ) {
         datNew( xloc, "EXTRA", "EXTRA", 0, NULL, status );
         exloc = NULL;
         datFind( xloc, "EXTRA", &exloc, status );

         nex = astMapSize( extra );
         for( i = 0; i < nex; i++ ) {
            key = astMapKey( extra, i );
            if( astMapGet0D( extra, key, &dval ) ) {
               datNew( exloc, key, "_DOUBLE", 0, NULL, status );
               datFind( exloc, key, &dloc, status );
               datPutD( dloc, 0, NULL, &dval, status );
               datAnnul( &dloc, status );
            }
         }

         datAnnul( &exloc, status );

      }

/* Release the extension locator. */
      datAnnul( &xloc, status );
   }

/* If required set the Unit component to "BAD". */
   if( bad ) ndfCput( "BAD", indf, "Unit", status );

/* Switch group history and provenance recording back to their original
   states. */
   ndgHltgh( old_ghstate, NULL, status );
   ndgHltpv( old_pvstate, NULL, status );

/* End the NDF context */
   ndfEnd( status );

/* Release the cell locator. */
   datAnnul( &cloc, status );

}
