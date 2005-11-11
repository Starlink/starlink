#include "sae_par.h"
#include "cupid.h"
#include "mers.h"
#include "ndf.h"
#include "star/hds.h"
#include <stdio.h>


void cupidHdsClump( char *cloc, double sum, double *par, double rms, 
                    int ndim, int *lbox, int *ubox, int list_size, 
                    double *mlist, int *plist, int *lbnd, int iclump,
                    int *dax ){
/*
*  Name:
*     cupidHdsClump

*  Purpose:
*     Create an HDS object containing a description of a single clump.

*  Synopsis:
*     void cupidHdsClump( char *cloc, double sum, double *par, double rms, 
*                         int ndim, int *lbox, int *ubox, int list_size, 
*                         double *mlist, int *plist, int *lbnd, int iclump,
*                         int *dax )

*  Description:
*     This function creates a temporary HDS object with type and name both 
*     equal to "CLUMP", and stores in it:
*
*     - The integrated intensity in the clump
*     - The parameters of a Gaussian approximation to the clump.
*     - An NDF containing an image of the clump (the bounds of the NDF will 
*       be the smallest possible which still encompass the clump).

*  Parameters:
*     cloc
*        Pointer to a character string in which to store the HDS locator
*        for the newly created temporary object.
*     sum
*        The integrated intensity in the clump.
*     par
*        Pointer to an array holding the parameters of a Gaussian
*        approximation to the clump. How many of these are used depends on 
*        the value of "ndim": if "ndim" is 1 only elements 0 to 3 are used, 
*        if "ndim" is 2 only elements 0 to 6 are used, if "ndim" is 3 all 
*        elements are used. All axis values are represented in GRID pixels: 
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

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     10-NOV-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   char *cen[] = { "CENTRE1", "CENTRE2", "CENTRE3" };
   char *fwhm[] = { "FWHM1", "FWHM2", "FWHM3" };
   char *vgrad[] = { "VGRAD1", "VGRAD2", "VGRADq3" };

   char dloc[ DAT__SZLOC + 1 ]; /* Component locator */
   char name[ DAT__SZNAM + 1 ]; /* New component name */
   double *ipd;                 /* Pointer to Data array */
   double *m;                   /* Pointer to next data value */
   double dval;                 /* Double value to store */
   int *p;                      /* Pointer to next grid axis value */
   int el;                      /* Number of elements mapped */
   int elb[ 3 ];                /* The lower NDF limit on each external axis */
   int elbox[ 3 ];              /* The lower box limit on each external axis */
   int estep[ 3 ];              /* The step size on each external axis */
   int eub[ 3 ];                /* The upper NDF limit on each external axis */
   int i;                       /* Point index */
   int indf;                    /* NDF identifier */
   int iv;                      /* 1D vector index for current data value */
   int j;                       /* Axis index */
   int lb[ 3 ];                 /* Lower pixel index bounds of NDF */
   int place;                   /* NDF place holder */
   int step[ 3 ];               /* The step size on each internal axis */
   int ub[ 3 ];                 /* Upper pixel index bounds of NDF */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Create the structure as a temporary HDS object. It will be copied to a
   permanent location before the program exits. */
   datTemp( "CLUMP", 0, NULL, cloc, status );
   sprintf( name, "CLUMP%d", iclump );
   datRenam( cloc, name, status );

/* Store the integrated intensity in the clump. */
   datNew( cloc, "SUM", "_DOUBLE", 0, NULL, status );
   datFind( cloc, "SUM", dloc, status );
   datPutD( dloc, 0, NULL, &sum, status );
   datAnnul( dloc, status );

/* Store the parameters of the Gaussian approximation, taking out the
   normalisation by the RMS noise level.*/
   datNew( cloc, "PEAK", "_DOUBLE", 0, NULL, status );
   datFind( cloc, "PEAK", dloc, status );
   dval = rms*par[ 0 ];
   datPutD( dloc, 0, NULL, &dval, status );
   datAnnul( dloc, status );

   datNew( cloc, "OFFSET", "_DOUBLE", 0, NULL, status );
   datFind( cloc, "OFFSET", dloc, status );
   dval = rms*par[ 1 ];
   datPutD( dloc, 0, NULL, &dval, status );
   datAnnul( dloc, status );
      
   datNew( cloc, cen[ dax[ 0 ] ], "_DOUBLE", 0, NULL, status );
   datFind( cloc, cen[ dax[ 0 ] ], dloc, status );
   dval = par[ 2 ] + lbnd[ dax[ 0 ] ] - 1.5;
   datPutD( dloc, 0, NULL, &dval, status );
   datAnnul( dloc, status );
      
   datNew( cloc, fwhm[ dax[ 0 ] ], "_DOUBLE", 0, NULL, status );
   datFind( cloc, fwhm[ dax[ 0 ] ], dloc, status );
   datPutD( dloc, 0, NULL, par + 3, status );
   datAnnul( dloc, status );

   lb[ 0 ] = lbox[ 0 ] - 1 + lbnd[ dax[ 0 ] ];
   ub[ 0 ] = ubox[ 0 ] - 1 + lbnd[ dax[ 0 ] ];
   step[ 0 ] = 1;
      
   if( ndim > 1 ) {
      datNew( cloc, cen[ dax[ 1 ] ], "_DOUBLE", 0, NULL, status );
      datFind( cloc, cen[ dax[ 1 ] ], dloc, status );
      dval = par[ 4 ] + lbnd[ dax[ 1 ] ] - 1.5;
      datPutD( dloc, 0, NULL, &dval, status );
      datAnnul( dloc, status );
         
      datNew( cloc, fwhm[ dax[ 1 ] ], "_DOUBLE", 0, NULL, status );
      datFind( cloc, fwhm[ dax[ 1 ] ], dloc, status );
      datPutD( dloc, 0, NULL, par + 5, status );
      datAnnul( dloc, status );
         
      datNew( cloc, "ANGLE", "_DOUBLE", 0, NULL, status );
      datFind( cloc, "ANGLE", dloc, status );
      dval = par[ 6 ]*AST__DR2D;
      datPutD( dloc, 0, NULL,  &dval, status );
      datAnnul( dloc, status );
         
      lb[ 1 ] = lbox[ 1 ] - 1 + lbnd[ dax[ 1 ] ];
      ub[ 1 ] = ubox[ 1 ] - 1 + lbnd[ dax[ 1 ] ];
      step[ 1 ] = ub[ 0 ] - lb[ 0 ] + 1;

      if( ndim > 2 ) {

         datNew( cloc, cen[ dax[ 2 ] ], "_DOUBLE", 0, NULL, status );
         datFind( cloc, cen[ dax[ 2 ] ], dloc, status );
         dval = par[ 7 ] + lbnd[ dax[ 2 ] ] - 1.5;
         datPutD( dloc, 0, NULL, &dval, status );
         datAnnul( dloc, status );
            
         datNew( cloc, fwhm[ dax[ 2 ] ], "_DOUBLE", 0, NULL, status );
         datFind( cloc, fwhm[ dax[ 2 ] ], dloc, status );
         datPutD( dloc, 0, NULL, par + 8, status );
         datAnnul( dloc, status );
         
         datNew( cloc, vgrad[ dax[ 0 ] ], "_DOUBLE", 0, NULL, status );
         datFind( cloc, vgrad[ dax[ 0 ] ], dloc, status );
         datPutD( dloc, 0, NULL, par + 9, status );
         datAnnul( dloc, status );

         datNew( cloc, vgrad[ dax[ 1 ] ], "_DOUBLE", 0, NULL, status );
         datFind( cloc, vgrad[ dax[ 1 ] ], dloc, status );
         datPutD( dloc, 0, NULL, par + 10, status );
         datAnnul( dloc, status );

         lb[ 2 ] = lbox[ 2 ] - 1 + lbnd[ dax[ 2 ]];
         ub[ 2 ] = ubox[ 2 ] - 1 + lbnd[ dax[ 2 ]];
         step[ 2 ] = ( ub[ 1 ] - lb[ 1 ] + 1 )*step[ 1 ];

      }
   }      

/* Convert lbox, lb, ub and step from internal axis numbering to external axis
   numbering. */
   for( i = 0; i < ndim; i++ ) {
     elbox[ dax[ i ] ] = lbox[ i ];
     elb[ dax[ i ] ] = lb[ i ];
     eub[ dax[ i ] ] = ub[ i ];
     estep[ dax[ i ] ] = step[ i ];
   }      

/* Create an NDF containing the model values calculated above with bad
   values at all other pixels. The size of this NDF is the minimum needed
   to contain the clump. */
   ndfOpen( cloc, "MODEL", "WRITE", "NEW", &indf, &place, status );
   ndfNew( "_DOUBLE", ndim, elb, eub, &place, &indf, status );

/* Map the NDFs Data array, filling it with bad values. */
   ndfMap( indf, "DATA", "_DOUBLE", "WRITE/BAD", (void *) &ipd, &el, status );
   if( ipd ) {

/* Store every supplied model value in the NDF data array. */
      m = mlist;
      p = plist;
      for( i = 0; i < list_size; i++ ) {

/* Find the 1D vector index into the NDF data array corresponding to the
   grid indices (within the user supplied NDF) of the current point.*/
         iv = *(p++) - elbox[ 0 ];
         for( j = 1; j < ndim; j++ ) iv += ( *(p++) - elbox[ j ] )*estep[ j ];

/* Store the value. */
         ipd[ iv ] = *(m++);
 
      }
   }

/* Annul the NDF identifier. */
   ndfAnnul( &indf, status );

}

