#include "sae_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "dat_par.h"
#include "cupid.h"
#include <stdio.h>

/* Global Variables: */
/* ================= */
/* A structure holding the global parameters of the GaussClump algorithm 
   needed by this function. These are set by function cupidGaussClumps. */
extern CupidGC cupidGC;


void cupidGCDump( ){
/*
*  Name:
*     cupidGCDump

*  Purpose:
*     Dump the contents of the cupidGC structure.

*  Synopsis:
*     void cupidGCDump( )

*  Description:
*     This function is a diagnostic function which dumps the contents of
*     the cupidGC structure.

*  Parameters:

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     21-OCT-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   int indf, place, i, el, lbnd[3], ubnd[3];
   double *data;
   static int ii = 0;
   char name[ 100 ];

/*   

   double beam_sq;         ! Square of spatial beam FWHM in pixels    
   double velres_sq;       ! Square of velocity resolution in pixels    
   double ymax;            ! Largest data value in section being fitted 
   double x_max[ 3 ];      ! Grid coords of "ymax" value 
   double *data;           ! Pointer to copy of data section being fitted 
   double *weight;         ! Pointer to weights for section being fitted 
   double *res;            ! Pointer to array to receive scale residuals 
   double lbnd[ 3 ];       ! Lower grid bounds of section being fitted 
   double ubnd[ 3 ];       ! Upper grid bounds of section being fitted 
   int ndf;                ! Number of degrees of freedom 
   int nel;                ! Number of pixels in section being fitted 
   double sa;              ! Chi-square stiffness parameter sa 
   double s0p1;            ! Chi-square stiffness parameter s0, minus 1.0 
   double sc4;             ! Four times chi-square stiffness parameter sc 
   int nf;                 ! The invocation count from calcf 
   int ndim;               ! Number of pixel axes in the data array 

*/

   for( i = 0; i < cupidGC.ndim; i++ ) {
      lbnd[ i ] = cupidGC.lbnd[ i ];
      ubnd[ i ] = cupidGC.ubnd[ i ];
   }


   sprintf( name, "data%d", ++ii );
   printf("Dumping %s\n", name );
   ndfOpen( DAT__ROOT, name, "WRITE", "NEW", &indf, &place, status );
   ndfNew( "_DOUBLE", cupidGC.ndim, lbnd, ubnd, &place, &indf, status );
   ndfMap( indf, "DATA", "_DOUBLE", "WRITE", &data, &el, status );
   for( i = 0; i < el; i++ ) data[ i ] = cupidGC.data[ i ];
   ndfAnnul( &indf, status );

   sprintf( name, "res%d", ii );
   ndfOpen( DAT__ROOT, name, "WRITE", "NEW", &indf, &place, status );
   ndfNew( "_DOUBLE", cupidGC.ndim, lbnd, ubnd, &place, &indf, status );
   ndfMap( indf, "DATA", "_DOUBLE", "WRITE", &data, &el, status );
   for( i = 0; i < el; i++ ) data[ i ] = cupidGC.res[ i ];
   ndfAnnul( &indf, status );

}


