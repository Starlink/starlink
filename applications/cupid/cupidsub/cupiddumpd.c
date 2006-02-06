#include "sae_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "ast.h"
#include "dat_par.h"
#include "cupid.h"
#include <stdio.h>

void cupidDumpD( double *array, int ndim, int *dims, int *slbnd ){
/*
*  Name:
*     cupidDumpD

*  Purpose:
*     Dump a double array.

*  Synopsis:
*     void cupidDumpD( double *array, int ndim, int *dims, int *slbnd )

*  Description:
*     This function is a diagnostic function which dumps the supplied
*     array.

*  Parameters:
*     array
*        Pointer to the array to be dumped. 
*     ndim
*        The number of pixel axes.
*     dims
*        Pointer to the size of each pixel axis.
*     slbnd
*        Pointer to the lower pixel bounds of each pixel axis.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     7-DEC-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   double *adata;
   int indf, place, i, el, lbnd[3], ubnd[3];
   static int jj = 0;
   char name[ 100 ];

   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = slbnd[ i ];
      ubnd[ i ] = dims[ i ] + slbnd[ i ] - 1;
   }

   jj++;
   sprintf( name, "ddata%d", jj );
   printf("   DumpDng %s\n", name );
   ndfOpen( NULL, name, "WRITE", "NEW", &indf, &place, status );
   ndfNew( "_DOUBLE", ndim, lbnd, ubnd, &place, &indf, status );
   ndfMap( indf, "DATA", "_DOUBLE", "WRITE", (void *) &adata, &el, status );
   if( adata ) {
      for( i = 0; i < el; i++ ) adata[ i ] = array[ i ];
   }
   ndfAnnul( &indf, status );
}

