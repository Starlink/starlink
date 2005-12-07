#include "sae_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "ast.h"
#include "dat_par.h"
#include "cupid.h"
#include <stdio.h>

void cupidCFDump( int *array, int ndim, int *dims, int *slbnd ){
/*
*  Name:
*     cupidCFDump

*  Purpose:
*     Dump a ClumpFind pixel assignment array.

*  Synopsis:
*     void cupidCFDump( int *array, int ndim, int *dims, int *slbnd )

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
   int *adata;
   int indf, place, i, el, lbnd[3], ubnd[3];
   static int jj = 0;
   char name[ 100 ];

   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = slbnd[ i ];
      ubnd[ i ] = dims[ i ] + slbnd[ i ] - 1;
   }

   jj++;
   sprintf( name, "ipa%d", jj );
   printf("   Dumping %s\n", name );
   ndfOpen( NULL, name, "WRITE", "NEW", &indf, &place, status );
   ndfNew( "_INTEGER", ndim, lbnd, ubnd, &place, &indf, status );
   ndfMap( indf, "DATA", "_INTEGER", "WRITE", (void *) &adata, &el, status );
   for( i = 0; i < el; i++ ) adata[ i ] = array[ i ];
   ndfAnnul( &indf, status );
}

