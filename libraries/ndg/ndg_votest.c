#include "ndg.h"
#include "sae_par.h"
#include <stdio.h>
#include <string.h>

main(){
   NdgProvenance *prov1 = NULL;
   NdgProvenance *prov2 = NULL;
   int status;
   int indf;
   const char *xml1 = NULL;
   const char *xml2 = NULL;

   ndfFind( NULL, "provtest", &indf, &status ); 
   prov1 = ndgReadProv( indf, "FRED", &status );
   ndfAnnul( &indf, &status );

   xml1 = ndgWriteVotProv( prov1, &status );
   prov2 = ndgReadVotProv( xml1, "path", "creator", &status );
   xml2 = ndgWriteVotProv( prov2, &status );

   if( status != SAI__OK ) {
      printf("ndg_votest failed\n");

   } else if( strcmp( xml1, xml2 ) ) {
      printf("ndg_votest failed\n");
      printf("%s\n\n-------------------------\n\n%s\n", xml1, xml2 );

   } else {
      printf("ndg_votest passed\n" );
   }

   xml1 = astFree( (void *) xml1 );
   xml2 = astFree( (void *) xml2 );
   prov1 = ndgFreeProv( prov1, &status );
   prov2 = ndgFreeProv( prov2, &status );
}

