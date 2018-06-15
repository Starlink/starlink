#include "chr.h"
#include "sae_par.h"
#include <stdio.h>

void testAppnd( int *status );
void testClean( int *status );
void testCtod( int *status );
void testCtoi( int *status );
void testCtor( int *status );
void testFandl( int *status );
void testFill( int *status );
void testFparx( int *status );
void testIsalm( int *status );
void testIsnam( int *status );
void testItoc( int *status );
void testLdblk( int *status );
void testLen( int *status );
void testPutc( int *status );
void testPuti( int *status );
void testRmblk( int *status );
void testSimlr( int *status );
void testUcase( int *status );

int main(){
   int status = SAI__OK;

   testAppnd( &status );
   testClean( &status );
   testCtod( &status );
   testCtoi( &status );
   testCtor( &status );
   testFandl( &status );
   testFill( &status );
   testFparx( &status );
   testIsalm( &status );
   testIsnam( &status );
   testItoc( &status );
   testLdblk( &status );
   testLen( &status );
   testPutc( &status );
   testPuti( &status );
   testRmblk( &status );
   testSimlr( &status );
   testUcase( &status );

   if( status != SAI__OK ) printf("CHR C test failed\n");

   return status;
}
