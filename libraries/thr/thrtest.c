#include "thr.h"
#include <assert.h>
#include <stdio.h>

typedef struct JobData {
   int start;
} JobData;

void worker( void *data, int *status );

#define NW 2

int main( void ){
   int i;
   JobData data[ NW ];
   int status = 0;
   ThrWorkForce *wf = thrCreateWorkforce( NW, &status );

   for( i = 0; i < NW; i++ ) {
      data[ i ].start = i;
      thrAddJob( wf, 0, data + i, worker,  0, NULL, &status );
   }

   thrWait( wf, &status );

   assert( data[ 0 ].start == 45 );
   assert( data[ 1 ].start == 56 );

   wf = thrDestroyWorkforce( wf );

}

void worker( void *data, int *status ){
   JobData *jobdata = (JobData *) data;
   int j;
   int old = jobdata->start;
   for( j = old; j < old + 10; j++ ) {
      jobdata->start += j;
   }
}


