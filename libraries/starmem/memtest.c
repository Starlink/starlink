#include "mem.h"
#include <assert.h>
#include <stdio.h>

int main( void )
{
  int i;
  int **p;
  int *q;

  starMemInit();
  for (i = 0; i < 10000000; ++i)
   {
     p = starCalloc(1,sizeof(int *));
     q = starMallocAtomic(sizeof(int));
     assert(*p == 0);
     *p = starRealloc(q, 2 * sizeof(int));
     starFree( *p );
     starFree( p );
   }
  return 0;
}

