/*
 * This file contains utility functions from saoimage (1.23.2) required for
 * the histeq code
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static char *errnote = " allocation failure\n";
/*
 * Subroutine:	calloc_errchk
 * Purpose:	Calloc with printf'less error message and exit for failure
 * Note:	If message is given, print it and exit on failure
 * Note:	If no message is given, return 0 on failure
 */
char *calloc_errchk ( count, size, errmess )
     int count;
     unsigned int size;
     char *errmess;
{
  char *space;
  char *calloc();

  if( (space = (char *)calloc((unsigned)count, size)) == NULL ) {
    if( errmess == NULL )
      return(0);
    else {
      fputs (errmess, stderr);
      fputs (errnote, stderr);
      exit( 100 );
    }
  }
  return( space );
}

