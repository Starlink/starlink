#include <stdlib.h>

float
random_ ( int *iseed )
/*
**  - - - - - - -
**   r a n d o m
**  - - - - - - -
**
**  Generate pseudo-random real number in the range 0 <= x < 1.
**
**  (single precision)
**
**  This function is designed to replace the Fortran->C interface routine
**  random(3f) on systems which do not have this library (for example Linux)
**
**  Fortran call:   X = RANDOM(ISEED)
**
**  Given:
**     iseed     int     seed value
**
**     If iseed !=0  random-number generator is initialised and first number
**                   is returned.
**        iseed == 0 next number in the sequence is returned
**
**  B.K.McIlwrath    Starlink   12 January 1996
*/
{
   if( *iseed != 0 )
      srand(*iseed);

   return (float) rand() / (float) RAND_MAX;
}
