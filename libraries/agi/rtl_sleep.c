#include <sys/types.h>
#include <sys/time.h>

int
sleep_ ( int *delay )
/*
**  - - -
**  sleep 
**  - - -
**
**  Suspend process execution for specified delay in seconds
**
**
**  This function is designed to replace the Fortran->C interface routine
**  sleep(3f) on systems which do not have this library (for example Linux)
**
**  Fortran call:  CALL SLEEP(DELAY) 
**
**  Given:
**     delay     int    delay in seconds 
**
**  B.K.McIlwrath    Starlink   12 January 1996
*/
{
   struct timeval timer;

   timer.tv_sec  = (int) *delay;
   timer.tv_usec = 0;

   select(0, 0, 0, 0, &timer);

   return 0;
}
