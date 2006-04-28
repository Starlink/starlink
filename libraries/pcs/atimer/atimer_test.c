/*
*+
*  Name:
*     atimer_test

*  Purpose:
*     Test atimer

*  Language:
*     Starlink C

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/    

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>
#include <errno.h>

#include "atimer_par.h"
#include "atimer.h"


void handler
(
int par
)

{
   printf ( "%d\n", par );
}

int main()

{

   int status;
   char instring[80];
   int ans;

   status = 0;
   atimer_settimr ( 1000, 1000, handler, &status );
   atimer_settimr ( 2000, 2000, handler, &status );
   atimer_settimr ( 3000, 3000, handler, &status );
   atimer_settimr ( 4000, 4000, handler, &status );

   atimer_cantim ( 3000, &status );

   for ( ; ; )
   {
      ans = getchar();
      if ( ans == 0 )
      {
         if ( errno == EINTR )
         {
/*   Input interrupted by the timer, try again */
         }
         else
         {
            perror ( "timetest finished" );
            break;
         }
      }
      else if ( ans == EOF )
      {
         printf ( "timetest finishing, EOF received\n" );
         break;
      }
      else
      {
         printf ( "timetest finishing, input received\n" );
         break;
      }
   }
}
