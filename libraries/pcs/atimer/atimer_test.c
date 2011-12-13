/*
*+
*  Name:
*     atimer_test

*  Purpose:
*     Test atimer

*  Language:
*     Starlink C

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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
