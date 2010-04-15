/*
*+
*  Name:
*     antmaster

*  Purpose:
*     Test adamnet

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
#include <time.h>
#include <sys/types.h>
#include <sys/time.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <unistd.h>
#include "sae_par.h"
#include "adam_defns.h"
#include "dtask_err.h"             /* dtask error codes */

#include "messys_len.h"
#include "messys_err.h"
#include "messys_par.h"

#include "ams.h"

int main()
{
   int outmsg_status;
   int outmsg_function;
   int outmsg_context;
   int outmsg_length;
   char outmsg_name[32];
   char outmsg_value[MSG_VAL_LEN];
   int inmsg_status;
   int inmsg_context;
   int inmsg_length;
   char inmsg_name[32];
   char inmsg_value[MSG_VAL_LEN];
   int status;
   int path;
   int messid;
   int j;

   status = 0;
   outmsg_status = SAI__OK;
   outmsg_function = MESSYS__MESSAGE;
   outmsg_context = OBEY;
   outmsg_length = 16;

   strcpy ( outmsg_name, "trace" );
   strcpy ( outmsg_value, "" );

   ams_init ( "master", &status );
   if ( status != SAI__OK )
   {
      printf ( "master - bad status after ams_init\n" );
   }
/*   ams_path ( "cosaxp0.roe.ac.uk!!slave", &path, &status ); */
/*   ams_path ( "resun06!!slave", &path, &status ); */
   ams_path ( "revaxi!!trace", &path, &status );
   if ( status != SAI__OK )
   {
      printf ( "master - bad status after ams_path\n" );
   }
   else
   {
      printf ( "master - got path ok = %d\n", path );
   }

   for ( j=0; j<1; j++ )
   {
      ams_send ( path, outmsg_function, outmsg_status, outmsg_context,
        outmsg_name, outmsg_length, outmsg_value, &messid, &status );
      if ( status != SAI__OK )
      {
         printf ( "master: bad status after send\n" );
      }
      else
      {
      printf ( "master - sent message ok\n" );
      }
      ams_getreply ( MESSYS__INFINITE, path, messid, 32, MSG_VAL_LEN,
        &inmsg_status, &inmsg_context, inmsg_name, &inmsg_length,
        inmsg_value, &status );
      if ( status != SAI__OK )
      {
         printf ( "master: bad status on first getreply = %d\n", status );
      }
      else
      {
      printf ( "master - got first message ok\n" );
      printf ( "master - first string = %s\n", inmsg_value );
      printf ( "master - first in status = %d\n", inmsg_status );
      }
      for ( ; ; )
      {
         ams_getreply ( MESSYS__INFINITE, path, messid, 32, MSG_VAL_LEN,
           &inmsg_status, &inmsg_context, inmsg_name, &inmsg_length,
           inmsg_value, &status );
         if ( inmsg_status != MESSYS__MESSAGE )
         {
            break;
         }
         else
         {
            printf ( "%s", inmsg_value );
         }
      }
   }
   if ( status != 0 )
   {
      printf ( "master: bad status = %d\n", status );
   }
   else
   {
      printf ( "master: received - %s\n", inmsg_value );
   }

   status = SAI__OK;
/*   ams_receive ( MESSYS__INFINITE, 32, MSG_VAL_LEN, &inmsg_status,
     &inmsg_context, inmsg_name, &inmsg_length, inmsg_value, &path,
     &messid, &status ); */
   kill ( getpid(), SIGINT );
   return 0;
}
