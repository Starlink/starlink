/* amsmaster
 * A test of ams - run in conjunction with amsslave
 *   % amsslave &
 *   % amsmaster
 *
 * History:
 *   xx-xxx-1994 (BDK):
 *      Original
 *    7-JUL-1994 (AJC):
 *      Tidy header files (some now renamed).
 *
*/
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <unistd.h>

#include "sae_par.h"
#include "adam_defns.h"
#include "messys_len.h"
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

   strcpy ( outmsg_name, "junk" );
   strcpy ( outmsg_value, "master calling" );

   ams_init ( "master", &status );
   if ( status != SAI__OK )
   {
      printf ( "master - bad status after ams_init\n" );
   }
   ams_path ( "slave", &path, &status );
   if ( status != SAI__OK )
   {
      printf ( "master - bad status after ams_path\n" );
   }
   else
   {
      printf ( "master - path set up ok\n" );
   }

   for ( j=0; j<100; j++ )
   {
      ams_send ( path, outmsg_function, outmsg_status, outmsg_context, 
        outmsg_name, outmsg_length, outmsg_value, &messid, &status );
      ams_getreply ( MESSYS__INFINITE, path, messid, 32, MSG_VAL_LEN, 
        &inmsg_status, &inmsg_context, inmsg_name, &inmsg_length, 
        inmsg_value, &status );
      ams_getreply ( MESSYS__INFINITE, path, messid, 32, MSG_VAL_LEN, 
        &inmsg_status, &inmsg_context, inmsg_name, &inmsg_length, 
        inmsg_value, &status );
   }
   if ( status != 0 )
   {
      printf ( "master: bad status = %d\n", status );
   }
   else
   {
      printf ( "master: received - %s\n", inmsg_value );
   }

   /* Removed to stop signal failing "make check" */
   /*kill ( getpid(), SIGINT );*/
   return 0;
}
