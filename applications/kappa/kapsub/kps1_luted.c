#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "f77.h"
#include "sae_par.h"
#include "merswrap.h"
#include <tcl.h>

extern F77_SUBROUTINE(err_rep)( CHARACTER(param), CHARACTER(mess),
                                INTEGER(STATUS) TRAIL(param) TRAIL(mess) );

void Error( const char *, int * );
char *cstring( const char *, int, int * );

F77_SUBROUTINE(kps1_luted)( CHARACTER(CMD), INTEGER(STATUS) 
                            TRAIL(CMD) ){
/*
*  Name:
*     kps1_luted

*  Purpose:
*     Activates the main LutEdit tcl script.

*  Description:
*     This C function executes the LutEdit Tcl script in a child process.

*  Parameters:
*     CMD = CHARACTER * ( * ) (Given)
*        A string representing the command to be used to execute the
*        lutedit script.
*     STATUS = INTEGER (Given and Returned)
*        The inherited global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-NOV-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/

   GENPTR_CHARACTER(CMD)
   GENPTR_INTEGER(STATUS)

#define BUFLEN 512
   char buf[BUFLEN];
   char outfile_name[255];
   char *script = NULL;
   int report;
   char *cmd;
   FILE *fd = NULL;

/* Check the global status. */
   if( *STATUS != SAI__OK ) return;

/* Get a null terminated copy of the command. */
   cmd = cstring( CMD, CMD_length, STATUS );
   if ( cmd ) {

/* Get a unique temporary file name. This file is used to collect
   any standard output from the TCL script. */
      if( !tmpnam( outfile_name ) ){
         *STATUS = SAI__ERROR;
         Error( "Unable to create a temporary file name using \"tmpnam\".", 
                 STATUS );
         return;
      } 

/* Construct the full command for the TCL script (redirecting standard
   output and error to the temporary file chosen above). */
      if( *STATUS == SAI__OK ){
         script = (char *) malloc( (size_t) ( strlen( cmd )
                                            + strlen( " 1>" )  
                                            + strlen( outfile_name ) 
                                            + strlen( " 2>&1" ) 
                                            + 1 ) );
         if( !script ) {
            *STATUS = SAI__ERROR;
            Error( "Failed to allocate memory for full TCL command .", 
                    STATUS );
         } else {
            strcpy( script, cmd );
            strcpy( script + strlen( script ), " 1>" );
            strcpy( script + strlen( script ), outfile_name );
            strcpy( script + strlen( script ), " 2>&1" );
         }
      }

/* Execute the TCL script. */
      if( *STATUS == SAI__OK ){
         (void) system( script );

/* Attempt to open the file containing the standard output and error from 
   the TCL script. */
         fd = fopen( outfile_name, "r" );

/* If succesful, display each non-null line of the file and report an error. */
         if( fd ) {

            report = 0;
            while( fgets( buf, BUFLEN, fd ) ){
               if( strlen( buf ) ) {
                  msgOut( " ", buf, STATUS );
                  report = 1;
               }
            }

            if( report && *STATUS == SAI__OK ){
               *STATUS = SAI__ERROR;
               Error( "Messages received from the TCL script.", STATUS );
            }
 
            fclose( fd );
            remove( outfile_name );
         }
      }

/* Free the memory holding the TCL script name. */
      if( script ) free( script );

/*  Free memory used to hold the null-terminated file name */
      free( cmd );

   }
}

void Error( const char *text, int *STATUS ) {
/*
*  Name:
*     Error

*  Purpose:
*     Report an error using EMS.

*  Description:
*     The supplied text is used as the text of the error message.
*     A blank parameter name is used.

*  Parameters:
*     text
*        The error message text. Only the first 80 characters are used.
*     STATUS
*        A pointer to the global status value. This should have been set
*        to a suitable error value before calling this function.

*  Notes:
*     - If a NULL pointer is supplied for "text", no error is reported.
*/

   DECLARE_CHARACTER(param,1);
   DECLARE_CHARACTER(mess,80);
   int j;

/* Check the supplied pointer. */
   if( text ) {

/* Set the parameter name to a blank string. */
      param[0] = ' ';

/* Copy the first "mess_length" characters of the supplied message into 
      "mess". */
      strncpy( mess, text, mess_length );

/* Pad any remaining bytes with spaces (and replace the terminating null
   character with a space). */
      for( j = strlen(mess); j < mess_length; j++ ) {
         mess[ j ] = ' ';
      }

/* Report the error. */
      F77_CALL(err_rep)( CHARACTER_ARG(param), CHARACTER_ARG(mess),
                         INTEGER_ARG(STATUS) TRAIL_ARG(param) 
                         TRAIL_ARG(mess) );
   }
}

char *cstring( const char *fstring, int len, int *STATUS ) {
/*
*  Name:
*     cstring

*  Purpose:
*     Returns a pointer to dynaically allocated memory holding a null
*     terminated copy of an F77 string.

*  Description:
*     This function returns a pointer to dynaically allocated memory 
*     holding a null terminated copy of an F77 string. The pointer should
*     be freed using free() when no longer needed.

*  Parameters:
*     fstring = const char * (Given)
*        The f77 string.
*     len = int (Given)
*        The length of the f77 string to be stored.
*     STATUS = int * (Given and Returned)
*        The inherited status.
*     
*/

   char mess[81];
   char *ret;

   ret = NULL;

/* Check the inherited status. */
   if( *STATUS != SAI__OK ) return;

/* Find the length excluding any trailing spaces. */
   len = len - 1;
   while( len && fstring[len] == ' ' ) len--;
   len++;

/* Allocate memory to hold a null-terminated copy of the supplied F77
   string. */
   ret = (char *) malloc ( sizeof(char)*(size_t) ( len + 1 ) );

/* If successful, copy the string, and append a trailing null character. */
   if ( ret ) {
      memcpy( ret, fstring, len );

      ret[ len ] = 0;

/* Report an error if the memory could not be allocated. */
   } else {
      *STATUS = SAI__ERROR;
      sprintf( mess, "Unable to allocate %d bytes of memory. ", len + 1 );
      Error( mess, STATUS );
   }

   return ret;
}

