#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include "f77.h"
#include "mers.h"
#include "ems.h"
#include "sae_par.h"
#include "star/grp.h"

F77_SUBROUTINE(ndg1_regsb)( CHARACTER(RE), INTEGER(IGRP0),
                            INTEGER(IGRP), INTEGER(SIZE), INTEGER(STATUS)
                            TRAIL(RE) ){
/*
*+
*  Name:
*     NDG1_RESGB

*  Purpose:

*  Description:

*  Parameters:
*     RE = _CHAR (Given)
*       Regular expression to apply to group members.
*     IGRP0 = INTEGER (Given)
*       Input group
*     IGRP  = INTEGER (Given & Returned)
*       Output group containing modified file names.
*     SIZE = INTEGER (Returned)
*       Size of output group after modification.
*     STATUS = INTEGER (Given & Returned)
*       Inherited status

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-AUG-2000 (DSB):
*        Original version.
*     27-FEB-2006 (TIMJ):
*        Use C API for Grp. Use MERS for error handling.
*        Free Grp resources. Use fdopen rather than fopen to open the temp
*        file. Safer than removing a file and re-opening it.
*     28-FEB-2006 (DSB):
*        Use grpF2C.
*     15-JUL-2008 (TIMJ):
*        Tweak to GRP C API.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

   GENPTR_CHARACTER(RE)
   GENPTR_INTEGER(IGRP0)
   GENPTR_INTEGER(IGRP)
   GENPTR_INTEGER(SIZE)
   GENPTR_INTEGER(STATUS)

#define BUFLEN 256

   FILE *fd = NULL;
   Grp *igrp0;
   Grp *igrp;
   char *script;
   char buf[BUFLEN];
   char infile_name[255];
   char name[GRP__SZNAM+1];
   char outfile_name[255];
   int added;
   int flag;
   size_t i;
   int ifd = 0;
   int istat;
   int ofd = 0;
   size_t size;

/* initialise return value */
   *SIZE = 0;

/* Check the global status. */
   if( *STATUS != SAI__OK ) return;

/* Get C Grp pointers for the supplied Fortran identifiers */
   igrp = (Grp *) grpF2C( *IGRP, STATUS );
   igrp0 = (Grp *) grpF2C( *IGRP0, STATUS );

/* Get an input and output file handle */
   strcpy( infile_name, "regsb_inXXXXXX" );
   ifd = mkstemp( infile_name );
   if( ifd == -1 ){
      *STATUS = SAI__ERROR;
      errSyser( "SYS", errno );
      errRep( " ", "Unable to create a temporary \"regsb_in\" file name: ^SYS",
              STATUS );
      goto CLEANUP;
   }

   strcpy( outfile_name, "regsb_outXXXXXX" );
   ofd = mkstemp( outfile_name );
   if( ifd == -1 ){
      *STATUS = SAI__ERROR;
      errSyser( "SYS", errno );
      errRep( " ", "Unable to create a temporary \"regsb_out\" file name: ^SYS",
              STATUS );
      /* close the first file */
      close(ifd);
      goto CLEANUP;
   } else {
     /* Close the output filehandle since we need this for sed output.
	Do not remove the file since we want to avoid a possible race condition.
	Force sed to append to the empty file */
     close(ofd);
   }

/* open the input file descriptor for write */
   fd = fdopen( ifd, "w" );
   if( !fd ){
      *STATUS = SAI__ERROR;
      errSyser( "SYS", errno );
      errRep( " ", "Unable to open FILE* from existing file descriptor using \"fdopen\": ^SYS",
	      STATUS );
      /* Close the first file descriptor */
      close(ifd);
      goto CLEANUP;
   }

/* Get the number of names to store in the file. */
   size = grpGrpsz( igrp0, STATUS );

/* Write the supplied names to the text file. */
   for( i = 1; i <= size && *STATUS == SAI__OK; i++ ){
      grpInfoc( igrp0, i, "NAME", name, (GRP__SZNAM+1), STATUS );
      fprintf( fd, "%s\n", name );
   }

/* Close the input file - this will free the original file descriptor */
   fclose( fd );

/* Execute the sed command, writing the results to the output file. */
   if( *STATUS == SAI__OK ){

/* Allocate memory for the sed command string. */
      script = (char *) malloc( (size_t) ( strlen( "sed -e '" )
                                           + RE_length
                                           + strlen( "' " )
                                           + strlen( infile_name )
                                           + strlen( " 1>" )
                                           + strlen( outfile_name )
                                           + strlen( " 2>&1" )
                                           + 1 ) );
      if( !script ) {
         *STATUS = SAI__ERROR;
         errRep( " ", "NDG_REGSB: Failed to allocate memory for sed command.",
                 STATUS );

/* If OK, construct the sed command string. */
      } else {
         strcpy( script, "sed -e '" );
         strncpy( script + 8, RE, RE_length );
         strcpy( script + 8 + RE_length, "' " );
         strcpy( script + 10 + RE_length, infile_name );
         strcpy( script + strlen( script ), " 1>" );
         strcpy( script + strlen( script ), outfile_name );
         strcpy( script + strlen( script ), " 2>&1" );

/* Execute the command, sending standard out and error to the output file. */
         istat = system( script );

/* Remove the input file. */
         remove( infile_name );

/* Deallocate the command string. */
         free( script );

/* Set STATUS and report an error if anything went wrong in the system
   call. */
         if( istat ) {
	   *STATUS = SAI__ERROR;
	   emsSetnc( "RE", RE, RE_length );
	   errRep(" ", "Supplied sed expression \"^RE\" could not be used", STATUS );

/* Attempt to copy error messages from the output file to the error system. */
	   fd = fopen( outfile_name, "r" );
	   if( fd ) {
	     while( fgets( buf, BUFLEN, fd ) ){
	       if( strlen( buf ) ) {
		 errRep(" ", buf, STATUS );
	       }
	     }
	     fclose( fd );
	   }

	   goto CLEANUP;
         }

/* Attempt to open the output file. */
         fd = fopen( outfile_name, "r" );

/* If succesful, extract each line of the file and append it to the
   returned group. Check for NDFs which did not match the supplied RE.
   These will be unchanged in the output file. Replace these lines with
   blanks to avoid the input NDF being used as the output NDF. */
         if( fd ) {
            i = 1;
            while( fgets( buf, BUFLEN, fd ) ){
               if( strlen( buf ) ) {
		 grpInfoc( igrp0, i, "NAME", name, (GRP__SZNAM+1), STATUS );
                  if( strncmp( name, buf, strlen( name ) ) ){
		    grpGrpex( buf, NULL, igrp, &size, &added, &flag, STATUS );
                  }
               }
               i++;
            }

/* Close the output file. */
            fclose( fd );

/* Get the number of names now in the returned group. */
            size = grpGrpsz( igrp, STATUS );
	    *SIZE = size;
         }
      }
   }

 CLEANUP:
/* Remove files without checking return value */
   remove( outfile_name );
   remove( infile_name );
}
