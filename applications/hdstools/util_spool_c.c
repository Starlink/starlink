/*
*+
*  Name:
*     UTIL_SPOOL

*  Purpose:
*     Spool a file to a printer

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL UTIL_SPOOL( FILE, TYPE, DELETE, STATUS )

*  Description:
*     Spool a file to the printer using a command defined by the ASTERIX
*     environment variable AST_<TYPE>_SPOOL. Errors by the spooling process
*     are reported and flushed to the environment. The value of TYPE can
*     be anything as long as the appropriate environment variables set.
*     Examples might be FORTRAN, PS or TEXT.
*     {routine_description}

*  Arguments:
*     FILE = CHARACTER*(*) (given)
*        The name of the file to be spooled
*     TYPE = CHARACTER*(*) (given)
*        The type describing the contents or format of the file
*     DELETE = LOGICAL (given)
*        Delete the file after it is spooled?
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  References:
*     util Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/util.html

*  Keywords:
*     package:util, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     AJC: Alan J Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     29 Jul 1993 (DJA):
*        Original version.
*      5-Dec-2001 (AJC):
*        Revise and improve
*        New form of ems and cnf routine name
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*
 *  Include files
 */
#include "sae_par.h"
#include "f77.h"
#include "cnf.h"
#include "ems.h"                       /* Error handling */
#include <stdio.h>                     /* i/o handling */
#include <stdlib.h>
#include <string.h>                    /* String handling */
#include <ctype.h>                     /* Character handling */

#ifndef VAX
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#endif


/*
 *  Body of code
 */
F77_SUBROUTINE(util_spool)( CHARACTER(file), CHARACTER(type), LOGICAL(del),
                            INTEGER(status) TRAIL(file) TRAIL(type) )
  {
  GENPTR_CHARACTER(file)
  GENPTR_CHARACTER(type)
  GENPTR_LOGICAL(del)
  GENPTR_INTEGER(status)

  int   cstatus;                        /* Child creation status */
  int   wstatus;                        /* Child creation status */
  char  *esym;                 		/* Environment variable name */
  char	*fname;				/* File to be spooled */
  char	*tname;				/* File type */
  char  *spcmd;                         /* Pointer to spool env variable */
  char  *argv[10];                      /* argv for execvp call */
  int   splen;				/* Length of *spcmd */
  int   i;                              /* Loop counter */
  DECLARE_CHARACTER(cstr,132);          /* VMS command line */

/* Check inherited global stratus on entry */
   if ( *status != SAI__OK ) return;

/* Make space for new string */
   esym = (char *) malloc(type_length+1+10);

/* Construct environment variable name */
   strncpy( esym, "AST_", 4 );
   strncpy( esym+4, type, type_length );
   strcpy( esym+4+type_length, "_SPOOL" );

/* Get spool command */
   spcmd = getenv( esym );
   free( esym );

/* Import filename to C string */
   fname = cnfCreim( file, file_length );


#ifdef VAX
   splen = strlen(spcmd);
   strcpy( cstr, spcmd );
   cstr[splen] = ' ';
   strcpy( cstr + splen + 1, fname );

/* Deletion required? */
   if ( F77_ISTRUE(*del) )

/* Simple print command? */
      if ( strncmp(cstr,"PRINT",5) == 0 ) {
/* Tack on a delete qualifier */
         strcpy( cstr + splen +
         strlen(fname) + 1 , "/DELETE" );
      }

   lib$spawn( CHARACTER_ARG(cstr) TRAIL(cstr) );

#else

/* Spool command defined?  If not, use lpr */
   if ( spcmd != NULL ) {
      strcpy( cstr, spcmd );

/* Construct the args array for the exec call */
      argv[0] = strtok( cstr, " " );
      i = 1;
      while( ( argv[i++]=strtok(NULL," ") ) != NULL ){};
      argv[i--] = NULL;
      argv[i] = fname;

/* Successful child creation returns zero to the child process and the */
/* +ve PID to the parent */
      if ( (cstatus = vfork()) == 0 ) {

         if ( cstatus = execvp( argv[0], argv ) == -1 ) {
            emsSetc( "CMD", cstr );
            emsSetc( "CMD", " " );
            emsSetc( "CMD", fname );
            emsRep( " ", "Failed to execute command ^CMD", status );

            _exit(1);			/* Kills the child process */
         }

      } else if ( cstatus <= -1 ) {
/* Parent only, failed to create child */
         *status = SAI__ERROR;
         emsRep( " ", "Failed to create sub-process", status );

      } else {
/* Parent, successful child creation */
/* Wait for child to finish */
         wstatus = wait( &cstatus );

/* Delete the file? */
         if ( F77_ISTRUE(*del) ) {

/* Try to delete it */
            if ( remove( fname ) ) {

               *status = SAI__ERROR;
               emsSetc( "FILE", fname );
               emsRep( " ",
                 "Unable to delete ^FILE after spooling", status );
            }
         }
      }
#endif

/* Free space */
      cnfFree( fname );
   } else {
      *status = SAI__ERROR;
      emsSetc( "FILE", fname );
      emsRep( " ", "File ^FILE not spooled", status );
      emsSetnc( "TYPE", type, type_length );
      emsRep( " ", "No spool command defined for file type ^TYPE", status );
   }

}
