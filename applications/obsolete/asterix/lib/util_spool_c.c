/*+
 *  Name:
 *     util_spool_c.c
 *
 *  Purpose:
 *     Spool a file to the printer using a command defined by the ASTERIX
 *     environment variable AST_<TYPE>_SPOOL. Errors by the spooling process
 *     are reported and flushed to the environment.
 *
 *  Invokation :
 *
 *     From FORTRAN use as follows,
 *
 *       CALL UTIL_SPOOL( FILE, TYPE, DELETE, STATUS )
 *
 *     where FILE is the file to be spooled, and TYPE is one of the file
 *     types for which a spool command is defined, eg. TEXT or PS.
 *
 *  Language:
 *
 *     Starlink ANSI C
 *
 *  Description:
 *
 *  Authors:
 *
 *     David J. Allan (BHVAD::DJA)
 *
 *  History:
 *
 *    29-JUL-1993 (DJA):
 *        Original version.
 *- */

/* Include Statements: */
#include "sae_par.h"
#include "f77.h"                       /* c <-> FORTRAN interfacing */
#include "cnf.h"                       /* c <-> FORTRAN strings */
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

F77_SUBROUTINE(util_spool)( CHARACTER(file), CHARACTER(type),
      LOGICAL(del), INTEGER(status) TRAIL(file) TRAIL(type) )
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
  char  *spcmd;                         /* The spool command */
  int   splen;				/* Length of *spcmd */
  DECLARE_CHARACTER(cstr,132);          /* VMS command line */

  if ( (*status) != SAI__OK ) 		/* Check status */
    return;

  esym = (char *)                       /* Make space for new string */
         malloc(type_length+1+10);
  strncpy( esym, "AST_", 4 );           /* Construct environment variable */
  strncpy( esym+4, type, type_length );
  strcpy( esym+4+type_length, "_SPOOL" );

  spcmd = getenv( esym );		/* Get spool command */
  free( esym );

  if ( spcmd )                          /* Spool command defined? */
    {
    fname = cnf_creim( file,            /* Import filename to C string */
              file_length );

    splen = strlen(spcmd);
    strcpy( cstr, spcmd );
    cstr[splen] = ' ';
    strcpy( cstr + splen + 1, fname );

#ifdef VAX
    if ( F77_ISTRUE(*del) )		/* Deletion required? */
      if ( strncmp(cstr,"PRINT",5) == 0 )	/* Simple print command? */
        {
        strcpy( cstr + splen + 		/* Tack on a delete qualifier */
          strlen(fname) + 1 , "/DELETE" );
        }

    lib$spawn( CHARACTER_ARG(cstr) TRAIL(cstr) );

#else
    if ( (cstatus = vfork()) == 0 )	/* Successful child creation returns */
					/* zero to the child process and the */
					/* +ve PID to the parent */
      {
      if ( cstatus = execlp( spcmd, 	/* Child ok, so try to run command */
                      spcmd, fname,
                        (char *)0 ) == -1 )
        {
        printf( "Failed to execute command %s %s\n", spcmd, fname );

        _exit(1);			/* Kills the child process */
        }
      }
    else if ( cstatus <= -1 )		/* Parent only, failed to create child */
      {
      *status = SAI__ERROR;
      ems_rep_c( " ", "Failed to create sub-process", status );
      }
    else				/* Parent, successful child creation */
      {
      wstatus = wait( &cstatus );	/* Wait for child to finish */

      if ( F77_ISTRUE(*del) )		/* Delete the file? */
	{
	if ( remove( fname ) )		/* Try to delete it */
          {
          *status = SAI__ERROR;
          ems_setc_c( "FILE", file, file_length );
          ems_rep_c( " ", "Unable to delete ^FILE after spooling", status );
          }
	}
      }
#endif

    cnf_free( fname );			/* Free space */
    }
  else
    {
    *status = SAI__ERROR;
    ems_setc_c( "FILE", file, file_length );
    ems_rep_c( " ", "File ^FILE not spooled", status );
    ems_setc_c( "TYPE", type, type_length );
    ems_rep_c( " ", "No spool command defined for file type ^TYPE", status );
    }

  }
