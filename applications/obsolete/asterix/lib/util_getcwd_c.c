/*+
 *  Name:
 *     util_getcwd_c.c
 *
 *  Purpose:
 *     Returns the name of the current working directory.
 *
 *  Language:
 *
 *     Starlink ANSI C
 *
 *  Description:
 *
 *
 *  Invokation:
 *
 *     CALL UTIL_GETCWD( DIRNAME, STATUS )
 *
 *  Authors:
 *
 *     Richard D. Saxton (LTVAD::RDS)
 *
 *  History:
 *
 *      9-Mar-1993 (RDS):
 *        Original version.
 *     26-Nov-1993 (DJA):
 *        Given correct name UTIL_GETCWD. Error handling corrected.
 *- */

/* Include Statements: */
#include "sae_par.h"
#include "cnf.h"
#include "f77.h"
#include "ems.h"          /* Error handling */

/* VMS definitions
 */
#if defined(VAX)
#include <unixlib.h>			/* For getcwd */

/* UNIX definitions
 */
#else
#include <sys/param.h>			/* Brings in getwd and MAXPATHLEN */

#endif


F77_SUBROUTINE(util_getcwd)( CHARACTER(pathnm), INTEGER(status) TRAIL(pathnm))
  {
  GENPTR_CHARACTER(pathnm)
  GENPTR_INTEGER(status)

  char		*pthstr;		/* C string holding directory name */

#if defined(VAX)
#else
   char ptest[MAXPATHLEN];
#endif

  if ( *status != SAI__OK )		/* Check status on entry */
    return;

/* Get the current working directory and set status GOOD if the operation
 * is successful */

/*   Use system specific call to get the current directory */

#if defined(VAX)

  pthstr = cnf_creat(pathnm_length+1);  /* Make temp space size of Fortran */
					/* string + space for trailing null */

  *status = getcwd( pthstr, 		/* Set status from getcwd result */
                    pathnm_length+1 )
            ? SAI__OK : SAI__ERROR;

  cnf_exprt( pthstr, pathnm, 		/* Export C string to Fortran */
	     pathnm_length );

  if ( pthstr ) cnf_free( pthstr );	/* Free temporary string */
#else

  *status = getwd( ptest )		/* Set status from getwd result */
            ? SAI__OK : SAI__ERROR;

  if ( strlen(ptest) > pathnm_length ) 	/* Will we have to truncate the text? */
    {
    *status = SAI__ERROR;
    ems_rep_c( " ", "Text truncated copying current directory name", status );
    }
  else
    {
    cnf_exprt( ptest, pathnm, 		/* Export C string to Fortran */
	     pathnm_length );
    }
#endif

  if (*status != SAI__OK )		/* Report errors */
    {
    ems_rep_c( " ", "Error finding current directory", status );
    ems_rep_c( " ", "...from UTIL_GETCWD", status );
    }
  }
