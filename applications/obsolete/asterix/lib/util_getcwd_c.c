/*
*+
*  Name:
*     UTIL_GETCWD

*  Purpose:
*     Returns the name of the current working directory.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL UTIL_GETCWD( PATHNM, STATUS )

*  Description:
*     Returns the name of the current working directory.

*  Arguments:
*     PATHNM = CHARACTER*(*) (returned)
*        The current working directory
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
*     {enter_new_authors_here}

*  History:
*      9 Mar 1993 (RDS):
*        Original version.
*     26 Nov 1993 (DJA):
*        Given correct name UTIL_GETCWD. Error handling corrected.
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
#include "ems.h"          /* Error handling */

/* VMS definitions
 */
#if defined(VAX)
#include <unixlib.h>			/* For getcwd */

/* UNIX definitions
 */
#else
#include <unistd.h>

#endif


/*
 *  Body of code
 */
F77_SUBROUTINE(util_getcwd)( CHARACTER(pathnm), INTEGER(status) TRAIL(pathnm))
  {
  GENPTR_CHARACTER(pathnm)
  GENPTR_INTEGER(status)

  char		*pthstr;		/* C string holding directory name */

#if defined(VAX)
#else
   char *ptest;
#endif

/* Check inherited global status on entry */
  if ( *status != SAI__OK )
    return;

/* Get the current working directory and set status GOOD if the operation
 * is successful */

/*   Use system specific call to get the current directory */

#if defined(VAX)

/* Make temp space size of Fortran string + space for trailing null */
  pthstr = cnf_creat(pathnm_length+1);

/* Set status from getcwd result */
  *status = getcwd( pthstr, pathnm_length+1 ) ? SAI__OK : SAI__ERROR;

/* Export C string to Fortran */
  cnf_exprt( pthstr, pathnm, pathnm_length );

/* Free temporary string */
  if ( pthstr ) cnf_free( pthstr );
#else

/* Get working directory */
  ptest = getcwd( (char *)0, pathnm_length + 1 );

/* Will we have to truncate the text? */
  if ( ! ptest ) {
    *status = SAI__ERROR;
    ems_rep_c( " ", "Out of memory getting current directory name", status );
    }
  else {

/* Export C string to Fortran */
    cnf_exprt( ptest, pathnm, pathnm_length );

    free( ptest );
    }
#endif

/* Report errors */
  if (*status != SAI__OK ) {
    ems_rep_c( " ", "Error finding current directory", status );
    ems_rep_c( " ", "...from UTIL_GETCWD", status );
    }
  }
