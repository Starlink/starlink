/*
*+
*  Name:
*     UTIL_DELETE

*  Purpose:
*     Deletes a file from the file system

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL UTIL_DELETE( SPEC, STATUS )

*  Description:
*     Deletes a file from the file system.

*  Arguments:
*     SPEC = CHARACTER*(*) (given)
*        The file specification of the file to be deleted
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

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     util Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/util.html

*  Keywords:
*     package:util, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     AJC: Alan J. Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     26-Nov-1993 (DJA):
*        Original version.
*      5-Dec-2001 (AJC)
*        New EMS and CNF routine names etc
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
#include <errno.h>
#include <string.h>

/* VMS definitions
 */
#if defined(VAX)
F77_INTEGER_FUNCTION(lib$delete_file)( CHARACTER(arg) TRAIL(arg) );
#else
# include <stdio.h>
#endif

/*
 *  Body of code
 */
F77_SUBROUTINE(util_delete)( CHARACTER(spec), INTEGER(status) TRAIL(spec) )
  {
  GENPTR_CHARACTER(spec)
  GENPTR_INTEGER(status)

  char		*cspec;			/* C string holding file to delete */
  int		lstat;			/* Delete status */

/* Check inherited global stratus on entry */
  if ( *status != SAI__OK )
    return;

#if defined(VAX)
  lstat = F77_EXTERNAL_NAME(lib$delete_file)( CHARACTER_ARG(spec)
           TRAIL_ARG(spec) );
  if ( lstat != 1 ) {
    emsSyser( "REASON", lstat ); 
    *status = SAI__ERROR;
    }

#else
/* Import Fortran string to C */
  cspec = cnfCreim( spec, spec_length );

/* Try to remove file */
  if ( remove(cspec) ) {
    emsSyser( "REASON", errno ); 
    *status = SAI__ERROR;
    }

/* Free temporary string */
  if ( spec )
    cnfFree( cspec );			
#endif

/* Report errors */
  if (*status != SAI__OK ) {
    emsSetnc( "FILE", spec, spec_length );
    emsRep( " ", "Error deleting the file /^FILE/ - ^REASON", status );
    emsRep( " ", "...from UTIL_DELETE", status );
    }
  }
