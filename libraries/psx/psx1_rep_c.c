#include "ems.h"		 /* EMS function prototypes		    */


void psx1_rep_c( char *param, char *text, int *status )

/*
*+
*  Name:
*     psx1_rep_c

*  Purpose:
*     Provide a PSX wrap up for ems_rep_c

*  Language:
*     ANSI C

*  Invocation:
*     psx1_rep_c( param, text, status )

*  Description:
*     Provide a wrap up for the C callable EMS routine ems_rep_c so that
*     error reporting can easily be replaced with calls to something
*     other than EMS should the need arise.

*  Arguments:
*     char *param (Given)
*     char *text (Given)
*     int *status (Given and Returned via Pointer)

*  External Routines Used:
*     EMS:
*        ems_rep_c

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     15-APR-1991 (PMA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
------------------------------------------------------------------------------
*/


{
   ems_rep_c( param, text, status );
}
