#include "star/mem.h"            /* Starlink malloc routines */
#include "f77.h"                 /* CNF macros and prototypes               */

void cnfFreef( F77_CHARACTER_ARG_TYPE *temp )

/*
*+
*  Name:
*     cnfFreef

*  Purpose:
*     Return temporary space allocated by cnfCref or cnfCrefa

*  Language:
*     ANSI C

*  Invocation:
*     cnfFreef( temp )

*  Description:
*     Return temporary storage space that was allocated by a previous
*     call to cnfCref or cnfCrefa. A special routine must be used in
*     case the Fortran string is handled via a descriptor which is pointed
*     to by "temp".

*  Arguments:
*     F77_CHARACTER_ARG_TYPE *temp (Given)
*        A pointer to the string.

*  Notes:
*     -  In this case, the source code for this function is trivial, 
*        being merely a call to the C run-time library routine, free(). 
*        However, it is included for completeness.

*  Copyright:
*     Copyright (C) 1996 CCLRC

*  Authors:
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     12-JAN-1996 (AJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*...........................................................................*/

{
   starFree( (void *)temp );
}

