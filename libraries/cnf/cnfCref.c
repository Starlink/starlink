#include <stdlib.h>		 /* Standard C run-time library		    */
#include "f77.h"                 /* CNF macros and prototypes               */

F77_CHARACTER_ARG_TYPE *cnfCref( int length )

/*
*+
*  Name:
*     cnfCref

*  Purpose:
*     Create a Fortran string and return a pointer to it.

*  Language:
*     ANSI C

*  Invocation:
*     pointer = cnfCref( length )

*  Description:
*     Create a temporary Fortran string and return a pointer to it.
*     The space allocated is sufficient to contain a string with
*     "length" characters. The string is not initialised.
*     

*  Arguments:
*     int length (Given)
*        The length of the string in characters.

*  Returned Value:
*     F77_CHARACTER_ARG_TYPE *cnfCreat
*        A pointer to the storage that has been allocated by this routine.
*        It should be freed after use using "cnfFree" (not "free").

*  Notes:
*     -  The pointer may not point directly at the string of characters.
*        It is the thing required to be passed with the CHARACTER_ARG
*        macro to a Fortran routine.
*     -  If the routine could not create the space, then it returns a
*        null pointer.

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
/* Local Variables:							    */

   F77_CHARACTER_ARG_TYPE *ptr;			 /* A pointer to the string allocated       */


/* Allocate the space.							    */

   ptr = (F77_CHARACTER_ARG_TYPE *)malloc( (size_t)( length?length:1 ) );

/* Check for malloc returning a null value. If it does not, set the string  */
/* to the null character.						    */

   if( ptr != 0 )
      ptr[0] = '\0';

   return( ptr );
}
