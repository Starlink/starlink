#include <stdlib.h>		 /* Standard C run-time library		    */
#include "f77.h"                 /* CNF macros and prototypes               */
char *cnfCreat( int length )

/*
*+
*  Name:
*     cnfCreat

*  Purpose:
*     Create a temporary C string and return a pointer to it

*  Language:
*     ANSI C

*  Invocation:
*     pointer = cnfCreat( length )

*  Description:
*     Create a temporary C string and return a pointer to it.
*     The space allocated to the C string is `length' characters and is
*     initialized to the null string.

*  Arguments:
*     int length (Given)
*        The length of the space to be allocated in characters.

*  Returned Value:
*     char *cnfCreat
*        A pointer to the storage that has been allocated by this routine.
*        It should be freed after use using "free" (not cnfFree).

*  Notes:
*     -  If the argument is given as N then there is room to store N-1
*        characters plus a trailing null character in a C string.
*     -  If the routine could not create the space, then it returns a
*        null pointer.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     26-MAR-1991 (PMA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*...........................................................................*/

{
/* Local Variables:							    */

   char *ptr;			 /* A pointer to the storage allocated.	    */


/* Allocate the space.							    */

   ptr = (char *)malloc( (size_t)( length ) );

/* Check for malloc returning a null value. If it does not, set the string  */
/* to the null character.						    */

   if( ptr != 0 )
      ptr[0] = '\0';

   return( ptr );
}
