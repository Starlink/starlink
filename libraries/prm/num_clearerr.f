      SUBROUTINE NUM_CLEARERR()
*+
*  Name:
*     NUM_CLEARERR

*  Purpose:
*     Clear the global numeric error flag

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL NUM_CLEARERR()

*  Description:
*     Clears the NUM_ERROR status flag. This should be called prior to
*     performing numeric calculations with the low level NUM_ routines
*     in order to check whether a floating point exception has occurred.
*     No arguments are given and none are returned.

*  Authors:
*     Tim Jenness (JAC, Hawaii)

*  History:
*     1-OCT-2004 (TIMJ):
*        Original version

*  Notes:
*     This routine should always be called prior to using NUM_ routines
*     if the result of the routine is to be checked using NUM_WASOK or
*     NUM_GETERR

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! For good status

*  Global Variables:
      INCLUDE 'NUM_CMN'          ! Define the NUM_ERROR flag
*.

*     Clear the error flag unconditionally
      NUM_ERROR = SAI__OK

      END

