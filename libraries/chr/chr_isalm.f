      LOGICAL FUNCTION CHR_ISALM( CVALUE )
*+
*  Name:
*     CHR_ISALM

*  Purpose:
*     Return whether a character is alphanumeric.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_ISALM( CVALUE )

*  Description:
*     Determine whether a character is alphanumeric, i.e. A - Z, 
*     a - z, 0 - 9 or _. Note that this routine treats the underscore 
*     character as an alphanumeric character.

*  Arguments:
*     CVALUE = CHARACTER (Given)
*        The character to be tested.

*  Returned Value:
*     CHR_ISALM = LOGICAL
*        Returns .TRUE. if the given character is alphanumeric,
*        returns .FALSE. otherwise.

*  Algorithm:
*     Use CHR_ISALF, CHR_ISNUM and an explicit test for '_'.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     DLT: D.L. Terrett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     16-NOV-1984 (ACD):
*        Documentation improved.
*     26-OCT-1988 (AJC):
*        Documentation improved.
*     25-JAN-1990 (DLT):
*        Use local variable instead of function name
*        in tests because of DECstation compiler bugs.
*     5-FEB-1991 (PCTR):
*        New code to conform more closely to Fortran 77.
*     31-OCT-1991 (PCTR):
*        Documentation improved.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER CVALUE

*  External References:
      LOGICAL CHR_ISALF          ! Whether alphabetic
      LOGICAL CHR_ISDIG          ! Whether digit

*.

*  Initialise the returned value.
      CHR_ISALM = .TRUE.

*  Perform the tests.
      IF ( .NOT. CHR_ISALF( CVALUE ) ) THEN

         IF ( .NOT. CHR_ISDIG( CVALUE ) ) THEN
            CHR_ISALM = ( CVALUE .EQ. '_' )
         END IF
      END IF

      END
