      LOGICAL FUNCTION CHR_ISALF( CVALUE )
*+
*  Name:
*     CHR_ISALF

*  Purpose:
*     Return whether a character is alphabetic.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_ISALF( CVALUE )

*  Description:
*     The given character is tested for being alphabetic, i.e. A - Z 
*     or a - z.

*  Arguments:
*     CVALUE = CHARACTER (Given)
*        The character to be tested.

*  Returned Value:
*     CHR_ISALF = LOGICAL
*        Returns .TRUE. if the given character is alphabetic,
*        returns .FALSE. otherwise.

*  Algorithm:
*     Check the ASCII value of given character against the range of
*     ASCII values for upper and lowercase alphabetic characters
*     using the LLE and LGE intrinsic functions.

*  Authors:
*     ASOC5: Dave Baines (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     DLT: D.L. Terrett (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-JUN-1984 (ASOC5):
*        Original version.
*     2-SEP-1988 (AJC):
*        Remove INCLUDE 'SAE_PAR'.
*     26-OCT-1988 (AJC):
*        Improve documentation.
*     25-JAN-1990 (DLT):
*        Use local variable instead of function name for tests
*        and ICHAR instead of parameters because of DECstation
*        compiler bugs.
*     5-FEB-1991 (PCTR):
*        Changed code to conform more closely to Fortran 77.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER CVALUE

*.

*  Perform the ASCII comparison tests.
      IF ( LGE( CVALUE, 'A' ) .AND. LLE( CVALUE, 'Z' ) ) THEN
         CHR_ISALF = .TRUE.
      ELSE
         CHR_ISALF = ( LGE( CVALUE, 'a' ) .AND. LLE( CVALUE,  'z' ) )
      END IF

      END
