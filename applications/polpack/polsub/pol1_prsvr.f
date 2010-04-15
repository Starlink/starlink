      SUBROUTINE POL1_PRSVR( SVERS, M, N, R, STATUS )
*+
*  Name:
*     POL1_PRSVR

*  Purpose:
*     Extract fields from a version number string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_PRSVR( SVERS, M, N, R, STATUS )

*  Description:
*     This routine extracts the fields from a version number string of
*     the form "m.n-r" where m, n and r are integers. An error is
*     reported if the string is not of this form, except that trailing
*     fields may be omitted if they are zero. The string must not be
*     totally blank.

*  Arguments:
*     SVERS = CHARACTER*(*) (Given)
*        The version number string.
*     M = LOGICAL (Returned)
*        The major version number.
*     N = LOGICAL (Returned)
*        The minor version number.
*     R = LOGICAL (Returned)
*        The revision number.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-APR-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER SVERS*(*)

*  Arguments Returned:
      INTEGER M
      INTEGER N
      INTEGER R

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      CHARACTER VERS*30          ! Version string stripped of parentheses
      INTEGER DOT                ! Index of first dot
      INTEGER F                  ! Index of first non-blank character
      INTEGER HYP                ! Index of first hyphen
      INTEGER L                  ! Index of last non-blank character
      INTEGER LM                 ! Length of major version field
      INTEGER LN                 ! Length of minor version field
      INTEGER LR                 ! Length of revision field
      INTEGER LV                 ! Used length of whole string
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the string has delimiting parentheses, remove them.
      VERS = SVERS
      CALL CHR_FANDL( SVERS, F, L )
      IF( F + 1 .LE. L - 1 ) THEN

         IF( SVERS( F : F ) .EQ. '(' .AND.
     :       SVERS( L : L ) .EQ. ')' ) VERS = SVERS( F + 1 : L - 1 )

      END IF

*  Find the used length of the version string.
      LV = CHR_LEN( VERS )

*  Find the first dot. If no dot use the end of the string.
      DOT = INDEX( VERS, '.' )
      IF( DOT .EQ. 0 ) DOT = LV + 1

*  Find the first hyphen. If no hyphen use the end of the string.
      HYP = INDEX( VERS, '-' )
      IF( HYP .EQ. 0 ) HYP = LV + 1

*  Find the length of each field.
      LM = DOT - 1
      LN = HYP - DOT - 1
      IF( HYP .GT. 0 ) THEN
         LR = CHR_LEN( VERS ) - HYP
      ELSE
         LR = 0
      END IF

*  Get the revision field.
      IF( LR .LE. 0 ) THEN
         R = -999
      ELSE
         CALL CHR_CTOI( VERS( HYP + 1: ), R, STATUS )
      END IF

*  Get the minor version field.
      IF( LN .LE. 0 ) THEN
         N = -999
      ELSE
         CALL CHR_CTOI( VERS( DOT + 1 : HYP - 1 ), N, STATUS )
      END IF

*  Get the major version field.
      IF( LM .LE. 0 ) THEN
         M = -999
      ELSE
         CALL CHR_CTOI( VERS( : DOT - 1 ), M, STATUS )
      END IF

*  Check that no leading fields are missing.
      IF( M .EQ. -999 ) THEN
         STATUS = SAI__ERROR
      ELSE IF( N .EQ. -999 .AND. R .NE. -999 ) THEN
         STATUS = SAI__ERROR
      END IF

*  Report an error if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'V', VERS )
         CALL ERR_REP( 'POL1_PRSVR_ERR1', 'Illegal POLPACK version '//
     :                 'number ''^V'' encountered (possible '//
     :                 'programming error).', STATUS )
      END IF

      END
