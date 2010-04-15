      SUBROUTINE CONVPOS( STRING, ANGLE, STATUS )
*+
*  Name:
*     CONVPOS

*  Purpose:
*     Convert a free format string to a number

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CONVPOS( STRING, ANGLE, STATUS )

*  Description:
*     CONVPOS picks out up to 3 components of a sexagesimal string in
*     free format. It returns the value of the number which may be in
*     the character string and may have up to N numbers in character
*     substrings.  They are delimited by any non-numeric character

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The numeric substrings
*     ANGLE = DOUBLE PRECISION (Returned)
*        The evaluated angle in degrees or hours
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  {algorithmic_step}
*     [algorithm_description]...

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  Authors:
*     ANO: Someone (Somewhere)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     Sometime (ANO):
*        Original version.
*     8-FEB-1993 (PMA):
*        Converted to report errors with ERR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHT_ERR'          ! CHART error constants

*  Arguments Given:
      CHARACTER * ( * ) STRING

*  Arguments Returned:
      DOUBLE PRECISION ANGLE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL VAL
      DOUBLE PRECISION VAL       ! Convert string to value

*  Local Variables:
      DOUBLE PRECISION PART( 3 ) ! [local_variable_description]
      LOGICAL NEGNUM             ! [local_variable_description]
      LOGICAL NUMBER             ! [local_variable_description]
      CHARACTER * ( 1 ) CHAR     ! [local_variable_description]
      INTEGER K                  ! Loop counter
      INTEGER NCHAR              ! [local_variable_description]
      INTEGER LENGTH             ! Length of string
      INTEGER N1                 ! [local_variable_description]
      INTEGER N2                 ! [local_variable_description]

*  Internal References:
      INTEGER LEN                ! Full Length of a character string
      INTEGER INDEX              ! Position of a character in a string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO K = 1, 3
         PART( K ) = 0.0D0
      END DO
      LENGTH = LEN( STRING )
      NCHAR = 0
      NEGNUM = INDEX( STRING, '-' ) .NE. 0

*  If it is a negative number, skip the minus sign.
      IF ( NEGNUM ) NCHAR = NCHAR + 1
      DO K = 1, 3
100      CONTINUE
         NCHAR = NCHAR + 1

*  Look for a Numeric Character
         IF ( NCHAR .GE. LENGTH ) THEN
            IF ( K .EQ. 1 ) THEN
*  Error, no numbers found.
**             ierr = 1
               STATUS = CHT__FMTERR
               CALL ERR_REP( ' ', 'Format error', STATUS )
            END IF
            GOTO 800
         END IF
         CHAR = STRING( NCHAR:NCHAR )
         IF ( .NOT. NUMBER( CHAR ) ) GOTO 100

*  Found Start of a Number
         N1 = NCHAR
110      CONTINUE
         NCHAR = NCHAR + 1
         IF ( NCHAR .GE. LENGTH ) GOTO 800
         CHAR = STRING( NCHAR:NCHAR )
         IF ( NUMBER( CHAR ) ) GOTO 110

*  End of Numeric Field Found. Evaluate the Substring.
         N2 = NCHAR -1
         PART( K ) = VAL( STRING, N1, N2, STATUS )
         IF ( K .GT. 1 .AND. PART( K ) .GT. 6.0D1 ) THEN
*           ierr = 2
            STATUS = CHT__IVSEX
            CALL ERR_REP( 'CONVPOS_IVSEX',
     :         'Invalid sexagesimal number', STATUS )
         END IF
      END DO

800   CONTINUE
      ANGLE = PART( 1 ) + PART( 2 ) / 6.0D1 + PART( 3 ) / 3.6D3
      IF ( NEGNUM ) ANGLE = -ANGLE

850   CONTINUE
      END
