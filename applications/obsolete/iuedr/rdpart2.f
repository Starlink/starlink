      SUBROUTINE RDPART2( FD, NAME, NOT, TYPE, STATUS )
*+
*  Name:
*     SUBROUTINE RDPART2

*  Purpose:
*     Read the sequence (NAME,NOT,TYPE) from the given logical unit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDPART2( FD, NAME, NOT, TYPE, STATUS )

*  Arguments:
*     FD = INTEGER (Given)
*        Fortran logical unit from which the data are read.
*     NAME = BYTE( 16 ) (Returned)
*        The name of the part of the data following.
*     NOT = LOGICAL (Returned)
*        Whether the named part of the data is defined.
*     TYPE = BYTE( 16 ) (Returned)
*        The "type" of the named part of the data.
*     STATUS = INTEGER (Returned)
*        The global status.

*  Description:
*     Use Fortran sequential I/O to read the required data.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-1982 (JRG):
*       Original version (IUEDR Vn. 1.0).
*     22-SEP-1988 (PCTR):
*       IUEDR Vn. 2.0.
*     07-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER FD            ! Fortran logical unit.

*  Arguments Returned:
      BYTE NAME( 16 )       ! Part name.

      LOGICAL NOT           ! Whether defined.

      BYTE TYPE( 16 )       ! Part type if defined.

*  Status:
      INTEGER STATUS        ! Global status.

*  Local Variables:
      INTEGER IOSTAT        ! Fortran I/O status.
      INTEGER LENGTH        ! String length.
      INTEGER I

      CHARACTER * ( 16 ) CNAME ! CHARACTER NAME
      CHARACTER * ( 16 ) CTYPE ! CHARACTER TYPE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read the header information from the file.
      READ( FD, '( 1X, A )', IOSTAT = IOSTAT ) CNAME
      I = 15
      DO WHILE ( CNAME( I : I ) .EQ. ' ' )
         I = I - 1
      END DO
      CNAME( I + 1 : I + 1 ) = '\\'
      READ( FD, '( 1X, L4 )', IOSTAT = IOSTAT ) NOT
      READ( FD, '( 1X, A )', IOSTAT = IOSTAT ) CTYPE
      I = 15
      DO WHILE ( CTYPE( I : I ) .EQ. ' ' )
         I = I - 1
      END DO
      CTYPE( I + 1 : I + 1 ) = '\\'
      CALL STR_UCASE ( CNAME )
      CALL STR_UCASE ( CTYPE )
      CALL GEN_CTOS( CNAME, 16, NAME, LENGTH )
      CALL GEN_CTOS( CTYPE, 16, TYPE, LENGTH )
      IF ( IOSTAT .NE. 0 ) THEN
         CALL ERROUT( 'Error: reading item header\\', STATUS )

      ELSE
         IF ( NOT ) THEN
            CALL STR_TERM( 0, 16, TYPE )
         END IF
      END IF

      END
