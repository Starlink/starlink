      SUBROUTINE WRPART2( FD, NAME, NOT, TYPE, STATUS )
*+
*  Name:
*     SUBROUTINE WRPART2

*  Purpose:
*     Write the sequence (NAME,NOT,TYPE) to the given logical unit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WRPART2( FD, NAME, NOT, TYPE, STATUS )

*  Arguments:
*     FD = INTEGER (Given)
*        Fortran logical unit to which to write the data.
*     NAME = BYTE( 16 ) (Given)
*        The name of the part of the data to write.
*     NOT = LOGICAL (Given)
*        Whether the named part of the data is defined (i.e. has
*        meaningful values).
*     TYPE = BYTE( 16 ) (Given)
*        The "type" of the named part of the data.
*     STATUS = INTEGER (Returned)
*        The global status.

*  Description:
*     Use Fortran sequential I/O to write the data to the logical unit.

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
      INTEGER FD            ! File dscriptor.

      BYTE NAME( 16 )       ! Part name.

      LOGICAL NOT           ! Whether defined.

      BYTE TYPE( 16 )       ! Part type if defined.

*  Status:
      INTEGER STATUS        ! Global status.

*  Local Variables:
      BYTE VNAME( 16 )      ! Temporary NAME.
      BYTE VTYPE( 16 )      ! Temporary TYPE.

      INTEGER IOSTAT        ! Fortran I/O status.
      INTEGER LENGTH        ! String length.

      CHARACTER * ( 16 ) CNAME ! CHARACTER NAME
      CHARACTER * ( 16 ) CTYPE ! CHARACTER TYPE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the header information to file.
      CALL STR_MOVE( NAME, 16, VNAME )
      CALL STR_MOVE( TYPE, 16, VTYPE )
      CALL GEN_STOC( VNAME, 16, CNAME, LENGTH )
      CALL GEN_STOC( VTYPE, 16, CTYPE, LENGTH )
      WRITE( FD, '( 1X, A )', IOSTAT = IOSTAT ) CNAME
      WRITE( FD, '( 1X, L4 )', IOSTAT = IOSTAT ) NOT
      WRITE( FD, '( 1X, A )', IOSTAT = IOSTAT ) CTYPE

      IF ( IOSTAT .NE. 0 ) THEN
         CALL ERROUT( 'Error: writing item header\\', STATUS )
      END IF

      END
