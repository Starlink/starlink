      SUBROUTINE WRPART( FD, NAME, NOT, TYPE, STATUS )
*+
*  Name:
*     SUBROUTINE WRPART

*  Purpose:
*     Write the sequence (NAME,NOT,TYPE) to the given logical unit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WRPART( FD, NAME, NOT, TYPE, STATUS )

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
*     Use unformatted sequential Fortran I/O to write the data.

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
      BYTE VNAME( 16 )      ! Temporary name.
      BYTE VTYPE( 16 )      ! Temporary type.

      INTEGER IOSTAT        ! Fortran I/O status.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the header information to file.
      CALL STR_MOVE( NAME, 16, VNAME )
      CALL STR_MOVE( TYPE, 16, VTYPE )
      WRITE( FD, IOSTAT = IOSTAT ) VNAME, NOT, VTYPE

      IF ( IOSTAT .NE. 0 ) THEN
         CALL ERROUT( 'Error: writing item header\\', STATUS )
      END IF

      END
