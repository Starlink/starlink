      SUBROUTINE SLING0( IGRP, INDX, TYPE, STATUS )
*+
*  Name:
*     SLING0

*  Purpose:
*     See if the next name in a group is a keyword.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLING0( IGRP, INDX, TYPE, STATUS )

*  Description:
*     The next name is obtained and compared with the list of valid
*     keywords; MERIDIAN, PARALLEL, GREAT CIRCLE and POLYLINE. If an
*     unambiguous match is found (abbreviations allowed), then the
*     corresponding keyword (full form) is returned in TYPE and index
*     is increment by 1. Otherwise, TYPE and INDX are left at their
*     supplied values. An error is reported if an ambiguous
*     abbreviation is found.

*  Arguments:
*     IGRP = INTEGER (Given)
*        GRP identifier for the group.
*     INDX = INTEGER (Given and Returned)
*        The index of the next name to be read. Incremented by one on
*        return is the name is a valid keyword.
*     TYPE = CHARACTER * ( * ) (Given and Returned)
*        The current curve type. Left unchanged if the name is not a
*        valid keyword.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-FEB-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER IGRP

*  Arguments Given and Returned:
      INTEGER INDX
      CHARACTER TYPE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string.

*  Local Variables:
      CHARACTER NAME*(GRP__SZNAM) ! Name read from group.

      INTEGER LNAME              ! Used length of NAME.
      INTEGER NMATCH             ! No. of keywords matched by the name.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the next name from the group, remove leading blanks and get its
*  length.
      CALL GRP_GET( IGRP, INDX, 1, NAME, STATUS )
      CALL CHR_LDBLK( NAME )
      LNAME = CHR_LEN( NAME )

*  See if it is a keyword. If it is, return the type of curves
*  currently being drawn.
      NMATCH = 0

      IF( INDEX( 'MERIDIAN', NAME( :LNAME ) ) .EQ. 1 ) THEN
         NMATCH = NMATCH + 1
         TYPE = 'MERIDIAN'
      END IF

      IF( INDEX( 'PARALLEL', NAME( :LNAME ) ) .EQ. 1 ) THEN
         NMATCH = NMATCH + 1
         TYPE = 'PARALLEL'
      END IF

      IF( INDEX( 'GREAT CIRCLE', NAME( :LNAME ) ) .EQ. 1 ) THEN
         NMATCH = NMATCH + 1
         TYPE = 'GREAT CIRCLE'
      END IF

      IF( INDEX( 'POLYLINE', NAME( :LNAME ) ) .EQ. 1 ) THEN
         NMATCH = NMATCH + 1
         TYPE = 'POLYLINE'

      END IF

*  If an ambiguous abbreviation was found, report an error and abort.
      IF( NMATCH .GT. 1 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'N', NAME )
         CALL ERR_REP( 'SLING0_ERR1',
     :          'SLING0: Ambiguous keyword ^N found in input text file',
     :                    STATUS )

*  If a single match was found, Increment the index of the next name to
*  be read.
      ELSE IF( NMATCH .EQ. 1 .AND. STATUS .EQ. SAI__OK ) THEN
         INDX = INDX + 1

      END IF

      END
