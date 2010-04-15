      SUBROUTINE PON_GETCOL( PAR, COLLAB, COLNUM, STATUS )
*+
*  Name:
*     PON_GETCOL

*  Purpose:
*     Get the column number of a given column.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_GETCOL( PAR, COLLAB, COLNUM, STATUS )

*  Description:
*     For the PONGO column type specified by PAR, this routine will get
*     the value of the parameter and then check to see if it is an
*     INTEGER. If it not is an INTEGER, the parameter is checked
*     against the column names supplied in COLLAB. The routine reports
*     what column number the name translates to as well as reporting
*     any errors.

*  Arguments:
*     PAR = CHARACTER * ( * ) (Given)
*        The name of the PONGO column parameter.
*     COLLAB( * ) = CHARACTER * ( * ) (Given)
*        The names of the data columns.
*     COLNUM = INTEGER (Returned)
*        The number of the column (counting from 1).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     10-APR-1990 (JBVAD::PAH):
*        Original version.
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     3-JUN-1994 (PDRAPER):
*        Deal with status returns correctly.
*     6-JUN-1994 (PDRAPER):
*        Removed unused argument NCOLS.
*     4-JUL-1994 (PDRAPER):
*        Corrected error message to show values (not message tokens).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants

*  Arguments Given:
      CHARACTER * ( * ) PAR
      CHARACTER * ( * ) COLLAB( * )

*  Arguments Returned:
      INTEGER COLNUM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER INTCMD             ! INTCMD(LIST, value) returns the index
                                 ! of VALUE in the array LIST
*  Local Variables:
      CHARACTER * ( LENLAB ) CTEMP ! Character value of returned column
                                 ! specifier

*.

*  Check inherited global status.
      IF ( STATUS.NE.SAI__OK ) RETURN

      CALL PAR_GET0C( PAR, CTEMP, STATUS )

*  Try to interpret PAR as an INTEGER.
      CALL CHR_CTOI( CTEMP, COLNUM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         COLNUM = INTCMD( COLLAB, CTEMP )
         IF ( COLNUM.GT.0 ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF
      END IF

*  Set column name for error messages.
      CALL MSG_SETC( 'COLNAM', PAR )
      CALL MSG_SETC( 'COLVAL', CTEMP )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Report the column number to be read.
         CALL MSG_SETI( 'COLNUM', COLNUM )
         CALL MSG_OUT( ' ',
     :                 '^COLNAM - ^COLVAL is column number ^COLNUM.',
     :                 STATUS )
      ELSE
         COLNUM = 0
         CALL ERR_REP( 'PON_GETCOL_BADC',
     :                 '^COLNAM incorrectly specified as ^COLVAL.',
     :                 STATUS )
      END IF

      END
* $Id$
