      SUBROUTINE PON_GTCLAB( FD, MAXLABELS, HARDCOM, NCOLS, COLLAB,
     :                       STATUS )
*+
*  Name:
*     PON_GTCLAB

*  Purpose:
*     Read column labels from the data file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_GTCLAB( FD, MAXLABELS, HARDCOM, NCOLS, COLLAB, STATUS )

*  Description:
*     The column labels are read from the data file and the file is
*     rewound.

*  Arguments:
*     FD = INTEGER (Given)
*        FIO file descriptor.
*     MAXLABELS = INTEGER (Given)
*        Maximun number of labels allowed.
*     HARDCOM = CHARACTER * ( 1 ) (Given)
*        Character used to mark the start of the line containing column
*        labels.
*     NCOLS = INTEGER (Returned)
*        The number of column labels actually read.
*     COLLAB( MAXLABELS ) = CHARACTER * ( * ) (Returned)
*        The column labels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W.Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     9-FEB-1990 (JBVAD::PAH):
*        Original version.
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     6-JUN-1994 (PDRAPER):
*        Now uses PON_PARSE instead of PARSE.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants
      INCLUDE 'FIO_PAR'          ! FIO public global constants

*  Arguments Given:
      INTEGER FD
      INTEGER MAXLABELS

      CHARACTER * ( 1 ) HARDCOM

*  Arguments Returned:
      INTEGER NCOLS

      CHARACTER * ( * ) COLLAB( MAXLABELS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IRECL              ! Number of input records

      CHARACTER * ( MAXBUF ) CHRBUF ! Character input buffer

*.

*  Check inherited global status.
      IF ( STATUS.NE.SAI__OK ) RETURN

      CALL FIO_READ( FD, CHRBUF, IRECL, STATUS )

      IF ( STATUS.EQ.SAI__OK ) THEN
         IF ( CHRBUF( 1:1 ).EQ.HARDCOM .AND. CHRBUF(2:2).EQ.'$' ) THEN

*        Get the separate column labels.
            CALL PON_PARSE( CHRBUF( 3 : IRECL ), '$', MAXCOL - 1,
     :                      COLLAB, NCOLS, STATUS )
            COLLAB( NCOLS+1 ) = ' '
         ELSE
            NCOLS = 0
            COLLAB( 1 ) = ' '
         END IF
      END IF

      CALL FIO_RWIND( FD, STATUS )

      END
* $Id$
