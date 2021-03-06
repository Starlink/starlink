      SUBROUTINE WCI_CNS2S( SYSIN, POSIN, SYSOUT, POSOUT, STATUS )
*+
*  Name:
*     WCI_CNS2S

*  Purpose:
*     Convert a coordinate pair from one system to another

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI_CNS2S( SYSIN, POSIN, SYSOUT, POSOUT, STATUS )

*  Description:
*     Converts a celestial position in one coordinate system to
*     the same sky point in a second system. This is performed
*     by converting to the FK5 system in the epoch defined by
*     the input, precessing this position to the output epoch
*     and converting to the output system.

*  Arguments:
*     SYSIN = INTEGER (given)
*        The coordinate system of the supplied position
*     POSIN[2] = DOUBLE (given)
*        The supplied position, in coordinate system specified by SYSIN
*     SYSOUT = INTEGER (given)
*        The coordinate system of the required position
*     POSOUT[2] = DOUBLE (returned)
*        The input position converted to the system specified by SYSOUT
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     PROGRAM SIMPLE
*
*     DOUBLE PRECISION        RTOD
*       PARAMETER             ( RTOD = 180D0 / 3.141592653... )
*     DOUBLE PRECISION        IPOS(2)
*     DOUBLE PRECISION        OPOS(2)
*     INTEGER                 SYS1, SYS2
*     DATA                    IPOS/insert data here/
*
*     CALL WCI_NEWSYS( 'FK4', 1950D0, 1972D0, SYS1, STATUS )
*     CALL WCI_NEWSYS( 'FK5', 2000D0, 1995D0, SYS2, STATUS )
*
*     CALL WCI_CNS2S( SYS2, IPOS, SYS1, OPOS, STATUS )
*
*     PRINT *, OPOS(1)*RTOD, OPOS(2)*RTOD
*
*     END
*
*        Example above converts a position defined in the FK5 equinox 2000
*        reference frame, with epoch 1995, into the position in FK4 as it
*        would have been at the epoch 1972.

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     The WCI package must have been initialised, and the identifiers
*     SYSIN and SYSOUT defined.

*  Side Effects:
*     None

*  Algorithm:
*     The algorithms used by the convertor routines WCI_CNS2Z and WCI_CNZ2S
*     are identical to those used in the Starlink COCO utility, and that
*     program can be used as a test of this routine.

*  Accuracy:
*     The test procedure tst_wci_cns2s applying 2 WCI_CNS2S operations
*     to shift from FK4 -> FK5 and back to FK4, with different equinoxes
*     and epochs, averaged over the whole celestial sphere, recovers
*     the original position to an average accuracy of 0.00108 arcsec, with
*     a worst case of 0.00132 arcsec.

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     WCI:
*        WCI_CNS2Z	- Convert to standard system
*        WCI_CNZ2S	- Convert from standard system

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     WCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/wci.html

*  Keywords:
*     package:wci, usage:public, coordinate conversion

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     4 Jan 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PKG'

*  Arguments Given:
      INTEGER			SYSIN, SYSOUT
      DOUBLE PRECISION 		POSIN(2)

*  Arguments Returned:
      DOUBLE PRECISION 		POSOUT(2)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      DOUBLE PRECISION		TMPPOS(2)		! Temporary position

*  External References:
      EXTERNAL			AST_QPKGI
        LOGICAL			AST_QPKGI
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( WCI__PKG ) ) CALL WCI1_INIT( STATUS )

*  The systems could be the same object
      IF ( SYSIN .EQ. SYSOUT ) THEN
        POSOUT(1) = POSIN(1)
        POSOUT(2) = POSIN(2)

*  Systems are different objects (although their parameters could be the same
      ELSE

*    Convert input position to J2000 FK5 at user supplied epoch
        CALL WCI_CNS2Z( SYSIN, POSIN, TMPPOS, STATUS )

*    Convert standard position to output system
        CALL WCI_CNZ2S( TMPPOS, SYSOUT, POSOUT, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI_CNS2S', STATUS )

      END
