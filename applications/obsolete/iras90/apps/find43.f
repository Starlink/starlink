      SUBROUTINE FIND43( SPSLG, SPSLGR, SPSTCL, TIMCT, SPSLGT, STATUS )
*+
*  Name:
*     FIND43

*  Purpose:
*     Calculates the solar longitude at the estimated crossing time

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND43( SPSLG, SPSLGR, SPSTCL, TIMCT, SPSLGT, STATUS )

*  Description:
*     Calculates the solar longitude at the estimated crossing time

*  Arguments:
*     SPSLG = REAL (Given)
*        Solar longitude at start of SOP
*     SPSLGR = REAL (Given)
*        Solar longitude rate for SOP
*     SPSTCL = DOUBLE PRECISION (Given)
*        Satcal time at start of SOP
*     TIMCT = DOUBLE PRECISION (Given)
*        SATCAL time of crossing time. ie best current estimate of the
*        time of closest approach of the observation to the source.
*     SPSLGT = REAL (Returned)
*        Solar longitude at crossing time.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  External Routines Used:
*     None

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     23-JAN-1992 (DCP):
*        Original version.
*        This original version is adapted from SUNLNG, a subroutine
*        of POSNTIM, contained in its utilities subdirectory.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL SPSLG
      REAL SPSLGR
      DOUBLE PRECISION SPSTCL
      DOUBLE PRECISION TIMCT

*  Arguments Returned:
      REAL SPSLGT

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculates the solar longitude at the estimated crossing time
      SPSLGT = SPSLG + SPSLGR * REAL( TIMCT - SPSTCL)

      END
