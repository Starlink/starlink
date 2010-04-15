      SUBROUTINE FIND46( TIMCT, SPPAFT, SPSTCL, SPSTCR, TIMYRS, STATUS )
*+
*  Name:
*     FIND46

*  Purpose:
*     To change a time from SATCAL to years ( with decimal places )

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND46( TIMCT, SPPAFT, SPSTCL, SPSTCR, TIMYRS, STATUS )

*  Description:
*     To change a time from SATCAL to years ( with decimal places )

*  Arguments:
*     TIMCT = DOUBLE PRECISION (Given)
*        SATCAL time of crossing time. ie best current estimate of the
*        time of closest approach of the observation to the source.
*     SPPAFT = DOUBLE PRECISION (Given)
*        PAF time for start of SOP
*     SPSTCL = DOUBLE PRECISION (Given)
*        Satcal time at start of SOP
*     SPSTCR = REAL (Given)
*        Satcal rate for SOP
*     TIMYRS = REAL (Returned)
*        Time at which ecliptic coords are calculated in years and
*        decimal years
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
*        This original version is adapted from YEAR, a subroutine
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
      DOUBLE PRECISION TIMCT
      DOUBLE PRECISION SPPAFT
      DOUBLE PRECISION SPSTCL
      REAL SPSTCR

*  Arguments Returned:
      REAL TIMYRS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL SECDAY                ! Seconds per day
      PARAMETER ( SECDAY = 86400.0 )

*  Local Variables:
      DOUBLE PRECISION TEMPTI    ! Holds intermediate values of time
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the time in seconds from the satcal base time
      TEMPTI = ( TIMCT - SPSTCL ) / SPSTCR

*  Calculate time in days from 1st Jan 1983
      TEMPTI = TEMPTI / SECDAY + SPPAFT

*  Calculate the time in years
      TIMYRS = 1983.0 + REAL( TEMPTI ) / 365.24

      END
