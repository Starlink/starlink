      SUBROUTINE FIND42( TIMEST, SPSTCL, SPSTCR, SPSTD, SPSTS, TIMEUT,
     : STATUS )
*+
*  Name:
*     FIND42

*  Purpose:
*     Translates satcal time to UTCS time

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND42( TIMEST, SPSTCL, SPSTCR, SPSTD, SPSTS, TIMEUT,
*     : STATUS )

*  Description:
*     Translates satcal time to UTCS time

*  Arguments:
*     TIMEST = DOUBLE PRECISION (Given)
*        Satcal time to be translated
*     SPSTCL = DOUBLE PRECISION (Given)
*        Satcal time at start of SOP
*     SPSTCR = REAL (Given)
*        Satcal rate for SOP
*     SPSTD = INTEGER (Given)
*        Days part of time of start of SOP
*     SPSTS = REAL (Given)
*        Secs part of time of start of SOP
*     TIMEUT = DOUBLE PRECISION (Returned)
*        Translated time in UCTS
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     None

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     28-JAN-1992 (DCP):
*        Original version.
*        This original version is adapted from SATCAL_TO_UTCS, a
*        subroutine of POSNTIM, contained in its utilities subdirectory.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      DOUBLE PRECISION TIMEST
      DOUBLE PRECISION SPSTCL
      REAL SPSTCR
      INTEGER SPSTD
      REAL SPSTS

*  Arguments Returned:
      DOUBLE PRECISION TIMEUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION UTBASE    ! The UCTS time of 0.0 1st Jan 1983
      PARAMETER ( UTBASE = 62985600.0 )
      REAL SECDAY                ! Seconds per day
      PARAMETER ( SECDAY = 86400.0 )
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the UCTS increment from 0.0h 1st Jan 1983 to the time
*  at which the SOP started
      TIMEUT = DFLOAT( SPSTD ) * SECDAY + SPSTS

*  Add the UCTS increment from the satcal time at the start of the SOP
*  to the satcal time for which translation is required.
      TIMEUT = TIMEUT + ( TIMEST - SPSTCL ) * SPSTCR

*  Add the UTCS time of 0.0 1st Jan 1983
      TIMEUT = UTBASE + TIMEUT

      END
