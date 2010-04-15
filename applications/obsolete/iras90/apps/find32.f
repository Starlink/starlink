      SUBROUTINE FIND32( SPSTD, SPSTS, SPSTY, SPPAFT, STATUS )
*+
*  Name:
*     FIND32

*  Purpose:
*     To calculate the PAF time (ie days since 0.0 1st Jan 1983)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND32( SPSTD, SPSTS, SPSTY, SPPAFT, STATUS )

*  Description:
*     To calculate the PAF time (ie days since 0.0 1st Jan 1983)
*     uses modified Julian date

*  Arguments:
*     SPSTD = INTEGER (Given)
*        Days part of time of start of SOP
*     SPSTS = REAL (Given)
*        Secs part of time of start of SOP
*     SPSTY = INTEGER (Given)
*        Years part of time of start of SOP
*     SPPAFT = DOUBLE PRECISION (Returned)
*        PAF time for start of SOP
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     None

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     20-JAN-1992 (DCP):
*        Original version.
*        This original version is adapted from PTFUT, a subroutine
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
      INTEGER SPSTD
      REAL SPSTS
      INTEGER SPSTY

*  Arguments Returned:
      DOUBLE PRECISION SPPAFT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL FIND31
      INTEGER FIND31                 ! Modified Julian Date

*  Local Constants:
      REAL SECDAY                ! Seconds per day
      PARAMETER ( SECDAY = 86400.0 )
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate PAF time as
*  Modified julian date of 0.0h 1st Jan of year given -
*  Modified julian date of 0.0h 1st Jan 1983 +
*  Days given + seconds given ( turned into decimal days )
      SPPAFT = DBLE ( FIND31( 1, 1, SPSTY ) - FIND31( 1, 1, 1983 ) +
     :                SPSTD + SPSTS/SECDAY )

      END
