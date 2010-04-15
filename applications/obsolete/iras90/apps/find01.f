      SUBROUTINE FIND01( SLPSI, SLTH, SPSPD, SLDPSI, SLDTH, STATUS )
*+
*  Name:
*     FIND01

*  Purpose:
*     To calculate the annual and orbital abberation error

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND01( SLPSI, SLTH, SPSPD, SLDPSI, SLDTH, STATUS )

*  Description:
*     To calculate the abberation error as delta psi, delta theta, given
*     psi and theta

*  Arguments:
*     SLPSI = REAL (Given)
*        Source psi
*     SLTH = REAL (Given)
*        Source theta
*     SPSPD = REAL (Given)
*        Sign of psi dot for SOP
*     SLDPSI = REAL (Returned)
*        Delta Psi correction for abberation error
*     SLDTH = REAL (Returned)
*        Delta Theta correction for abberation error
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
*        This original version is adapted from ABERR, a subroutine
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
      REAL SLPSI
      REAL SLTH
      REAL SPSPD

*  Arguments Returned:
      REAL SLDPSI
      REAL SLDTH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL SECTOR                ! Seconds to radians
      PARAMETER ( SECTOR = 4.8481366E-6 )
      REAL SLAB                  ! Constant of abberation Kappa
      PARAMETER ( SLAB = 20.5 )
      REAL OAB                   ! [constant_description]
      PARAMETER ( OAB = 5.0 )
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate delta theta
      SLDTH = SLAB * SIN( SLPSI ) * COS( SLTH ) * SECTOR

*  Calculate delta psi
      SLDPSI = ( SLAB * COS( SLPSI ) / SIN( SLTH ) + SPSPD * OAB )
     :         * SECTOR

      END
