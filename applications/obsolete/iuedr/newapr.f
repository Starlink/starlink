      SUBROUTINE NEWAPR( APR, STATUS )
*+
*  Name:
*     SUBROUTINE NEWAPR

*  Purpose:
*     Add an aperture to the current list, and fill in defaults.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NEWAPR( APR, STATUS )

*  Arguments:
*     APER = BYTE( 16 ) (Given)
*        Name of the new aperture.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     22-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     20-JAN-95 (MJC):
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
      BYTE APR( 16 )     ! Name of the aperture.

*  Status:
      INTEGER STATUS     ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDISP'
      INCLUDE 'CMECOR'
      INCLUDE 'CMWCOR'
      INCLUDE 'CMVEL'

*  Local Variables:
      INTEGER IAPER      ! Aperture index.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( NOHEAD ) THEN
         CALL ERROUT( 'Error: no header\\', STATUS )
         GO TO 999

      ELSE IF ( NAPER .GE. 2 ) THEN
         CALL ERROUT( 'Error: too many apertures\\', STATUS )
         GO TO 999
      END IF

*   Check that name is unique.
      CALL FNAPER( APR, IAPER )
      IF ( IAPER .GT. 0 ) THEN
         CALL ERROUT( 'Error: aperture is not unique\\', STATUS )
         GO TO 999
      END IF

*   Basics.
      CALL STR_MOVE( APR, 16, APERS( 1, NAPER + 1 ) )
      TSECS( NAPER + 1 ) = 1.0
      FSCALE( NAPER + 1 ) = 1.0
      UTS( NAPER + 1 ) = 0.0

*   Echelle shift.
      IF ( .NOT. NOECOR ) THEN
         ECOR( NAPER + 1 ) = 0.0
      END IF

*   Wavelength shift.
      IF ( .NOT. NOWCOR ) THEN
         WCOR( NAPER + 1 ) = 0.0
      END IF

*   Velcoity shift.
      IF ( .NOT. NOVEL ) THEN
         VEL( NAPER + 1 ) = 0.0
      END IF

*   Dispersion.
      IF ( .NOT. NODISP ) THEN
         DISPDS( NAPER + 1 ) = 0.0
         DISPDL( NAPER + 1 ) = 0.0
         DISPSG( NAPER + 1 ) = 0.0
         DISPLG( NAPER + 1 ) = 0.0
      END IF

*   Success, new aperture added.
      NAPER = NAPER + 1

 999  CONTINUE

      END
