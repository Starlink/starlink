      SUBROUTINE LOTEM( IAPER, STATUS )
*+
*  Name:
*     SUBROUTINE LOTEM

*  Purpose:
*      Generate the global aspects of the spectrum template.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOTEM( IAPER, STATUS )

*  Arguments:
*     IAPER = INTEGER (Given)
*        Which aperture is to be used.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     A slow coordinate transform is used.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*     Paul Rees          03-OCT-88     IUEDR Vn. 2.0
*     16-DEC-94 (MJC)
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     The (U,V) rotran was never correct, couldn't work out why.
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER IAPER      ! aperture number

*  Status:
      INTEGER STATUS     ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMFACE'
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Move dispersion constants.
      CALL MVDISP( IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: moving dispersion constants\\', STATUS )
         GO TO 999
      END IF

*   Set up coordinate transforms.
      CALL LOSET( IAPER )

*   Define ROTRAN.
      CALL SETUV( ANGLE, DBLE( CENTRE( 1 ) ), DBLE( CENTRE( 2 ) ) )

 999  CONTINUE
      END
