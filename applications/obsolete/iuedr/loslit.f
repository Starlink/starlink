      SUBROUTINE LOSLIT( IAPER, STATUS )
*+
*  Name:
*     SUBROUTINE LOSLIT

*  Purpose:
*     Define object and background channels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOSLIT( IAPER, STATUS )

*  Arguments:
*     IAPER = INTEGER (Given)
*        Which aperture is to be used.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     The object and background channel widths and positions are based
*     on the GSLIT, BDIST and BSLIT parameter values.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-8 (JRG):
*       IUEDR Vn. 1.0
*     08-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     16-DEC-94 (MJC):
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
      INTEGER IAPER         ! aperture index

*  Status:
      INTEGER STATUS        ! Global status.

*  External References:
      LOGICAL STR_SIMLR     ! caseless string equality

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMEXTP'

*  Local Variables:
      INTEGER IBKG          ! background channels index

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Set EXTP undefined.
      NOEXTP = .TRUE.

*   Fix number of backgrounds.
      NBKG = 2

*   Automatic slit mode
      IF ( AUSLIT ) THEN
         IF ( EXTND .AND. STR_SIMLR( 'LAP\\', APERS(1, IAPER) ) ) THEN
            GSLIT( 2 ) = 10.6

         ELSE
            GSLIT( 2 ) = 6.4
         END IF

         GSLIT( 1 ) = -GSLIT( 2 )
         BSLIT( 1 ) = 2.0
         BSLIT( 2 ) = 2.0
         BDIST( 2 ) = GSLIT( 2 ) + BSLIT( 2 ) + 2.0
         BDIST( 1 ) = GSLIT( 1 ) - BSLIT( 1 ) - 2.0
      END IF

*   Object slit radii.
      ROBJ( 1 ) = GSLIT( 1 )
      ROBJ( 2 ) = GSLIT( 2 )

*   Background slit radii.
      DO IBKG = 1, NBKG
         RBKG( 1, IBKG ) = BDIST( IBKG ) - BSLIT( IBKG )
         RBKG( 2, IBKG ) = BDIST( IBKG ) + BSLIT( IBKG )
      END DO

      NOEXTP = .FALSE.

      END
