      SUBROUTINE MVDISP( IAPER, STATUS )
*+
*  Name:
*     SUBROUTINE MVDISP

*  Description:
*     The contents of CMDISH are set up using the dispersion
*     constants and correction parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MVDISP( IAPER, STATUS )

*  Arguments:
*     IAPER = INTEGR (Given)
*        The aperture index.
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
*     03-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     09-AUG-94 (MJC):
*       IUEDR Vn. 3.1-2
*     30-MAR-95 (MJC):
*       IUEDR Vn. 3.2
*       Support for new-style dispersion relations.
*     {enter_further_changes_here}

*  Problems:
*     The expression should be reformulated so that use of FLOAT
*     coefficients is more effective.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDISP'
      INCLUDE 'CMDISH'

*  Arguments Given:
      INTEGER IAPER      ! aperture number

*  Status:
      INTEGER STATUS     ! Global status.

*  External References:
      LOGICAL STR_SIMLR      ! Caseless string equality.

*  Local Variables:
      INTEGER I          ! loop index
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Checks.
      IF ( NODISP ) THEN
         CALL ERROUT( 'Error: dispersion constants undefined\\',
     :                STATUS )
         GO TO 999
      END IF

*   Move the basic dispersion parameters across.
      DO I = 1, NDISP
         A( I ) = DISPS( I )
         B( I ) = DISPL( I )
      END DO

*   Apply Date/Temperature corrections.
*   New-style dispersion corrections.
      IF ( STR_SIMLR( 'IUE_DISPN\\', DISPTP ) ) THEN

*      Default corrections.
         A( 1 ) = A( 1 ) + DISPWS1
         B( 1 ) = B( 1 ) + DISPWL1

*      Correct for THDA.
         IF ( THDA .GT. 0.0 ) THEN
            A( 1 ) = A( 1 ) + DISPWS2 * THDA
            B( 1 ) = B( 1 ) + DISPWL2 * THDA

         ELSE
            A( 1 ) = A( 1 ) + DISPWS2 * DISPT0
            B( 1 ) = B( 1 ) + DISPWL2 * DISPT0
         END IF

*      Correct for DATE.
         IF ( DATE .GT. 0 ) THEN
            A( 1 ) = A( 1 ) + DISPWS3 * ( DATE - DISPD0 ) +
     :               DISPWS4 * ( DATE - DISPD0 ) * ( DATE - DISPD0 )
            B( 1 ) = B( 1 ) + DISPWL3 * ( DATE - DISPD0 ) +
     :               DISPWL4 * ( DATE - DISPD0 ) * ( DATE - DISPD0 )
         END IF

*   Old-style dispersion corrections.
      ELSE

*      Correct for THDA.
         IF ( THDA .GT. 0.0 ) THEN
            A( 1 ) = A( 1 ) + DISPST * ( THDA - DISPT0 )
            B( 1 ) = B( 1 ) + DISPLT * ( THDA - DISPT0 )
         END IF

*      Correct for DATE.
         IF ( DATE .GT. 0 ) THEN
            A( 1 ) = A( 1 ) + DISPSD * ( DATE - DISPD0 )
            B( 1 ) = B( 1 ) + DISPLD * ( DATE - DISPD0 )
         END IF
      END IF

*   Correct for required APERTURE.
      IF ( IAPER.GE.1 .AND. IAPER.LE.NAPER ) THEN
         A( 1 ) = A( 1 ) + DISPDS( IAPER )
         B( 1 ) = B( 1 ) + DISPDL( IAPER )
      END IF

*   Correct for arbitrary GSHIFT.
      IF ( IAPER.GE.1 .AND. IAPER.LE.NAPER ) THEN
         A( 1 ) = A( 1 ) + DISPSG( IAPER )
         B( 1 ) = B( 1 ) + DISPLG( IAPER )
      END IF

  999 CONTINUE

      END
