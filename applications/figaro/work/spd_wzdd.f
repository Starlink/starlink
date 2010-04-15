      SUBROUTINE SPD_WZDD( MAXTRI, INELM, NCOMP, CONT,
     :   CENTRE, PEAK, FWHM, INX, FITDAT, STATUS )
*+
*  Name:
*     SPD_WZDD

*  Purpose:
*     Calculate FITTRI fit data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZDD( MAXTRI, INELM, NCOMP, CONT,
*        CENTRE, PEAK, FWHM, INX, FITDAT, STATUS )

*  Description:
*     This routine calculates the fit data array for FITTRI. This is
*     either a sum of several triangular components.

*  Arguments:
*     MAXTRI = INTEGER (Given)
*        Size of arrays for triangle components.
*     INELM = INTEGER (Given)
*        Size of arrays INX and FITDAT.
*     NCOMP = INTEGER (Given)
*        The value of NCOMP is the number of components. NCOMP must be
*        greater than or equal to 0 and less than or equal to MAXTRI.
*     CONT = REAL (Given)
*        Indicates the level of the continuum.
*     CENTRE( MAXTRI ) = REAL (Given)
*        Centre position for each component.
*     PEAK( MAXTRI ) = REAL (Given)
*        Peak height for each component.
*     FWHM( MAXTRI ) = REAL (Given)
*        Half maximum width for each component.
*     INX( INELM ) = REAL (Given)
*        Given x value array.
*     FITDAT( INELM ) = REAL (Returned)
*        Data array with fit values.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set if the number of components is
*        out of range.

*  Authors:
*     ajlf: Amadeu Fernandes (UoE)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     31 Jan 1992 (ajlf):
*        Adapted from SPFFDT. Fine grid added.
*     23 Jul 1992 (hme):
*        Re-adapted from SPFFDT.
*     27 Jan 1995 (hme):
*        Renamed from SPAAV.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad values

*  Arguments Given:
      INTEGER MAXTRI
      INTEGER INELM
      INTEGER NCOMP
      REAL CONT
      REAL CENTRE( MAXTRI )
      REAL PEAK( MAXTRI )
      REAL FWHM( MAXTRI )
      REAL INX( INELM )

*  Arguments Returned:
      REAL FITDAT( INELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, C               ! Loop indices

*  Internal declarations:
      REAL SPD_UAATR                ! Fit function value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check number of components.
      IF ( NCOMP .LT. 0 .OR. NCOMP .GT. MAXTRI ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WZDD_E01',
     :      'SPD_WZDD: Error evaluating triangular profiles: ' //
     :      'Number of components is out of range.', STATUS )
         GO TO 500
      END IF

*  Set fit data to (constant) continuum.
      DO 1 I = 1, INELM
         FITDAT(I) = CONT
 1    CONTINUE

*  Add one component after the other.
      DO 3 C = 1, NCOMP

*     A delta distribution is ignored.
         IF ( FWHM(C) .GT. 0. ) THEN
            DO 2 I = 1, INELM
               FITDAT(I) = FITDAT(I) +
     :            SPD_UAATR( CENTRE(C), PEAK(C), FWHM(C), INX(I) )
 2          CONTINUE
         END IF
 3    CONTINUE

*  Return.
 500  CONTINUE
      END
