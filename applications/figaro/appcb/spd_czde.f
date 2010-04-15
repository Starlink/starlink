      SUBROUTINE SPD_CZDE( DIALOG, MAXTRI, NCOMP, FITPAR, FITDIM,
     :   CONT, CFLAGS, PFLAGS, SFLAGS, CENTRE, PEAK, FWHM, COVAR,
     :   STATUS )
*+
*  Name:
*     SPD_CZDE

*  Purpose:
*     Set the fit parameters for FITTRI.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZDE( DIALOG, MAXTRI, NCOMP, FITPAR, FITDIM, CONT,
*        CFLAGS, PFLAGS, SFLAGS, CENTRE, PEAK, FWHM, COVAR, STATUS )

*  Description:
*     Set the parameters for FITTRI.

*  Arguments:
*     DIALOG = LOGICAL (Given)
*        Unused.
*     MAXTRI = INTEGER (Given)
*        Size of arrays for Gauss components.
*     NCOMP = INTEGER (Returned)
*        The number of Gauss components. NCOMP must be less than or
*        equal to MAXTRI.
*     FITPAR = INTEGER (Returned)
*        Number of fit parameters.
*     FITDIM = INTEGER (Returned)
*        max( 1, FITPAR ). Useful for dimension purposes.
*     CONT = REAL (Returned)
*        For Gauss fits indicates the level of the continuum.
*     CFLAGS( MAXTRI ) = INTEGER (Given)
*        For each Gauss component I a value CFLAGS(I)=0 indicates that
*        CENTRE(I) holds a guess which is free to be fitted.
*        A positive value CFLAGS(I)=I indicates that CENTRE(I) is fixed.
*        A positive value CFLAGS(I)=J<I indicates that CENTRE(I) has to
*        keep a fixed offset from CENTRE(J).
*     PFLAGS( MAXTRI ) = INTEGER (Given)
*        For each Gauss component I a value PFLAGS(I)=0 indicates that
*        PEAK(I) holds a guess which is free to be fitted.
*        A positive value PFLAGS(I)=I indicates that PEAK(I) is fixed.
*        A positive value PFLAGS(I)=J<I indicates that PEAK(I) has to
*        keep a fixed ratio to PEAK(J).
*     SFLAGS( MAXTRI ) = INTEGER (Given)
*        For each Gauss component I a value SFLAGS(I)=0 indicates that
*        FWHM(I) holds a guess which is free to be fitted.
*        A positive value SFLAGS(I)=I indicates that FWHM(I) is fixed.
*        A positive value SFLAGS(I)=J<I indicates that FWHM(I) has to
*        keep a fixed ratio to FWHM(J).
*     CENTRE( MAXTRI ) = REAL (Returned)
*        Centre position for each Gauss component.
*     PEAK( MAXTRI ) = REAL (Returned)
*        Peak height for each Gauss component.
*     FWHM( MAXTRI ) = REAL (Returned)
*        Dispersion for each Gauss component. In the parameter system -
*        as opposed to the variable - the full width at half maximum
*        for each Gauss component is used. This routine does the
*        conversion.
*     COVAR( 9*MAXTRI*MAXTRI ) = DOUBLE PRECISION (Returned)
*        The covariance matrix. This is filled with zeros.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     22 Jul 1992 (hme):
*        Adapt from SPFSET.
*     27 Jan 1995 (hme):
*        Renamed from SPAAP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL DIALOG
      INTEGER MAXTRI

*  Arguments Returned:
      INTEGER NCOMP
      INTEGER FITPAR
      INTEGER FITDIM
      REAL CONT
      INTEGER CFLAGS( MAXTRI )
      INTEGER PFLAGS( MAXTRI )
      INTEGER SFLAGS( MAXTRI )
      REAL CENTRE( MAXTRI )
      REAL PEAK( MAXTRI )
      REAL FWHM( MAXTRI )
      DOUBLE PRECISION COVAR( 9 * MAXTRI * MAXTRI )

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      REAL RT8LN2                ! Square root of 8 ln(2)
      PARAMETER ( RT8LN2 = 2.354820 )

*  Local Variables:
      INTEGER NRET0              ! Minimum number of returned values
      INTEGER NRET               ! Number of returned values
      INTEGER I                  ! Loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Fill covariance with zeroes.
      DO 1 I = 1, 9*MAXTRI*MAXTRI
         COVAR(I) = 0D0
 1    CONTINUE

*  Get NCOMP.
      CALL PAR_GET0I( 'NCOMP', NCOMP, STATUS )
      CALL PAR_CANCL( 'NCOMP', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Continuum level.
      CALL PAR_GET0R( 'CONT', CONT, STATUS )
      CALL PAR_CANCL( 'CONT', STATUS )

*  Component centres, peaks, widths.
      CALL PAR_GET1R( 'CENTRE', MAXTRI, CENTRE, NRET0, STATUS )
      NRET0 = MIN( NCOMP, NRET0 )
      CALL PAR_GET1R( 'PEAK', MAXTRI, PEAK, NRET, STATUS )
      NRET0 = MIN( NRET, NRET0 )
      CALL PAR_GET1R( 'FWHM', MAXTRI, FWHM, NRET, STATUS )
      NRET0 = MIN( NRET, NRET0 )

*  Fit flags for centres, peaks, widths.
      CALL PAR_GET1I( 'CF', MAXTRI, CFLAGS, NRET, STATUS )
      NRET0 = MIN( NRET, NRET0 )
      CALL PAR_GET1I( 'PF', MAXTRI, PFLAGS, NRET, STATUS )
      NRET0 = MIN( NRET, NRET0 )
      CALL PAR_GET1I( 'WF', MAXTRI, SFLAGS, NRET, STATUS )
      NRET0 = MIN( NRET, NRET0 )

*  Cancel centres, peaks, FWHM'a.
      CALL PAR_CANCL( 'CF', STATUS )
      CALL PAR_CANCL( 'PF', STATUS )
      CALL PAR_CANCL( 'WF', STATUS )
      CALL PAR_CANCL( 'CENTRE', STATUS )
      CALL PAR_CANCL(   'PEAK', STATUS )
      CALL PAR_CANCL(   'FWHM', STATUS )

*  Default centres, peaks, FWHM'a.
      CALL PAR_DEF1I( 'CF', MAXTRI, CFLAGS, STATUS )
      CALL PAR_DEF1I( 'PF', MAXTRI, PFLAGS, STATUS )
      CALL PAR_DEF1I( 'WF', MAXTRI, SFLAGS, STATUS )
      CALL PAR_DEF1R( 'CENTRE', MAXTRI, CENTRE, STATUS )
      CALL PAR_DEF1R(   'PEAK', MAXTRI,   PEAK, STATUS )
      CALL PAR_DEF1R(   'FWHM', MAXTRI,   FWHM, STATUS )

*  Number of fit parameters.
      NCOMP = MIN( MAXTRI, NCOMP )
      NCOMP = MIN( NRET0, NCOMP )
      NCOMP = MAX( 1, NCOMP )
      FITPAR = 0
      DO 3 I = 1, NCOMP
         IF ( CFLAGS(I) .EQ. 0 ) FITPAR = FITPAR + 1
         IF ( PFLAGS(I) .EQ. 0 ) FITPAR = FITPAR + 1
         IF ( SFLAGS(I) .EQ. 0 ) FITPAR = FITPAR + 1
 3    CONTINUE

*  No. of parameters for dimension purposes.
      FITDIM = MAX( 1, FITPAR )

*  Return.
 500  CONTINUE
      END
