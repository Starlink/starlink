      SUBROUTINE SPD_CZEE( DIALOG, MAXGAU, NCOMP, FITPAR, FITDIM,
     :   CONT, CFLAGS, PFLAGS, SFLAGS, CENTRE, PEAK, SIGMA, COVAR,
     :   STATUS )
*+
*  Name:
*     SPD_CZEE

*  Purpose:
*     Set the fit parameters for FITGAUSS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZEE( DIALOG, MAXGAU, NCOMP, FITPAR, FITDIM, CONT,
*        CFLAGS, PFLAGS, SFLAGS, CENTRE, PEAK, SIGMA, COVAR, STATUS )

*  Description:
*     Set the fit parameters for FITGAUSS's Gauss or polynomial fit.

*  Arguments:
*     DIALOG = LOGICAL (Given)
*        Unused.
*     MAXGAU = INTEGER (Given)
*        Size of arrays for Gauss components.
*     NCOMP = INTEGER (Returned)
*        The number of Gauss components. NCOMP must be less than or
*        equal to MAXGAU.
*     FITPAR = INTEGER (Returned)
*        Number of fit parameters.
*     FITDIM = INTEGER (Returned)
*        max( 1, FITPAR ). Useful for dimension purposes.
*     CONT = REAL (Returned)
*        For Gauss fits indicates the level of the continuum.
*     CFLAGS( MAXGAU ) = INTEGER (Given)
*        For each Gauss component I a value CFLAGS(I)=0 indicates that
*        CENTRE(I) holds a guess which is free to be fitted.
*        A positive value CFLAGS(I)=I indicates that CENTRE(I) is fixed.
*        A positive value CFLAGS(I)=J<I indicates that CENTRE(I) has to
*        keep a fixed offset from CENTRE(J).
*     PFLAGS( MAXGAU ) = INTEGER (Given)
*        For each Gauss component I a value PFLAGS(I)=0 indicates that
*        PEAK(I) holds a guess which is free to be fitted.
*        A positive value PFLAGS(I)=I indicates that PEAK(I) is fixed.
*        A positive value PFLAGS(I)=J<I indicates that PEAK(I) has to
*        keep a fixed ratio to PEAK(J).
*     SFLAGS( MAXGAU ) = INTEGER (Given)
*        For each Gauss component I a value SFLAGS(I)=0 indicates that
*        SIGMA(I) holds a guess which is free to be fitted.
*        A positive value SFLAGS(I)=I indicates that SIGMA(I) is fixed.
*        A positive value SFLAGS(I)=J<I indicates that SIGMA(I) has to
*        keep a fixed ratio to SIGMA(J).
*     CENTRE( MAXGAU ) = REAL (Returned)
*        Centre position for each Gauss component.
*     PEAK( MAXGAU ) = REAL (Returned)
*        Peak height for each Gauss component.
*     SIGMA( MAXGAU ) = REAL (Returned)
*        Dispersion for each Gauss component. In the parameter system -
*        as opposed to the variable - the full width at half maximum
*        for each Gauss component is used. This routine does the
*        conversion.
*     COVAR( 9*MAXGAU*MAXGAU ) = DOUBLE PRECISION (Returned)
*        The covariance matrix. This is filled with zeros.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     timj: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     02 May 1991 (hme):
*        Original version (SETFIT).
*     03 May 1991 (hme):
*        PAR_: Always and get all elements and use ppath
*        'CURRENT,DEFAULT'. No setting of dynamic defaults.
*     08 May 1991 (hme):
*        Change parameters names CFLAGS, PFLAGS, SFLAGS to CF, PF, WF.
*        Use FWHM instead of SIGMA.
*     31 May 1991 (hme):
*        Try a different way to deal with array parameters.
*     12 Jun 1991 (hme):
*        Return a zero-filled covariance matrix for the Gauss
*        parameters. Ban FWHM in favour of SIGMA.
*     22 Jul 1991 (hme):
*        Introduce FITDIM.
*     23 Jul 1991 (hme):
*        Introduce ITER.
*     21 Nov 1991 (hme):
*        Ask for all Gauss parameters first, and then for all flags.
*     23 Apr 1992 (hme):
*        Rename the TYPE parameter to NCOMP. This is because FITGAUSS no
*        longer fits polynomials. Remove ITER, Disable polynomials.
*     27 Jan 1995 (hme):
*        Rename from SPFSET.
*     19 Oct 2006 (timj):
*        Fix valgrind warning by only using NCOMP rather than MAXGAU
*        when looping.
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
      INTEGER MAXGAU

*  Arguments Returned:
      INTEGER NCOMP
      INTEGER FITPAR
      INTEGER FITDIM
      REAL CONT
      INTEGER CFLAGS( MAXGAU )
      INTEGER PFLAGS( MAXGAU )
      INTEGER SFLAGS( MAXGAU )
      REAL CENTRE( MAXGAU )
      REAL PEAK( MAXGAU )
      REAL SIGMA( MAXGAU )
      DOUBLE PRECISION COVAR( 9 * MAXGAU * MAXGAU )

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
      DO 1 I = 1, 9*MAXGAU*MAXGAU
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
*  (FWHM in parameters, SIGMA in variables).
      CALL PAR_GET1R( 'CENTRE', MAXGAU, CENTRE, NRET0, STATUS )
      NRET0 = MIN( NCOMP, NRET0 )
      CALL PAR_GET1R( 'PEAK', MAXGAU, PEAK, NRET, STATUS )
      NRET0 = MIN( NRET, NRET0 )
      CALL PAR_GET1R( 'FWHM', MAXGAU, SIGMA, NRET, STATUS )
      NRET0 = MIN( NRET, NRET0 )

*  Fit flags for centres, peaks, widths.
      CALL PAR_GET1I( 'CF', MAXGAU, CFLAGS, NRET, STATUS )
      NRET0 = MIN( NRET, NRET0 )
      CALL PAR_GET1I( 'PF', MAXGAU, PFLAGS, NRET, STATUS )
      NRET0 = MIN( NRET, NRET0 )
      CALL PAR_GET1I( 'WF', MAXGAU, SFLAGS, NRET, STATUS )
      NRET0 = MIN( NRET, NRET0 )

*  Cancel centres, peaks, FWHM'a.
      CALL PAR_CANCL( 'CF', STATUS )
      CALL PAR_CANCL( 'PF', STATUS )
      CALL PAR_CANCL( 'WF', STATUS )
      CALL PAR_CANCL( 'CENTRE', STATUS )
      CALL PAR_CANCL(   'PEAK', STATUS )
      CALL PAR_CANCL(   'FWHM', STATUS )

*  Default centres, peaks, FWHM'a.
      CALL PAR_DEF1I( 'CF', MAXGAU, CFLAGS, STATUS )
      CALL PAR_DEF1I( 'PF', MAXGAU, PFLAGS, STATUS )
      CALL PAR_DEF1I( 'WF', MAXGAU, SFLAGS, STATUS )
      CALL PAR_DEF1R( 'CENTRE', MAXGAU, CENTRE, STATUS )
      CALL PAR_DEF1R(   'PEAK', MAXGAU,   PEAK, STATUS )
      CALL PAR_DEF1R(   'FWHM', MAXGAU,  SIGMA, STATUS )

*  Convert FWHM to dispersion.
      DO 2 I = 1, NCOMP
         SIGMA(I) = SIGMA(I) / RT8LN2
 2    CONTINUE

*  Number of fit parameters.
      NCOMP = MIN( MAXGAU, NCOMP )
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
