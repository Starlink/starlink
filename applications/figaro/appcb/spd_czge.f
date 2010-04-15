      SUBROUTINE SPD_CZGE( DIALOG, MAXBBS, NCOMP, FITPAR, FITDIM,
     :   CFLAGS, PFLAGS, SFLAGS, THETA, ALPHA, TEMPE, COVAR,
     :   STATUS )
*+
*  Name:
*     SPD_CZGE

*  Purpose:
*     Set the fit parameters for FITBB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZGE( DIALOG, MAXBBS, NCOMP, FITPAR, FITDIM,
*        CFLAGS, PFLAGS, SFLAGS, THETA, ALPHA, TEMPE, COVAR, STATUS )

*  Description:
*     Set the parameters for FITBB.

*  Arguments:
*     DIALOG = LOGICAL (Given)
*        Unused.
*     MAXBBS = INTEGER (Given)
*        Size of arrays for components.
*     NCOMP = INTEGER (Returned)
*        The number of components. NCOMP must be less than or equal to
*        MAXBBS.
*     FITPAR = INTEGER (Returned)
*        Number of fit parameters.
*     FITDIM = INTEGER (Returned)
*        max( 1, FITPAR ). Useful for dimension purposes.
*     CFLAGS( MAXBBS ) = INTEGER (Given)
*        For each component I a value CFLAGS(I)=0 indicates that
*        THETA(I) holds a guess which is free to be fitted.
*        A positive value CFLAGS(I)=I indicates that THETA(I) is fixed.
*        A positive value CFLAGS(I)=J<I indicates that THETA(I) has to
*        keep a fixed offset from THETA(J).
*     PFLAGS( MAXBBS ) = INTEGER (Given)
*        For each component I a value PFLAGS(I)=0 indicates that
*        ALPHA(I) holds a guess which is free to be fitted.
*        A positive value PFLAGS(I)=I indicates that ALPHA(I) is fixed.
*        A positive value PFLAGS(I)=J<I indicates that ALPHA(I) has to
*        keep a fixed offset to ALPHA(J).
*     SFLAGS( MAXBBS ) = INTEGER (Given)
*        For each Gauss component I a value SFLAGS(I)=0 indicates that
*        TEMPE(I) holds a guess which is free to be fitted.
*        A positive value SFLAGS(I)=I indicates that TEMPE(I) is fixed.
*        A positive value SFLAGS(I)=J<I indicates that TEMPE(I) has to
*        keep a fixed offset to TEMPE(J).
*     THETA( MAXBBS ) = REAL (Returned)
*        Scaling constant for each component.
*     ALPHA( MAXBBS ) = REAL (Returned)
*        Emissivity exponent for each component.
*     TEMPE( MAXBBS ) = REAL (Returned)
*        Colour temperature for each component.
*     COVAR( 9*MAXBBS*MAXBBS ) = DOUBLE PRECISION (Returned)
*        The covariance matrix. This is filled with zeros.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     26 Jan 1993 (hme):
*        Adapt from SPAAP.
*     27 Jan 1995 (hme):
*        Renamed from SPACQ.
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
      INTEGER MAXBBS

*  Arguments Returned:
      INTEGER NCOMP
      INTEGER FITPAR
      INTEGER FITDIM
      INTEGER CFLAGS( MAXBBS )
      INTEGER PFLAGS( MAXBBS )
      INTEGER SFLAGS( MAXBBS )
      REAL THETA( MAXBBS )
      REAL ALPHA( MAXBBS )
      REAL TEMPE( MAXBBS )
      DOUBLE PRECISION COVAR( 9 * MAXBBS * MAXBBS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NRET0              ! Minimum number of returned values
      INTEGER NRET               ! Number of returned values
      INTEGER I                  ! Loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Fill covariance with zeroes.
      DO 1 I = 1, 9*MAXBBS*MAXBBS
         COVAR(I) = 0D0
 1    CONTINUE

*  Get NCOMP.
      CALL PAR_GET0I( 'NCOMP', NCOMP, STATUS )
      CALL PAR_CANCL( 'NCOMP', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Component thetas, alphas, widths.
      CALL PAR_GET1R( 'THETA', MAXBBS, THETA, NRET0, STATUS )
      NRET0 = MIN( NCOMP, NRET0 )
      CALL PAR_GET1R( 'ALPHA', MAXBBS, ALPHA, NRET, STATUS )
      NRET0 = MIN( NRET, NRET0 )
      CALL PAR_GET1R( 'LGTEMP',  MAXBBS, TEMPE, NRET, STATUS )
      NRET0 = MIN( NRET, NRET0 )

*  Fit flags for thetas, alphas, widths.
      CALL PAR_GET1I( 'SF', MAXBBS, CFLAGS, NRET, STATUS )
      NRET0 = MIN( NRET, NRET0 )
      CALL PAR_GET1I( 'AF', MAXBBS, PFLAGS, NRET, STATUS )
      NRET0 = MIN( NRET, NRET0 )
      CALL PAR_GET1I( 'TF', MAXBBS, SFLAGS, NRET, STATUS )
      NRET0 = MIN( NRET, NRET0 )

*  Cancel thetas, alphas, temperatures.
      CALL PAR_CANCL( 'SF', STATUS )
      CALL PAR_CANCL( 'AF', STATUS )
      CALL PAR_CANCL( 'TF', STATUS )
      CALL PAR_CANCL( 'THETA', STATUS )
      CALL PAR_CANCL( 'ALPHA', STATUS )
      CALL PAR_CANCL( 'LGTEMP', STATUS )

*  Default thetas, alphas, temperatures.
      CALL PAR_DEF1I( 'SF', MAXBBS, CFLAGS, STATUS )
      CALL PAR_DEF1I( 'AF', MAXBBS, PFLAGS, STATUS )
      CALL PAR_DEF1I( 'TF', MAXBBS, SFLAGS, STATUS )
      CALL PAR_DEF1R( 'THETA', MAXBBS, THETA, STATUS )
      CALL PAR_DEF1R( 'ALPHA', MAXBBS, ALPHA, STATUS )
      CALL PAR_DEF1R( 'LGTEMP', MAXBBS, TEMPE, STATUS )

*  Number of fit parameters.
      NCOMP = MIN( MAXBBS, NCOMP )
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
