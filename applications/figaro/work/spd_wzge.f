      SUBROUTINE SPD_WZGE( NCOMP, FITDIM, CFLAGS, PFLAGS, SFLAGS,
     :   VARSCL, COVAR, CVAR, PVAR, WVAR, STATUS )
*+
*  Name:
*     SPD_WZGE

*  Purpose:
*     Variances of multi-Planck profile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZGE( NCOMP, FITDIM, CFLAGS, PFLAGS, SFLAGS,
*        VARSCL, COVAR, CVAR, PVAR, WVAR, STATUS )

*  Description:
*     This routine takes fit flags and the covariance of parameters of a
*     multi-Planck profile fit and works the parameter variances.
*
*     The function described by the fit parameters is the sum of up to
*     six diluted Planck profiles. Each profile is specified by three
*     parameters. Each of these 18 or less parameters may be fixed,
*     free, or tied to the corresponding parameter of another component.
*     These parameter properties are specified by flags. Ties mean
*     constant offset. Any component can be tied only to an earlier
*     component (with smaller array index) and only to a free parameter.
*     For the free parameters the covariance matrix is given to this
*     routine.

*  Arguments:
*     NCOMP = INTEGER (Given)
*        The number of components.
*     FITDIM = INTEGER (Given)
*        The dimensions of the covariance matrix. Usually this is the
*        number of free parameters, but it must be at least 1.
*     CFLAGS( 6 ) = INTEGER (Given)
*        For each component I a value CFLAGS(I)=0 indicates that
*        CENTRE(I) holds a guess which is free to be fitted.
*        A positive value CFLAGS(I)=I indicates that CENTRE(I) is fixed.
*        A positive value CFLAGS(I)=J<I indicates that CENTRE(I) has to
*        keep a fixed offset from CENTRE(J).
*     PFLAGS( 6 ) = INTEGER (Given)
*        For each component I a value PFLAGS(I)=0 indicates that
*        PEAK(I) holds a guess which is free to be fitted.
*        A positive value PFLAGS(I)=I indicates that PEAK(I) is fixed.
*        A positive value PFLAGS(I)=J<I indicates that PEAK(I) has to
*        keep a fixed offset to PEAK(J).
*     SFLAGS( 6 ) = INTEGER (Given)
*        For each component I a value SFLAGS(I)=0 indicates that
*        FWHM(I) holds a guess which is free to be fitted.
*        A positive value SFLAGS(I)=I indicates that FWHM(I) is fixed.
*        A positive value SFLAGS(I)=J<I indicates that FWHM(I) has to
*        keep a fixed offset to FWHM(J).
*     VARSCL = REAL (Given)
*        Scaling factor for the covariance matrix. That matrix is not
*        taken literally here. Each time a matrix element is used, it is
*        multiplied with this number to be sure to have the actual
*        covariance. The motivation is that while fitting variances may
*        have been unknown. Then the matrix would be only proportional,
*        but not equal, to the covariance matrix.
*        Usually, if variances were known, this factor will be one. And
*        if variances were unknown, this factor will be the square of
*        rms.
*     COVAR( FITDIM, FITDIM ) = DOUBLE PRECISION (Given)
*        VARSCL * COVAR is the matrix of covariances between free
*        parameters.
*     CVAR( NCOMP ) = REAL (Returned)
*        Variance of first parameter for each component.
*     PVAR( NCOMP ) = REAL (Returned)
*        Variance of second parameter for each component.
*     WVAR( NCOMP ) = REAL (Returned)
*        Variance of third parameter for each component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     26 Jan 1993 (hme):
*        Adapted from SPAAY.
*     27 Jan 1995 (hme):
*        Renamed from SPACV.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NCOMP
      INTEGER FITDIM
      INTEGER CFLAGS( NCOMP )
      INTEGER PFLAGS( NCOMP )
      INTEGER SFLAGS( NCOMP )
      REAL VARSCL
      DOUBLE PRECISION COVAR( FITDIM, FITDIM )

*  Arguments Returned:
      REAL CVAR( NCOMP )
      REAL PVAR( NCOMP )
      REAL WVAR( NCOMP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Counter for components
      INTEGER J                  ! Counter for parameters

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  J counts free parameters, I counts components. The relation
*  between the two is not simple because of fixed and tied
*  parameters. First come all (free) centres, then all peaks, then all
*  dispersions.
      J = 0

*  Variance of I-th centre.
      DO 1 I = 1, NCOMP

*     If this centre was free, its variance is on the diagonal of the
*     covariance.
         IF ( CFLAGS(I) .EQ. 0 ) THEN
            J = J+1
            CVAR(I) = REAL(COVAR(J,J)) * VARSCL

*     Else if this centre was fixed, its variance is zero.
         ELSE IF ( CFLAGS(I) .EQ. I ) THEN
            CVAR(I) = 0.

*     Else (this centre was tied to another), its variance is the same.
*     Since a tie relates always to an earlier component (CFLAGS(I)<I),
*     we can assume that CVAR(CFLAGS(I)) is already known.
         ELSE
            CVAR(I) = CVAR(CFLAGS(I))
         END IF
 1    CONTINUE

*  Variance of I-th peak.
      DO 2 I = 1, NCOMP

*     If this peak was free, its variance is on the diagonal of the
*     covariance.
         IF ( PFLAGS(I) .EQ. 0 ) THEN
            J = J+1
            PVAR(I) = REAL(COVAR(J,J)) * VARSCL

*     Else if this peak was fixed, its variance is zero.
         ELSE IF ( PFLAGS(I) .EQ. I ) THEN
            PVAR(I) = 0.

*     Else (this peak was tied to another), its variance derives from
*     that variance.
*     Since a tie relates always to an earlier component (PFLAGS(I)<I),
*     we can assume that PVAR(PFLAGS(I)) is already known.
         ELSE
            PVAR(I) = PVAR(PFLAGS(I))
         END IF
 2    CONTINUE

*  Variance of I-th FWHM.
      DO 3 I = 1, NCOMP

*     If this FWHM was free, its variance is on the diagonal of the
*     covariance.
         IF ( SFLAGS(I) .EQ. 0 ) THEN
            J = J+1
            WVAR(I) = REAL(COVAR(J,J)) * VARSCL

*     Else if this FWHM was fixed, its variance is zero.
         ELSE IF ( SFLAGS(I) .EQ. I ) THEN
            WVAR(I) = 0.

*     Else (this FWHM was tied to another), its variance derives from
*     that variance.
*     Since a tie relates always to an earlier component (SFLAGS(I)<I),
*     we can assume that WVAR(SFLAGS(I)) is already known.
         ELSE
            WVAR(I) = WVAR(SFLAGS(I))
         END IF
 3    CONTINUE

*  Return.
      END
