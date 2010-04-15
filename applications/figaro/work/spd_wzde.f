      SUBROUTINE SPD_WZDE( NCOMP, FITDIM, CFLAGS, PFLAGS, SFLAGS, PEAK,
     :   FWHM, VARSCL, COVAR, FLUX, CVAR, PVAR, WVAR, FVAR, STATUS )
*+
*  Name:
*     SPD_WZDE

*  Purpose:
*     Parameters and variances of multi-triangle profile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZDE( NCOMP, FITDIM, CFLAGS, PFLAGS, SFLAGS, PEAK,
*        FWHM, VARSCL, COVAR, FLUX, CVAR, PVAR, WVAR, FVAR, STATUS )

*  Description:
*     This routine takes fit parameters, fit flags, and the covariance
*     of parameters of a multi-triangle profile fit and works out more
*     (dependent) parameters and all parameter variances.
*
*     The function described by the fit parameters is the sum of up to
*     six triangular profiles. Each profile is specified by centre,
*     peak and full width at half maximum. Each of these 18 or less
*     parameters may be fixed, free, or tied to the corresponding
*     parameter of another component. These parameter properties are
*     specified by flags. Ties mean constant offset for centres and
*     constant ratio for peaks and widths. Any component can be tied
*     only to an earlier component (with smaller array index) and only
*     to a free parameter. For the free parameters the covariance matrix
*     is given to this routine.
*
*     This routine calculates the additional parameter line integral for
*     each component. It also calculates the variances of all
*     parameters, taking care of the interrelations (covariance) between
*     them.

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
*        keep a fixed ratio to PEAK(J).
*     SFLAGS( 6 ) = INTEGER (Given)
*        For each component I a value SFLAGS(I)=0 indicates that
*        FWHM(I) holds a guess which is free to be fitted.
*        A positive value SFLAGS(I)=I indicates that FWHM(I) is fixed.
*        A positive value SFLAGS(I)=J<I indicates that FWHM(I) has to
*        keep a fixed ratio to FWHM(J).
*     PEAK( 6 ) = REAL (Given)
*        Peak height for each component.
*     FWHM( 6 ) = REAL (Given)
*        Full width at half maximum for each component.
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
*     FLUX( NCOMP ) = REAL (Returned)
*        Line integral for each component.
*     CVAR( NCOMP ) = REAL (Returned)
*        Variance of centre position for each component.
*     PVAR( NCOMP ) = REAL (Returned)
*        Variance of peak height for each component.
*     WVAR( NCOMP ) = REAL (Returned)
*        Variance of FWHM for each component.
*     FVAR( NCOMP ) = REAL (Returned)
*        Variance of line integral for each component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     23 Jul 1992 (hme):
*        Adapted from SPABY.
*     27 Jan 1995 (hme):
*        Renamed from SPAAY.
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
      REAL PEAK( NCOMP )
      REAL FWHM( NCOMP )
      REAL VARSCL
      DOUBLE PRECISION COVAR( FITDIM, FITDIM )

*  Arguments Returned:
      REAL FLUX( NCOMP )
      REAL CVAR( NCOMP )
      REAL PVAR( NCOMP )
      REAL WVAR( NCOMP )
      REAL FVAR( NCOMP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXTRI             ! Maximum no. of Gauss components
      PARAMETER ( MAXTRI = 6 )

*  Local Variables:
      INTEGER I, I1, I2          ! Counters for components
      INTEGER J, J1, J2          ! Counters for parameters
      INTEGER JOFP( MAXTRI )     ! Which free parameter is I-th peak
      INTEGER JOFS( MAXTRI )     ! Which free parameter is I-th FWHM
      REAL COVPW                 ! Covariance between peak and FWHM

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  J counts free parameters, I counts Gauss components. The relation
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
*  We also store the number of the free parameter in the mysterious JOFP
*  array. That will be used to resolve the line integral covariance
*  dependencies.
      DO 2 I = 1, NCOMP

*     If this peak was free, its variance is on the diagonal of the
*     covariance.
         IF ( PFLAGS(I) .EQ. 0 ) THEN
            J = J+1
            PVAR(I) = REAL(COVAR(J,J)) * VARSCL
            JOFP(I) = J

*     Else if this peak was fixed, its variance is zero.
         ELSE IF ( PFLAGS(I) .EQ. I ) THEN
            PVAR(I) = 0.
            JOFP(I) = 0

*     Else (this peak was tied to another), its variance derives from
*     that variance.
*     Since a tie relates always to an earlier component (PFLAGS(I)<I),
*     we can assume that PVAR(PFLAGS(I)) is already known.
         ELSE
            PVAR(I) = PVAR(PFLAGS(I))
     :         * ( PEAK(I) / PEAK(PFLAGS(I)) ) ** 2
            JOFP(I) = 0
         END IF
 2    CONTINUE

*  Variance of I-th FWHM.
*  We also store the number of the free parameter in the mysterious JOFS
*  array. That will be used to resolve the line integral covariance
*  dependencies.
      DO 3 I = 1, NCOMP

*     If this FWHM was free, its variance is on the diagonal of the
*     covariance.
         IF ( SFLAGS(I) .EQ. 0 ) THEN
            J = J+1
            WVAR(I) = REAL(COVAR(J,J)) * VARSCL
            JOFS(I) = J

*     Else if this FWHM was fixed, its variance is zero.
         ELSE IF ( SFLAGS(I) .EQ. I ) THEN
            WVAR(I) = 0.
            JOFS(I) = 0

*     Else (this FWHM was tied to another), its variance derives from
*     that variance.
*     Since a tie relates always to an earlier component (SFLAGS(I)<I),
*     we can assume that WVAR(SFLAGS(I)) is already known.
         ELSE
            WVAR(I) = WVAR(SFLAGS(I))
     :         * ( FWHM(I) / FWHM(SFLAGS(I)) ) ** 2
            JOFS(I) = 0
         END IF
 3    CONTINUE

*  Integral and its variance for I-th component. Use error propagation
*  with covariances. Resolve ties properly.
      DO 4 I = 1, NCOMP

*     The flux is simple.
         FLUX(I) = PEAK(I) * FWHM(I)

*     Calculate the variance of the flux. For this we need to know,
*     which components I1 and I2 peak and FWHM are tied to, and
*     which free parameters J1 and J2 those are. The I1 and I2 are of
*     course in the fit flags. But if a parameter was free, its flag is
*     0.
         I1 = PFLAGS(I)
         IF ( I1 .EQ. 0 ) I1 = I
         I2 = SFLAGS(I)
         IF ( I2 .EQ. 0 ) I2 = I

*     Now find which free parameters are involved. If one of the
*     parameters is fixed, J1 or J2 will be 0, indicating that
*     the covariance of peak and FWHM should be taken as 0.0.
         J1 = JOFP(I1)
         J2 = JOFS(I2)

*     Covariance between the two free parameters involved.
         IF ( J1 .EQ. 0 .OR. J2 .EQ. 0 ) THEN
            COVPW = 0.
         ELSE
            COVPW = COVAR(J1,J2) * VARSCL
         END IF

*     Flux variance.
         FVAR(I) = FLUX(I) * FLUX(I)
     :      * ( PVAR(I1) / PEAK(I1) ** 2
     :        + WVAR(I2) / FWHM(I2) ** 2
     :        + 2. * COVPW / PEAK(I1) / FWHM(I2) )
 4    CONTINUE

*  Return.
      END
