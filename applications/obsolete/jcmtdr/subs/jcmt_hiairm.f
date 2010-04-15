      SUBROUTINE JCMT_HIGH_AIRMASS (SECZ, IFLAG, AIRMASS)

*
*     For zenith angle greater than 60 degrees (secz = 2.0), give
*       corrected air mass value, given Ian Coulson's refraction
*       and account for curvature of Earth
*
*     history:
*      04-May-1988 : Original release (JACH::KEVIN)
*      21-MAY-1991: REVAD::JFL
*         Renamed JCMT_HIGH_AIRMASS for Figaro NOD2 replacement
*     endhistory
*
*     Type declarations:
      IMPLICIT NONE

*     Input:
      DOUBLE PRECISION SECZ		! secant of zenith angle
      INTEGER IFLAG 	                ! 1 = make airmass correction

*     Output:
      DOUBLE PRECISION AIRMASS    	! our best guess at actual air mass value

*     Local variables:
      DOUBLE PRECISION A, B, B_REFR, C
      DOUBLE PRECISION Q
      DOUBLE PRECISION R_EARTH	        ! mean radius of the Earth (km)
      DOUBLE PRECISION HEIGHT_MK	! height of Mauna Kea at JCMT (km)
      DOUBLE PRECISION SCALE_HEIGHT     ! guess at scale height of atmosphere above MK
      DOUBLE PRECISION H_DEG, H_NEW
      DOUBLE PRECISION SINH, TANZ
      DOUBLE PRECISION REFR
      DOUBLE PRECISION X, Z

      DATA A / 39.8 /	! arcsec (at T=0 deg C, p = 624 mb, rel H = 50%,
*			!         for a wavelength of 1 mm)

      DATA Q / 57.29577951 /	! 180 / pi

      DATA R_EARTH / 6371.0 /      ! mean radius of Earth
      DATA HEIGHT_MK / 4.1 /       ! altitude of JCMT
      DATA SCALE_HEIGHT / 2.0 /    ! according to Bill Duncan 4-May-1988

      IF ( SECZ .LE. 1.0 ) THEN
         AIRMASS = 1.0
         GOTO 900
      ENDIF

*     Obtain trigonometric elevation above horizon

      Z = ACOS ( 1.0 / SECZ ) 	! zenith angle in radians

      H_DEG = 90.0 - Z * Q

*     If IFLAG = 1, THEN
*       compute refraction correction, for a wavelength of 1 mm,
*       temp of 0 deg C, pressure of 624 mb, and relative humidity of 50 %
*       See Ian Coulson's memos of 20 and 22 Feb 1988.

      IF ( IFLAG .EQ. 1 ) THEN
        B_REFR = - 0.0242 - 0.00212 * H_DEG +
     *                      0.0000676 * H_DEG * H_DEG
        TANZ = TAN ( Z )
        REFR = A * TANZ + B_REFR * TANZ**3.0

*        TYPE *, ' Refr = ', REFR, ' (arcsec)'	! commented out

        H_NEW = H_DEG + REFR / 3600.0
        SINH = SIN ( H_NEW / Q )
      ELSE
        SINH = 1.0 / SECZ
      ENDIF

*     Let R = radius of Earth
*         M = height of Mauna Kea
*         L = scale height of atmosphere
*         h = elevation of object above horizon
*         X = path length through atmosphere  (air mass = X/L)
*
*     It can be shown that:
*
*     (R + M + L)**2 = (R + M)**2 + X**2 +
*                      2 * (R + M) * X * cos (90 + h)
*
*     This can be reduced to the standard quadratic equation:
*
*     0 = A * X**2 + B * X + C, where
*
*     A = 1
*     B = 2 * (R + M) * sin(h)    [because cos (90 + h) = sin(h)]
*     C = [(R + M)**2 - (R + M + L)**2]
*       = [ - 2.0 * R * L - 2.0 * M * L - L**2 ]

      B = 2.0 * ( R_EARTH + HEIGHT_MK ) * SINH		! always > 0

      C = - 2.0 * R_EARTH * SCALE_HEIGHT
     *    - 2.0 * HEIGHT_MK * SCALE_HEIGHT
     *    -       SCALE_HEIGHT * SCALE_HEIGHT

*     Now solve quadratic equation, giving positive solution,
*       as X > 0

      X = ( - B + SQRT ( B*B - 4.0 * C ) ) * 0.5

      AIRMASS = X / SCALE_HEIGHT

900   CONTINUE

      END
