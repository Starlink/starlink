      SUBROUTINE SCULIB_AIRMASS (Z, AIRMASS, STATUS)
*+
*  Name:
*     SCULIB_AIRMASS

*  Purpose:
*     calculate the airmass for a given zenith distance

*  Description:
*     This routine calculates the airmass corresponding to the input
*     zenith distance. The airmass is just sec(Z) until it reaches 2,
*     after which a more complex algorithm developed by Ian Coulson
*     is used (it is described by comments in the code). There may be
*     a discontinuity in returned airmasses around the value 2, I
*     haven't checked.

*  Invocation:
*     CALL SCULIB_AIRMASS (Z, AIRMASS, STATUS)

*  Arguments:
*     Z               = DOUBLE PRECISION (Given)
*           zenith distance (radians)
*     AIRMASS         = DOUBLE PRECISION (Returned)
*           airmass
*     STATUS          = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (REVAD::JFL)
*     Tim Jenness (JAC, Hawaii)

*  Copyright:
*     Copyright (C) 1993-1999, 2005 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     5-AUG-1993: Original version.
*     $Log$
*     Revision 1.7  2006/07/05 21:27:57  timj
*     output elevation when below horizon
*
*     Revision 1.6  2005/09/22 01:54:43  timj
*     pull constants out of loops and make sure that we minimize type conversion by making SCULIB_AIRMASS use double precision
*
*     Revision 1.5  1999/08/19 03:36:58  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.4  1999/08/03 19:34:43  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.3  1996/08/02 01:55:11  timj
*     Z was zenith distance NOT elevation so change back to COS
*
*     Revision 1.2  1996/08/01  21:24:03  timj
*     Change AIRMASS def to 1/SIN from 1/COS
*
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      DOUBLE PRECISION Z

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION AIRMASS

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      DOUBLE PRECISION A, B, B_REFR, C
      DOUBLE PRECISION Q
      DOUBLE PRECISION R_EARTH	        ! mean radius of the Earth (km)
      DOUBLE PRECISION HEIGHT_MK	! height of Mauna Kea at JCMT (km)
      DOUBLE PRECISION SCALE_HEIGHT     ! guess at scale height of atmosphere above MK
      DOUBLE PRECISION H_DEG, H_NEW
      DOUBLE PRECISION SINH, TANZ
      DOUBLE PRECISION REFR
      DOUBLE PRECISION X
      DOUBLE PRECISION COS_Z

*  Internal References:

*  Local data:
      DATA A / 39.8 /   ! arcsec (at T=0 deg C, p = 624 mb, rel H = 50%,
                        ! for a wavelength of 1 mm)
      DATA Q / 57.29577951 /     ! 180 / pi
      DATA R_EARTH / 6371.0 /    ! mean radius of Earth
      DATA HEIGHT_MK / 4.1 /     ! altitude of JCMT
      DATA SCALE_HEIGHT / 2.0 /  ! according to Bill Duncan 4-May-1988

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Pre cos
      COS_Z = COS(Z)

      IF (COS_Z .LE. 0.0D0) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETD( 'EL', 90.0D0 - ( Z * Q ) )
         CALL ERR_REP (' ',
     :        'SCULIB_AIRMASS: point is below horizon (^EL deg)',
     :        STATUS)
      ELSE

         AIRMASS = 1.0D0 / COS_Z

         IF (AIRMASS .GT. 2.0) THEN

*  obtain trigonometric elevation above horizon

            H_DEG = 90.0D0 - Z * Q

*  compute refraction correction, for a wavelength of 1 mm,
*  temp of 0 deg C, pressure of 624 mb, and relative humidity of 50 %
*  See Ian Coulson's memos of 20 and 22 Feb 1988.

            B_REFR = - 0.0242D0 - 0.00212D0 * H_DEG +
     :        0.0000676D0 * H_DEG * H_DEG
            TANZ = TAN (Z)
            REFR = A * TANZ + B_REFR * TANZ**3.0
            H_NEW = H_DEG + REFR / 3600.0
            SINH = SIN ( H_NEW / Q )

*  Let R = radius of Earth
*      M = height of Mauna Kea
*      L = scale height of atmosphere
*      h = elevation of object above horizon
*      X = path length through atmosphere  (air mass = X/L)
*
*  It can be shown that:
*
*     (R + M + L)**2 = (R + M)**2 + X**2 +
*                      2 * (R + M) * X * cos (90 + h)
*
*  This can be reduced to the standard quadratic equation:
*
*     0 = A * X**2 + B * X + C, where
*
*     A = 1
*     B = 2 * (R + M) * sin(h)    [because cos (90 + h) = sin(h)]
*     C = [(R + M)**2 - (R + M + L)**2]
*       = [ - 2.0 * R * L - 2.0 * M * L - L**2 ]

            B = 2.0 * ( R_EARTH + HEIGHT_MK ) * SINH
            C = - 2.0 * R_EARTH * SCALE_HEIGHT
     :        - 2.0 * HEIGHT_MK * SCALE_HEIGHT
     :        -       SCALE_HEIGHT * SCALE_HEIGHT

*  Now solve quadratic equation, giving positive solution, as X > 0

            X = (- B + SQRT (B*B - 4.0 * C)) / 2.0
            AIRMASS = X / SCALE_HEIGHT

         END IF
      END IF

      END
