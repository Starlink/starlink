

      SUBROUTINE ELF1_ANGLES(X,Y,XC,YC,ANGLE,STATUS)
*+
*  Name:
*     ELF1_ANGLES

*  Purpose:
*     Determine the angle of a point relative to a specified origin.
*     Direction of increasing angle clockwise and origin vertical.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_ANGLES(X,Y,XC,YC,ANGLE,STATUS)

*  Description:
*     Determines the angle between a point and the vertical axis of the
*     image (using a user provided origin). The ATAN function is used.
*     To determine the quadrant knowledge of the x/y displacements from
*     the given origin is employed. To avoid overflow errors, zero
*     y displacements are handled carefully.

*  Arguments:
*     X = REAL (Given)
*        X co-ordinate of the pixel required
*     Y = REAL (Given)
*        Y co=ordinate of the pixel required.
*     XC = REAL (Given)
*        X co-ordinate for the galaxy centre.
*     YC = REAL (Given)
*        Y co-ordinate for the galaxy centre.
*     ANGLE = REAL (Returned)
*        Angle the pixel makes with the galaxy origin. Units degrees.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     4-Apr-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELF_PAR'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      REAL X                          ! X location of pixel
      REAL XC                         ! X location of the galaxy origin
      REAL Y                          ! Y location of pixel
      REAL YC                         ! Y location of the galaxy origin

*  Arguments Returned:
      REAL ANGLE                      ! The angle the point/origin
                                      ! line makes with the vertical

*  Arguments Given and Returned:

*  Local variables:
      REAL ATNVAL                     ! Arctan value
      REAL RX                         ! X displacement of pixel
      REAL RY                         ! Y displacement of pixel
      REAL VALUE                      ! Temporary storage

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Find the displacements from the origin in terms of x and y.
      RX=X-XC
      RY=Y-YC

*   Deal with purely vertical displacements.
      IF (ABS(RX).LT.1E-20) THEN

*      Assign angle as 0 or 180 degrees.
         IF (RY.LT.0.0) THEN
            ANGLE=ELF__PIVAL
         ELSE
            ANGLE=0.0
         END IF

      ELSE

*      Deal with purely horizontal displacements.
         IF (RY.EQ.0) THEN

*         Assign angle as 90 or 270 degrees.
            IF (X-XC.LT.0.0) THEN
               ANGLE=1.5*ELF__PIVAL
            ELSE
               ANGLE=ELF__PIVAL/2.0
            END IF

         ELSE

*         Deal with all other cases.
            VALUE=RX/RY
            ATNVAL=ATAN(VALUE)

*         Sort out the value depending on the quadrant.
            IF (VALUE.GT.0.0) THEN
               IF (RX.GT.0.0) THEN
                  ANGLE=ATNVAL
               ELSE
                  ANGLE=ELF__PIVAL+ABS(ATNVAL)
               END IF
            ELSE
               IF (RX.GT.0.0) THEN
                  ANGLE=ELF__PIVAL+ATNVAL
               ELSE
                  ANGLE=2.0*ELF__PIVAL+ATNVAL
               END IF
            END IF

         END IF

      END IF

*   Convert to degrees.
      ANGLE=ANGLE/ELF__PIVAL*180.

 9999 CONTINUE

      END





      SUBROUTINE ELP1_ANGLES(X,Y,XC,YC,ANGLE,STATUS)
*+
*  Name:
*     ELP1_ANGLES

*  Purpose:
*     Determine the angle of a point relative to a specified origin.
*     Direction of increasing angle clockwise and origin vertical.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELP1_ANGLES(X,Y,XC,YC,ANGLE,STATUS)

*  Description:
*     Determines the angle between a point and the vertical axis of the
*     image (using a user provided origin). The ATAN function is used.
*     To determine the quadrant knowledge of the x/y displacements from
*     the given origin is employed. To avoid overflow errors, zero
*     y displacements are handled carefully.

*  Arguments:
*     X = REAL (Given)
*        X co-ordinate of the pixel required
*     Y = REAL (Given)
*        Y co=ordinate of the pixel required.
*     XC = REAL (Given)
*        X co-ordinate for the galaxy centre.
*     YC = REAL (Given)
*        Y co-ordinate for the galaxy centre.
*     ANGLE = REAL (Returned)
*        Angle the pixel makes with the galaxy origin. Units degrees.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     4-Apr-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELP_PAR'               ! ELLPAR constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      REAL X                          ! X location of pixel
      REAL XC                         ! X location of the galaxy origin
      REAL Y                          ! Y location of pixel
      REAL YC                         ! Y location of the galaxy origin

*  Arguments Returned:
      REAL ANGLE                      ! The angle the point/origin
                                      ! line makes with the vertical

*  Arguments Given and Returned:

*  Local variables:
      REAL ATNVAL                     ! Arctan value
      REAL RX                         ! X displacement of pixel
      REAL RY                         ! Y displacement of pixel
      REAL VALUE                      ! Temporary storage

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Find the displacements from the origin in terms of x and y.
      RX=X-XC
      RY=Y-YC

*   Deal with purely vertical displacements.
      IF (ABS(RX).LT.1E-20) THEN

*      Assign angle as 0 or 180 degrees.
         IF (RY.LT.0.0) THEN
            ANGLE=ELP__PIVAL
         ELSE
            ANGLE=0.0
         END IF

      ELSE

*      Deal with purely horizontal displacements.
         IF (RY.EQ.0) THEN

*         Assign angle as 90 or 270 degrees.
            IF (X-XC.LT.0.0) THEN
               ANGLE=1.5*ELP__PIVAL
            ELSE
               ANGLE=ELP__PIVAL/2.0
            END IF

         ELSE

*         Deal with all other cases.
            VALUE=RX/RY
            ATNVAL=ATAN(VALUE)

*         Sort out the value depending on the quadrant.
            IF (VALUE.GT.0.0) THEN
               IF (RX.GT.0.0) THEN
                  ANGLE=ATNVAL
               ELSE
                  ANGLE=ELP__PIVAL+ABS(ATNVAL)
               END IF
            ELSE
               IF (RX.GT.0.0) THEN
                  ANGLE=ELP__PIVAL+ATNVAL
               ELSE
                  ANGLE=2.0*ELP__PIVAL+ATNVAL
               END IF
            END IF

         END IF

      END IF

*   Convert to degrees.
      ANGLE=ANGLE/ELP__PIVAL*180.

 9999 CONTINUE

      END
