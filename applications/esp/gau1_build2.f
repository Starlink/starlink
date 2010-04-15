      SUBROUTINE GAU1_BUILD2(NSOUR,BACK,PRANGE,PASS,ELEMS,
     :                       ARRAY3,STATUS)
*+
*  Name:
*     GAU1_BUILD2

*  Purpose:
*     Creates a whole image model from the parameters supplied.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_BUILD2(NSOUR,BACK,PRANGE,PASS,ELEMS,
*                      ARRAY3,STATUS)

*  Description:
*     Create an image from the Gaussians defined.

*  Arguments:
*     NSOUR = Integer (Given)
*        Number of sources in the image.
*     BACK = REAL (Given)
*        Background count.
*     PRANGE(2) = INTEGER (Given)
*        Dimensions of the image
*     PASS(10,7) = REAL (Given)
*        The Gaussian parameters.
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     ARRAY3(ELEMS) = REAL (Returned)
*        The image to be constructed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     6-May-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ELEMS                   ! The number of image pixels
      INTEGER NSOUR                   ! Number of sources in the image
      INTEGER PRANGE(2)               ! Dimensions of the image
      REAL BACK                       ! Background count
      REAL PASS(10,7)                 ! Current parameter estimates

*  Arguments Returned:
      REAL ARRAY3(ELEMS)              ! The imaged pixels

*  Local variables:
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Pixel counter
      INTEGER XMAX                    ! Length of X axis
      REAL ANGLE                      ! Rotation angle
      REAL PIC                        ! Degrees/radians conversion
      REAL PIOV2                      ! Conversion factor
      REAL PI2360                     ! Conversion factor
      REAL RD                         ! Radius
      REAL V1                         ! Multiplying factor
      REAL X                          ! X coordinate
      REAL XV                         ! X coordinate
      REAL X1                         ! Displacement from source origin
      REAL Y                          ! Y coordinate
      REAL YV                         ! Y coordinate
      REAL Y1                         ! Displacement from source origin
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the array to zero.
      DO 5 I=1,ELEMS
         ARRAY3(I)=0.0
 5    CONTINUE

*   Set useful constants.
      PI2360=3.1415926*2./360.
      PIOV2 =3.1415926/2.0

*   Set up value for width of image.
      XMAX=PRANGE(1)

*   For each source.
      DO 10 I=1,NSOUR

*      Define a converted angle.
         PIC=PASS(I,7)*PI2360

*      Scaling factor for pixel brightness.
         V1=PASS(I,4)

*      For each element of the array.
         DO 20 J=1,ELEMS

*         Determine the current coordinates.
            Y=INT(1+J/XMAX)
            X=J-(Y-1)*XMAX

*         Determine angle and distance from origin.
            X1=REAL(X)-PASS(I,1)
            Y1=REAL(Y)-PASS(I,2)
            IF (ABS(X1).GT.1E-10) THEN
               ANGLE=ATAN(Y1/X1)-PIC
               RD=SQRT(X1**2+Y1**2)
            ELSE
               ANGLE=PIOV2-PIC
               RD=Y1
            END IF

*         Find value of X and Y relative to unrotated Gaussian.
            XV=(RD*COS(ANGLE)/PASS(I,5))**2
            YV=(RD*SIN(ANGLE)/PASS(I,6))**2

*         Calculate pixel brightness and add it to the current value.
            ARRAY3(J)=ARRAY3(J)+V1*EXP(-(ABS(YV)+ABS(XV))/4.)

 20      CONTINUE


 10   CONTINUE

 9999 CONTINUE

      END
