      SUBROUTINE GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,ELEMS,UPIX,
     :                      ARRAY2,ARRAY3,ARRAY4,
     :                      ARRAY5,ARRAY6,RESID,STATUS)
*+
*  Name:
*     GAU1_BUILD

*  Purpose:
*     Creates a model image from the parameters supplied and
*     then subtracts it from the real image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,ELEMS,UPIX,
*                     ARRAY2,ARRAY3,ARRAY4,ARRAY5,ARRAY6,
*                     RESID,STATUS)

*  Description:
*     Create an image from the Gaussians defined and then subtract
*     it from the source image. The value RESID contains a
*     weighted residual which is used to minimise the actual
*     residuals.

*  Arguments:
*     NSOUR = Integer (Given)
*        Number of sources in the image.
*     BACK = REAL (Given)
*        Background count.
*     PRANGE(2) = INTEGER (Given)
*        Dimensions of the image
*     RLIM(10) = REAL (Given)
*        Maximum source radii
*     PASS(10,7) = REAL (Given)
*        The Gaussian parameters.
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     UPIX = INTEGER (Given)
*        Number of pixels in the image to be used.
*     ARRAY2(ELEMS) = REAL (Returned)
*        A masking array.  Really?  I think it's the image array [NG]
*     ARRAY3(ELEMS) = REAL (Returned)
*        The image to be constructed.
*     ARRAY4(UPIX) = INTEGER (Given)
*        Indices of the good pixels.
*     ARRAY5(UPIX) = INTEGER (Given)
*        X co-ordinate of the good pixels.
*     ARRAY6(UPIX) = INTEGER (Given)
*        Y co-ordinate of the good pixels.
*     RESID = REAL (Returned)
*        Mean pixel value after subtraction.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*     [NG] The parameters pass(i,j) seem to be as follows:
*     pass(i,1): x-coord of gaussian
*     pass(i,2): y-coord
*     pass(i,3): ?
*     pass(i,4): peak height/pixels
*     pass(i,5): major-axis sigma/pixels
*     pass(i,6): minor-axis sigma/pixels
*     pass(i,7): angle of major axis

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     26-Mar-1996 (GJP)
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
      INTEGER UPIX                    ! Number of non-bad pixels
c$$$      INTEGER ARRAY4(ELEMS)           ! Indices array
c$$$      INTEGER ARRAY5(ELEMS)           ! X co-ordinate array
c$$$      INTEGER ARRAY6(ELEMS)           ! Y co-ordinate array
      INTEGER ARRAY4(UPIX)            ! Indices array
      INTEGER ARRAY5(UPIX)            ! X co-ordinate array
      INTEGER ARRAY6(UPIX)            ! Y co-ordinate array
      INTEGER NSOUR                   ! Number of sources in the image
      INTEGER PRANGE(2)               ! Dimensions of the image
      REAL ARRAY2(ELEMS)              ! Masking array
      REAL BACK                       ! Background count
      REAL PASS(10,7)                 ! Current parameter estimates
      REAL RLIM(10)                   ! Maximum source radius

*  Arguments Returned:
      REAL ARRAY3(ELEMS)              ! The imaged pixels
      REAL RESID                      ! Arbitrary pixel value residue

*  Local variables:
      INTEGER ADD                     ! Pixel array address
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Pixel counter
      REAL ANGLE                      ! Rotation angle
      REAL PIC                        ! Degrees/radians conversion
      REAL D1                         ! Displacement form origin
      REAL D2                         ! Displacement from origin
      REAL PIOV2                      ! Conversion factor
      REAL PI2360                     ! Conversion factor
      REAL RD                         ! Radius
      REAL V1                         ! Multiplying factor
      REAL XV                         ! X coordinate
      REAL YV                         ! Y coordinate
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the array to bad.
      DO 5 I=1,ELEMS
         ARRAY3(I)=VAL__BADR
 5    CONTINUE

*   Clear those that will participate
      DO 8 I=1,UPIX
         ARRAY3(ARRAY4(I))=0.0
 8    CONTINUE

*   Set useful constants.
      PI2360=3.1415926*2./360.
      PIOV2 =3.1415926/2.0

*   For each Gaussian.
      DO 10 I=1,NSOUR

*      Define a converted angle (ie, convert pass(i,7) to radians [NG])
         PIC=PASS(I,7)*PI2360

*      Scaling factor for pixel brightness.
         V1=PASS(I,4)

*      Look at all pixels within a square about the origin.
*      (within a square? - I see no limit! [NG])
         DO 30 J=1,UPIX

*         Determine angle and distance from origin.
            D1=REAL(ARRAY6(J))-PASS(I,2)
            D2=REAL(ARRAY5(J))-PASS(I,1)
            IF (ABS(D2).GT.1E-10) THEN
               ANGLE=ATAN(D1/D2)-PIC
               RD=SQRT(D1**2+D2**2)
            ELSE
               ANGLE=PIOV2-PIC
               RD=D1
            END IF
*         Angle is the position of this point rel. major axis
*         (given by pass(i,7)), anticlockwise in radians [NG]

*         Find value of X and Y relative to unrotated Gaussian.
            XV=(RD*COS(ANGLE)/PASS(I,5))**2
            YV=(RD*SIN(ANGLE)/PASS(I,6))**2

*         Calculate pixel brightness and add it to the current value.
            ADD=ARRAY4(J)
*         These ABS() aren't necessary!  Should it be dividing by 4?
*            ARRAY3(ADD)=ARRAY3(ADD)+V1*EXP(-(ABS(YV)+ABS(XV))/4.)
            ARRAY3(ADD)=ARRAY3(ADD)+V1*EXP(-(YV+XV)/4.)

 30      CONTINUE

 10   CONTINUE

*   Look through all the pixels subtracting the value in the created array
*   from the source image.
      RESID=0.0
      DO 100 I=1,UPIX

*      Get the index of the next pixel.
         J=ARRAY4(I)

*      Calculate the residuals.
         RESID=RESID+ABS(ARRAY2(J)-ARRAY3(J))*ABS(ARRAY2(J))

*      Assign the output image value.
         ARRAY3(J)=ARRAY2(J)-ARRAY3(J)

 100  CONTINUE

*   Normalise the result.
      RESID=SQRT(RESID/REAL(UPIX))

 9999 CONTINUE

      END
