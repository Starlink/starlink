

      SUBROUTINE ELF1_INTER0(ELEMS,ARRAY,NUMPOI,XR,YR,PRANGE,USED,
     :                       VA,STATUS)
*+
*  Name:
*     ELF1_INTER0

*  Purpose:
*     Interpolates the value of a point from the image using bi-linear
*     interpolation. This is performed for all the points in the current
*     ellipse being considered.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_INTER0(ELEMS,ARRAY,NUMPOI,XR,YR,PRANGE,USED,
*                      VA,STATUS)

*  Description:
*     For each of the image locations required the values for each of the
*     four image points surrrounding it are determined. These are then used
*     to derive the value at the required location.
*
*     A value is not generated for a given point if any of the four points
*     is BAD or off the edge of the image.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        The image array.
*     NUMPOI = INTEGER (Given)
*        The number of ellipse points defined.
*     XR(ELF__MXPOI) = REAL (Given)
*        X co-ordinates of the 'fit' ellipse points.
*     YR(ELF__MXPOI) = REAL (Given)
*        Y co-ordinates of the 'fit' ellipse points.
*     PRANGE(2) = INTEGER (Given)
*        Dimensions of the image.
*     USED(ELF__MXPOI) = INTEGER (Returned)
*        Was a value pixel count value found for a given ellipse
*        'fit' point.
*     VA(ELF__MXPOI) = REAL (Returned)
*        The value found at the current ellipse fit locations. The position
*        is defined in XR and YR arrays.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     26-Mar-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'ELF_PAR'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER NUMPOI                  ! Number of ellipse points defined
      INTEGER PRANGE(2)               ! Size of X and Y axes of the image
      REAL ARRAY(ELEMS)               ! Image array
      REAL XR(ELF__MXPOI)             ! X co-ord for the translated ellipse
      REAL YR(ELF__MXPOI)             ! Y co-ord for the translated ellipse

*  Arguments Returned:
      INTEGER USED(ELF__MXPOI)        ! Maximum number of ellipse points
      REAL VA(ELF__MXPOI)             ! Array containing the pixel count
                                      ! values found for ellipse points

*  Arguments Given and Returned:

*  Local variables:
      INTEGER I                       ! Loop counter
      INTEGER XL                      ! The X and Y co-ordinates of the
      INTEGER XH                      ! box within which the current X
      INTEGER YL                      ! Y location of the current
      INTEGER YH                      ! ellipse point may be found
      REAL FX                         ! Fractional X value
      REAL FY                         ! Fractional Y value
      REAL VALUE1                     ! Value at one of the points
                                      ! defining the box.
      REAL VALUE2                     ! See above.
      REAL VALUE3                     ! See above.
      REAL VALUE4                     ! See above.

*.
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Look at each of the ellipse 'fit' locations in turn.
      DO 5 I=1,NUMPOI

*      Assign the X and Y co-ordinates of a box surrounding the
*      required loaction.
         XL=INT(XR(I))
         XH=XL+1
         YL=INT(YR(I))
         YH=YL+1

*      Ignore the current value if the x or y values required are off
*      the image. Also set the flag.
         IF ((XL.GE.1).AND.(XH.LE.PRANGE(1)).AND.
     :       (YL.GE.1).AND.(YH.LE.PRANGE(2))) THEN

*         Calculate the array elements in which the
*         image pixels are and obtain the values.

*         Bottom left corner of box.
            VALUE1=ARRAY((YL-1)*PRANGE(1)+XL)

*         Top left corner of box.
            VALUE2=ARRAY((YH-1)*PRANGE(1)+XL)

*         Bottom right corner of box.
            VALUE3=ARRAY((YL-1)*PRANGE(1)+XH)

*         Top right corner of box.
            VALUE4=ARRAY((YH-1)*PRANGE(1)+XH  )

*         Check to ensure that no bad points were present.
            IF ((VALUE1.NE.VAL__BADR).AND.(VALUE2.NE.VAL__BADR)
     :         .AND.(VALUE3.NE.VAL__BADR).AND.
     :                  (VALUE4.NE.VAL__BADR)) THEN

*            Calculate the fractional x and y values.
               FX=XR(I)-XL
               FY=YR(I)-YL

*            Assign the value.
               VA(I)=(1.-FX)*(1.-FY)*VALUE1+(1.-FX)*FY*VALUE2+
     :                         FX*(1.-FY)*VALUE3+FX*FY*VALUE4

               USED(I)=1

            END IF

         END IF

 5    CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE ELP1_INTER0(ELEMS,ARRAY,NUMPOI,XR,YR,PRANGE,USED,
     :                       VA,STATUS)
*+
*  Name:
*     ELP1_INTER0

*  Purpose:
*     Interpolates the value of a point from the image using bi-linear
*     interpolation. This is performed for all the points in the current
*     ellipse being considered.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELP1_INTER0(ELEMS,ARRAY,NUMPOI,XR,YR,PRANGE,USED,
*                      VA,STATUS)

*  Description:
*     For each of the image locations required the values for each of the
*     four image points surrrounding it are determined. These are then used
*     to derive the value at the required location.
*
*     A value is not generated for a given point if any of the four points
*     is BAD or off the edge of the image.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        The image array.
*     NUMPOI = INTEGER (Given)
*        The number of ellipse points defined.
*     XR(ELP__MXPOI) = REAL (Given)
*        X co-ordinates of the 'fit' ellipse points.
*     YR(ELP__MXPOI) = REAL (Given)
*        Y co-ordinates of the 'fit' ellipse points.
*     PRANGE(2) = INTEGER (Given)
*        Dimensions of the image.
*     USED(ELP__MXPOI) = INTEGER (Returned)
*        Was a value pixel count value found for a given ellipse
*        'fit' point.
*     VA(ELP__MXPOI) = REAL (Returned)
*        The value found at the current ellipse fit locations. The position
*        is defined in XR and YR arrays.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     26-Mar-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'ELP_PAR'               ! ELLPRO constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER NUMPOI                  ! Number of ellipse points defined
      INTEGER PRANGE(2)               ! Size of X and Y axes of the image
      REAL ARRAY(ELEMS)               ! Image array
      REAL XR(ELP__MXPOI)             ! X co-ord for the translated ellipse
      REAL YR(ELP__MXPOI)             ! Y co-ord for the translated ellipse

*  Arguments Returned:
      INTEGER USED(ELP__MXPOI)        ! Maximum number of ellipse points
      REAL VA(ELP__MXPOI)             ! Array containing the pixel count
                                      ! values found for ellipse points

*  Arguments Given and Returned:

*  Local variables:
      INTEGER I                       ! Loop counter
      INTEGER XL                      ! The X and Y co-ordinates of the
      INTEGER XH                      ! box within which the current X
      INTEGER YL                      ! Y location of the current
      INTEGER YH                      ! ellipse point may be found
      REAL FX                         ! Fractional X value
      REAL FY                         ! Fractional Y value
      REAL VALUE                      ! Value at the required location
      REAL VALUE1                     ! Value at one of the points
                                      ! defining the box.
      REAL VALUE2                     ! See above.
      REAL VALUE3                     ! See above.
      REAL VALUE4                     ! See above.

*.
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Look at each of the ellipse 'fit' locations in turn.
      DO 5 I=1,NUMPOI

*      Assign the X and Y co-ordinates of a box surrounding the
*      required loaction.
         XL=INT(XR(I))
         XH=XL+1
         YL=INT(YR(I))
         YH=YL+1

*      Ignore the current value if the x or y values required are off
*      the image. Also set the flag.
         IF ((XL.GE.1).AND.(XH.LE.PRANGE(1)).AND.
     :       (YL.GE.1).AND.(YH.LE.PRANGE(2))) THEN

*         Calculate the array elements in which the
*         image pixels are and obtain the values.

*         Bottom left corner of box.
            VALUE1=ARRAY((YL-1)*PRANGE(1)+XL)

*         Top left corner of box.
            VALUE2=ARRAY((YH-1)*PRANGE(1)+XL)

*         Bottom right corner of box.
            VALUE3=ARRAY((YL-1)*PRANGE(1)+XH)

*         Top right corner of box.
            VALUE4=ARRAY((YH-1)*PRANGE(1)+XH)

*         Check to ensure that no bad points were present.
            IF ((VALUE1.NE.VAL__BADR).AND.(VALUE2.NE.VAL__BADR)
     :         .AND.(VALUE3.NE.VAL__BADR).AND.
     :                  (VALUE4.NE.VAL__BADR)) THEN

*            Calculate the fractional x and y values.
               FX=XR(I)-XL
               FY=YR(I)-YL

*            Assign the value.
               VALUE=(1.-FX)*(1.-FY)*VALUE1+(1.-FX)*FY*VALUE2
               VA(I)=VALUE+FX*(1.-FY)*VALUE3+FX*FY*VALUE4
               USED(I)=1

            END IF

         END IF

 5    CONTINUE

 9999 CONTINUE

      END
