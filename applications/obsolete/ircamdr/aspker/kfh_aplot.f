*+  KFH_APLOT - Plots a pixel array on the ARGS.
      SUBROUTINE KFH_APLOT(XL,YL,XH,YH,XD,YD,DATA,LO,HI,IMAGE,
     :                     STATUS)
*    Description :
*     This subroutine will plot a pixel array on the ARGS.
*    Invocation :
*     CALL KFH_APLOT(XL,YL,XH,YH,XDIM,YDIM,DATA,LO,HI,IMAGE,STATUS)
*    Parameters :
*     XL = REAL
*        X-coordinate of bottom left hand corner of the image.
*     YL = REAL
*        Y-coordinate of bottom left hand corner of the image.
*     XH = REAL
*        X-coordinate of the top right hand corner of the image.
*     YH = REAL
*        Y-coordinate of the top right hand corner of the image.
*     XD = INTEGER
*        X dimension of image.
*     YD = INTEGER
*        Y dimension of image.
*     DATA(XD,YD) = REAL
*        Array holding image.
*     LO = REAL
*        Lowest value - to be scaled to 0 on the ARGS
*     HI = REAL
*        Highest value - to be scaled to 255 on the ARGS
*     IMAGE(XD,YD) = INTEGER
*        Space in which to invert image.
*     STATUS = INTEGER
*        Status value on entering this subroutine.
*    Method :
*
*     Scaling of (DATA-LO)/(HI-LO) and truncation outside the
*     range LO to HI is applied before dsiplay.
*     The image is inverted in the Y direction because of the
*     GKS routine GKS_PXA which treats the image as though it
*     were a matrix. The GKS routine is then called to plot the
*     image.
*    Authors :
*     A.P.Horsfield (RGVAD::KFH)
*    History :
*     22 July 1983: Original (RGVAD::KFH)
*     26 Sept 1983: LO and HI added (KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      REAL FACTOR			! Factor used in scaling
      INTEGER XD			! The x dimension of
*					! the image.
      INTEGER YD			! The y dimension of
*					! the image.
      REAL DATA(XD,YD)			! Array containing
*					! the image.
      REAL HI				! High value for scaling
      INTEGER I				! General variable.
      INTEGER IMAGE(XD,YD)		! Array containing
*					! the inverted image.
      INTEGER J				! General variable.
      INTEGER K				! General variable.
      REAL LO				! Low value for scaling
      REAL XH				! X-coordinate of the
*					! top right hand
*					! corner of the image.
      REAL XL				! X-coordinate of the
*					! bottom left hand
*					! corner of the image.
      REAL YH				! The y coordinate of
*					! the top right hand
*					! corner of the image.
      REAL YL				! Y-coordinate of the
*					! bottom left hand
*					! corner of the image.
*-

*
*    If the status is bad, then return to the calling program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Invert the image with scaling.
*
         IF (LO.NE.HI) THEN
            FACTOR = 255.0/(HI-LO)
         ELSE
            FACTOR = 1.0
         END IF

         DO I = 1,YD,1
            K = (YD+1)-I
            DO J = 1,XD,1
               IF (DATA(J,I).LT.LO) THEN
                  IMAGE(J,K)=0
               ELSE IF (DATA(J,I).GT.HI) THEN
                  IMAGE(J,K)=255
               ELSE
                  IMAGE(J,K) = (DATA(J,I) - LO) * FACTOR
               END IF
            ENDDO
         ENDDO

*
*       Display the image.
*

         CALL GKS_PXA(XL,YL,XH,YH,XD,YD,IMAGE)

      ENDIF

      END
