*+  KFH_DFLASH - Flashes image onto ARGS screen.
      SUBROUTINE KFH_DFLASH(IMAGE,XDIM,YDIM,SCRTCH,STATUS)
*    Description :
*     This subroutine displays an image onto the ARGS screen
*     without scaling it in any way.
*    Invocation :
*     CALL KFH_DFLASH(IMAGE,XDIM,YDIM,SCRTCH,STATUS)
*    Parameters :
*     IMAGE(XDIM,YDIM) = REAL
*           This array contains the raw image data.
*     XDIM = INTEGER
*           This is the x dimension of the image.
*     YDIM = INTEGER
*           This is the y dimension of the image.
*     SCRTCH(XDIM,YDIM) = INTEGER
*           This is an array into which the image is put
*           before being displayed.
*     STATUS = INTEGER
*           Value of the status on entering this subroutine.
*    Method :
*     The image data is transferred into the scratch area
*     having been converted from reals to integers. The
*     image is also inverted in the proccess, as the GKS
*     routine for displaying the image on the ARGS inverts
*     the image.
*    Authors :
*     A.P.Horsfield (RGVAD::KFH)
*     S.Chan (RGVAD::KFH)
*    History :
*     27 July 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER XDIM			! X dimension of image.
      INTEGER YDIM			! Y dimension of image.
      INTEGER I				! General variable.
      REAL IMAGE(XDIM,YDIM)		! The array containing
*					! the original image.
      INTEGER J				! General variable.
      INTEGER K				! General variable.
      REAL MAXV                         ! Upper value of the
*                                       ! image.
      REAL MINV                         ! Lower value of the
*                                       ! image.
      INTEGER SCRTCH(XDIM,YDIM)		! Area in which image
*					! is stored before
*					! being displayed.
*-

*
*    If the status is bad on entry, then return to the calling
*    program, otherwise flash the image onto the screen.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Set up upper and lower values of the image.
*

         MINV = 0.0
         MAXV = 255.0

         CALL PAR_DEF0R('LOW',MINV,STATUS)
         CALL PAR_PUT0R('LOW',MINV,STATUS)
         CALL PAR_DEF0R('HIGH',MAXV,STATUS)
         CALL PAR_PUT0R('HIGH',MAXV,STATUS)

*
*       Flash the image.
*

         DO I = 1,YDIM,1

            K = (YDIM+1)-I

            DO J = 1,XDIM,1
               SCRTCH(J,I) = NINT(IMAGE(J,K))
            ENDDO

         ENDDO

         CALL GKS_PXA(256.0-REAL(XDIM)/2.0,256.0-REAL(YDIM)/2.0,
     :                256.0+REAL(XDIM)/2.0,256.0+REAL(YDIM)/2.0,
     :                XDIM,YDIM,SCRTCH)

      ENDIF

      END
