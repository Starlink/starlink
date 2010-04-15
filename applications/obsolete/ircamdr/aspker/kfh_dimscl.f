*+  KFH_DIMSCL - User defines limits of image scaling.
      SUBROUTINE KFH_DIMSCL(IMAGE,XDIM,YDIM,SCRTCH,UPPER,LOWER,
     : MINV,MAXV,FOUND,BLNKVL,NINTS,STATUS)
*    Description :
*     The user is given the minimum and maximum values found
*     in the image, and is then asked for the limits between
*     which he wants the image scaled. The image is then
*     scaled such that the lower limit and all values are
*     set to 0, the upper limit and all values above are set
*     to 255, and all values in between are assigned values
*     using linear interpolation between the limits.
*    Invocation :
*     CALL KFH_DIMSCL(IMAGE,XDIM,YDIM,SCRTCH,UPPER,LOWER,
*    : MINV,MAXV,FOUND,BLNKVL,NINTS,STATUS)
*    Parameters :
*     IMAGE(XDIM,YDIM) = REAL
*           The original, unscaled image data.
*     XDIM = INTEGER
*           The x dimension of the image.
*     YDIM = INTEGER
*           The y dimension of the image.
*     SCRTCH(XDIM,YDIM) = INTEGER
*           The scaled version of the image.
*     UPPER = REAL
*           The upper limit used for scaling the image.
*     LOWER = REAL
*           The lower limit used for scaling the image.
*     MINV = REAL
*           The minimum value of the image.
*     MAXV = REAL
*           The maximum value of the image.
*     FOUND = LOGICAL
*           Flag is set if the maximum and minimum values
*           of the image have been found.
*     BLNKVL = REAL
*           Holds the value of DATA_BLANK or -32767.
*     NINTS = INTEGER
*           The number of greyscale intensities available
*           on the chosen device.
*     STATUS = INTEGER
*           Value of the status on entering this subroutine.
*    Method :
*     First the maximum and minimum values in the image
*     are found, and displayed to the user. The user is then
*     prompted for the limits he wishes to use. If they are
*     not legal then the minimum and maximum values are used.
*     The scaled image is then produced. The scaled version
*     is inverted so that it will appear the write way round
*     when displayed on the ARGS as the GKS routine to display
*     the image inverts it.
*    Authors :
*    A.P.Horsfield (RGVAD::KFH)
*    S.Chan (RGVAD::KFH)
*    History :
*     28 July 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      REAL BLNKVL                       ! The value of DATA_BLANK
*                                       ! or the default value
*                                       ! of -32767.
      LOGICAL FOUND                      ! This flag is set if
*                                       ! the maximum and
*                                       ! minimum values of the
*                                       ! image data have been
*                                       ! found.
      INTEGER XDIM			! The x dimension of the
*					! image.
      INTEGER YDIM			! The y dimension of the
*					! image.
      INTEGER I				! General variable.
      REAL IMAGE(XDIM,YDIM)		! The raw image data.
      INTEGER J				! General variable.
      INTEGER K				! General variable.
      REAL LOWER			! The lower limit used
*					! for the scaling.
      REAL MAXV				! The maximum value in
*					! the image.
      REAL MINV				! The minimum value in
*					! the image.
      INTEGER NINTS                     ! The number of greyscale
*                                       ! intensities available on
*                                       ! the chosen device.
      INTEGER SCRTCH(XDIM,YDIM)		! The area in which the
*					! the scaled image is
*					! stored.
      REAL UPPER			! The upper limit used
*					! for the scaling.
*-

*
*    If the status is bad on entry, then return to the calling
*    program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Find the maximum and minimum values of the image,
*       excluding the invalid values (BLNKVL).
*

         IF (.NOT.FOUND) THEN

            MAXV = IMAGE(1,1)
            MINV = IMAGE(1,1)

            DO I = 1,YDIM,1
               DO J = 1,XDIM,1
                  IF (IMAGE(J,I).NE.BLNKVL) THEN
                     IF (IMAGE(J,I).GT.MAXV) THEN
                        MAXV = IMAGE(J,I)
                     ELSEIF (IMAGE(J,I).LT.MINV) THEN
                        MINV = IMAGE(J,I)
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
         ENDIF

*
*       Display the minimum value and prompt the user for the
*       the lower limit to be used in the scaling.
*

         CALL MSG_SETR('MINVAL',MINV)
         CALL MSG_OUT('PVLO','MINIMUM VALUE IS ^MINVAL',STATUS)

         CALL PAR_GET0R('LOW',LOWER,STATUS)

*
*       If the input was not legal then set the lower limit to
*       the minimum value found in the image.
*

         IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_ANNUL(STATUS)
            LOWER = MINV
         ENDIF

*
*       Clear up parameters.
*

*         CALL PAR_CANCL('LOW',STATUS)

*
*       Display the maximum value to the user and prompt him for
*       the upper limit to be used for the scaling.
*

         CALL MSG_SETR('MAXVAL',MAXV)
         CALL MSG_OUT('PVHI','MAXIMUM VALUE IS ^MAXVAL',STATUS)

         CALL PAR_GET0R('HIGH',UPPER,STATUS)

*
*       If the input is not legal then set the upper limit to
*       the largest value found in the image.
*

         IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_ANNUL(STATUS)
            UPPER = MAXV
         ENDIF

*
*       Clear up parameters.
*

*         CALL PAR_CANCL('HIGH',STATUS)

*
*       Scale and invert the image.
*

         DO I = 1,YDIM,1

            K = (YDIM+1)-I

            DO J = 1,XDIM,1
               SCRTCH(J,I) = MIN(MAX(NINT((IMAGE(J,K)-LOWER)*
     :          REAL(NINTS-1)/(UPPER-LOWER)),0),NINTS-1)
            ENDDO

         ENDDO

      ENDIF

      END
