
*+  KFH_DFAINT - Scales image to range mean-s.d. to mean+7*s.d..
      SUBROUTINE KFH_DFAINT(IMAGE,XDIM,YDIM,SCRTCH,BLNKVL,NINTS,STATUS)
*    Description :
*     This routine scales an image to the range mean-standard
*     deviation to mean+7 standard deviations. The values for
*     the range were found empirically.
*     The image is stored inverted so that when it is
*     displayed it will come out the right way around.
*    Invocation :
*     CALL KFH_DFAINT(IMAGE,XDIM,YDIM,SCRTCH,BLNKVL,NINTS,STATUS)
*    Parameters :
*     IMAGE(XDIM,YDIM) = REAL
*           This array contains the original image.
*     XDIM = INTEGER
*           The x dimension of the image.
*     YDIM = INTEGER
*           The y dimension of the image.
*     SCRTCH(XDIM,YDIM) = INTEGER
*           The array into which the scaled image is put.
*     BLNKVL = REAL
*           This variable holds either a DATA_BLANK value
*           or the value of -32767.
*     NINTS = INTEGER
*           The number of greyscale intensities available on
*           the chosen device.
*     STATUS = INTEGER
*           Value of the status on entry.
*    Method :
*     First the mean and standard deviation calculated from:
*     Mean = E(X)
*     Standard Deviation = SQRT(E(X*X)-E(X)*E(X))
*     The data is then scaled to the range 0 to 255 using
*     the equation:
*     NEW X = MIN ( MAX ( ( X - ( MEAN - S.D. ) ) * 255.0 /
*             ( ( MEAN + 7*S.D. ) - ( MEAN - S.D. ) ) ,
*             0 ) , 255 )
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
      REAL BLNKVL                       ! Value of DATA_BLANK
*                                       ! or the default value
*                                       ! of -32767.
      INTEGER XDIM			! X dimension of the
*					! image.
      INTEGER YDIM			! Y dimension of the
*					! image.
      INTEGER I				! General variable.
      REAL IMAGE(XDIM,YDIM)		! Image data.
      INTEGER J				! General variable.
      INTEGER K				! General variable.
      REAL LOWER			! Lower limit used in
*					! scaling.
      REAL MEAN				! Mean value of data.
      INTEGER NINTS                     ! The number of intensities
*                                       ! available on the chosen
*                                       ! device.
      INTEGER SCRTCH(XDIM,YDIM)		! Area into which the
*					! scaled image is put.
      REAL SIGMA			! Standard deviation of
*					! the image data.
      REAL SX				! Sum of the image data
*					! elements.
      REAL SXX				! Sum of the squares
*					! of the image data
*					! elements.
      REAL UPPER			! Upper limit used for
*					! scaling.
      INTEGER VALCNT                    ! Count of valid values
*                                       ! of data elements in
*                                       ! the image i.e. those
*                                       ! values which are not
*                                       ! equal to BLNKVL.
*-

*
*    If the status on entry of this subroutine is bad, then
*    return to the calling program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Calculate the sum of the data elements and of the squares
*       of the data elements in the image,excluding the invalid
*       elements (BLNKVL).
*

         SX = 0.0
         SXX = 0.0
         VALCNT = 0

         DO I = 1,YDIM,1
            DO J = 1,XDIM,1
               IF (IMAGE(J,I).NE.BLNKVL) THEN
                  SX = SX+IMAGE(J,I)
                  SXX = SXX+IMAGE(J,I)*IMAGE(J,I)
                  VALCNT = VALCNT + 1
               ENDIF
            ENDDO
         ENDDO

*
*       Calculate the mean and standard deviation of the image.
*

         MEAN = SX/REAL(VALCNT)

         SIGMA = SQRT(SXX/REAL(VALCNT)-MEAN*MEAN)

*
*       Calculate the upper and lower limits between which the
*       image will be scaled.
*

         LOWER = MEAN-SIGMA
         CALL PAR_DEF0R('LOW',LOWER,STATUS)
         CALL PAR_PUT0R('LOW',LOWER,STATUS)

         UPPER = MEAN+7.0*SIGMA
         CALL PAR_DEF0R('HIGH',UPPER,STATUS)
         CALL PAR_PUT0R('HIGH',UPPER,STATUS)

*
*       Scale each element of the image and store it in the
*       scratch area in such a way that the scaled image is
*       inverted.
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
