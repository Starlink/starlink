*+  KFH_GNRMP - Generates a histogram of an image.
      SUBROUTINE KFH_GNRMP(IMAGE,DIMS,LO,HI,RAMP,STATUS)
*    Description :
*     This subroutine generates a histogram of the image,
*     between the limits LO and HI,
*     and the stores it in a rectangular array ready to
*     be displayed.
*    Invocation :
*     CALL KFH_GNRMP(%VAL(POINTER),DIMS,LO,HI,RAMP,STATUS)
*    Parameters :
*     POINTER = INTEGER
*        This pointer points to the image which is to used
*        to form the histogram.
*     DIMS(2) = INTEGER
*        This array contains the x and y dimensions of the
*        image.
*     LO = REAL
*        The lowest value to be included in the histogram
*     HI = REAL
*        The highest value to be included in the histogram.,
*     RAMP(256,160) = REAL
*        The array containing the histogram in a form that
*        can readily be displayed.
*     STATUS = INTEGER
*        The status value on entering this subroutine.
*    Method :
*     A histogram of the image is compiled, and then normalised
*     so that the largest entry is 160. The output array is set
*     up such that it consists of a series of coloumns, each
*     corresponding to a different pen, and whose length is
*     given by the value of the histogram.
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     K.F.Hartley (RGVAD::KFH)
*     A.P.Horsfield (RGVAD::KFH)
*    History :
*     22 July 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER DIMS(2)			! The dimensions of the
*					! image.
      REAL HI				! The lowest pixel value in the
*					! histogram.
      INTEGER HIST(256)			! The array containing
*					! the histogram.
      INTEGER I				! General variable.
      REAL IMAGE(DIMS(1),DIMS(2))	! The array containing
*					! the image.
      INTEGER J				! General variable.
      INTEGER K				! General variable.
      REAL LO				! The lowest value in the
*					! histogram.
      REAL MAXVAL			! The maximum value
*					! of an array.
      REAL MINVAL			! The minimum value
*					! of an array.
      INTEGER PEN			! The pen number to
*					! be given to a column
*					! of the output array.
      REAL RAMP(256,160)		! The array containing
*					! the histogram in a
*					! form that can be
*					! readily displayed.
*-

*
*    If the status is bad, then exit from this routine and return
*    to the calling program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       This version has been modified so that external limits
*       are imposed on the range of pixel values to be used
*       in the histogram, rather than finding the minimum and maximum
*       in the array.
*

         MAXVAL = HI
         MINVAL = LO

*
*       Clear the histogram array.
*

         DO I = 1,256,1
            HIST(I) = 0
         ENDDO

*
*       Build the histogram.
*

         DO J = 1,DIMS(2),1
            DO I = 1,DIMS(1),1

               K = INT((IMAGE(I,J)-MINVAL)/(MAXVAL-MINVAL)
     :           *256.0)+1

               IF (K.GE.1.AND.K.LE.256) THEN
                  HIST(K) = HIST(K) + 1
               END IF

            ENDDO
         ENDDO

*
*       Find the maximum value in the histogram.
*

         MAXVAL = HIST(1)

         DO I = 1,256,1
            IF (HIST(I).GT.MAXVAL) MAXVAL = HIST(I)
         ENDDO

*
*       Scale the histogram so that all entries lie in the range
*       1 to 160.
*

         DO I = 1,256,1
            HIST(I) = INT(REAL(HIST(I))*160.0/REAL(MAXVAL))+1
         ENDDO

*
*       Build the output array from the histogram, taking care
*       not to use special pens.
*

         DO I = 1,256,1

            IF (MOD(I-1,8).EQ.0) THEN
               PEN = I
            ELSE
               PEN = I-1
            ENDIF

            DO J =1,HIST(I),1
               RAMP(I,J) = PEN
            ENDDO

            IF (HIST(I).LT.160) THEN

               DO J = HIST(I)+1,160,1
                  RAMP(I,J) = 0
               ENDDO

            ENDIF

         ENDDO

      ENDIF

      END
