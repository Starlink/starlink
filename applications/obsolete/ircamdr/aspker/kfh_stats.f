
*+  KFH_STATS - Calculates the statistics of an image.
      SUBROUTINE KFH_STATS(IMAGE,XDIM,YDIM,X1,Y1,X2,Y2,STATUS)
*    Description :
*     This routine computes the key statistical parameters
*     of an image. Firstly , it determines the minimum and
*     maximum values of the image. Then the median , the
*     mode , the mean , the standard deviation , and the
*     standard deviations rejecting points more than three
*     sigma away from the mean and mode are calculated.
*     Also , a histogram is calculated for the determination
*     of the mode if one has not already been calculated (in
*     the option HIST). All these values are displayed to
*     the user.
*    Invocation :
*     CALL KFH_STATS(IMAGE,XDIM,YDIM,X1,Y1,X2,Y2,STATUS)
*    Parameters :
*     IMAGE(XDIM,YDIM) = REAL
*           This array holds the original image data.
*     XDIM = INTEGER
*           This is the X dimension of the image.
*     YDIM = INTEGER
*           This is the Y dimension of the image.
*     X1 = INTEGER
*           The lower X bound of the region of the image.
*     Y1 = INTEGER
*           The lower Y bound of the region of the image.
*     X2 = INTEGER
*           The upper X bound of the region of the image.
*     Y2 = INTEGER
*           The upper Y bound of the region of the image.
*     STATUS = INTEGER
*           The status value on entry to this subroutine.
*    Method :
*     The minimum and maximum values of the image are
*     first calculated and displayed to the user. If a
*     histogram has already been calculated ( from the
*     option HIST) then this is used to determine the
*     determine the mode of the image. If not , a
*     histogram is produced.
*     The median is found by transferring the image
*     data from a 2-D array to a 1-D array and then
*     sorting it. If the total number of elements is
*     odd , then the median will be the middle value
*     of the sorted array. If even , the median is the
*     arithmetic mean of the two centre values.
*     The mode is found by searching through the
*     histogram for the value with the highest frequency.
*     The mean and standard deviation are calculated
*     from :
*      MEAN = E(X)
*      S.D. = SQRT(E(X**2)-E(X)**2)
*     The standard deviations plus/minus 3 sigma away
*     from the mean or mode are determined by excluding
*     the values that lie outside -3 sigma<value<+3 sigma
*     and recalculating the standard deviation.
*     All the statistics of the image are displayed to
*     the user.
*    Authors :
*     S.Chan (RGVAD::KFH)
*    History :
*     13 September 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER XDIM                       ! X-dimension of the image.
      INTEGER YDIM                       ! Y-dimension of the image.
      INTEGER CNT                        ! Count of each element
*                                        ! in the region.
      DOUBLE PRECISION COPY(270000)      ! A 1-dimensional array holding
*                                        ! the image data elements ready
*                                        ! for sorting.
      INTEGER COUNT                      ! Holds the highest number
*                                        ! of elements.
      REAL ELEMENT                       ! Holds each element of the
*                                        ! region.
      INTEGER HDIM                       ! Half the total dimensions.
      INTEGER I                          ! General variable.
      INTEGER IFAIL                      ! Parameter used in the sorting
*                                        ! routine.
      REAL IMAGE(XDIM,YDIM)              ! Array containing the image
*                                        ! data.
      INTEGER J                          ! General variable.
      REAL MAXVAL                        ! The maximum value of the
*                                        ! image.
      REAL MEAN                          ! Mean value of the image data.
      REAL MEAN1                         ! The new mean.
      REAL MEANP                         ! The mean plus 3 sigma.
      REAL MEANM                         ! The mean minus 3 sigma.
      REAL MEDIAN                        ! The median of the image.
      REAL MINVAL                        ! The minimum value of the
*                                        ! image.
      REAL MODE                          ! The mode of the image.
      REAL MODEP                         ! The mode plus 3 sigma.
      REAL MODEM                         ! The mode minus 3 sigma.
      REAL MODSUM                        ! The new sum , excluding entries
*                                        ! more than 3 sigma away from the
*                                        ! mode.
      REAL MSUMSQ                        ! The new sum of squares ,
*                                        ! excluding values more than 3
*                                        ! sigma away from the mode.
      REAL NSUMSQ                        ! The new sum of squares ,
*                                        ! excluding values more than 3
*                                        ! sigma away from the mean.
      INTEGER NUM                        ! The new number of entries ,
*                                        ! excluding values more than 3
*                                        ! sigma away from the mean.
      INTEGER NUMB                       ! The new number of entries ,
*                                        ! excluding values more than 3
*                                        ! sigma away from the mode.
      REAL NWSUM                         ! The new sum , excluding entries
*                                        ! more than 3 sigma away from the
*                                        ! mean.
      REAL SIGMA                         ! Standard deviation about the
*                                        ! mean of the image data.
      REAL SIGMA1                        ! The new standard deviation
*                                        ! about the mean.
      REAL SIGMA2                        ! The new standard deviation
*                                        ! about the mode.
      REAL SIGMOD                        ! Standard deviation about the
*                                        ! mode of the image data.
      REAL SUM                           ! Sum of the image data
*                                        ! elements.
      REAL SUMSQ                         ! Sum of the squares of the
*                                        ! image data elements.
      INTEGER TEMPX                      ! Temporary location.
      INTEGER TEMPY                      ! Temporary location.
      INTEGER TOTDIM                     ! The total number of elements
*                                        ! in the region.
      REAL XO                            ! General variable.
      INTEGER X1                         ! Lower X bound of the region
*                                        ! of the image.
      INTEGER Y1                         ! Lower Y bound of the region
*                                        ! of the image.
      INTEGER X2                         ! Upper X bound of the region
*                                        ! of the image.
      INTEGER Y2                         ! Upper Y bound of the region
*                                        ! of the image.
*-

*
*    If status is bad, then return to the calling program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Re-arrange coordinates , if necessary.
*

         IF (X1.GT.X2) THEN

            TEMPX = X1
            X1 = X2
            X2 = TEMPX

         ENDIF

         IF (Y1.GT.Y2) THEN

            TEMPY = Y1
            Y1 = Y2
            Y2 = TEMPY

         ENDIF

*
*       Determine the minimum and maximum values of
*       the image.
*

         MAXVAL = IMAGE(X1,Y1)
         MINVAL = IMAGE(X1,Y1)

         DO I = Y1,Y2,1

            DO J = X1,X2,1

               IF (IMAGE(J,I).GT.MAXVAL) THEN

                  MAXVAL = IMAGE(J,I)

               ELSEIF (IMAGE(J,I).LT.MINVAL) THEN

                  MINVAL = IMAGE(J,I)

               ENDIF

            END DO

         END DO

*
*       Display the minimum value of the image.
*

         CALL MSG_OUT('LINE',' ',STATUS)
         CALL MSG_SETR('MINV',MINVAL)
         CALL MSG_OUT('LOVAL','   MINIMUM .............. ^MINV',
     :    STATUS)

*
*       Display the maximum value of the image.
*

         CALL MSG_SETR('MAXV',MAXVAL)
         CALL MSG_OUT('HIVAL','   MAXIMUM .............. ^MAXV',
     :    STATUS)

*
*       Calculate the median.
*

         TOTDIM = (X2-X1+1)*(Y2-Y1+1)

*
*       Copy the image data into a 1-dimensional array
*       ready for sorting.
*

         CALL KFH_IMCOPY(IMAGE,COPY,XDIM,YDIM,X1,Y1,X2,Y2)

*
*       Sort the 1-dimensional array of image data elements
*       into ascending order.
*

         IFAIL = 0
         CALL M01ANF(COPY,1,TOTDIM,IFAIL)

*
*       If the number of elements in the array is odd , then
*       the median will be the middle value of the sorted
*       array. Otherwise , the median will be the arithmetic
*       mean of the two middle elements.
*

         HDIM = TOTDIM/2

         IF ((HDIM*2).NE.TOTDIM) THEN

            MEDIAN = SNGL(COPY(HDIM+1))

         ELSE

            MEDIAN = SNGL((COPY(HDIM)+COPY(HDIM+1))/2)

         ENDIF

*
*       Display the median.
*

         CALL MSG_SETR('MEDIAN',MEDIAN)
         CALL MSG_OUT('MED','   MEDIAN ............... ^MEDIAN',
     :    STATUS)
*
*       Calculate the mode.
*

         CNT = 0
         COUNT = 0
         ELEMENT = SNGL(COPY(1))

         DO I = 1,TOTDIM

            IF (SNGL(COPY(I)).EQ.ELEMENT) THEN

               CNT = CNT + 1

            ELSE IF (CNT.GE.COUNT) THEN

               MODE = ELEMENT
               COUNT = CNT
               CNT = 1
               ELEMENT = SNGL(COPY(I))

            ELSE

               CNT = 1
               ELEMENT = SNGL(COPY(I))

            ENDIF

         END DO

         IF (CNT.GE.COUNT) THEN

            MODE = ELEMENT

         ENDIF


         IF (MODE.EQ.1) THEN

            CALL MSG_OUT('NOMODE','No mode exists',STATUS)

         ELSE

            CALL MSG_SETR('MODE',MODE)
            CALL MSG_OUT('MMODE','   MODE ................. ^MODE',
     :       STATUS)

         ENDIF

*
*       Calculate the sum and the sum of squares of the data
*       elements in the image.
*

         SUM = 0.0
         SUMSQ = 0.0

         DO I = Y1,Y2,1

            DO J = X1,X2,1

               SUM = SUM + IMAGE(J,I)
               SUMSQ = SUMSQ + (IMAGE(J,I)*IMAGE(J,I))

            END DO

         END DO

*
*       Calculate the mean.
*

         MEAN = SUM/REAL(TOTDIM)

*
*       Display the mean.
*

         CALL MSG_SETR('MEAN',MEAN)
         CALL MSG_OUT('MMEAN','   MEAN ................. ^MEAN',
     :    STATUS)

*
*       Standard deviation about the mean.
*

         XO = MEAN
         SIGMA = SQRT((SUMSQ - 2*SUM*XO + TOTDIM*XO*XO)/(TOTDIM))

*
*       Display standard deviation about the mean.
*

         CALL MSG_SETR('STDEV1',SIGMA)
         CALL MSG_OUT('SIGMA1','   S.D. about MEAN ...... ^STDEV1',
     :    STATUS)

*
*       Standard deviation about the mode.
*

         XO = MODE
         SIGMOD = SQRT((SUMSQ - 2*SUM*XO + TOTDIM*XO*XO)/(TOTDIM))

*
*       Display standard deviation about the mode.
*

         CALL MSG_SETR('STDEV2',SIGMOD)
         CALL MSG_OUT('SIGMA2','   S.D. about MODE ...... ^STDEV2',
     :    STATUS)

*
*       Calculating the standard deviations rejecting points more
*       than 3 sigma away from the mean and mode.
*

         MEANP = MEAN + 3*SIGMA
         MEANM = MEAN - 3*SIGMA
         NWSUM = 0.0
         NSUMSQ = 0.0
         NUM = 0

         MODEP = MODE + 3*SIGMOD
         MODEM = MODE - 3*SIGMOD
         MODSUM = 0.0
         MSUMSQ = 0.0
         NUMB = 0

         DO I = Y1,Y2,1

            DO J = X1,X2,1

               IF (IMAGE(J,I).GE.MEANM.AND.IMAGE(J,I).LE.MEANP)
     :            THEN

                  NWSUM = NWSUM + IMAGE(J,I)
                  NSUMSQ = NSUMSQ + (IMAGE(J,I)*IMAGE(J,I))
                  NUM = NUM +1

               ENDIF

               IF (IMAGE(J,I).GE.MODEM.AND.IMAGE(J,I).LE.MODEP)
     :            THEN

                  MODSUM = MODSUM + IMAGE(J,I)
                  MSUMSQ = MSUMSQ + (IMAGE(J,I)*IMAGE(J,I))
                  NUMB = NUMB + 1

               ENDIF

            END DO

         END DO

*
*       New standard deviation about the mean.
*

         MEAN1 = NWSUM/REAL(NUM)
         SIGMA1 = SQRT((NSUMSQ - 2*NWSUM*MEAN1 + TOTDIM*
     :    MEAN1*MEAN1)/(TOTDIM))

*
*       Display the new standard deviation about the mean.
*
         CALL MSG_SETR('SD1',SIGMA1)
         CALL MSG_OUT('SIG1','   New S.D. about MEAN .. ^SD1',
     :    STATUS)

*
*       Standard deviation about the mode.
*

         SIGMA2 = SQRT((MSUMSQ - 2*MODSUM*MODE + TOTDIM
     :    *MODE*MODE)/(TOTDIM))

*
*       Display the new standard deviation about the mode.
*

         CALL MSG_SETR('SD2',SIGMA2)
         CALL MSG_OUT('SIG2','   New S.D. about MODE .. ^SD2',
     :    STATUS)
         CALL MSG_OUT('LINE',' ',STATUS)

      ENDIF

      END
