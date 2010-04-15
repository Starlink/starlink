

      SUBROUTINE HIS1_MEDMO(ELEMS,ARRAY,POINT2,POINT3,BARSIZ,
     :                      BINWID,LOW,ADEV,SFACT,SEEHIS,
     :                      NUMBER,SDEV,BARRAY,SMOBAR,
     :                      MEDIAN,PEAKV,SFACTA,MODE,STATUS)
*+
*  Name:
*     HIS1_MEDMO

*  Purpose:
*     Creates a histogram array from the image array and finds its
*     mode.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_MEDMO(ELEMS,ARRAY,POINT2,POINT3,BARSIZ,
*                     BINWID,LOW,ADEV,SFACT,SEEHIS,
*                     NUMBER,SDEV,BARRAY,SMOBAR,
*                     MEDIAN,PEAKV,SFACTA,MODE,STATUS)

*  Description:
*     Places the values from the mapped image array into a binning
*     array. The array is used as a histogram. The routine then
*     determines values for the peak height, mode, median, and std dev
*     of the histogram.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image. Units pixels.
*     ARRAY(ELEMS)= REAL (Given)
*        Array containing the image data.
*     POINT2 = INTEGER (Given)
*        Memory pointer to the binning array.
*     POINT3 = INTEGER (Given)
*        Memory pointer to the smoothed binning array.
*     BARSIZ = INTEGER (Given)
*        Size (no. of elements) of the binning arrays used.
*     BINWID = REAL (Given)
*        The width (range of values) of pixel count that are stored
*        within each element of the binning histogram array. Units counts.
*     LOW = REAL (Given)
*        Lowest count value in the image. Also used as an array
*        offset index for the arrays SMOBAR and BARRAY. Units counts.
*     ADEV = DOUBLE PRECISION (Given)
*        Absolute deviation of the image pixels count distribution.
*        Units counts.
*     SFACT = INTEGER (Given)
*        Requested Gaussian filter radius. -1 if the filter radius
*        is to be selected for you, 0 if no smoothing is to be done
*        otherwise any number less than HIS__SFLIM (see include file)
*        may be used. Units counts.
*     SEEHIS = LOGICAL (Given)
*        Graphics histogram to be displayed flag.
*     NUMBER = INTEGER (Given)
*        The number of of non-bad pixels.Units pixels.
*     SDEV(2) = DOUBLE PRECISION (Given and Returned)
*        Standard deviation of the image pixel count distribution
*        and the background count standard deviation. Units counts.
*     BARRAY(BARSIZ) = DOUBLE PRECISION (Returned)
*        The binning array.
*     SMOBAR(BARSIZ) = DOUBLE PRECISION (Returned)
*        The smoothed binning array.
*     MEDIAN = DOUBLE PRECISION (Returned)
*        The estimated median value for the image pixel counts.
*        Units counts.
*     PEAKV(3) = DOUBLE PRECISION (Returned)
*        Esimates of the peak number of pixels found with a given count
*        value in the count value versus occurence histogram.
*        Units pixels.
*     SFACTA = INTEGER (Returned)
*        Gaussian filter radius actually employed when smoothing
*        the array (SMOBAR).
*     MODE(4) = DOUBLE PRECISION (Returned)
*        Estimated values for the mode value of the image pixel
*        count distribution. Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-May-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'HIS_PAR'               ! HISTPEAK system constants
      INCLUDE 'CNF_PAR'

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER BARSIZ                  ! Size of the binning arrays used
      INTEGER NUMBER                  ! The number of pixels to be used
      INTEGER POINT2                  ! Pointer to the binning array
      INTEGER POINT3                  ! Pointer to the smoothed bin array
      INTEGER SFACT                   ! Requested radius for the Gaussian
                                      ! filter used to smooth the
                                      ! histogram
      REAL ARRAY(ELEMS)               ! Array containing the image data
      REAL BINWID                     ! Width of the bins used to find
                                      ! median and mode (only differs
                                      ! from 1 when count range exceeds
                                      ! HIS__BINLI)
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION ADEV           ! Absolute deviation of data
      LOGICAL SEEHIS                  ! User graphics or not choice

*  Arguments Given and Returned:
      DOUBLE PRECISION SDEV(2)        ! Standard deviation of the data
                                      ! and the background count std dev

*  Arguments Returned:
      INTEGER SFACTA                  ! Radius of the Gaussian
                                      ! filter actually used to smooth
                                      ! the histogram
      DOUBLE PRECISION BARRAY(BARSIZ) ! Binning array for the pixel cnts
      DOUBLE PRECISION MEDIAN         ! Median value for the image
      DOUBLE PRECISION MODE(4)        ! Mode values for the image
      DOUBLE PRECISION PEAKV(3)       ! Highest value found in the
      DOUBLE PRECISION SMOBAR(BARSIZ) ! Smoothed binning array

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER HIIND                   ! Temporary store
      INTEGER I                       ! Temporary loop variable
      INTEGER INDEX                   ! Array element index in which to
                                      ! bin a given pixel count value
      INTEGER J                       ! Temporary loop variable
      INTEGER LOIND                   ! Temporary store
      INTEGER MODEC                   ! Bin array index corresponding
                                      ! to the bin array modal value
      INTEGER NUMDAT                  ! Number of data points in the
                                      ! array passed to
                                      ! subroutine HIS1_LINRE
      REAL CONS                       ! Constant term of linear
                                      ! relationship fitted by
                                      ! subroutine HIS1_LINRE
      REAL GRAD                       ! Gradient term of linear
                                      ! relationship fitted by
                                      ! subroutine HIS1_LINRE
      REAL HEIG(HIS__CHORM)           ! The values at which chords
                                      ! (slices) were taken through the
                                      ! histogram
      REAL VALUE1                     ! Temporary storage variable
      REAL X1(HIS__CHORM)             ! X value array passed to
                                      ! subroutine HIS1_LINRE
      REAL Y1(HIS__CHORM)             ! Y value array passed to
                                      ! subroutine HIS1_LINRE
      DOUBLE PRECISION HALF           ! Half the number of non-bad
                                      ! in the binning arrays
      DOUBLE PRECISION HIVAL          ! Highest value found in the bin
                                      ! array
      DOUBLE PRECISION LOVAL          ! Lowest value found in the bin
                                      ! array
      DOUBLE PRECISION SFTOT          ! Total of smoothing factors used
      DOUBLE PRECISION SMOFAC(-HIS__SFLIM:HIS__SFLIM)
                                      ! Smoothing factors for a Gaussian
                                      ! filter to smooth array BARRAY
      DOUBLE PRECISION SMOTOT         ! Total of values in the smoothed
                                      ! bin array SMOBAR
      DOUBLE PRECISION TOTAL          ! Sum of the bin array BARRAY
      DOUBLE PRECISION VALUE          ! Temporary storage variable

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Clear the contents of the BARRAY and SMOBAR.
      DO 11 I=1,BARSIZ
         SMOBAR(I)=0.0
         BARRAY(I)=0.0
 11   CONTINUE

*   Assign all non-bad pixels of the image
*   array to a binning array to allow the mode and median
*   to be calculated.
      DO 312 I=1,ELEMS

         VALUE1=ARRAY(I)
         IF (VALUE1.NE.VAL__BADR) THEN

*         Calculate which bin element an image pixel count must be
*         assigned to.
            INDEX=INT((VALUE1-LOW)/BINWID+1)

*         Increment the count in the appropriate bin.
            BARRAY(INDEX)=BARRAY(INDEX)+1.

         END IF
 312  CONTINUE

*   Look through the bin array to find the highest value therein.
*   This is taken as a simple first estimate of the mode value.
      MODEC=0
      LOVAL=0
      HIVAL=0
      DO 320 I=1,BARSIZ

*      Set HIVAL and MODEC as a new highest value has been found.
         IF (BARRAY(I).GT.HIVAL) THEN
            MODEC=I
            HIVAL=BARRAY(I)
         END IF

*      Reset LOVAL as a new lowest value has been found.
         IF (BARRAY(I).LT.LOVAL) LOVAL=BARRAY(I)

 320  CONTINUE

*   Assigned unsmoothed mode and peak value.
      MODE(1)=LOW+(REAL(MODEC)-1.)*BINWID
      PEAKV(1)=HIVAL

*   Sum the elements of the bin array and stop when the sum exceeds
*   half that of the number of non-bad pixels in the image.
      INDEX=0
      TOTAL=0.0
      HALF=REAL(NUMBER)/2.

*   Add bin array contents to the total until it exceeds HALF.
      DO WHILE ((TOTAL.LT.HALF).AND.(INDEX.LE.BARSIZ))
         INDEX=INDEX+1
         TOTAL=TOTAL+BARRAY(INDEX)
      END DO

*   Remove the contents of the last bin (the one that took TOTAL above
*   the value of HALF) and decrement INDEX so that it now points to
*   the array element before HALF was exceeded.
      TOTAL=TOTAL-BARRAY(INDEX)
      INDEX=INDEX-1

*   Use approximate interpolation to estimate the median position
*   given the contents of the bin in which it occurs.
      MEDIAN=LOW+(INDEX-1)*BINWID

      SFACTA=SFACT
      IF (SFACTA.EQ.-1) THEN

*      Use the absolute deviation as an upper limit for the smoothing
*      filter radius.
         SFACTA=NINT(ADEV/BINWID)

*      Look through the BARRAY to find if all the points within +-
*      SFACTA indices of the modal index have values greater than
*      20% of the highest value. Retains the first values from either
*      side of the mode that are not.
         IF (SFACTA.LT.1) SFACTA=1

*      Calculate an average value for the region of the BARRAY around the
*      largest value.
         VALUE=0.0
         J=0
         DO 329 I=MODEC-1,MODEC+1
            IF ((I.GE.1).AND.(I.LE.BARSIZ)) THEN
               VALUE=VALUE+BARRAY(I)
               J=J+1
            END IF
 329     CONTINUE
         VALUE=0.2*VALUE/REAL(J)

*      Look for the lower limit.
         LOIND=MODEC-SFACTA
         IF (LOIND.LT.1) LOIND=1
         DO 330 I=MODEC-SFACTA,MODEC-1
            IF ((I.GE.1).AND.(I.LE.BARSIZ)) THEN
               IF (BARRAY(I).LT.VALUE) LOIND=I
            END IF
 330     CONTINUE

*      Look for the upper limit.
         HIIND=MODEC+SFACTA
         IF (HIIND.GT.BARSIZ) HIIND=BARSIZ
         DO 331 I=MODEC+SFACTA,MODEC+1,-1
            IF ((I.GE.1).AND.(I.LE.BARSIZ)) THEN
               IF (BARRAY(I).LT.VALUE) HIIND=I
            END IF
 331     CONTINUE

*      Calculate the filter radius.
         SFACTA=NINT((HIIND-LOIND)/2.)

*      Impose an upper limit.
         IF (SFACTA.GT.HIS__SFLIM) SFACTA=HIS__SFLIM

      ELSE

*      Set the filter radius and impose an upper limit.
         SFACTA=SFACTA/BINWID
         IF (SFACTA.GT.HIS__SFLIM) SFACTA=HIS__SFLIM

      END IF

*   Calculate the weighting factors that should be applied to pixels
*   when using the Gaussian filter to smooth the histogram.
      IF (SFACTA.EQ.0) THEN

*      Only one point is to be included in the smoothing routine. ie
*      no smoothing to take place so the weighting factor for that
*      pixel is 1.0.
         SMOFAC(0)=1.0

      ELSE

*      Setup the weighting array.
         TOTAL=0.0
         DO 350 I=0,SFACTA
            SMOFAC(I)=1./SQRT(2.*HIS__PIVAL)/(SFACTA/3.)
            SMOFAC(I)=SMOFAC(I)*EXP(-.5*(REAL(I)/(SFACTA/3.))**2)
            SMOFAC(-I)=SMOFAC(I)
            IF (I.EQ.0) THEN
               TOTAL=TOTAL+SMOFAC(I)
            ELSE
               TOTAL=TOTAL+SMOFAC(I)*2.
            END IF
 350     CONTINUE

*      Modify the weighting factors so that the sum of them is unity.
         DO 360 I=-SFACTA,SFACTA
            SMOFAC(I)=SMOFAC(I)/TOTAL
 360     CONTINUE

      END IF

*   Smooth the BARRAY and put the new values into array SMOBAR.
*   Also determine the total of the SMOBAR array.
      SMOTOT=0.0
      DO 380 I=SFACTA+1,BARSIZ-SFACTA-1

*      Look at the histogram elements on either side of the
*      element being considered and calculate the contribution
*      from each.
         DO 370 J=-SFACTA,SFACTA

*         Accumulate each contribution.
            SMOBAR(I)=SMOBAR(I)+SMOFAC(J)*BARRAY(I+J)
 370     CONTINUE

*      Determine the total for the smoothed array.
         SMOTOT=SMOTOT+SMOBAR(I)
 380  CONTINUE

*   Smooth the data at the edges of the array.
*   Low value edge.
      DO 382 I=1,SFACTA

*      Look at the histogram elements on either side of the
*      element being considered and calculate the contribution
*      from each.
         SFTOT=0.0
         DO 381 J=-SFACTA,SFACTA

*         Only use values that are defined. i.e. within the array bounds.
            IF ((I+J.GE.1).AND.(I+J.LE.BARSIZ)) THEN
               SMOBAR(I)=SMOBAR(I)+SMOFAC(J)*BARRAY(I+J)
               SFTOT=SFTOT+SMOFAC(J)
            END IF
 381     CONTINUE

*      Modify smoothed value to approx. take into account
*      the missing data points.
         IF (SMOBAR(I).GT.0.0) SMOBAR(I)=SMOBAR(I)/SFTOT

*      Determine the total for the smoothed array.
         SMOTOT=SMOTOT+SMOBAR(I)
 382  CONTINUE

*   Smooth the data at the edges of the array.
*   high value edge.
      DO 384 I=BARSIZ-SFACTA,BARSIZ

*      Set initial value of the smoothed array element.
         SMOBAR(I)=0.0

*      Look at the histogram elements on either side of the
*      element being considered and calculate the contribution
*      from each.
         SFTOT=0.0
         DO 383 J=-SFACTA,SFACTA

*         Only use values that are defined. i.e. within the array bounds.
            IF ((I+J.GE.1).AND.(I+J.LE.BARSIZ)) THEN
               SMOBAR(I)=SMOBAR(I)+SMOFAC(J)*BARRAY(I+J)
               SFTOT=SFTOT+SMOFAC(J)
            END IF
 383     CONTINUE

*      Modify smoothed value to approx. take into account
*      the missing data points.
         IF (SMOBAR(I).GT.0.0) SMOBAR(I)=SMOBAR(I)/SFTOT

*      Determine the total for the smoothed array.
         SMOTOT=SMOTOT+SMOBAR(I)
 384  CONTINUE

*   Convert the SFACTA value used to show the radius of the smoothing
*   filter in terms of the actual array indices used (necessary only
*   when the difference between HIGH and LOW is greater
*   than HIS__BINLI.
      SFACTA=NINT(SFACTA*BINWID)


*   Search the array of smoothed values for its modal value and also
*   recalculate/estimate the value of the mode.
      MODEC=0
      LOVAL=VAL__MAXD
      HIVAL=VAL__MIND
      DO 390 I=1,BARSIZ

*      Reset HIVAL and MODEC as a new highest value has been found.
         IF (SMOBAR(I).GT.HIVAL) THEN
            MODEC=I
            HIVAL=SMOBAR(I)
         END IF

*      Reset LOVAL as a new lowest value has been found
         IF (SMOBAR(I).LT.LOVAL) LOVAL=SMOBAR(I)

 390  CONTINUE

*   Assigned smoothed mode value.
      MODE(2)=LOW+(REAL(MODEC)-1.)*BINWID
      PEAKV(2)=HIVAL

*   Sum the elements of the smoothed bin array and stop when the sum
*   exceeds half that of the number of non-bad pixels in the image.
      INDEX=0
      TOTAL=0.0
      HALF=SMOTOT/2.

*   Add bin array contents to the total until it exceeds HALF.
      DO WHILE ((TOTAL.LT.HALF).AND.(INDEX.LE.BARSIZ))
         INDEX=INDEX+1
         TOTAL=TOTAL+SMOBAR(INDEX)
      END DO

*   Remove the contents of the last bin (the one that took TOTAL above
*   the value of HALF) and decrement INDEX so that it now points to
*   the array element before HALF was exceeded.
      TOTAL=TOTAL-SMOBAR(INDEX)
      INDEX=INDEX-1

*   Use approximate interpolation to estimate the median position
*   given the contents of the bin in which it occurs.
      MEDIAN=LOW+(INDEX-1+(HALF-TOTAL)/SMOBAR(INDEX+1))*BINWID

*   Take chords through the histogram peak and get values for
*   histogram chord
      CALL HIS1_CHORD(HIVAL,LOVAL,MODEC,%VAL(CNF_PVAL(POINT3)),LOW,ADEV,
     :                BARSIZ,BINWID,NUMDAT,HEIG,X1,Y1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the linear relationship between histogram width
*   and value at which the width was determined. Extrapolate to zero
*   width (peak) and thereby estimate a mode value.
      IF (NUMDAT.GT.2) THEN
         CALL HIS1_LINRE(X1,Y1,NUMDAT,GRAD,CONS,STATUS)
         IF (NUMDAT.GT.2) MODE(3)=CONS
      END IF

*   Set up the data for matrix inversion to provide
*   an interpolated value for the mode.
      CALL HIS1_PARA(ADEV,%VAL(CNF_PVAL(POINT3)),BARSIZ,LOW,BINWID,
     :               MODE,SDEV,PEAKV,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Display histogram if user has opted for graphics.
      IF (SEEHIS) THEN
         CALL HIS1_GRAPH(ADEV,%VAL(CNF_PVAL(POINT2)),
     :                   PEAKV(1),LOW,MODE,MEDIAN,
     :                   SDEV,%VAL(CNF_PVAL(POINT3)),
     :                   BARSIZ,HEIG,X1,Y1,NUMDAT,
     :                   BINWID,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE HSB1_MEDMO(TYPE,ELEMS,ARRAY,POINT2,POINT3,BARSIZ,
     :                      BINWID,LOW,ADEV,SFACT,
     :                      NUMBER,STATUS,SDEV,BARRAY,SMOBAR,
     :                      MEDIAN,PEAKV,SFACTA,MODE)
*+
*  Name:
*     HSB1_MEDMO

*  Purpose:
*     Creates a histogram array from the image array and finds its
*     mode.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSB1_MEDMO(TYPE,ELEMS,ARRAY,POINT2,POINT3,BARSIZ,
*                     BINWID,LOW,ADEV,SFACT,
*                     NUMBER,STATUS,SDEV,BARRAY,SMOBAR,
*                     MEDIAN,PEAKV,SFACTA,MODE)

*  Description:
*     Places the values from the mapped image array into a binning
*     array. The array is used as a histogram. The routine then
*     determines values for the peak height, mode, median and standard
*     deviation of the histogram.

*  Arguments:
*     TYPE = INTEGER (Given)
*        Method to be used to calculate mode vale.
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image. Units pixels.
*     ARRAY(ELEMS)= REAL (Given)
*        Array containing the image data.
*     POINT2(1) = INTEGER (Given)
*        Memory pointer to the binning array.
*     POINT3(1) = INTEGER (Given)
*        Memory pointer to the smoothed binning array.
*     BARSIZ = INTEGER (Given)
*        Size (no. of elements) of the binning arrays used.
*     BINWID = REAL (Given)
*        The width (range of values) of pixel count that are stored
*        within each element of the binning histogram array. Units counts.
*     LOW = REAL (Given)
*        Lowest count value in the image. Also used as an array
*        offset index for the arrays SMOBAR and BARRAY. Units counts.
*     ADEV = DOUBLE PRECISION (Given)
*        Absolute deviation of the image pixels count distribution.
*        Units counts.
*     SFACT = INTEGER (Given)
*        Requested Gaussian filter radius. -1 if the filter radius
*        is to be selected for you, 0 if no smoothing is to be done
*        otherwise any number less than HSB__SFLIM (see include file)
*        may be used. Units counts.
*     NUMBER = INTEGER (Given)
*        The number of pixels that are non-bad. Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     SDEV(2) = DOUBLE PRECISION (Given and Returned)
*        Standard deviation of the image pixel count distribution
*        and the background count standard deviation. Units counts.
*     BARRAY(BARSIZ) = DOUBLE PRECISION (Returned)
*        The binning array.
*     SMOBAR(BARSIZ) = DOUBLE PRECISION (Returned)
*        The smoothed binning array.
*     MEDIAN = DOUBLE PRECISION (Returned)
*        The estimated median value for the image pixel counts.
*        Units counts.
*     PEAKV(3) = DOUBLE PRECISION (Returned)
*        Esimates of the peak number of pixels found with a given count
*        value in the count value versus occurence histogram.
*        Units pixels.
*     SFACTA = INTEGER (Returned)
*        Gaussian filter radius actually employed when smoothing
*        the array (SMOBAR).
*     MODE(4) = DOUBLE PRECISION (Returned)
*        Estimated values for the mode value of the image pixel
*        count distribution. Units counts.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-May-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'HSB_PAR'               ! HSUB system constants
      INCLUDE 'CNF_PAR'

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER BARSIZ                  ! Size of the binning arrays used
      INTEGER NUMBER                  ! The number of pixels to be used
      INTEGER POINT2(1)               ! Pointer to the binning array
      INTEGER POINT3(1)               ! Pointer to the smoothed bin array
      INTEGER SFACT                   ! Requested radius for the Gaussian
                                      ! filter used to smooth the
                                      ! histogram
      INTEGER TYPE                    ! Defines the method of calculation
                                      ! for mode to be chosen
      REAL ARRAY(ELEMS)               ! Array containing the image data
      REAL BINWID                     ! Width of the bins used to find
                                      ! median and mode (only differs
                                      ! from 1 when count range exceeds
                                      ! HSB__BINLI)
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION ADEV           ! Absolute deviation of data

*  Arguments Given and Returned:
      DOUBLE PRECISION SDEV(2)        ! Standard deviation

*  Arguments Returned:
      INTEGER SFACTA                  ! Radius of the Gaussian
                                      ! filter actually used to smooth
                                      ! the histogram
      DOUBLE PRECISION BARRAY(BARSIZ) ! Binning array for the pixel cnts
      DOUBLE PRECISION MEDIAN         ! Median value for the image
      DOUBLE PRECISION MODE(4)        ! Mode values for the image
      DOUBLE PRECISION PEAKV(3)       ! Highest value found in the
      DOUBLE PRECISION SMOBAR(BARSIZ) ! Smoothed binning array

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER HIIND                   ! Temporary store
      INTEGER I                       ! Temporary loop variable
      INTEGER INDEX                   ! Array element index in which to
                                      ! bin a given pixel count value
      INTEGER J                       ! Temporary loop variable
      INTEGER LOIND                   ! Temporary store
      INTEGER MODEC                   ! Bin array index corresponding
                                      ! to the bin array modal value
      INTEGER NUMDAT                  ! Number of data points in the
                                      ! array passed to
                                      ! subroutine HSB1_LINRE
      REAL CONS                       ! Constant term of linear
                                      ! relationship fitted by
                                      ! subroutine HSB1_LINRE
      REAL GRAD                       ! Gradient term of linear
                                      ! relationship fitted by
                                      ! subroutine HSB1_LINRE
      REAL HEIG(HSB__CHORM)           ! The values at which chords
                                      ! (slices) were taken through the
                                      ! histogram
      REAL VALUE1                     ! Temporary storage variable
      REAL X1(HSB__CHORM)             ! X value array passed to
                                      ! subroutine HSB1_LINRE
      REAL Y1(HSB__CHORM)             ! Y value array passed to
                                      ! subroutine HSB1_LINRE
      DOUBLE PRECISION HALF           ! Half the number of non-bad
                                      ! in the binning arrays
      DOUBLE PRECISION HIVAL          ! Highest value found in the bin
                                      ! array
      DOUBLE PRECISION LOVAL          ! Lowest value found in the bin
                                      ! array
      DOUBLE PRECISION SFTOT          ! Total of smoothing factors used
      DOUBLE PRECISION SMOFAC(-HSB__SFLIM:HSB__SFLIM)
                                      ! Smoothing factors for a Gaussian
                                      ! filter to smooth array BARRAY
      DOUBLE PRECISION SMOTOT         ! Total of values in the smoothed
                                      ! bin array SMOBAR
      DOUBLE PRECISION TOTAL          ! Sum of the bin array BARRAY
      DOUBLE PRECISION VALUE          ! Temporary storage variable

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Clear the contents of the BARRAY and SMOBAR.
      DO 11 I=1,BARSIZ
         SMOBAR(I)=0.0
         BARRAY(I)=0.0
 11   CONTINUE

*   Assign all non-bad pixels of the image
*   array to a binning array to allow the mode and median
*   to be calculated.
      DO 312 I=1,ELEMS
         VALUE1=ARRAY(I)
         IF (VALUE1.NE.VAL__BADR) THEN

*         Calculate which bin element an image pixel count must be
*         assigned to.
            INDEX=INT((VALUE1-LOW)/BINWID+1.)

*         Increment the count in the appropriate bin.
            BARRAY(INDEX)=BARRAY(INDEX)+1.
         END IF
 312  CONTINUE

*   Look through the bin array to find the highest value therein.
*   This is taken as a simple first estimate of the mode value.
      MODEC=0
      LOVAL=0
      HIVAL=0
      DO 320 I=1,BARSIZ

*      Set HIVAL and MODEC as a new highest value has been found.
         IF (BARRAY(I).GT.HIVAL) THEN
            MODEC=I
            HIVAL=BARRAY(I)
         END IF

*      Reset LOVAL as a new lowest value has been found.
         IF (BARRAY(I).LT.LOVAL) LOVAL=BARRAY(I)

 320  CONTINUE

*   Assigned unsmoothed mode and peak value.
      MODE(1)=LOW+(REAL(MODEC)-1.)*BINWID
      PEAKV(1)=HIVAL

*     Bypass the rest of this subroutine since the required mode
*     type has been derived.
      IF ((TYPE.LT.2).AND.(TYPE.NE.0)) GOTO 9999

*   Sum the elements of the bin array and stop when the sum exceeds
*   half that of the number of non-bad pixels in the image.
      INDEX=0
      TOTAL=0.0
      HALF=REAL(NUMBER)/2.

*   Add bin array contents to the total until it exceeds HALF.
      DO WHILE ((TOTAL.LT.HALF).AND.(INDEX.LE.BARSIZ))
         INDEX=INDEX+1
         TOTAL=TOTAL+BARRAY(INDEX)
      END DO

*   Remove the contents of the last bin (the one that took TOTAL above
*   the value of HALF) and decrement INDEX so that it now points to
*   the array element before HALF was exceeded.
      TOTAL=TOTAL-BARRAY(INDEX)
      INDEX=INDEX-1

*   Use approximate interpolation to estimate the median position
*   given the contents of the bin in which it occurs.
      MEDIAN=LOW+(INDEX-1)*BINWID

      SFACTA=SFACT
      IF (SFACTA.EQ.-1) THEN

*      Use the absolute deviation as an upper limit for the smoothing
*      filter radius.
         SFACTA=NINT(ADEV/BINWID)

*      Look through the BARRAY to find if all the points within +-
*      SFACTA indices of the modal index have values greater than
*      20% of the highest value. Retains the first values from either
*      side of the mode that are not.
         IF (SFACTA.LT.1) SFACTA=1

*      Calculate an average value for the region of the BARRAY around the
*      largest value.
         VALUE=0.0
         J=0
         DO 329 I=MODEC-1,MODEC+1
            IF ((I.GE.1).AND.(I.LE.BARSIZ)) THEN
               VALUE=VALUE+BARRAY(I)
               J=J+1
            END IF
 329     CONTINUE
         VALUE=0.2*VALUE/REAL(J)

*      look for the lower limit.
         LOIND=MODEC-SFACTA
         IF (LOIND.LT.1) LOIND=1
         DO 330 I=MODEC-SFACTA,MODEC-1
            IF ((I.GE.1).AND.(I.LE.BARSIZ)) THEN
               IF (BARRAY(I).LT.VALUE) LOIND=I
            END IF
 330     CONTINUE

*      Look for the upper limit.
         HIIND=MODEC+SFACTA
         IF (HIIND.GT.BARSIZ) HIIND=BARSIZ
         DO 331 I=MODEC+SFACTA,MODEC+1,-1
            IF ((I.GE.1).AND.(I.LE.BARSIZ)) THEN
               IF (BARRAY(I).LT.VALUE) HIIND=I
            END IF
 331     CONTINUE

*      Calculate the filter radius
         SFACTA=NINT((HIIND-LOIND)/2.)

*      Impose an upper limit.
         IF (SFACTA.GT.HSB__SFLIM) SFACTA=HSB__SFLIM

      ELSE

*      Set the filter radius and impose an upper limit.
         SFACTA=SFACTA/BINWID
         IF (SFACTA.GT.HSB__SFLIM) SFACTA=HSB__SFLIM

      END IF


*   Calculate the weighting factors that should be applied to pixels
*   when using the Gaussian filter to smooth the histogram.
      IF (SFACTA.EQ.0) THEN

*      Only one point is to be included in the smoothing routine. ie
*      no smoothing to take place so the weighting factor for that
*      pixel is 1.0.
         SMOFAC(0)=1.0

      ELSE

*      Setup the weighting array.
         TOTAL=0.0
         DO 350 I=0,SFACTA
            SMOFAC(I)=1./SQRT(2.*HSB__PIVAL)/(SFACTA/3.)
            SMOFAC(I)=SMOFAC(I)*EXP(-.5*(REAL(I)/(SFACTA/3.))**2)
            SMOFAC(-I)=SMOFAC(I)
            IF (I.EQ.0) THEN
               TOTAL=TOTAL+SMOFAC(I)
            ELSE
               TOTAL=TOTAL+SMOFAC(I)*2.
            END IF
 350     CONTINUE

*      Modify the weighting factors so that the sum of them is unity.
         DO 360 I=-SFACTA,SFACTA
            SMOFAC(I)=SMOFAC(I)/TOTAL
 360     CONTINUE

      END IF

*   Smooth the BARRAY and put the new values into array SMOBAR.
*   Also determine the total of the SMOBAR array.
      SMOTOT=0.0
      DO 380 I=SFACTA+1,BARSIZ-SFACTA-1

*      Look at the histogram elements on either side of the
*      element being considered and calculate the contribution
*      from each.
         DO 370 J=-SFACTA,SFACTA

*         Accumulate each contribution.
            SMOBAR(I)=SMOBAR(I)+SMOFAC(J)*BARRAY(I+J)
 370     CONTINUE

*      Determine the total for the smoothed array.
         SMOTOT=SMOTOT+SMOBAR(I)
 380  CONTINUE

*   Smooth the data at the edges of the array.
*   Low value edge.
      DO 382 I=1,SFACTA

*      Look at the histogram elements on either side of the
*      element being considered and calculate the contribution
*      from each.
         SFTOT=0.0
         DO 381 J=-SFACTA,SFACTA

*         Only use values that are defined. i.e. within the array bounds.
            IF ((I+J.GE.1).AND.(I+J.LE.BARSIZ)) THEN
               SMOBAR(I)=SMOBAR(I)+SMOFAC(J)*BARRAY(I+J)
               SFTOT=SFTOT+SMOFAC(J)
            END IF
 381     CONTINUE

*      Modify smoothed value to approx. take into account
*      the missing data points.
         IF (SMOBAR(I).GT.0.0) SMOBAR(I)=SMOBAR(I)/SFTOT

*      Determine the total for the smoothed array.
         SMOTOT=SMOTOT+SMOBAR(I)
 382  CONTINUE

*   Smooth the data at the edges of the array.
*   high value edge.
      DO 384 I=BARSIZ-SFACTA,BARSIZ

*      Set initial value of the smoothed array element.
         SMOBAR(I)=0.0

*      Look at the histogram elements on either side of the
*      element being considered and calculate the contribution
*      from each.
         SFTOT=0.0
         DO 383 J=-SFACTA,SFACTA

*         Only use values that are defined. i.e. within the array bounds.
            IF ((I+J.GE.1).AND.(I+J.LE.BARSIZ)) THEN
               SMOBAR(I)=SMOBAR(I)+SMOFAC(J)*BARRAY(I+J)
               SFTOT=SFTOT+SMOFAC(J)
            END IF
 383     CONTINUE

*      Modify smoothed value to approx. take into account
*      the missing data points.
         IF (SMOBAR(I).GT.0.0) SMOBAR(I)=SMOBAR(I)/SFTOT

*      Determine the total for the smoothed array.
         SMOTOT=SMOTOT+SMOBAR(I)
 384  CONTINUE

*   Convert the SFACTA value used to show the radius of the smoothing
*   filter in terms of the actual array indices used (necessary only
*   when the difference between HIGH and LOW is greater
*   than HSB__BINLI.
      SFACTA=NINT(SFACTA*BINWID)

*   Search the array of smoothed values for its modal value and also
*   recalculate/estimate the value of the mode.
      MODEC=0
      LOVAL=VAL__MAXD
      HIVAL=VAL__MIND
      DO 390 I=1,BARSIZ

*      Reset HIVAL and MODEC as a new highest value has been found.
         IF (SMOBAR(I).GT.HIVAL) THEN
            MODEC=I
            HIVAL=SMOBAR(I)
         END IF

*      Reset LOVAL as a new lowest value has been found
         IF (SMOBAR(I).LT.LOVAL) LOVAL=SMOBAR(I)

 390  CONTINUE

*   Assigned smoothed mode value.
      MODE(2)=LOW+(REAL(MODEC)-1.)*BINWID
      PEAKV(2)=HIVAL

*     Bypass the rest of this subroutine since the required mode
*     type has been derived.
      IF ((TYPE.LT.3).AND.(TYPE.NE.0)) GOTO 9999

*   Sum the elements of the smoothed bin array and stop when the sum
*   exceeds half that of the number of non-bad pixels in the image.
      INDEX=0
      TOTAL=0.0
      HALF=SMOTOT/2.

*   Add bin array contents to the total until it exceeds HALF.
      DO WHILE ((TOTAL.LT.HALF).AND.(INDEX.LE.BARSIZ))
         INDEX=INDEX+1
         TOTAL=TOTAL+SMOBAR(INDEX)
      END DO

*   Remove the contents of the last bin (the one that took TOTAL above
*   the value of HALF) and decrement INDEX so that it now points to
*   the array element before HALF was exceeded.
      TOTAL=TOTAL-SMOBAR(INDEX)
      INDEX=INDEX-1

*   Use approximate interpolation to estimate the median position
*   given the contents of the bin in which it occurs.
      MEDIAN=LOW+(INDEX-1+(HALF-TOTAL)/SMOBAR(INDEX+1))*BINWID

*   Take chords through the histogram peak and get values for
*   histogram chord
      CALL HSB1_CHORD(HIVAL,LOVAL,MODEC,%VAL(CNF_PVAL(POINT3(1))),
     :                LOW,ADEV,
     :                BARSIZ,BINWID,STATUS,NUMDAT,HEIG,X1,Y1)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the linear relationship between histogram width
*   and value at which the width was determined. Extrapolate to zero
*   width (peak) and thereby estimate a mode value.
      IF (NUMDAT.GT.2) THEN
         CALL HSB1_LINRE(X1,Y1,NUMDAT,STATUS,GRAD,CONS)
         IF (NUMDAT.GT.2) MODE(3)=CONS
      END IF

*     Bypass the rest of this subroutine since the required mode
*     type has been derived.
      IF ((TYPE.LT.4).AND.(TYPE.NE.0)) GOTO 9999

*   Set up the data for matrix inversion to provide
*   an interpolated value for the mode.
      CALL HSB1_PARA(ADEV,%VAL(CNF_PVAL(POINT3(1))),BARSIZ,LOW,BINWID,
     :               MODE,SDEV,PEAKV,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

      END


      SUBROUTINE LOB1_MEDMO(ELEMS,ARRAY,POINT2,POINT3,BARSIZ,
     :                      BINWID,LOW,ADEV,SFACT,
     :                      NUMBER,SDEV,BARRAY,SMOBAR,
     :                      MEDIAN,PEAKV,SFACTA,MODE,STATUS)
*+
*  Name:
*     LOB1_MEDMO

*  Purpose:
*     Creates a histogram array from the image array and finds its
*     mode.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOB1_MEDMO(ELEMS,ARRAY,POINT2,POINT3,BARSIZ,
*                     BINWID,LOW,ADEV,SFACT,
*                     NUMBER,SDEV,BARRAY,SMOBAR,
*                     MEDIAN,PEAKV,SFACTA,MODE,STATUS)

*  Description:
*     Places the values from the mapped image array into a binning
*     array. The array is used as a histogram. The routine then
*     determines values for the peak height, mode, median and standard
*     deviation of the histogram.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image. Units pixels.
*     ARRAY(ELEMS)= REAL (Given)
*        Array containing the image data.
*     POINT2(1) = INTEGER (Given)
*        Memory pointer to the binning array.
*     POINT3(1) = INTEGER (Given)
*        Memory pointer to the smoothed binning array.
*     BARSIZ = INTEGER (Given)
*        Size (no. of elements) of the binning arrays used.
*     BINWID = REAL (Given)
*        The width (range of values) of pixel count that are stored
*        within each element of the binning histogram array. Units counts.
*     LOW = REAL (Given)
*        Lowest count value in the image. Also used as an array
*        offset index for the arrays SMOBAR and BARRAY. Units counts.
*     ADEV = DOUBLE PRECISION (Given)
*        Absolute deviation of the image pixels count distribution.
*        Units counts.
*     SFACT = INTEGER (Given)
*        Requested Gaussian filter radius. -1 if the filter radius
*        is to be selected for you, 0 if no smoothing is to be done
*        otherwise any number less than LOB__SFLIM (see include file)
*        may be used. Units counts.
*     NUMBER = INTEGER (Given)
*        The number of pixels that are non-bad. Units pixels.
*     SDEV(2) = DOUBLE PRECISION (Given and Returned)
*        Standard deviation of the image pixel count distribution
*        and the background standard deviation. Units counts.
*     BARRAY(BARSIZ) = DOUBLE PRECISION (Returned)
*        The binning array.
*     SMOBAR(BARSIZ) = DOUBLE PRECISION (Returned)
*        The smoothed binning array.
*     MEDIAN = DOUBLE PRECISION (Returned)
*        The estimated median value for the image pixel counts.
*        Units counts.
*     PEAKV(3) = DOUBLE PRECISION (Returned)
*        Esimates of the peak number of pixels found with a given count
*        value in the count value versus occurence histogram.
*        Units pixels.
*     SFACTA = INTEGER (Returned)
*        Gaussian filter radius actually employed when smoothing
*        the array (SMOBAR).
*     MODE(4) = DOUBLE PRECISION (Returned)
*        Estimated values for the mode value of the image pixel
*        count distribution. Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-May-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'LOB_PAR'               ! LOBACK system constants
      INCLUDE 'CNF_PAR'

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER BARSIZ                  ! Size of the binning arrays used
      INTEGER NUMBER                  ! The number of pixels to be used
      INTEGER POINT2(1)               ! Pointer to the binning array
      INTEGER POINT3(1)               ! Pointer to the smoothed bin array
      INTEGER SFACT                   ! Requested radius for the Gaussian
                                      ! filter used to smooth the
                                      ! histogram
      REAL ARRAY(ELEMS)               ! Array containing the image data
      REAL BINWID                     ! Width of the bins used to find
                                      ! median and mode (only differs
                                      ! from 1 when count range exceeds
                                      ! LOB__BINLI)
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION ADEV           ! Absolute deviation of data

*  Arguments Given and Returned:
      DOUBLE PRECISION SDEV(2)        ! Standard deviation and standard deviation
                                      ! of pixel count values

*  Arguments Returned:
      INTEGER SFACTA                  ! Radius of the Gaussian
                                      ! filter actually used to smooth
                                      ! the histogram
      DOUBLE PRECISION BARRAY(BARSIZ) ! Binning array for the pixel cnts
      DOUBLE PRECISION MEDIAN         ! Median value for the image
      DOUBLE PRECISION MODE(4)        ! Mode values for the image
      DOUBLE PRECISION PEAKV(3)       ! Highest value found in the
      DOUBLE PRECISION SMOBAR(BARSIZ) ! Smoothed binning array

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER HIIND                   ! Temporary store
      INTEGER I                       ! Temporary loop variable
      INTEGER INDEX                   ! Array element index in which to
                                      ! bin a given pixel count value
      INTEGER J                       ! Temporary loop variable
      INTEGER LOIND                   ! Temporary store
      INTEGER MODEC                   ! Bin array index corresponding
                                      ! to the bin array modal value
      INTEGER NUMDAT                  ! Number of data points in the
                                      ! array passed to
                                      ! subroutine LOB1_LINRE
      REAL CONS                       ! Constant term of linear
                                      ! relationship fitted by
                                      ! subroutine LOB1_LINRE
      REAL GRAD                       ! Gradient term of linear
                                      ! relationship fitted by
                                      ! subroutine LOB1_LINRE
      REAL HEIG(LOB__CHORM)           ! The values at which chords
                                      ! (slices) were taken through the
                                      ! histogram
      REAL VALUE1                     ! Temporary storage variable
      REAL X1(LOB__CHORM)             ! X value array passed to
                                      ! subroutine LOB1_LINRE
      REAL Y1(LOB__CHORM)             ! Y value array passed to
                                      ! subroutine LOB1_LINRE
      DOUBLE PRECISION HALF           ! Half the number of non-bad
                                      ! in the binning arrays
      DOUBLE PRECISION HIVAL          ! Highest value found in the bin
                                      ! array
      DOUBLE PRECISION LOVAL          ! Lowest value found in the bin
                                      ! array
      DOUBLE PRECISION SFTOT          ! Total of smoothing factors used
      DOUBLE PRECISION SMOFAC(-LOB__SFLIM:LOB__SFLIM)
                                      ! Smoothing factors for a Gaussian
                                      ! filter to smooth array BARRAY
      DOUBLE PRECISION SMOTOT         ! Total of values in the smoothed
                                      ! bin array SMOBAR
      DOUBLE PRECISION TOTAL          ! Sum of the bin array BARRAY
      DOUBLE PRECISION VALUE          ! Temporary storage variable

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Clear the contents of the BARRAY and SMOBAR.
      DO 11 I=1,BARSIZ
         SMOBAR(I)=0.0
         BARRAY(I)=0.0
 11   CONTINUE

*   Assign all non-bad pixels of the image
*   array to a binning array to allow the mode and median
*   to be calculated.
      DO 312 I=1,ELEMS
         VALUE1=ARRAY(I)
         IF (VALUE1.NE.VAL__BADR) THEN

*         Calculate which bin element an image pixel count must be
*         assigned to.
            INDEX=INT((VALUE1-LOW)/BINWID+1.)

*         Increment the count in the appropriate bin.
            BARRAY(INDEX)=BARRAY(INDEX)+1.
         END IF
 312  CONTINUE

*   Look through the bin array to find the highest value therein.
*   This is taken as a simple first estimate of the mode value.
      MODEC=0
      LOVAL=0
      HIVAL=0
      DO 320 I=1,BARSIZ

*      Set HIVAL and MODEC as a new highest value has been found.
         IF (BARRAY(I).GT.HIVAL) THEN
            MODEC=I
            HIVAL=BARRAY(I)
         END IF

*      Reset LOVAL as a new lowest value has been found.
         IF (BARRAY(I).LT.LOVAL) LOVAL=BARRAY(I)

 320  CONTINUE

*   Assigned unsmoothed mode and peak value.
      MODE(1)=LOW+(REAL(MODEC)-1.)*BINWID
      PEAKV(1)=HIVAL

*   Sum the elements of the bin array and stop when the sum exceeds
*   half that of the number of non-bad pixels in the image.
      INDEX=0
      TOTAL=0.0
      HALF=REAL(NUMBER)/2.

*   Add bin array contents to the total until it exceeds HALF.
      DO WHILE ((TOTAL.LT.HALF).AND.(INDEX.LE.BARSIZ))
         INDEX=INDEX+1
         TOTAL=TOTAL+BARRAY(INDEX)
      END DO

*   Remove the contents of the last bin (the one that took TOTAL above
*   the value of HALF) and decrement INDEX so that it now points to
*   the array element before HALF was exceeded.
      TOTAL=TOTAL-BARRAY(INDEX)
      INDEX=INDEX-1

*   Use approximate interpolation to estimate the median position
*   given the contents of the bin in which it occurs.
      MEDIAN=LOW+(INDEX-1)*BINWID

      SFACTA=SFACT
      IF (SFACTA.EQ.-1) THEN

*      Use the absolute deviation as an upper limit for the smoothing
*      filter radius.
         SFACTA=NINT(ADEV/BINWID)

*      Look through the BARRAY to find if all the points within +-
*      SFACTA indices of the modal index have values greater than
*      20% of the highest value. Retains the first values from either
*      side of the mode that are not.
         IF (SFACTA.LT.1) SFACTA=1

*      Calculate an average value for the region of the BARRAY around the
*      largest value.
         VALUE=0.0
         J=0
         DO 329 I=MODEC-1,MODEC+1
            IF ((I.GE.1).AND.(I.LE.BARSIZ)) THEN
               VALUE=VALUE+BARRAY(I)
               J=J+1
            END IF
 329     CONTINUE
         VALUE=0.2*VALUE/REAL(J)

*      look for the lower limit.
         LOIND=MODEC-SFACTA
         IF (LOIND.LT.1) LOIND=1
         DO 330 I=MODEC-SFACTA,MODEC-1
            IF ((I.GE.1).AND.(I.LE.BARSIZ)) THEN
               IF (BARRAY(I).LT.VALUE) LOIND=I
            END IF
 330     CONTINUE

*      Look for the upper limit.
         HIIND=MODEC+SFACTA
         IF (HIIND.GT.BARSIZ) HIIND=BARSIZ
         DO 331 I=MODEC+SFACTA,MODEC+1,-1
            IF ((I.GE.1).AND.(I.LE.BARSIZ)) THEN
               IF (BARRAY(I).LT.VALUE) HIIND=I
            END IF
 331     CONTINUE

*      Calculate the filter radius.
         SFACTA=NINT((HIIND-LOIND)/2.)

*      Impose an upper limit.
         IF (SFACTA.GT.LOB__SFLIM) SFACTA=LOB__SFLIM

      ELSE

*      Set the filter radius and impose an upper limit.
         SFACTA=SFACTA/BINWID
         IF (SFACTA.GT.LOB__SFLIM) SFACTA=LOB__SFLIM

      END IF


*   Calculate the weighting factors that should be applied to pixels
*   when using the Gaussian filter to smooth the histogram.
      IF (SFACTA.EQ.0) THEN

*      Only one point is to be included in the smoothing routine. ie
*      no smoothing to take place so the weighting factor for that
*      pixel is 1.0.
         SMOFAC(0)=1.0

      ELSE

*      Setup the weighting array.
         TOTAL=0.0
         DO 350 I=0,SFACTA
            SMOFAC(I)=1./SQRT(2.*LOB__PIVAL)/(SFACTA/3.)
            SMOFAC(I)=SMOFAC(I)*EXP(-.5*(REAL(I)/(SFACTA/3.))**2)
            SMOFAC(-I)=SMOFAC(I)
            IF (I.EQ.0) THEN
               TOTAL=TOTAL+SMOFAC(I)
            ELSE
               TOTAL=TOTAL+SMOFAC(I)*2.
            END IF
 350     CONTINUE

*      Modify the weighting factors so that the sum of them is unity.
         DO 360 I=-SFACTA,SFACTA
            SMOFAC(I)=SMOFAC(I)/TOTAL
 360     CONTINUE

      END IF

*   Smooth the BARRAY and put the new values into array SMOBAR.
*   Also determine the total of the SMOBAR array.
      SMOTOT=0.0
      DO 380 I=SFACTA+1,BARSIZ-SFACTA-1

*      Look at the histogram elements on either side of the
*      element being considered and calculate the contribution
*      from each.
         DO 370 J=-SFACTA,SFACTA

*         Accumulate each contribution.
            SMOBAR(I)=SMOBAR(I)+SMOFAC(J)*BARRAY(I+J)
 370     CONTINUE

*      Determine the total for the smoothed array.
         SMOTOT=SMOTOT+SMOBAR(I)
 380  CONTINUE

*   Smooth the data at the edges of the array.
*   Low value edge.
      DO 382 I=1,SFACTA

*      Look at the histogram elements on either side of the
*      element being considered and calculate the contribution
*      from each.
         SFTOT=0.0
         DO 381 J=-SFACTA,SFACTA

*         Only use values that are defined. i.e. within the array bounds.
            IF ((I+J.GE.1).AND.(I+J.LE.BARSIZ)) THEN
               SMOBAR(I)=SMOBAR(I)+SMOFAC(J)*BARRAY(I+J)
               SFTOT=SFTOT+SMOFAC(J)
            END IF
 381     CONTINUE

*      Modify smoothed value to approx. take into account
*      the missing data points.
         IF (SMOBAR(I).GT.0.0) SMOBAR(I)=SMOBAR(I)/SFTOT

*      Determine the total for the smoothed array.
         SMOTOT=SMOTOT+SMOBAR(I)
 382  CONTINUE

*   Smooth the data at the edges of the array.
*   high value edge.
      DO 384 I=BARSIZ-SFACTA,BARSIZ

*      Set initial value of the smoothed array element.
         SMOBAR(I)=0.0

*      Look at the histogram elements on either side of the
*      element being considered and calculate the contribution
*      from each.
         SFTOT=0.0
         DO 383 J=-SFACTA,SFACTA

*         Only use values that are defined. i.e. within the array bounds.
            IF ((I+J.GE.1).AND.(I+J.LE.BARSIZ)) THEN
               SMOBAR(I)=SMOBAR(I)+SMOFAC(J)*BARRAY(I+J)
               SFTOT=SFTOT+SMOFAC(J)
            END IF
 383     CONTINUE

*      Modify smoothed value to approx. take into account
*      the missing data points.
         IF (SMOBAR(I).GT.0.0) SMOBAR(I)=SMOBAR(I)/SFTOT

*      Determine the total for the smoothed array.
         SMOTOT=SMOTOT+SMOBAR(I)
 384  CONTINUE

*   Convert the SFACTA value used to show the radius of the smoothing
*   filter in terms of the actual array indices used (necessary only
*   when the difference between HIGH and LOW is greater
*   than LOB__BINLI.
      SFACTA=NINT(SFACTA*BINWID)

*   Search the array of smoothed values for its modal value and also
*   recalculate/estimate the value of the mode.
      MODEC=0
      LOVAL=VAL__MAXD
      HIVAL=VAL__MIND
      DO 390 I=1,BARSIZ

*      Reset HIVAL and MODEC as a new highest value has been found.
         IF (SMOBAR(I).GT.HIVAL) THEN
            MODEC=I
            HIVAL=SMOBAR(I)
         END IF

*      Reset LOVAL as a new lowest value has been found
         IF (SMOBAR(I).LT.LOVAL) LOVAL=SMOBAR(I)

 390  CONTINUE

*   Assigned smoothed mode value.
      MODE(2)=LOW+(REAL(MODEC)-1.)*BINWID
      PEAKV(2)=HIVAL

*   Sum the elements of the smoothed bin array and stop when the sum
*   exceeds half that of the number of non-bad pixels in the image.
      INDEX=0
      TOTAL=0.0
      HALF=SMOTOT/2.

*   Add bin array contents to the total until it exceeds HALF.
      DO WHILE ((TOTAL.LT.HALF).AND.(INDEX.LE.BARSIZ))
         INDEX=INDEX+1
         TOTAL=TOTAL+SMOBAR(INDEX)
      END DO

*   Remove the contents of the last bin (the one that took TOTAL above
*   the value of HALF) and decrement INDEX so that it now points to
*   the array element before HALF was exceeded.
      TOTAL=TOTAL-SMOBAR(INDEX)
      INDEX=INDEX-1

*   Use approximate interpolation to estimate the median position
*   given the contents of the bin in which it occurs.
      MEDIAN=LOW+(INDEX-1+(HALF-TOTAL)/SMOBAR(INDEX+1))*BINWID

*   Take chords through the histogram peak and get values for
*   histogram chord
      CALL LOB1_CHORD(HIVAL,LOVAL,MODEC,%VAL(CNF_PVAL(POINT3(1))),
     :                LOW,ADEV,
     :                BARSIZ,BINWID,NUMDAT,HEIG,X1,Y1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the linear relationship between histogram width
*   and value at which the width was determined. Extrapolate to zero
*   width (peak) and thereby estimate a mode value.
      IF (NUMDAT.GT.2) THEN
         CALL LOB1_LINRE(X1,Y1,NUMDAT,GRAD,CONS,STATUS)
         IF (NUMDAT.GT.2) MODE(3)=CONS
      END IF

*   Set up the data for matrix inversion to provide
*   an interpolated value for the mode.
      CALL LOB1_PARA(ADEV,%VAL(CNF_PVAL(POINT3(1))),BARSIZ,LOW,BINWID,
     :               MODE,SDEV,PEAKV,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

      END
