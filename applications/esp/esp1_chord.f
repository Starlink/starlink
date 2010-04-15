

      SUBROUTINE HIS1_CHORD(HIVAL,LOVAL,MODEC,SMOBAR,LOW,ADEV,
     :                      BARSIZ,BINWID,NUMDAT,HEIG,X1,Y1,STATUS)
*+
*  Name:
*     HIS1_CHORD

*  Purpose:
*     Estimate histogram mode by examining chords through peak.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_CHORD(HIVAL,LOVAL,MODEC,SMOBAR,LOW,ADEV,
*                     BARSIZ,BINWID,NUMDAT,HEIG,X1,Y1,STATUS)

*  Description:
*     Determines the length of chords through the histogram
*     peak at a variety of percentages of histogram heights.

*  Arguments:
*     HIVAL = DOUBLE PRECISION (Given)
*        Highest value found in the smoothed bin array.
*     LOVAL = DOUBLE PRECISION (Given)
*        Lowest value found in the smoothed bin array.
*     MODEC = INTEGER (Given)
*        Index of highest value in smoothed bin array.
*     SMOBAR(BARSIZ) = DOUBLE PRECISION (Given)
*        Smoothed bin array of image pixel counts.
*     LOW = REAL (Given)
*        Lowest count value in image. Used as an array index offset
*        for the SMOBAR and BARRAY arrays. Units counts.
*     ADEV = DOUBLE PRECISION (Given)
*        Absolute deviation of pixel values in the image. Units counts.
*     BARSIZ = INTEGER (Given)
*        Size (no. elements) of the binning arrays used.
*     BINWID = REAL (Given)
*        Width of each bin in the bin arrays. Units counts.
*     NUMDAT = INTEGER (Returned)
*        The number of legal histogram chords obtained.
*     HEIG(HIS__CHORM) = REAL (Returned)
*        The height at which the chord through the histogram occurs.
*     X1(HIS__CHORM) = REAL (Returned)
*        Length of chord through the histogram.
*     Y1(HIS__CHORM) = REAL (Returned)
*        Midpoint x index of chords through the histogram.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     8-June-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'HIS_PAR'               ! HISTPEAK system variables

*  Arguments Given:
      INTEGER BARSIZ                  ! Size of the binning arrays
      INTEGER MODEC                   ! Bin array index corresponding to
                                      ! the element containing HIVAL
      REAL BINWID                     ! Width of the bins used to find
                                      ! median and mode (only differs
                                      ! from 1 when the count range
                                      ! exceeds BINSIZ)
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION ADEV           ! Absolute deviation of image
                                      ! pixels
      DOUBLE PRECISION HIVAL          ! Highest value in smoothed bin
                                      ! array
      DOUBLE PRECISION LOVAL          ! Lowest value in smoothed bin
                                      ! array
      DOUBLE PRECISION SMOBAR(BARSIZ) ! Smoothed bin array

*  Arguments Returned:
      INTEGER NUMDAT                  ! Number of sections through
                                      ! histogram found
      REAL HEIG(HIS__CHORM)           ! The histogram values at which
                                      ! chords were taken through the
                                      ! histogram
      REAL X1(HIS__CHORM)             ! Length of the chord through
                                      ! the histogram
      REAL Y1(HIS__CHORM)             ! X index of midpoint of chord
                                      ! through the histogram

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Temporary loop variable
      INTEGER J                       ! Temporary loop variable
      INTEGER N1                      ! The number of possible values
                                      ! that were found for the index of
                                      ! one end of the chord through the
                                      ! histogram
      INTEGER N2                      ! Same as for N1 but the other end
      INTEGER S1                      ! The sign of the difference between
                                      ! the value of the previous histogram
                                      ! element and the value at which
                                      ! the chord is being taken
      INTEGER S2                      ! The sign of the difference between
                                      ! the value of the next histogram
                                      ! element and the value at which
                                      ! the chord is being taken
      INTEGER SLICE                   ! Temporary loop variable
      REAL AV1                        ! Average value for the chord
                                      ! (slice) start index on the left
                                      ! hand side of the histogram
      REAL AV2                        ! Same as AV1 but right hand side
      REAL HEIGHT                     ! Value at which the current chord
                                      ! (slice) through the histogram
                                      ! is taken
      REAL VALUE                      ! Temporary value

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Find the width of the smoothed histogram over a range of fractions
*   of the histogram mode value. This eventually provides a further
*   estimate for the location of the mode.
      NUMDAT=0
      DO 500 SLICE=2,38,2

*      Define the height at which the chord is taken as a decreasing
*      value somewhere between the highest value found in the histogram
*      and the lowest. The very top and bottom of the histogram are
*      excluded.
         HEIGHT=NINT((1.-REAL(SLICE)/100.)*(HIVAL-LOVAL)+LOVAL)

*      Search for the points on the left hand side of the histogram
*      peak where two histogram values are either side of the required
*      slice height.
         AV1=0.0
         N1=0
         J=2
         IF (J.LT.MODEC-ADEV) J=MODEC-ADEV
         DO 410 I=J,MODEC-1

*         Establish whether the smoothed histogram elements at index
*         I-1 and I+1 are bigger or smaller than the required
*         slice value and exclude them if they are zero.

            VALUE=SMOBAR(I-1)
            S1=INT(SIGN(1.,VALUE-HEIGHT))
            VALUE=SMOBAR(I+1)
            S2=INT(SIGN(1.,VALUE-HEIGHT))

*         If the elements at I-1 and I+1 are on either side of
*         the required slice value then store the index I.
*         If the histogram is noisy the final value will be an
*         average value.
            IF (S1*S2.EQ.-1) THEN
               N1=N1+1
               AV1=AV1+REAL(I)
            END IF
 410     CONTINUE

*      Search for the points on the right hand side of the histogram
*      peak where two histogram values are either side of the required
*      slice height.
         AV2=0.0
         N2=0
         J=BARSIZ-1
         IF (J.GT.MODEC+ADEV) J=MODEC+ADEV
         DO 420 I=MODEC+1,J

*         Establish whether the smoothed histogram elements at index
*         I-1 and I+1 are bigger or smaller than the required
*         slice value and exclude them if they are zero.

            VALUE=SMOBAR(I-1)
            S1=INT(SIGN(1.,VALUE-HEIGHT))
            VALUE=SMOBAR(I+1)
            S2=INT(SIGN(1.,VALUE-HEIGHT))

*         If the elements at I-1 and I+1 are on either side of
*         the required slice value then store the index I.
*         If the histogram is noisy the final value will be an
*         average value.
            IF (S1*S2.EQ.-1) THEN
               N2=N2+1
               AV2=AV2+REAL(I)
            END IF
 420     CONTINUE

*      Check to see if a legal (two ended) slice through the histogram
*      was found at the current height value.

         IF ((N1.GT.0).AND.(N2.GT.0).AND.
     :        (AV2/REAL(N2)-AV1/REAL(N1).GT.1.)) THEN

*         Use the current slice through the histogram if both ends
*         were found and are not adjacent.

*         Modify useful data points counter and store the histogram
*         height at which the slice was taken.

            NUMDAT=NUMDAT+1
            HEIG(NUMDAT)=HEIGHT

*         Store values of histogram width at various fractions of the
*         histogram mode count and also the approximate histogram centre
*         point at each width.
            X1(NUMDAT)=SQRT((AV2/REAL(N2)-AV1/REAL(N1))/2.*BINWID)
            Y1(NUMDAT)=LOW+((AV2/REAL(N2)+AV1/REAL(N1))/2.-1.)*BINWID

         END IF

 500  CONTINUE

*   Check that there are two or more data points.
      IF (NUMDAT.LT.2) THEN
        CALL MSG_OUT(' ','WARNING!!!',STATUS)
        CALL MSG_OUT(' ','There are less than two successful'//
     :                   ' slices through the histogram peak.'//
     :                   ' Consequently, the projected mean cannot'//
     :                   ' be found!',STATUS)
        GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE HSB1_CHORD(HIVAL,LOVAL,MODEC,SMOBAR,LOW,ADEV,
     :                      BARSIZ,BINWID,STATUS,NUMDAT,HEIG,X1,Y1)
*+
*  Name:
*     HSB1_CHORD

*  Purpose:
*     Estimate histogram mode by examining chords through peak.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSB1_CHORD(HIVAL,LOVAL,MODEC,SMOBAR,LOW,ADEV,
*                     BARSIZ,BINWID,STATUS,NUMDAT,HEIG,X1,Y1)

*  Description:
*     Determines the length of chords through the histogram
*     peak at a variety of percentages of histogram heights.

*  Arguments:
*     HIVAL = DOUBLE PRECISION (Given)
*        Highest value found in the smoothed bin array.
*     LOVAL = DOUBLE PRECISION (Given)
*        Lowest value found in the smoothed bin array.
*     MODEC = INTEGER (Given)
*        Index of highest value in smoothed bin array.
*     SMOBAR(BARSIZ) = DOUBLE PRECISION (Given)
*        Smoothed bin array of image pixel counts.
*     LOW = REAL (Given)
*        Lowest count value in image. Used as an array index offset
*        for the SMOBAR and BARRAY arrays. Units counts.
*     ADEV = DOUBLE PRECISION (Given)
*        Absolute deviation of pixel values in the image. Units counts.
*     BARSIZ = INTEGER (Given)
*        Size (no. elements) of the binning arrays used.
*     BINWID = REAL (Given)
*        Width of each bin in the bin arrays. Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     NUMDAT = INTEGER (Returned)
*        The number of legal histogram chords obtained.
*     HEIG(HSB__CHORM) = REAL (Returned)
*        The height at which the chord through the histogram occurs.
*     X1(HSB__CHORM) = REAL (Returned)
*        Length of chord through the histogram.
*     Y1(HSB__CHORM) = REAL (Returned)
*        Midpoint x index of chords through the histogram.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     8-June-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'HSB_PAR'               ! HSUB system variables

*  Arguments Given:
      INTEGER BARSIZ                  ! Size of the binning arrays
      INTEGER MODEC                   ! Bin array index corresponding to
                                      ! the element containing HIVAL
      REAL BINWID                     ! Width of the bins used to find
                                      ! median and mode (only differs
                                      ! from 1 when the count range
                                      ! exceeds BINSIZ)
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION ADEV           ! Absolute deviation of image
                                      ! pixels
      DOUBLE PRECISION HIVAL          ! Highest value in smoothed bin
                                      ! array
      DOUBLE PRECISION LOVAL          ! Lowest value in smoothed bin
                                      ! array
      DOUBLE PRECISION SMOBAR(BARSIZ) ! Smoothed bin array

*  Arguments Returned:
      INTEGER NUMDAT                  ! Number of sections through
                                      ! histogram found
      REAL HEIG(HSB__CHORM)           ! The histogram values at which
                                      ! chords were taken through the
                                      ! histogram
      REAL X1(HSB__CHORM)             ! Length of the chord through
                                      ! the histogram
      REAL Y1(HSB__CHORM)             ! X index of midpoint of chord
                                      ! through the histogram

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Temporary loop variable
      INTEGER J                       ! Temporary loop variable
      INTEGER N1                      ! The number of possible values
                                      ! that were found for the index of
                                      ! one end of the chord through the
                                      ! histogram
      INTEGER N2                      ! Same as for N1 but the other end
      INTEGER S1                      ! The sign of the difference between
                                      ! the value of the previous histogram
                                      ! element and the value at which
                                      ! the chord is being taken
      INTEGER S2                      ! The sign of the difference between
                                      ! the value of the next histogram
                                      ! element and the value at which
                                      ! the chord is being taken
      INTEGER SLICE                   ! Temporary loop variable
      REAL AV1                        ! Average value for the chord
                                      ! (slice) start index on the left
                                      ! hand side of the histogram
      REAL AV2                        ! Same as AV1 but right hand side
      REAL HEIGHT                     ! Value at which the current chord
                                      ! (slice) through the histogram
                                      ! is taken
      REAL VALUE                      ! Temporary value

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Find the width of the smoothed histogram over a range of fractions
*   of the histogram mode value. This eventually provides a further
*   estimate for the location of the mode.
      NUMDAT=0
      DO 500 SLICE=2,38,2

*      Define the height at which the chord is taken as a decreasing
*      value somewhere between the highest value found in the histogram
*      and the lowest. The very top and bottom of the histogram are
*      excluded.
         HEIGHT=NINT((1.-REAL(SLICE)/100.)*(HIVAL-LOVAL)+LOVAL)

*      Search for the points on the left hand side of the histogram
*      peak where two histogram values are either side of the required
*      slice height.
         AV1=0.0
         N1=0
         J=2
         IF (J.LT.MODEC-ADEV) J=MODEC-ADEV
         DO 410 I=J,MODEC-1

*         Establish whether the smoothed histogram elements at index
*         I-1 and I+1 are bigger or smaller than the required
*         slice value and exclude them if they are zero.

            VALUE=SMOBAR(I-1)
            S1=INT(SIGN(1.,VALUE-HEIGHT))
            VALUE=SMOBAR(I+1)
            S2=INT(SIGN(1.,VALUE-HEIGHT))

*         If the elements at I-1 and I+1 are on either side of
*         the required slice value then store the index I.
*         If the histogram is noisy the final value will be an
*         average value.
            IF (S1*S2.EQ.-1) THEN
               N1=N1+1
               AV1=AV1+REAL(I)
            END IF
 410     CONTINUE

*      Search for the points on the right hand side of the histogram
*      peak where two histogram values are either side of the required
*      slice height.
         AV2=0.0
         N2=0
         J=BARSIZ-1
         IF (J.GT.MODEC+ADEV) J=MODEC+ADEV
         DO 420 I=MODEC+1,J

*         Establish whether the smoothed histogram elements at index
*         I-1 and I+1 are bigger or smaller than the required
*         slice value and exclude them if they are zero.

            VALUE=SMOBAR(I-1)
            S1=INT(SIGN(1.,VALUE-HEIGHT))
            VALUE=SMOBAR(I+1)
            S2=INT(SIGN(1.,VALUE-HEIGHT))

*         If the elements at I-1 and I+1 are on either side of
*         the required slice value then store the index I.
*         If the histogram is noisy the final value will be an
*         average value.
            IF (S1*S2.EQ.-1) THEN
               N2=N2+1
               AV2=AV2+REAL(I)
            END IF
 420     CONTINUE

*      Check to see if a legal (two ended) slice through the histogram
*      was found at the current height value.

         IF ((N1.GT.0).AND.(N2.GT.0).AND.
     :        (AV2/REAL(N2)-AV1/REAL(N1).GT.1.)) THEN

*         Use the current slice through the histogram if both ends
*         were found and are not adjacent.

*         Modify useful data points counter and store the histogram
*         height at which the slice was taken.

            NUMDAT=NUMDAT+1
            HEIG(NUMDAT)=HEIGHT

*         Store values of histogram width at various fractions of the
*         histogram mode count and also the approximate histogram centre
*         point at each width.
            X1(NUMDAT)=SQRT((AV2/REAL(N2)-AV1/REAL(N1))/2.*BINWID)
            Y1(NUMDAT)=LOW+((AV2/REAL(N2)+AV1/REAL(N1))/2.-1.)*BINWID

         END IF

 500  CONTINUE

*   Check that there are two or more data points.
      IF (NUMDAT.LT.2) THEN
        CALL MSG_OUT(' ','WARNING!!!',STATUS)
        CALL MSG_OUT(' ','There are less than two successful'//
     :                   ' slices through the histogram peak.'//
     :                   ' Consequently, the projected mean cannot'//
     :                   ' be found!',STATUS)
        GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE LOB1_CHORD(HIVAL,LOVAL,MODEC,SMOBAR,LOW,ADEV,
     :                      BARSIZ,BINWID,NUMDAT,HEIG,X1,Y1,STATUS)
*+
*  Name:
*     LOB1_CHORD

*  Purpose:
*     Estimate histogram mode by examining chords through peak.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOB_CHORD(HIVAL,LOVAL,MODEC,SMOBAR,LOW,ADEV,
*                     BARSIZ,BINWID,NUMDAT,HEIG,X1,Y1,STATUS)

*  Description:
*     Determines the length of chords through the histogram
*     peak at a variety of percentages of histogram heights.

*  Arguments:
*     HIVAL = DOUBLE PRECISION (Given)
*        Highest value found in the smoothed bin array.
*     LOVAL = DOUBLE PRECISION (Given)
*        Lowest value found in the smoothed bin array.
*     MODEC = INTEGER (Given)
*        Index of highest value in smoothed bin array.
*     SMOBAR(BARSIZ) = DOUBLE PRECISION (Given)
*        Smoothed bin array of image pixel counts.
*     LOW = REAL (Given)
*        Lowest count value in image. Used as an array index offset
*        for the SMOBAR and BARRAY arrays. Units counts.
*     ADEV = DOUBLE PRECISION (Given)
*        Absolute deviation of pixel values in the image. Units counts.
*     BARSIZ = INTEGER (Given)
*        Size (no. elements) of the binning arrays used.
*     BINWID = REAL (Given)
*        Width of each bin in the bin arrays. Units counts.
*     NUMDAT = INTEGER (Returned)
*        The number of legal histogram chords obtained.
*     HEIG(LOB__CHORM) = REAL (Returned)
*        The height at which the chord through the histogram occurs.
*     X1(LOB__CHORM) = REAL (Returned)
*        Length of chord through the histogram.
*     Y1(LOB__CHORM) = REAL (Returned)
*        Midpoint x index of chords through the histogram.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     8-June-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'LOB_PAR'               ! LOBACK system variables

*  Arguments Given:
      INTEGER BARSIZ                  ! Size of the binning arrays
      INTEGER MODEC                   ! Bin array index corresponding to
                                      ! the element containing HIVAL
      REAL BINWID                     ! Width of the bins used to find
                                      ! median and mode (only differs
                                      ! from 1 when the count range
                                      ! exceeds BINSIZ)
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION ADEV           ! Absolute deviation of image
                                      ! pixels
      DOUBLE PRECISION HIVAL          ! Highest value in smoothed bin
                                      ! array
      DOUBLE PRECISION LOVAL          ! Lowest value in smoothed bin
                                      ! array
      DOUBLE PRECISION SMOBAR(BARSIZ) ! Smoothed bin array

*  Arguments Returned:
      INTEGER NUMDAT                  ! Number of sections through
                                      ! histogram found
      REAL HEIG(LOB__CHORM)           ! The histogram values at which
                                      ! chords were taken through the
                                      ! histogram
      REAL X1(LOB__CHORM)             ! Length of the chord through
                                      ! the histogram
      REAL Y1(LOB__CHORM)             ! X index of midpoint of chord
                                      ! through the histogram

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Temporary loop variable
      INTEGER J                       ! Temporary loop variable
      INTEGER N1                      ! The number of possible values
                                      ! that were found for the index of
                                      ! one end of the chord through the
                                      ! histogram
      INTEGER N2                      ! Same as for N1 but the other end
      INTEGER S1                      ! The sign of the difference between
                                      ! the value of the previous histogram
                                      ! element and the value at which
                                      ! the chord is being taken
      INTEGER S2                      ! The sign of the difference between
                                      ! the value of the next histogram
                                      ! element and the value at which
                                      ! the chord is being taken
      INTEGER SLICE                   ! Temporary loop variable
      REAL AV1                        ! Average value for the chord
                                      ! (slice) start index on the left
                                      ! hand side of the histogram
      REAL AV2                        ! Same as AV1 but right hand side
      REAL HEIGHT                     ! Value at which the current chord
                                      ! (slice) through the histogram
                                      ! is taken
      REAL VALUE                      ! Temporary value

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Find the width of the smoothed histogram over a range of fractions
*   of the histogram mode value. This eventually provides a further
*   estimate for the location of the mode.
      NUMDAT=0
      DO 500 SLICE=2,38,2

*      Define the height at which the chord is taken as a decreasing
*      value somewhere between the highest value found in the histogram
*      and the lowest. The very top and bottom of the histogram are
*      excluded.
         HEIGHT=NINT((1.-REAL(SLICE)/100.)*(HIVAL-LOVAL)+LOVAL)

*      Search for the points on the left hand side of the histogram
*      peak where two histogram values are either side of the required
*      slice height.
         AV1=0.0
         N1=0
         J=2
         IF (J.LT.MODEC-ADEV) J=MODEC-ADEV
         DO 410 I=J,MODEC-1

*         Establish whether the smoothed histogram elements at index
*         I-1 and I+1 are bigger or smaller than the required
*         slice value and exclude them if they are zero.

            VALUE=SMOBAR(I-1)
            S1=INT(SIGN(1.,VALUE-HEIGHT))
            VALUE=SMOBAR(I+1)
            S2=INT(SIGN(1.,VALUE-HEIGHT))

*         If the elements at I-1 and I+1 are on either side of
*         the required slice value then store the index I.
*         If the histogram is noisy the final value will be an
*         average value.
            IF (S1*S2.EQ.-1) THEN
               N1=N1+1
               AV1=AV1+REAL(I)
            END IF
 410     CONTINUE

*      Search for the points on the right hand side of the histogram
*      peak where two histogram values are either side of the required
*      slice height.
         AV2=0.0
         N2=0
         J=BARSIZ-1
         IF (J.GT.MODEC+ADEV) J=MODEC+ADEV
         DO 420 I=MODEC+1,J

*         Establish whether the smoothed histogram elements at index
*         I-1 and I+1 are bigger or smaller than the required
*         slice value and exclude them if they are zero.

            VALUE=SMOBAR(I-1)
            S1=INT(SIGN(1.,VALUE-HEIGHT))
            VALUE=SMOBAR(I+1)
            S2=INT(SIGN(1.,VALUE-HEIGHT))

*         If the elements at I-1 and I+1 are on either side of
*         the required slice value then store the index I.
*         If the histogram is noisy the final value will be an
*         average value.
            IF (S1*S2.EQ.-1) THEN
               N2=N2+1
               AV2=AV2+REAL(I)
            END IF
 420     CONTINUE

*      Check to see if a legal (two ended) slice through the histogram
*      was found at the current height value.

         IF ((N1.GT.0).AND.(N2.GT.0).AND.
     :        (AV2/REAL(N2)-AV1/REAL(N1).GT.1.)) THEN

*         Use the current slice through the histogram if both ends
*         were found and are not adjacent.

*         Modify useful data points counter and store the histogram
*         height at which the slice was taken.

            NUMDAT=NUMDAT+1
            HEIG(NUMDAT)=HEIGHT

*         Store values of histogram width at various fractions of the
*         histogram mode count and also the approximate histogram centre
*         point at each width.
            X1(NUMDAT)=SQRT((AV2/REAL(N2)-AV1/REAL(N1))/2.*BINWID)
            Y1(NUMDAT)=LOW+((AV2/REAL(N2)+AV1/REAL(N1))/2.-1.)*BINWID

         END IF

 500  CONTINUE

*   Check that there are two or more data points.
      IF (NUMDAT.LT.2) THEN
        CALL MSG_OUT(' ','WARNING!!!',STATUS)
        CALL MSG_OUT(' ','There are less than two successful'//
     :                   ' slices through the histogram peak.'//
     :                   ' Consequently, the projected mean cannot'//
     :                   ' be found!',STATUS)
        GOTO 9999
      END IF

 9999 CONTINUE

      END
