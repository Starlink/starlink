

      SUBROUTINE HIS1_PARA(ADEV,SMOBAR,BARSIZ,LOW,BINWID,
     :                     MODE,SDEV,PEAKV,STATUS)
*+
*  Name:
*     HIS1_PARA

*  Purpose:
*     Estimate histogram mode by parabolic fitting of the histogram peak.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_PARA(ADEV,SMOBAR,BARSIZ,LOW,BINWID,
*                    MODE,SDEV,PEAKV,STATUS)

*  Description:
*     Logarithmically transforms the values of the smoothed
*     histogram array (SMOBAR) and then 'fits' a parabola to the
*     points near to the peak. The fitting is carried out by routine
*     HIS1_GAUJO but the data is passed in array VECTOR and
*     preprocessed (to reduce the memory requirement) array INPMAT.
*
*     The coefficients for the parabola are used to determine the
*     mode, standard deviation and height of the histogram peak.
*

*  Arguments:
*     ADEV = DOUBLE PRECISION (Given)
*        Absolute deviation of the image data. Units counts.
*     SMOBAR(BARSIZ) = DOUBLE PRECISION (Given)
*        The smoothed binning array.
*     BARSIZ = INTEGER (Given)
*        Size (no. of elements) of the binning array used.
*     LOW = REAL (Given)
*        Lowest count value in the image. Also used as an array
*        offset index for the arrays SMOBAR and BARRAY. Units counts.
*     BINWID = REAL (Given)
*        Width of each binning array element. Units counts.
*     MODE(4) = DOUBLE PRECISION (Given and Returned)
*        Estimated modal values for the image data. Units counts.
*     SDEV(2) = DOUBLE PRECISION (Given and Returned)
*        Standard deviations for the image pixel count distribution
*        and background count standard deviation. Units counts.
*     PEAKV(3) = DOUBLE PRECISION (Given and Returned)
*        Esimates of the peak number of pixels found with a given count
*        value in the count value versus occurence histogram.
*        Units pixels.
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
      INTEGER BARSIZ                  ! Size of the binning arrays used
      REAL BINWID                     ! Width of each binning array
                                      ! elements
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION ADEV           ! Absolute deviation of image data
      DOUBLE PRECISION SMOBAR(BARSIZ) ! Smoothed bin array

*  Arguments Given and Returned:
      DOUBLE PRECISION MODE(4)        ! Mode values for the image data
      DOUBLE PRECISION SDEV(2)        ! Background standard deviation
      DOUBLE PRECISION PEAKV(3)       ! Highest value found in the
                                      ! histogram array
*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Temporary loop variable
      INTEGER J                       ! Temporary loop variable
      INTEGER K                       ! Temporary loop variable
      INTEGER L                       ! Temporary loop variable
      REAL INPMAT(3,3)                ! Matrix array passed to
                                      ! subroutine HIS1_GAUJO
      REAL VECTOR(3)                  ! Vector array in which parabola
                                      ! coefficients are returned from
                                      ! subroutine HIS1_GAUJO
      REAL DETERM                     ! Inverted matrix determinant
                                      ! (used to indicate failure)
      REAL RANGE                      ! Range of histogram elements over
                                      ! which the parabolic interpolation
                                      ! will be applied
      REAL VALUE                      ! Temporary value
      REAL XX(3)                      ! Temporary array

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Model the part of the smoothed histogram within approx. +-1 absolute
*   deviations of the estimated mode value and use parabolic
*   interpolation to obtain another value for mode. This is possible
*   since applying a logarithmic transform to a Gaussian distribution
*   makes it become a parabola.

*   Set the initial width for the part of the histogram to be used for
*   fitting a parabola.
      RANGE=ADEV/BINWID+1.0
      IF (RANGE.LT.3.0) RANGE=3.0

*   Perform the matrix inversion. If it fails and the number of data
*   points is not too big, increase the number of data points
*   included and try again.
      DETERM=0.0
      DO WHILE (((ABS(DETERM).LT.HIS__VSMAL).
     :            OR.(VECTOR(3).GE.0.0)).AND.
     :               (RANGE.LE.2.0*ADEV/BINWID))

*      Clear the arrays to be used.
         DO 450 I=1,3
            VECTOR(I)=0.0
            DO 440 J=1,3
               INPMAT(J,I)=0.0
 440        CONTINUE
 450     CONTINUE

*      Use only data points near the current mode estimate.
*      Ensure that more points are taken from the low side of the
*      Gaussian curve.
         L=(MODE(2)-LOW)/BINWID+1
         DO 480 I=NINT(-RANGE*1.5),NINT(RANGE)

*         Define the array index.
            J=L+I

*         Avoid looking at data points that are beyond the array bounds.
            IF ((J.GE.1).AND.(J.LE.BARSIZ)) THEN

*            Avoid taking the log. of zero and avoid using
*            distribution outliers.
               IF ((SMOBAR(J).GT.HIS__VSMAL).AND.
     :                       (SMOBAR(J).GT.PEAKV(2)*0.2)) THEN

*               Prepare matrix coefficients for inversion.
                  VALUE=SMOBAR(J)
                  VALUE=ALOG(VALUE)
                  XX(1)=1.0
                  XX(2)=REAL(I)
                  XX(3)=XX(2)*XX(2)

                  DO 470 J=1,3
                     VECTOR(J)=VECTOR(J)+XX(J)*VALUE
                     DO 460 K=1,3
                        INPMAT(K,J)=INPMAT(K,J)+XX(K)*XX(J)
 460                 CONTINUE
 470              CONTINUE
               END IF
            END IF
 480     CONTINUE

*      If sufficient data points are available, perform the matrix
*      inversion.
         IF (INPMAT(1,1).GT.2.0) THEN
            CALL HIS1_GAUJO(VECTOR,INPMAT,DETERM,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999
         END IF

*      Increase the range of points to be used.
         RANGE=RANGE*1.05

      END DO

*   Assign interpolated mode, peak and std dev values.
      IF ((RANGE.LE.2.*ADEV/BINWID).AND.(ABS(DETERM).GT.HIS__VSMAL)
     :     .AND.(VECTOR(3).NE.0.0)) THEN
         MODE(4)=MODE(2)-VECTOR(2)/2./VECTOR(3)*BINWID
         SDEV(2)=SQRT(-1./VECTOR(3)/2.)*BINWID
         PEAKV(3)=EXP(VECTOR(1)-(VECTOR(2)/2.)**2/VECTOR(3))
      ELSE
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','A sensible interpolated value was'//
     :                 ' not available.',STATUS)
        GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE HSB1_PARA(ADEV,SMOBAR,BARSIZ,LOW,BINWID,
     :                     MODE,SDEV,PEAKV,STATUS)
*+
*  Name:
*     HSB1_PARA

*  Purpose:
*     Estimate histogram mode by parabolic fitting of the histogram peak.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSB1_PARA(ADEV,SMOBAR,BARSIZ,LOW,BINWID,
*                    MODE,SDEV,PEAKV,STATUS)

*  Description:
*     Logarithmically transforms the values of the smoothed
*     histogram array (SMOBAR) and then 'fits' a parabola to the
*     points near to the peak. The fitting is carried out by routine
*     HSB1_GAUJO but the data is passed in array VECTOR and
*     preprocessed (to reduce the memory requirement) array INPMAT.
*
*     The coefficients for the parabola are used to determine the
*     mode, standard deviation and height of the histogram peak.
*

*  Arguments:
*     ADEV = DOUBLE PRECISION (Given)
*        Absolute deviation of the image data. Units counts.
*     SMOBAR(BARSIZ) = DOUBLE PRECISION (Given)
*        The smoothed binning array.
*     BARSIZ = INTEGER (Given)
*        Size (no. of elements) of the binning array used.
*     LOW = REAL (Given)
*        Lowest count value in the image. Also used as an array
*        offset index for the arrays SMOBAR and BARRAY. Units counts.
*     BINWID = REAL (Given)
*        Width of each binning array element. Units counts.
*     MODE(4) = DOUBLE PRECISION (Given and Returned)
*        Estimated modal values for the image data. Units counts.
*     SDEV(2) = DOUBLE PRECISION (Given and Returned)
*        Standard deviations for the image pixel count distribution
*        and the background count standard deviation. Units counts.
*     PEAKV(3) = DOUBLE PRECISION (Given and Returned)
*        Esimates of the peak number of pixels found with a given count
*        value in the count value versus occurence histogram.
*        Units pixels.
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
      INCLUDE 'HSB_PAR'               ! HSUB system variables

*  Arguments Given:
      INTEGER BARSIZ                  ! Size of the binning arrays used
      REAL BINWID                     ! Width of each binning array
                                      ! elements
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION ADEV           ! Absolute deviation of image data
      DOUBLE PRECISION SMOBAR(BARSIZ) ! Smoothed bin array

*  Arguments Given and Returned:
      DOUBLE PRECISION MODE(4)        ! Mode values for the image data
      DOUBLE PRECISION SDEV(2)        ! Standard deviation
      DOUBLE PRECISION PEAKV(3)       ! Highest value found in the
                                      ! histogram array
*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Temporary loop variable
      INTEGER J                       ! Temporary loop variable
      INTEGER K                       ! Temporary loop variable
      INTEGER L                       ! Temporary loop variable
      REAL INPMAT(3,3)                ! Matrix array passed to
                                      ! subroutine HSB1_GAUJO
      REAL VECTOR(3)                  ! Vector array in which parabola
                                      ! coefficients are returned from
                                      ! subroutine HSB1_GAUJO
      REAL DETERM                     ! Inverted matrix determinant
                                      ! (used to indicate failure)
      REAL RANGE                      ! Range of histogram elements over
                                      ! which the parabolic interpolation
                                      ! will be applied
      REAL VALUE                      ! Temporary value
      REAL XX(3)                      ! Temporary array

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Model the part of the smoothed histogram within approx. +-1 absolute
*   deviations of the estimated mode value and use parabolic
*   interpolation to obtain another value for mode. This is possible
*   since applying a logarithmic transform to a Gaussian distribution
*   makes it become a parabola.

*   Set the initial width for the part of the histogram to be used for
*   fitting a parabola.
      RANGE=ADEV/BINWID+1.0
      IF (RANGE.LT.3.0) RANGE=3.0

*   Perform the matrix inversion. If it fails and the number of data
*   points is not too big, increase the number of data points
*   included and try again.
      DETERM=0.0
      DO WHILE (((ABS(DETERM).LT.HSB__VSMAL).
     :            OR.(VECTOR(3).GE.0.0)).AND.
     :                (RANGE.LE.2.0*ADEV/BINWID))

*      Clear the arrays to be used.
         DO 450 I=1,3
            VECTOR(I)=0.0
            DO 440 J=1,3
               INPMAT(J,I)=0.0
 440        CONTINUE
 450     CONTINUE

*      Use only data points near the current mode estimate.
         L=(MODE(2)-LOW)/BINWID+1
         DO 480 I=NINT(-RANGE*1.5),NINT(RANGE)

*         Define the array index.
            J=L+I

*         Avoid looking at data points that are beyond the array bounds.
            IF ((J.GE.1).AND.(J.LE.BARSIZ)) THEN

*            Avoid taking the log. of zero and avoid using
*            distribution outliers.
               IF ((SMOBAR(J).GT.HSB__VSMAL).AND.
     :                       (SMOBAR(J).GT.PEAKV(2)*0.2)) THEN

*               Prepare matrix coefficients for inversion.
                  VALUE=SMOBAR(J)
                  VALUE=ALOG(VALUE)
                  XX(1)=1.0
                  XX(2)=REAL(I)
                  XX(3)=XX(2)*XX(2)

                  DO 470 J=1,3
                     VECTOR(J)=VECTOR(J)+XX(J)*VALUE
                     DO 460 K=1,3
                        INPMAT(K,J)=INPMAT(K,J)+XX(K)*XX(J)
 460                 CONTINUE
 470              CONTINUE
               END IF
            END IF
 480     CONTINUE

*      If sufficient data points are available, perform the matrix
*      inversion.
         IF (INPMAT(1,1).GT.2.0)
     :       CALL HSB1_GAUJO(INPMAT,STATUS,VECTOR,DETERM)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Increase the range of points to be used.
         RANGE=RANGE*1.05

      END DO

*   Assign interpolated mode, peak and standard deviation values.
      IF ((RANGE.LE.2.*ADEV/BINWID).AND.(ABS(DETERM).GT.HSB__VSMAL)
     :     .AND.(VECTOR(3).NE.0.0)) THEN
         MODE(4)=MODE(2)-VECTOR(2)/2./VECTOR(3)*BINWID
         SDEV(2)=SQRT(-1./VECTOR(3)/2.)*BINWID
         PEAKV(3)=EXP(VECTOR(1)-(VECTOR(2)/2.)**2/VECTOR(3))
      ELSE
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','No sensible interpolated mode.',STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE LOB1_PARA(ADEV,SMOBAR,BARSIZ,LOW,BINWID,
     :                     MODE,SDEV,PEAKV,STATUS)
*+
*  Name:
*     LOB1_PARA

*  Purpose:
*     Estimate histogram mode by parabolic fitting of the histogram peak.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOB1_PARA(ADEV,SMOBAR,BARSIZ,LOW,BINWID,
*                    MODE,SDEV,PEAKV,STATUS)

*  Description:
*     Logarithmically transforms the values of the smoothed
*     histogram array (SMOBAR) and then 'fits' a parabola to the
*     points near to the peak. The fitting is carried out by routine
*     LOB1_GAUJO but the data is passed in array VECTOR and
*     preprocessed (to reduce the memory requirement) array INPMAT.
*
*     The coefficients for the parabola are used to determine the
*     mode, standard deviation and height of the histogram peak.
*

*  Arguments:
*     ADEV = DOUBLE PRECISION (Given)
*        Absolute deviation of the image data. Units counts.
*     SMOBAR(BARSIZ) = DOUBLE PRECISION (Given)
*        The smoothed binning array.
*     BARSIZ = INTEGER (Given)
*        Size (no. of elements) of the binning array used.
*     LOW = REAL (Given)
*        Lowest count value in the image. Also used as an array
*        offset index for the arrays SMOBAR and BARRAY. Units counts.
*     BINWID = REAL (Given)
*        Width of each binning array element. Units counts.
*     MODE(4) = DOUBLE PRECISION (Given and Returned)
*        Estimated modal values for the image data. Units counts.
*     SDEV(2) = DOUBLE PRECISION (Given and Returned)
*        Standard deviations for the image pixel count distribution
*        and the background count standard deviation. Units counts.
*     PEAKV(3) = DOUBLE PRECISION (Given and Returned)
*        Esimates of the peak number of pixels found with a given count
*        value in the count value versus occurence histogram.
*        Units pixels.
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
      INTEGER BARSIZ                  ! Size of the binning arrays used
      REAL BINWID                     ! Width of each binning array
                                      ! elements
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION ADEV           ! Absolute deviation of image data
      DOUBLE PRECISION SMOBAR(BARSIZ) ! Smoothed bin array

*  Arguments Given and Returned:
      DOUBLE PRECISION MODE(4)        ! Mode values for the image data
      DOUBLE PRECISION SDEV(2)        ! Background standard deviation
      DOUBLE PRECISION PEAKV(3)       ! Highest value found in the
                                      ! histogram array
*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Temporary loop variable
      INTEGER J                       ! Temporary loop variable
      INTEGER K                       ! Temporary loop variable
      INTEGER L                       ! Temporary loop variable
      REAL INPMAT(3,3)                ! Matrix array passed to
                                      ! subroutine LOB1_GAUJO
      REAL VECTOR(3)                  ! Vector array in which parabola
                                      ! coefficients are returned from
                                      ! subroutine LOB1_GAUJO
      REAL DETERM                     ! Inverted matrix determinant
                                      ! (used to indicate failure)
      REAL RANGE                      ! Range of histogram elements over
                                      ! which the parabolic interpolation
                                      ! will be applied
      REAL VALUE                      ! Temporary value
      REAL XX(3)                      ! Temporary array

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Model the part of the smoothed histogram within approx. +-1 absolute
*   deviations of the estimated mode value and use parabolic
*   interpolation to obtain another value for mode. This is possible
*   since applying a logarithmic transform to a Gaussian distribution
*   makes it become a parabola.

*   Set the initial width for the part of the histogram to be used for
*   fitting a parabola.
      RANGE=ADEV/BINWID+1.0
      IF (RANGE.LT.3.0) RANGE=3.0

*   Perform the matrix inversion. If it fails and the number of data
*   points is not too big, increase the number of data points
*   included and try again.
      DETERM=0.0
      DO WHILE (((ABS(DETERM).LT.LOB__VSMAL).
     :            OR.(VECTOR(3).GE.0.0)).AND.
     :                  (RANGE.LE.2.0*ADEV/BINWID))

*      Clear the arrays to be used.
         DO 450 I=1,3
            VECTOR(I)=0.0
            DO 440 J=1,3
               INPMAT(J,I)=0.0
 440        CONTINUE
 450     CONTINUE

*      Use only data points near the current mode estimate.
         L=(MODE(2)-LOW)/BINWID+1
         DO 480 I=NINT(-RANGE*1.5),NINT(RANGE)

*         Define the array index.
            J=L+I

*         Avoid looking at data points that are beyond the array bounds.
            IF ((J.GE.1).AND.(J.LE.BARSIZ)) THEN

*            Avoid taking the log. of zero and avoid using
*            distribution outliers.
               IF ((SMOBAR(J).GT.LOB__VSMAL).AND.
     :                       (SMOBAR(J).GT.PEAKV(2)*0.2)) THEN

*               Prepare matrix coefficients for inversion.
                  VALUE=SMOBAR(J)
                  VALUE=ALOG(VALUE)
                  XX(1)=1.0
                  XX(2)=REAL(I)
                  XX(3)=XX(2)*XX(2)

                  DO 470 J=1,3
                     VECTOR(J)=VECTOR(J)+XX(J)*VALUE
                     DO 460 K=1,3
                        INPMAT(K,J)=INPMAT(K,J)+XX(K)*XX(J)
 460                 CONTINUE
 470              CONTINUE
               END IF
            END IF
 480     CONTINUE

*      If sufficient data points are available, perform the matrix
*      inversion.
         IF (INPMAT(1,1).GT.2.0)
     :       CALL LOB1_GAUJO(VECTOR,INPMAT,DETERM,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Increase the range of points to be used.
         RANGE=RANGE*1.05

      END DO

*   Assign interpolated mode, peak and standard deviation values.
      IF ((RANGE.LE.2.*ADEV/BINWID).AND.(ABS(DETERM).GT.LOB__VSMAL)
     :     .AND.(VECTOR(3).NE.0.0)) THEN
         MODE(4)=MODE(2)-VECTOR(2)/2./VECTOR(3)*BINWID
         SDEV(2)=SQRT(-1./VECTOR(3)/2.)*BINWID
         PEAKV(3)=EXP(VECTOR(1)-(VECTOR(2)/2.)**2/VECTOR(3))
         IF ((SDEV(2).LE.0.0).OR.(PEAKV(3).LE.0.0)) THEN
            CALL MSG_OUT(' ','WARNING!!!',STATUS)
            CALL MSG_OUT(' ','Bad interpolation results.',STATUS)
         END IF
      ELSE
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','No sensible interpolated mode.',STATUS)
      END IF

 9999 CONTINUE

      END
