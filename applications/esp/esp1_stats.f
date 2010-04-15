

      SUBROUTINE ELF1_STATS(ELEMS,ARRP,XR,YR,NUMPOI,
     :                      PRANGE,USED,MEAN,SDP,RESDU,
     :                      VA,FOUND,STATUS)
*+
*  Name:
*     ELF1_STATS

*  Purpose:
*     Looks at the co-ordinates of the ellipse currently being considered and
*     determines what the image pixel value is associated with each image
*     location.
*
*     The values found are used to determine the standard deviation of
*     the ellipse points (ideally should be zero). This and the residual
*     value may be used to determine whether the current ellipse parameters
*     yield a better fit (lower resiudual) than those used previously.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_STATS(ELEMS,ARRP,XR,YR,NUMPOI,
*                     PRANGE,USED,MEAN,SDP,RESDU,VA,FOUND,STATUS

*  Description:
*     Using interpolation, the values associated with all the
*     ellipse fit points (contained in arrays XR and YR) are obtained.
*
*     These are then used to calculate an estimate of the mean pixel
*     count in the ellipse, the standard deviation thereof and some
*     weighted measure of the overall fit residuals.
*
*     An error message is returned if the values for too many points could
*     not be determined.
*

*  Arguments:
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     ARRP(1) = REAL (Given)
*        Pointer to the beginning of the image array.
*     XR(ELF__MXPOI) = REAL (Given)
*        X co-ordinates of the 'fit' ellipse points.
*     YR(ELF__MXPOI) = REAL (Given)
*        Y co-ordinates of the 'fit' ellipse points.
*     NUMPOI = INTEGER (Given)
*        Number of ellipse points for which co-ordinates have been calculated.
*     PRANGE(2) = INTEGER (Given)
*        Size of the image. Units pixels.
*     USED(ELF__MXPOI) = INTEGER (Returned)
*        Was a sensible value obtained for a given ellipse point via
*        interpolation?
*     MEAN = REAL (Returned)
*        Mean pixel value found around the ellipse. Units counts.
*     SDP = REAL (Returned)
*        Standard deviation of the MEAN. Units counts.
*     RESDU = REAL (Returned)
*        A measure of the variation in pixel value around the ellipse.
*     VA(ELF__MXPOI) = REAL (Returned)
*        The values of image pixel count at the x/y co-ordinates of
*        the ellipse points defined by arrays XR() and YR().
*     FOUND = INTEGER (Returned)
*        The number of ellipse points used.
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
      INCLUDE 'ELF_PAR'               ! ELLFOU constants
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ARRP(1)                 ! Image array pointer
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER NUMPOI                  ! Number of points on the ellipse
      INTEGER PRANGE(2)               ! Size of the image
      INTEGER USED(ELF__MXPOI)        ! Was an ellipse point used
      REAL XR(ELF__MXPOI)             ! X co-ord for the translated ellipse
      REAL YR(ELF__MXPOI)             ! Y co-ord for the translated ellipse

*  Arguments Returned:
      INTEGER FOUND                   ! Number of ellipse points for which
                                      ! a pixel count value was derived
      REAL MEAN                       ! Mean value of pixels around the
                                      ! 'fit' ellipse
      REAL RESDU                      ! A weighted residuals value for
                                      ! fit
      REAL VA(ELF__MXPOI)             ! Pixel values at each of the required
                                      ! points on the 'fit' ellipse
      REAL SDP                        ! Standard deviation of MEAN

*  Arguments Given and Returned:

*  Local variables:
      INTEGER I                       ! Loop variable
      REAL SUMSQ                      ! Sum of squares of deviation
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Clear the pixel assigned a value flag.
      DO 5 I=1,NUMPOI
         USED(I)=0
 5    CONTINUE

*   Use bi-linear interpolation.
      CALL ELF1_INTER0(ELEMS,%VAL(CNF_PVAL(ARRP(1))),
     :                 NUMPOI,XR,YR,PRANGE,USED,
     :                 VA,STATUS)

*   Calculate the mean pixel value around the ellipse and also the
*   number of points for which a value was interpolated successfully.
      FOUND=0
      MEAN=0.0
      DO 10 I=1,NUMPOI
         IF (USED(I).NE.0) THEN
            FOUND=FOUND+1
            MEAN=MEAN+VA(I)
         END IF
 10   CONTINUE

*   Determine the mean value.
      IF (FOUND.GT.1) THEN
         MEAN=MEAN/REAL(FOUND)
      ELSE
         MEAN=0.0
      END IF

*   Calculate the sum of squares of the residuals.
      SUMSQ=0.0
      DO 40 I=1,NUMPOI
         IF (USED(I).GT.0) SUMSQ=SUMSQ+(VA(I)-MEAN)*(VA(I)-MEAN)
 40   CONTINUE

*   Calculate the distribution standard deviation.
      SDP=SQRT(SUMSQ/(FOUND-1.))

*   Calculate the error in the mean estimated.
      SDP=SDP/SQRT(REAL(FOUND))

*   Construct some sort of measure of variation. Biased toward increasing
*   the mean value for a given ellipse.
      RESDU=SDP/(1.+ABS(MEAN))

      END


      SUBROUTINE ELP1_STATS(FAST,RTYPE,RADIUS,ELEMS,ARRP,XR,YR,NUMPOI,
     :                      FRACT,PRANGE,USED,MEAN,SDP,RESID,FLAG,
     :                      VA,FOUND,STAT,STATUS)
*+
*  Name:
*     ELP1_STATS

*  Purpose:
*     Looks at the co-ordinates of the ellipse currently being considered and
*     determines what the image pixel value is associated with each image
*     location.
*
*     The values found are used to determine the standard deviation of
*     the ellipse points (ideally should be zero). This and the residual
*     value may be used to determine whether the current ellipse parameters
*     yield a better fit (lower resiudual) than those used previously.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELP1_STATS(FAST,RTYPE,RADIUS,ELEMS,ARRP,XR,YR,NUMPOI,
*                     FRACT,PRANGE,USED,MEAN,SDP,RESID,FLAG,
*                     VA,FOUND,STAT,STATUS

*  Description:
*     Using interpolation, the values associated with all the
*     ellipse fit points (contained in arrays XR and YR) are obtained.
*
*     If the FAST parameter has been passed with a TRUE value then all
*     pixel count values are obtained by bi-linear interpolation.
*     Otherwise, for small radii (radius <= 3 pixels) two routines are
*     used to model the surface of the image. One requires a full 8x8
*     mesh, the other as many points as possible from the mesh. The mesh is
*     centred on the object of interest. These methods are substantially
*     slower. The second is only used if the first failed to find a complete
*     set of mesh points due to bad points or the object being near the
*     edge of the image.
*
*     These are then used to calculate an estimate of the mean pixel
*     count in the ellipse (returned in MEAN), the standard deviation
*     thereof and some weighted measure of the overall fit residuals.
*
*     The type of residual to be calculated is specified by RTYPE, which
*     should be one of ELP__RESxxx specified in elp_par.  This also
*     controls which type of statistic is returned in STAT.  Having said
*     that, it's really not clear what is the best type of residual to
*     use.  The original one - a weighted standard error - is rational,
*     but not obviously ideal.  I've added here the range and the
*     squared-differences as alternatives, selectable by
*     RTYPE=elp__resmd and elp__resls respectively.  (And yes, this
*     shouldn't be called a `residual' - it's a statistic, but it
*     would be dangerous to try changing variable names).
*
*     An error message is returned if the values for too many points could
*     not be determined.
*
*
*     CALL ELP1_STATS(FAST,RTYPE,RADIUS,ELEMS,ARRP,XR,YR,NUMPOI,
*                     FRACT,PRANGE,USED,MEAN,SDP,RESID,FLAG,
*                     VA,FOUND,STAT,STATUS
*  Arguments:
*     FAST = LOGICAL (Given)
*        Is the faster method of interpolation to be employed?
*     RTYPE = INTEGER (Given)
*        Which RESID and STAT is the routine to calculate.
*     RADIUS = REAL (Given)
*        The radius of the ellipse. Units pixels.
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     ARRP(1) = INTEGER (Given)
*        Pointer to the beginning of the image array.
*     XR(ELP__MXPOI) = REAL (Given)
*        X co-ordinates of the 'fit' ellipse points.
*     YR(ELP__MXPOI) = REAL (Given)
*        Y co-ordinates of the 'fit' ellipse points.
*     NUMPOI = INTEGER (Given)
*        Number of ellipse points for which co-ordinates have been calculated.
*     FRACT = REAL (Given)
*        Fraction of all of the ellipse points for which an interpolated value
*        must be derived if an ellipse is to be acceptable.
*     PRANGE(2) = INTEGER (Given)
*        Size of the image. Units pixels.
*     USED(ELP__MXPOI) = INTEGER (Returned)
*        Was a sensible value obtained for a given ellipse point via
*        interpolation?
*     MEAN = REAL (Returned)
*        Mean pixel value found around the ellipse. Units counts.
*     SDP = REAL (Returned)
*        Standard deviation of the MEAN. Units counts.
*     RESID = REAL (Returned)
*        A measure of the variation in pixel value around the ellipse.
*     FLAG = INTEGER (Returned)
*        Were pixel count values found for enough of the ellipse points.
*     VA(ELP__MXPOI) = REAL (Returned)
*        The values of image pixel count at the x/y co-ordinates of
*        the ellipse points defined by arrays XR() and YR().
*     FOUND = INTEGER (Returned)
*        The number of ellipse points used.
*     STAT = REAL (Returned)
*        Statistic appropriate to the type of residual.  See below.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     NG: Norman Gray (Starlink, Glasgow)

*  History:
*     26-Mar-1993 (GJP)
*     (Original version)
*     18 Aug 1998 (NG)
*     Modified to calculate median as well as mean.  Also
*     altered calculation of sample SD, to avoid roundoff error
*     (probably not a big issue, but easy and safe)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELP_PAR'               ! ELLPRO constants
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      LOGICAL FAST                    ! Use the faster interpolation method
                                      ! throughout
      INTEGER RTYPE                   ! Which type of residual to calculate
      INTEGER ARRP(1)                 ! Image array pointer
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER NUMPOI                  ! Number of points on the ellipse
      INTEGER PRANGE(2)               ! Size of the image
      INTEGER USED(ELP__MXPOI)        ! Was an ellipse point used
      REAL FRACT                      ! Fraction of the ellipse points
                                      ! that must be present for an
                                      ! acceptable fit
      REAL RADIUS                     ! Radius of the ellipse
      REAL XR(ELP__MXPOI)             ! X co-ord for the translated ellipse
      REAL YR(ELP__MXPOI)             ! Y co-ord for the translated ellipse

*  Arguments Returned:
      INTEGER FLAG                    ! Was a sensible residual obtained
      INTEGER FOUND                   ! Number of ellipse points for which
                                      ! a pixel count value was derived
      REAL MEAN                       ! Mean value of pixels around the
                                      ! 'fit' ellipse
      REAL STAT                       ! Statistic returned
      REAL RESID                      ! A weighted residuals value for
                                      ! fit
      REAL VA(ELP__MXPOI)             ! Pixel values at each of the required
                                      ! points on the 'fit' ellipse
      REAL SDP                        ! Standard deviation of MEAN

*  Arguments Given and Returned:

*  Local variables:
      INTEGER I                 ! Loop variable
      REAL SUMSQ                ! Sum of squares of deviation
      REAL SUMV                 ! sum of values
      REAL N                    ! =REAL(FOUND)
      REAL SQD                  ! Squared-difference

      INTEGER WS                ! The workspace
      INTEGER WSSIZE            ! Size of workspace given to esp1_median
      SAVE WS, WSSIZE
      DATA WSSIZE/-1/

*   Function
      REAL ESP1_MEDIAN          ! Returns median

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the 'residuals not found' flag
      FLAG=1

*   Clear the pixel assigned a value flag.
      DO 5 I=1,NUMPOI
         USED(I)=0
 5    CONTINUE

*   Use spline routines to construct a 2D surface and use that for
*   interpolating at small radii. Other wise use bi-linear interpolation.
      IF ((RADIUS.LE.3.26).AND.(.NOT.FAST)) THEN

*      Call the first interpolation system. Employs
*      pixels around that required. Full grid must be present
*      to suceed.
         CALL ELP1_INTER(ELEMS,%VAL(CNF_PVAL(ARRP(1))),
     :                   NUMPOI,XR,YR,PRANGE,USED,
     :                    VA,STATUS)

      ELSE

*        Use bi-linear interpolation.
         CALL ELP1_INTER0(ELEMS,%VAL(CNF_PVAL(ARRP(1))),
     :                    NUMPOI,XR,YR,PRANGE,USED,
     :                    VA,STATUS)

      END IF

*   Calculate the mean pixel value around the ellipse and also the
*   number of points for which a value was interpolated successfully.
      FOUND=0
      SUMV=0.0
      SUMSQ=0.0
      DO 10 I=1,NUMPOI
         IF (USED(I).NE.0) THEN
*         Calculate the sum of VA and VA^2
            FOUND=FOUND+1
            SUMV=SUMV+VA(I)
            SUMSQ=SUMSQ+VA(I)**2
         END IF
 10   CONTINUE

      N = REAL(FOUND)

*   Only continue if enough 'fit' ellipse pixels have been assigned
*   interpolated count values.
      IF (N.GE.FRACT*NUMPOI/100.) THEN

*      Set enough points found flag.
         FLAG=0

*      Determine the mean value.
         MEAN=SUMV/N

*      Calculate the sample standard deviation (as distinct from the
*      distribution sd) using
*      $S^2=\frac{n\sum^n X_i^2 - (\sum^n X_i)^2}{n(n-1)}$.  This is
*      equivalent to the definition $S^2=(\sum^n (X_i-\bar X)^2)/(n-1)$
*      but with less roundoff error.
         SQD = (N*SUMSQ-SUMV**2)/N
         SDP = SQRT (SQD/(N-1.))
*         SDP = SQRT( (N*SUMSQ-SUMV**2)/(N*(N-1.)))

*      Calculate the standard error in the mean ($S_{\bar X}=S/\sqrt n$).
*      OK - we should use the t-distribution for this, but this'll be
*      accurate enough as long as N >~ 30 or so.
         SDP=SDP/SQRT(N)

*      Just before going in to calculate the residual, shove out a
*      one-time message confirming which residual we're going to
*      calculate.  WSSIZE = -1 is the initial value - the test in
*      ELP__RESMD below depends simply on it being initialised negative.
         IF (WSSIZE .EQ. -1) THEN
            CALL MSG_BLANK (STATUS)
            IF (RTYPE .EQ. ELP__RESMN) THEN
               CALL MSG_OUT (' ','Residual calculation: weighted SD',
     :              STATUS)
            ELSEIF (RTYPE .EQ. ELP__RESMD) THEN
               CALL MSG_OUT (' ','Residual calculation: absolute diffs',
     :              STATUS)
            ELSEIF (RTYPE .EQ. ELP__RESLS) THEN
               CALL MSG_OUT (' ','Residual calculation: '//
     :              'unweighted least-squares', STATUS)
            ELSE
               CALL ERR_REP (' ','ELP1_STATS: unrecognised rtype',
     :              STATUS)
               STATUS = SAI__ERROR
               GOTO 9999
            ENDIF
            WSSIZE = -2 ! So we don't come here again
         ENDIF

         IF (RTYPE .EQ. ELP__RESMN) THEN
*         Calculate the residual used in the original version of ELLPRO -
*         the standard error, biased to weight more heavily ellipses
*         with higher means.
            RESID=SDP/(1.+ABS(MEAN))
            STAT = MEAN

         ELSE IF (RTYPE .EQ. ELP__RESMD) THEN
*         First calculate the median, and put it in STAT

*         First make sure that we have a workspace as least as big as
*         NUMPOI to pass to esp1_median.
            IF (WSSIZE .LT. NUMPOI) THEN
               IF (WSSIZE .GT. 0) THEN
*               Deallocate old workspace
                  CALL PSX_FREE (WS, STATUS)
               ENDIF
*            Allocate new workspace
               CALL PSX_CALLOC (NUMPOI, '_REAL', WS, STATUS)
               WSSIZE = NUMPOI
               IF (STATUS .NE. SAI__OK) GOTO 9999
            ENDIF
            STAT = ESP1_MEDIAN(VA,USED,NUMPOI,%VAL(CNF_PVAL(WS)),STATUS)

*         Use the sum of the absolute differences as the `residual'.
*         I've no detailed justification for this, but it `matches' the
*         use of the median as an estimator (mumble, bluff...)
            RESID=0.0
            DO I=1,NUMPOI
               IF (USED(I).NE.0) RESID=RESID+ABS(VA(I)-STAT)
            ENDDO

         ELSE IF (RTYPE .EQ. ELP__RESLS) THEN
*         A simple least-squares residual
            RESID = SQD
            STAT = MEAN

         ELSE
*         Unrecognised RTYPE
            CALL ERR_REP (' ','ELP1_STATS: unrecognised rtype', STATUS)
            STATUS = SAI__ERROR
            GOTO 9999

         ENDIF

      ELSE

*      Set flag due to the low number of pixels for which values
*      were determined.
         FLAG=1
         GOTO 9999

      END IF

 9999 CONTINUE

      END
