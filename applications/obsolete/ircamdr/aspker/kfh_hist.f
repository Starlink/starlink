
*+  KFH_HIST - Histogram equilisation of pens.
      SUBROUTINE KFH_HIST(SCRTCH,XDIM,YDIM,PENS,STATUS)
*    Description :
*     A histogram of the scaled image is produced, and is
*     used to assign colours to the pens in such a way that
*     the resultant image should contain as near a given
*     distribution of colour as possible. The only available
*     distribution is a linear one. The gradient of the
*     amount of the colour against the colour is defined
*     by the user, and must lie in the range -1 to 1.
*     Assuming for the moment that a grey scale is being
*     used as the colour set, a gradient of 0 should give
*     an even amount of each shade, a gradient of -1 will
*     give a dark image, and a gradient of +1 will give a
*     rather white image.
*    Invocation :
*     CALL KFH_HIST(SCRTCH,XDIM,YDIM,PENS,STATUS)
*    Parameters :
*     SCRTCH(XDIM,YDIM) = REAL
*           The array containing the scaled image.
*     XDIM = INTEGER
*           The x dimension of the image.
*     YDIM = INTEGER
*           The y dimension of the image.
*     PENS(0:255) = INTEGER
*           The array in which is returned the colour to be
*           assigned to each pen.
*     STATUS = INTEGER
*           The status after the running of this subroutine.
*    Method :
*     The user is asked for the shade (gradient of amount of
*     colour against the colour) which must lie in the range
*     -1 to +1. The minimum and maximum values in the scaled
*     image are found, and the histogram between these limits
*     is formed.
*     Let the probability of a particular pen r being used
*     be P(r), and the probability of a particular colour
*     s being used be Q(s). We require Q(s) = a+bs. Let the
*     function linking the pens with the colours be r=T(s).
*     From probability theory we have that P(r)=Q(s).ds/dr.
*     Thus P(r)dr=Q(s)ds. But the integral of P(r)dr is just
*     the cumulative frequency value for a pen, say C(r).
*     Hence we have that C(r)=as+bs*s/2+c. But when r=0, s=0.
*     Thus c=C(0). If the pens lie in the range 0 to n
*     and the colours in the range 0 to m, then when r=n, s=m
*     and C(n)=1. Hence 1=am+bm*m/2+C(0), from which we obtain
*     a=(1-C(0))/m-bm/2. We now have s as a unique function
*     of r :
*      s=-a/b-sqrt(a*a+2b(C(r)-C(0)))/b
*     The function C(r) is approximated by fitting a sixth
*     order polynomial to the discrete cumulative frquency
*     obtained from the histogram. The polynomial is generated
*     by using least squares. The solution of the equations
*     for the coefficients is done using Gaussian elimination
*     with partial pivoting.
*     For a much more detailed description of histogram
*     equilisation see 'Digital Image Processing' by P.Wintz
*     and R.C.Gonzalez.
*    Deficiencies :
*     <description of any deficiencies>
*    Authors :
*     Based on ADHC written by P.T.Wallace,K.F.Hartley and
*     W.F.Lupton.
*     K.F.Hartley (RGVAD::KFH)
*     A.P.Horsfield (RGVAD::KFH)
*     S.Chan (RGVAD::KFH)
*    History :
*     28 July 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local Constants :
      INTEGER NBINS			! Number of bins in the
      PARAMETER (NBINS = 256)		! histogram.
*    Local variables :
      INTEGER XDIM			! X dimension of image.
      INTEGER YDIM			! Y dimension of image.
      DOUBLE PRECISION A		! Coefficient in the
*					! expression relating
*					! colour to pen number.
      DOUBLE PRECISION C0		! Minimum value of the
*					! calculated cumulative
*					! frequency chart.
      DOUBLE PRECISION COEFF(0:7,0:6)	! Coefficients of the
*					! equations to be solved
*					! for least squares fit.
      INTEGER CUMUL(0:NBINS-1)		! The discrete cumul-
*					! ative frequency chart.
      INTEGER HIST(0:NBINS-1)		! The histogram array.
      INTEGER I				! General variable.
      INTEGER J				! General variable.
      INTEGER K				! General variable.
      INTEGER LOWER			! Lowest part of the
*					! cumulative frequency
*					! chart to which the
*					! polynomial will be
*					! fitted.
      REAL MAXV			        ! Maximum value in the
*					! image.
      DOUBLE PRECISION MAXVAL		! Maximum value of
*					! cofficient. Used for
*					! partial pivoting.
      REAL MINV			        ! Minimum value in the
*					! image.
      INTEGER PENS(0:255)		! The colours assigned
*					! to each pen.
      DOUBLE PRECISION POLCO(0:6)	! Polynomial coeff-
*					! icients.
      INTEGER POS			! Row containing the
*					! largest coefficient.
      LOGICAL REPEAT			! Flag used to simulate
*					! REPEAT..UNTIL loops
*					! using DO WHILE loops.
      REAL SCRTCH(XDIM,YDIM)		! The scaled image.
      DOUBLE PRECISION SHADE		! The relative amounts
*					! of each colour wanted.
      REAL RSHADE
      DOUBLE PRECISION STEP		! The difference in pen
*					! value between succes-
*					! sive pens.
      DOUBLE PRECISION SUM(0:12)	! The sums of powers of
*					! pen numbers between 0
*					! and 12.
      DOUBLE PRECISION TEMP		! General variable.
      INTEGER UPPER			! The largest part of
*					! the cumulative
*					! frequency chart to
*					! be used for the least
*					! squares fit.
      DOUBLE PRECISION X(0:NBINS-1)	! The normalised pen
*					! numbers in the usable
*					! range, used for the
*					! least squares fit.
      DOUBLE PRECISION XI		! Pen number used for
*					! evaluating the colour
*					! from the polynomial.
      DOUBLE PRECISION XJ		! Pen number raised to
*					! the some power.
      DOUBLE PRECISION XMAX		! Pen number at the
*					! highest point used
*					! for fit.
      DOUBLE PRECISION XMIN		! Pen number at the
*					! lowest point used
*					! for fit
      DOUBLE PRECISION Y(0:NBINS-1)	! The normalised value
*					! of the cumulative
*					! frequency chart.
*-

*
*    Get the shade of output the user wants.
*

      REPEAT = .TRUE.

      DO WHILE (REPEAT)

*
*       Prompt the user for the shade of output.
*

         CALL PAR_GET0R('SHADE',RSHADE,STATUS)
         SHADE=RSHADE

*
*       If the user gives an illegal answer then warn him and
*       try again.
*

         IF (STATUS.NE.SAI__OK) THEN

            CALL ERR_ANNUL(STATUS)
            CALL PAR_CANCL('SHADE',STATUS)

            CALL MSG_OUT('BDSHD','SHADE MUST BE A REAL NUMBER'/
     :       /' IN THE RANGE -1 TO +1.',STATUS)

*
*       If the answer is not in the range -1 to +1 then warn
*       the user and try again.
*

         ELSEIF (SHADE.LT.-1.0.OR.SHADE.GT.1.0) THEN

            CALL PAR_CANCL('SHADE',STATUS)

            CALL MSG_OUT('BDSHD','SHADE MUST BE A REAL NUMBER'/
     :       /' IN THE RANGE -1 TO +1.',STATUS)

*
*       As everything is in order, leave the loop.
*

         ELSE

            REPEAT = .FALSE.

         ENDIF

      ENDDO

      CALL PAR_CANCL('SHADE',STATUS)

*
*    Clear histogram array.
*

      DO I = 0,NBINS-1,1
         HIST(I) = 0
      ENDDO

*
*    Find the maximum and minimum values in the image.
*

*      MAXV = SCRTCH(1,1)
*      MINV = SCRTCH(1,1)

*      DO I = 1,YDIM,1
*         DO J = 1,XDIM,1
*            IF (SCRTCH(J,I).GT.MAXV) THEN
*               MAXV = SCRTCH(J,I)
*            ELSEIF(SCRTCH(J,I).LT.MINV) THEN
*               MINV = SCRTCH(J,I)
*            ENDIF
*         ENDDO
*      ENDDO

*
*    Get specified maximum and minimum values of the image.
*

      CALL PAR_GET0R('LOW',MINV,STATUS)

      IF (STATUS.NE.SAI__OK) THEN

         CALL ERR_ANNUL(STATUS)
         RETURN

      ENDIF

      CALL PAR_GET0R('HIGH',MAXV,STATUS)

      IF (STATUS.NE.SAI__OK) THEN

         CALL ERR_ANNUL(STATUS)
         RETURN

      ENDIF

*
      PRINT *,MINV,MAXV
*

*
*    If the all the values are not all identical create
*    the histogram.
*

      IF (MAXV.EQ.MINV) THEN
         STATUS = SAI__OK+1
      ELSE

*
*    Create histogram.
*

         DO I = 1,YDIM,1
            DO J = 1,XDIM,1

               K = MIN(MAX(INT((SCRTCH(J,I)-MINV)/
     :          (MAXV-MINV)*REAL(NBINS)),0),NBINS-1)

               HIST(K) = HIST(K)+1

            ENDDO
         ENDDO

*
*       Create the cumulative frequency chart.
*

         CUMUL(0) = HIST(0)

         DO I = 1,NBINS-1,1
            CUMUL(I) = CUMUL(I-1)+HIST(I)
         ENDDO

*
*       Set up data for least squares fit.
*

         DO I = 0,NBINS-1,1

*
*          Save the pen numbers.
*

            X(I) = DBLE(MINV) + DBLE(I*(MAXV-MINV))/
     :       DBLE(NBINS-1)

*
*          Save the normalized cumulative frequency chart.
*

            Y(I) = DBLE(CUMUL(I))/DBLE(CUMUL(NBINS-1))

         ENDDO

*
*       The fit is done between the 2% and 96% points of the
*       cumulative frequency chart.
*
*       Find the 2% point (approximately)
*

         LOWER = 0

         DO WHILE (Y(LOWER).LT.0.02)
            LOWER = LOWER+1
         ENDDO

*
*       Find the 96% point (approximately)
*

         UPPER = NBINS-1

         DO WHILE (Y(UPPER).GT.0.96)
            UPPER = UPPER-1
         ENDDO

*
*       If there are more than 20 points then do the fit.
*

         IF (UPPER-LOWER.LT.20) THEN
            STATUS = SAI__OK+2
         ELSE

*
*          For maximum accuracy normalize the pen numbers to
*          the range 0 to 1.
*

            XMIN = X(LOWER)
            XMAX = X(UPPER)

            DO I = LOWER,UPPER,1
               X(I) = (X(I)-XMIN)/(XMAX-XMIN)
            ENDDO

*
*          Set to zero the tables into which the sums will be
*          put.
*

            DO I = 0,12,1
               SUM(I) = 0.0D0
            ENDDO

            DO I = 0,6,1
               COEFF(7,I) = 0.0D0
            ENDDO

*
*          Form the sums of the powers of x (the pen numbers).
*

            DO I = LOWER,UPPER,1

               XJ = 1.0D0

               DO J = 0,12,1

                  SUM(J) = SUM(J)+XJ

                  IF (J.LT.7) COEFF(7,J) = COEFF(7,J)-Y(I)*XJ

                  XJ = XJ*X(I)

               ENDDO

            ENDDO

*
*          Transfer the sums to the coefficients array ready
*          for solving the linear simultaneous equations to
*          obtain the coefficients of the polynomial.
*

            DO I = 0,6,1
               DO J = 0,6,1
                  COEFF(J,I) = SUM(J+I)
               ENDDO
            ENDDO

*
*          Do Gaussian elimination to calculate the polynomial
*          coefficients.
*

            I = 0

            DO WHILE (I.LT.7.AND.STATUS.EQ.SAI__OK)

*
*             Find the maximum value in the column.
*

               MAXVAL = ABS(COEFF(I,I))
               POS = I

               DO J = I,6,1
                  IF (ABS(COEFF(I,J)).GT.MAXVAL) THEN
                     MAXVAL = ABS(COEFF(I,J))
                     POS = J
                  ENDIF
               ENDDO

*
*             If the maximum value is greater than 1E-10
*             then do the operations on the row, otherwise
*             stop the routine, as the results are going to
*             be very inaccurate.
*

               IF (MAXVAL.LT.1.0D-10) THEN
                  STATUS = SAI__OK+3
               ELSE

*
*                Swap the present row with that containing the
*                largest value.
*

                  IF (I.NE.POS) THEN
                     DO J = 0,7,1
                        TEMP = COEFF(J,I)
                        COEFF(J,I) = COEFF(J,POS)
                        COEFF(J,POS) = TEMP
                     ENDDO
                  ENDIF

*
*                Eliminate the first I-1 variables.
*

                  IF (I.NE.0) THEN
                     DO J = 0,I-1,1
                        DO K = 7,J,-1
                           COEFF(K,I) = COEFF(K,I)-COEFF(K,J)*
     :                      COEFF(J,I)
                        ENDDO
                     ENDDO
                  ENDIF

*
*                Divide throuought the Ith row by the Ith
*                element, so that the Ith element becomes
*                one.
*

                  DO J = 7,I,-1
                     COEFF(J,I) = COEFF(J,I)/COEFF(I,I)
                  ENDDO

*
*                Point to the next row.
*

                  I = I+1

               ENDIF

            ENDDO

*
*          If there were no problems before then calculate
*          the coefficients, and then assign colours to the
*          pens.
*

            IF (STATUS.EQ.SAI__OK) THEN

*
*             Substitute back to obtain the coefficients.
*

               POLCO(6) = -COEFF(7,6)

               DO I = 5,0,-1

                  POLCO(I) = -COEFF(7,I)

                  DO J = 6,I+1,-1
                     POLCO(I) = POLCO(I)-COEFF(J,I)*POLCO(J)
                  ENDDO

               ENDDO

*
*             Assign colours to the pens.
*

               STEP = DBLE(MAXV-MINV)/((XMAX-XMIN)*255.0D0)

               XI = 0.0D0

*
*             Calculate the minimum value for the cumulative
*             frequency chart.
*

               C0 = POLCO(0)

               IF (SHADE.EQ.0.0) THEN

*
*                Assign colours to the pens in such a way
*                that they are used in equal amounts in
*                the displayed image.
*

                  DO I = 0,255,1
                     IF (I.LT.LOWER) THEN
                        TEMP = 0.0D0
                     ELSEIF (I.GT.UPPER) THEN
                        TEMP = 1.0D0-C0
                     ELSE
                        TEMP = (POLCO(0)+XI*(POLCO(1)+XI*
     :                         (POLCO(2)+XI*(POLCO(3)+XI*
     :                         (POLCO(4)+XI*(POLCO(5)+XI*
     :                         (POLCO(6))))))))-C0
                        XI = XI+STEP
                     ENDIF
                     PENS(I) = MIN(MAX(INT(256.0D0*TEMP),0),255)
                  ENDDO

               ELSE

*
*                Assign colours to the pens such that the
*                distribution required by the user is obtained.
*

                  A = 1.0D0-C0-SHADE/2.0

                  DO I = 0,255,1
                     IF (I.LT.LOWER) THEN
                        TEMP = 0.0D0
                     ELSEIF (I.GT.UPPER) THEN
                        TEMP = 1.0D0-C0
                     ELSE
                        TEMP = (POLCO(0)+XI*(POLCO(1)+XI*
     :                         (POLCO(2)+XI*(POLCO(3)+XI*
     :                         (POLCO(4)+XI*(POLCO(5)+XI*
     :                         (POLCO(6))))))))-C0
                        XI = XI+STEP
                     ENDIF
                     PENS(I) = MIN(MAX(INT(256.0*(-A/SHADE+
     :                SQRT(A*A+2.0D0*SHADE*TEMP)/SHADE)),0),255)
                  ENDDO

               ENDIF

            ENDIF

         ENDIF

      ENDIF

      END
