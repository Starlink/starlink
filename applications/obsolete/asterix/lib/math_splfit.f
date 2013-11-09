*+  MATH_SPLFIT - Fits a spline to a 1-d array and outputs the coefficients.
	SUBROUTINE MATH_SPLFIT (NPTS, XIN, YIN, WEIGHT, XWORK,
     &            YWORK, WORK1, RWIDTH, N7, KNOT, COEFF, SS, STATUS)
*    Description :
*            This routine takes a weighted 1-d data series and fits it
*           with a cubic spline. The output coefficients can be fed into
*           the routine MATH_SPLREC which will take an array of X values
*           and produce the corresponding Y array.
*    Parameters :
*    Method :
*            The PDA routine PDA_DEFC is used.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton (LTVAD::RDS)
*     Richard Beard (Birmingham)
*    History :
*     4 May 1988 Original (LTVAD::RDS)
*     9 Jun 1997 Convert to PDA (RB)
*    27 Jun 1997 Correct knot placement (RB)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS                 !i/o: Status return from the NAG routine
*    Local Constants :
      INTEGER MAXKNOT                ! Max no of knots used
        PARAMETER (MAXKNOT=500)
      INTEGER MAXDATA		     ! Max no of data points allowed
        PARAMETER (MAXDATA=1024)
      INTEGER ORDER		     ! Order of spline (= degree + 1)
        PARAMETER (ORDER=4)	     ! (ie. bi-cubic)
*    Import :
      INTEGER NPTS                   ! Number of points in X and Y arrays
      REAL XIN(NPTS)                 ! Array of X values
      REAL YIN(NPTS)                 ! Array of Y values
      DOUBLE PRECISION WEIGHT(NPTS)  ! Array of weights for each point.
      DOUBLE PRECISION XWORK(NPTS)   ! Work space to copy X aray to double prec.
      DOUBLE PRECISION YWORK(NPTS)   ! Work space to copy Y aray to double prec.
      DOUBLE PRECISION WORK1(NPTS)   ! Work space for NAG routine
      REAL RWIDTH                    ! Distance in X between knots
*    Export :
      INTEGER N7                     ! Number of intervals of the spline +7
      DOUBLE PRECISION KNOT(MAXKNOT) ! Array of knot positions (fn of X)
      DOUBLE PRECISION COEFF(MAXKNOT)! Array of spline coefficients
      DOUBLE PRECISION SD(MAXDATA)   ! Reciprocal of weights
      INTEGER SS                     ! Sum of the residues (ie error)
*    Global variables :
*    Local variables :
      INTEGER L
        PARAMETER (L=(MAXKNOT-ORDER+3)*(ORDER+1)+(MAXKNOT+1)*(ORDER+1)+
     :                2*(MAXDATA)+MAXKNOT+ORDER*ORDER)
      DOUBLE PRECISION WORK2(L)  	   ! Work space for PDA_DEFC
      REAL DRANGE                          ! Range of X
      INTEGER J,K                          ! Loop variable
      INTEGER LP
      REAL KINC                            ! Increment between knots
      INTEGER NKNOT                        ! Number of knots.
*    Local data :
*-
      IF (STATUS .NE. SAI__OK) RETURN
      IF (NPTS .GT. 1024) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP('NPTS > 1024, SORRY...','from MATH_SPLFIT',STATUS)
         RETURN
      END IF
*
* Convert input real arrays to double precision
*
      DO LP = 1, NPTS
         XWORK(LP) = DBLE( XIN(LP) )
         YWORK(LP) = DBLE( YIN(LP) )
      END DO
*
* Calculate the number of knots and sections in the spline
*
      DRANGE = REAL(XWORK(NPTS) - XWORK(1))
      NKNOT = NINT( DRANGE / RWIDTH )
      N7 = NKNOT + (2*ORDER) - 1
*
* Set up knots
* Knots must be inside the data points
*
*   Set up the end point knots for PDA
      DO J = 1, ORDER-1
         KNOT(J) = XWORK(1)
         KNOT(N7-ORDER+1+J) = XWORK(NPTS)
      END DO

*   Set extreme knots just inside the data set.
      KNOT(ORDER) = XWORK(2)
      KNOT(N7-ORDER+1) = XWORK(NPTS-1)
*
*   Space interior knots equidistantly
      KINC = (XWORK(NPTS-1)-XWORK(2)) / REAL(NKNOT)
*
      DO J = ORDER+1, N7-ORDER
         K = J-ORDER
	 KNOT(J) = XWORK(2) + DBLE(K*KINC)
      END DO

      DO J = 1, NPTS
         IF ( WEIGHT(J) .NE. 0.0D0 ) THEN
            SD(J) = 1.0D0 / WEIGHT(J)
         ELSE
            SD(J) = 9.99D99
         END IF
      END DO
*
* Do spline fit. Status should be set to zero anyway, but just make sure.
*
      STATUS = 0
      CALL PDA_DEFC( NPTS, XWORK, YWORK, SD, ORDER, N7, KNOT, 1, J,
     :               COEFF, L, WORK2, STATUS )
      SS = 0
*
      IF ( J .EQ. -1 ) THEN
         CALL MSG_PRNT( 'Usage error from PDA_DEFC.' )
      ELSE IF ( J .EQ. 2 ) THEN
         CALL MSG_PRNT(' Not enough data processed for spline.' )
      END IF

      IF (STATUS .NE. SAI__OK .OR. J .NE. 1) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP('No spline fitted','from MATH_SPLFIT',STATUS)
      ENDIF
*
      END
