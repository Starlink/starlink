      SUBROUTINE MSC_CHEV(NDATA, NAXIS, AXIS, AXMIN, AXMAX, DEGREE,
     :                    MCHEB, CHEB, DATA, STATUS)

*+
*
*   Name:
*      SUBROUTINE MSC_CHEV
*
*   Description:
*      The data points in DATA(NDATA) with coordinates AXIS(NDATA,NAXIS)
*      are evaluated from Chebyshev Polynomials of degree DEGREE(NAXIS).
*      The coefficients are taken from CHEB(NCHEB), which should be
*      of size DEGREE(1)*DEGREE(2)* ...
*      The AXIS(*,IAXIS) values are scaled using AXMIN(IAXIS)
*      and AXMAX(IAXIS) so that they lie in the range (-1,+1).
*
*   History:
*      Jack Giddings      31-DEC-81     IUEDR Vn. 1.0
*      Paul Rees          20-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      Just smash it out!
*
*   Deficiencies:
*      It only handles the cases NAXIS=1 and NAXIS=2 because I couldn't
*      bother to work out the general n-D product algorithm.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NDATA               ! number of data points
      INTEGER NAXIS               ! number of coordinate axes

      REAL*8 AXIS(NAXIS, NDATA)     ! coordinates for data values
      REAL*8 AXMIN(NAXIS)           ! minimum coordinates for axes
      REAL*8 AXMAX(NAXIS)           ! maximum coordinates for axes

      INTEGER DEGREE(NAXIS)       ! polynomial degree+1 for axes
      INTEGER MCHEB               ! maximum number of coefficients

      REAL*8 CHEB(MCHEB)            ! Chebyshev coefficients

*   Export:
      REAL*8 DATA(NDATA)            ! data values

      INTEGER STATUS              ! status return

*   Local variables:
      INTEGER IAXIS               ! loop index
      INTEGER IDATA               ! loop index
      INTEGER IQ                  ! NQ accumulator
      INTEGER I1                  ! loop index
      INTEGER I2                  ! loop index
      INTEGER NQ                  ! number of coefficients

      REAL*8 AXVAL(2)               ! normalised axis values
      REAL*8 T1(36)                 ! Tn(axis1) values
      REAL*8 T2(36)                 ! Tn(axis2) values
      REAL*8 VALUE                  ! temporary result

*   Check against NAXIS < 1 or NAXIS > MAXIS
      IF (NAXIS.LT.1 .OR. NAXIS.GT.2) THEN

         STATUS = -3
         RETURN

      END IF

*   Determine number of coefficients, NQ, from DEGREE(NAXIS),
*   also check againts illegal values of DEGREE, AXMIN and AXMAX
      NQ = 1

      DO 100 IAXIS = 1, NAXIS

         IF (DEGREE(IAXIS).LE.0) THEN

            STATUS = -3
            RETURN

         ELSE IF (AXMIN(IAXIS).EQ.AXMAX(IAXIS)) THEN

            STATUS = -3
            RETURN

         END IF

         NQ = NQ*DEGREE(IAXIS)

 100  CONTINUE

*   Check that the number of coefficients is not excessive
      IF (NQ.GT.36 .OR. NQ.GT.MCHEB) THEN

         STATUS = -3
         RETURN

      END IF

*   Evaluate results
      DO 200 IDATA = 1, NDATA

         DO 150 IAXIS = 1, NAXIS

            AXVAL(IAXIS) = (AXIS(IAXIS, IDATA) - AXMIN(IAXIS))
     :                     /(AXMAX(IAXIS) - AXMIN(IAXIS))
            AXVAL(IAXIS) = 2.0*AXVAL(IAXIS) - 1.0

 150     CONTINUE

         VALUE = 0.0
         IQ = 0

         IF (NAXIS.EQ.1) THEN

            CALL MSC_CHEP(AXVAL(1), DEGREE(1), T1)

            DO 160 I1 = 1, DEGREE(1)

               IQ = IQ + 1
               VALUE = VALUE + CHEB(IQ)*T1(I1)

 160        CONTINUE

         ELSE IF (NAXIS.EQ.2) THEN

            CALL MSC_CHEP(AXVAL(1), DEGREE(1), T1)
            CALL MSC_CHEP(AXVAL(2), DEGREE(2), T2)

            DO 180 I1 = 1, DEGREE(1)

               DO 170 I2 = 1, DEGREE(2)

                  IQ = IQ + 1
                  VALUE = VALUE + CHEB(IQ)*T1(I1)*T2(I2)

 170           CONTINUE

 180        CONTINUE

         END IF

         DATA(IDATA) = VALUE

 200  CONTINUE

      STATUS = 0

      END
