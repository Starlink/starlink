      SUBROUTINE MSC_CHEF( NDATA, NAXIS, DATA, AXIS, AXMIN, AXMAX,
     :                     DEGREE, MCHEB, CHEB, STATUS )
*+
*  Name:
*     SUBROUTINE MSC_CHEF

*  Description:
*     The data points in DATA(NDATA) with coordinates AXIS(NDATA,NAXIS)
*     are fitted by Chebyshev Polynomials of degree DEGREE(NAXIS).
*     The coefficients are returned in CHEB(NCHEB), which should be
*     of size DEGREE(1)*DEGREE(2)* ...
*     The AXIS(*,IAXIS) values are scaled using AXMIN(IAXIS)
*     and AXMAX(IAXIS) so that they lie in the range (-1,+1).

*  Language:
*     Starlink Fortran 77

*  Method:
*     Just smash it out!

*  Deficiencies:
*     It only handles the cases NAXIS=1 and NAXIS=2 because I couldn't
*     bother to work out the general n-D product algorithm.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     31-DEC-81 (JRG):
*       IUEDR Vn. 1.0
*     20-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     06-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER NDATA        ! number of data points
      INTEGER NAXIS        ! number of coordinate axes

      REAL*8 DATA(NDATA)   ! data values
      REAL*8 AXIS(NAXIS, NDATA)  ! coordinates for data values
      REAL*8 AXMIN(NAXIS)  ! minimum coordinates for axes
      REAL*8 AXMAX(NAXIS)  ! maximum coordinates for axes

      INTEGER DEGREE(NAXIS)! polynomial degree+1 for axes
      INTEGER MCHEB        ! maximum number of returned coefficients

*  Arguments Returned:
      REAL*8 CHEB(MCHEB)   ! Chebyshev coefficients

      INTEGER STATUS       ! status return

*  Local Variables:
      REAL*8 AXVAL(2)      ! normalised axis values
      REAL*8 R             ! residual term
      REAL*8 S(36)         ! constraint terms
      REAL*8 T1(36)        ! Tn(axis1) values
      REAL*8 T2(36)        ! Tn(axis2) values

      REAL*8 A(36, 36)     ! constraint matrix
      REAL*8 T(36)         ! right hand side and solution

      INTEGER I            ! loop index
      INTEGER IAXIS        ! loop index
      INTEGER IDATA        ! loop index
      INTEGER IQ           ! NQ accumulator
      INTEGER I1           ! loop index
      INTEGER I2           ! loop index
      INTEGER J            ! loop index
      INTEGER NQ           ! number of coefficients
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check against NAXIS < 1 or NAXIS > MAXIS.
      IF (NAXIS.LT.1 .OR. NAXIS.GT.2) THEN
         STATUS = -3
         RETURN
      END IF

*   Determine number of coefficients, NQ, from DEGREE(NAXIS),
*   also check againts illegal values of DEGREE, AXMIN and AXMAX.
      NQ = 1

      DO IAXIS = 1, NAXIS
         IF ( DEGREE( IAXIS ) .LE. 0 ) THEN
            STATUS = -3
            RETURN

         ELSE IF ( AXMIN( IAXIS ) .EQ. AXMAX( IAXIS ) ) THEN
            STATUS = -3
            RETURN

         END IF

         NQ = NQ * DEGREE( IAXIS )
      END DO

*   Check that the number of coefficients is not excessive.
      IF ( NQ.GT.36 .OR. NQ.GT.MCHEB ) THEN
         STATUS = -3
         RETURN
      END IF

*   Check that there are enough data values.
      IF (NDATA.LT.NQ) THEN
         STATUS = -3
         RETURN
      END IF

*   Zero out constraint matrix.
      DO I = 1, NQ
         T( I ) = 0.0D0
         DO J = 1, NQ
            A( J, I ) = 0.0D0
         END DO
      END DO

*   Fill constraint matrix.
      DO IDATA = 1, NDATA
         DO IAXIS = 1, NAXIS
            AXVAL( IAXIS ) = ( AXIS( IAXIS, IDATA ) - AXMIN( IAXIS ) ) /
     :                       ( AXMAX( IAXIS ) - AXMIN( IAXIS ) )
            AXVAL( IAXIS ) = 2.0 * AXVAL( IAXIS ) - 1.0
         END DO

         IQ = 0

         IF ( NAXIS .EQ. 1 ) THEN
            CALL MSC_CHEP( AXVAL( 1 ), DEGREE( 1 ), T1 )
            DO I1 = 1, DEGREE( 1 )
               IQ = IQ + 1
               S( IQ ) = T1( I1 )
            END DO

         ELSE IF ( NAXIS .EQ. 2 ) THEN
            CALL MSC_CHEP( AXVAL( 1 ), DEGREE( 1 ), T1 )
            CALL MSC_CHEP( AXVAL( 2 ), DEGREE( 2 ), T2 )

            DO I1 = 1, DEGREE( 1 )
               DO I2 = 1, DEGREE( 2 )
                  IQ = IQ + 1
                  S( IQ ) = T1( I1 ) * T2( I2 )
               END DO
            END DO
         END IF

         R = DATA( IDATA )

         DO I = 1, NQ
            DO J = 1, NQ
               A( I, J ) = A( I, J ) + S( J ) * S( I )
            END DO
            T( I ) = T( I ) + R * S( I )
         END DO
      END DO

      CALL MSC_LUSLV( 36, NQ, A, T )

*   Put values into output.
      DO IQ = 1, NQ
         CHEB( IQ ) = T( IQ )
      END DO

      END
