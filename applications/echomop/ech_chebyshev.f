      SUBROUTINE ECH_CHEF( NDATA, NAXIS, DATA, AXIS, AXMIN, AXMAX,
     :           DEGREE, MCHEB, CHEB, STATUS )
*+
*  Name:
*     SUBROUTINE ECH_CHEF

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
      INTEGER NDATA        ! Number of data points.
      INTEGER NAXIS        ! Number of coordinate axes.

      DOUBLE PRECISION DATA( NDATA ) ! Data values.
      DOUBLE PRECISION AXIS( NAXIS, NDATA ) ! coordinates for data values.
      DOUBLE PRECISION AXMIN( NAXIS ) ! Minimum coordinates for axes.
      DOUBLE PRECISION AXMAX( NAXIS ) ! Maximum coordinates for axes.

      INTEGER DEGREE( NAXIS ) ! Polynomial degree+1 for axes.
      INTEGER MCHEB        ! Maximum number of returned coefficients.

*  Arguments Returned:
      DOUBLE PRECISION CHEB( MCHEB ) ! Chebyshev coefficients.

      INTEGER STATUS       ! Status return.

*  Local Variables:
      DOUBLE PRECISION AXVAL( 2 )    ! Normalised axis values.
      DOUBLE PRECISION R             ! Residual term.
      DOUBLE PRECISION S( 36 )       ! Constraint terms.
      DOUBLE PRECISION T1( 36 )      ! Tn(axis1) values.
      DOUBLE PRECISION T2( 36 )      ! Tn(axis2) values.

      DOUBLE PRECISION A( 36, 36 )   ! Constraint matrix.
      DOUBLE PRECISION T( 36 )       ! Right hand side and solution.

      INTEGER I            ! Loop index.
      INTEGER IAXIS        ! Loop index.
      INTEGER IDATA        ! Loop index.
      INTEGER IQ           ! Nq accumulator.
      INTEGER I1           ! Loop index.
      INTEGER I2           ! Loop index.
      INTEGER J            ! Loop index.
      INTEGER NQ           ! Number of coefficients.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine number of coefficients, NQ, from DEGREE(NAXIS),
*  also check againts illegal values of DEGREE, AXMIN and AXMAX.
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

*  Check that the number of coefficients is not excessive.
      IF ( NQ .GT. 36 .OR. NQ .GT. MCHEB ) THEN
         STATUS = -3
         RETURN
      END IF

*  Check that there are enough data values.
      IF ( NDATA .LT. NQ ) THEN
         STATUS = -3
         RETURN
      END IF

*  Zero out constraint matrix.
      DO I = 1, NQ
         T( I ) = 0.0D0
         DO J = 1, NQ
            A( J, I ) = 0.0D0
         END DO
      END DO

*  Fill constraint matrix.
      DO IDATA = 1, NDATA
         DO IAXIS = 1, NAXIS
            AXVAL( IAXIS ) = ( AXIS( IAXIS, IDATA ) - AXMIN( IAXIS ) ) /
     :                       ( AXMAX( IAXIS ) - AXMIN( IAXIS ) )
            AXVAL( IAXIS ) = 2.0 * AXVAL( IAXIS ) - 1.0
         END DO

         IQ = 0

         IF ( NAXIS .EQ. 1 ) THEN
            CALL ECH_CHEP( AXVAL( 1 ), DEGREE( 1 ), T1 )
            DO I1 = 1, DEGREE( 1 )
               IQ = IQ + 1
               S( IQ ) = T1( I1 )
            END DO

         ELSE IF ( NAXIS .EQ. 2 ) THEN
            CALL ECH_CHEP( AXVAL( 1 ), DEGREE( 1 ), T1 )
            CALL ECH_CHEP( AXVAL( 2 ), DEGREE( 2 ), T2 )

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

      CALL ECH_LUSLV( 36, NQ, A, T )

*  Put values into output.
      DO IQ = 1, NQ
         CHEB( IQ ) = T( IQ )
      END DO

      END


      SUBROUTINE ECH_CHEP( X, NTERM, TN )
*+
*  Name:
*     SUBROUTINE ECH_CHEP

*  Description:
*     Evaluate the Chebyshev Polynomials from 0 up to NTERM terms
*     ending in degree (NTERM-1).

*  Language:
*     Starlink Fortran 77

*  Method:
*     Use recurrence relation.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (Starlink, UCL)
*     {add_further_authors_here}

*  History:
*     31-DEC-1981 (JRG):
*       IUEDR Vn. 1.0
*     20-OCT-1988 (PCTR):
*       IUEDR Vn. 2.0
*     08-AUG-1996 (MJC):
*       Lifted code from IUEDR for ECHOMOP.
*     {add_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      DOUBLE PRECISION X ! X value (-1,+1).

      INTEGER NTERM      ! Number of terms.

*  Arguments Returned:
      DOUBLE PRECISION TN( NTERM ) ! Chebyshev function Tn(x).

*  Local Variables:
      INTEGER I          ! Recurrence cycle.
*.

      IF ( NTERM .GT. 2 ) THEN
         TN( 1 ) = 1.0
         TN( 2 ) = X
         DO I = 3, NTERM
            TN( I ) = 2.0 * X * TN( I - 1 ) - TN( I - 2 )
         END DO

      ELSE IF ( NTERM .EQ. 2 ) THEN
         TN( 1 ) = 1.0
         TN( 2 ) = X

      ELSE IF ( NTERM .EQ. 1 ) THEN
         TN( 1 ) = 1.0
      END IF

      END


      SUBROUTINE ECH_CHEV( NDATA, NAXIS, AXIS, AXMIN, AXMAX, DEGREE,
     :           MCHEB, CHEB, DATA, STATUS )
*+
*  Name:
*     SUBROUTINE ECH_CHEV

*  Description:
*     The data points in DATA(NDATA) with coordinates AXIS(NDATA,NAXIS)
*     are evaluated from Chebyshev Polynomials of degree DEGREE(NAXIS).
*     The coefficients are taken from CHEB(NCHEB), which should be
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
*     MJC: Martin Clayton (Starlink, UCL)
*     {add_further_authors_here}

*  History:
*     31-DEC-1981 (JRG):
*       IUEDR Vn. 1.0
*     20-OCT-1988 (PCTR):
*       IUEDR Vn. 2.0
*     08-AUG-1996 (MJC):
*       Lifted code from IUEDR for ECHOMOP.
*     {add_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER NDATA               ! Number of data points.
      INTEGER NAXIS               ! Number of coordinate axes.

      DOUBLE PRECISION AXIS( NAXIS, NDATA ) ! Coordinates for data values.
      DOUBLE PRECISION AXMIN( NAXIS )       ! Minimum coordinates for axes.
      DOUBLE PRECISION AXMAX( NAXIS )       ! Maximum coordinates for axes.

      INTEGER DEGREE( NAXIS )     ! Polynomial degree+1 for axes.
      INTEGER MCHEB               ! Maximum number of coefficients.

      DOUBLE PRECISION CHEB( MCHEB )        ! Chebyshev coefficients.

*  Arguments Returned:
      DOUBLE PRECISION DATA( NDATA )        ! Data values.

      INTEGER STATUS              ! Status return.

*  Local Variables:
      INTEGER IAXIS               ! Loop index.
      INTEGER IDATA               ! Loop index.
      INTEGER IQ                  ! NQ accumulator.
      INTEGER I1                  ! Loop index.
      INTEGER I2                  ! Loop index.
      INTEGER NQ                  ! Number of coefficients.

      DOUBLE PRECISION AXVAL( 2 ) ! Normalised axis values.
      DOUBLE PRECISION T1( 36 )   ! Tn(axis1) values.
      DOUBLE PRECISION T2( 36 )   ! Tn(axis2) values.
      DOUBLE PRECISION VALUE      ! Temporary result.
*.

*  Determine number of coefficients, NQ, from DEGREE(NAXIS),
*  also check againts illegal values of DEGREE, AXMIN and AXMAX.
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

*  Check that the number of coefficients is not excessive.
      IF ( NQ .GT. 36 .OR. NQ .GT. MCHEB ) THEN
         STATUS = -3
         RETURN
      END IF

*  Evaluate results.
      DO IDATA = 1, NDATA
         DO IAXIS = 1, NAXIS
            AXVAL( IAXIS ) = ( AXIS( IAXIS, IDATA ) - AXMIN( IAXIS ) )
     :            / ( AXMAX( IAXIS ) - AXMIN( IAXIS ) )
            AXVAL( IAXIS ) = 2.0 * AXVAL( IAXIS ) - 1.0
         END DO
         VALUE = 0.0
         IQ = 0
         IF ( NAXIS .EQ. 1 ) THEN
            CALL ECH_CHEP( AXVAL( 1 ), DEGREE( 1 ), T1 )
            DO I1 = 1, DEGREE( 1 )
               IQ = IQ + 1
               VALUE = VALUE + CHEB( IQ ) * T1( I1 )
            END DO

         ELSE IF ( NAXIS .EQ. 2 ) THEN
            CALL ECH_CHEP( AXVAL( 1 ), DEGREE( 1 ), T1 )
            CALL ECH_CHEP( AXVAL( 2 ), DEGREE( 2 ), T2 )
            DO I1 = 1, DEGREE( 1 )
               DO I2 = 1, DEGREE( 2 )
                  IQ = IQ + 1
                  VALUE = VALUE + CHEB( IQ ) * T1( I1 ) * T2( I2 )
               END DO
            END DO
         END IF

         DATA( IDATA ) = VALUE
      END DO

      STATUS = 0

      END


      SUBROUTINE ECH_LURED( M, N, A )
*+
*  Name:
*     ECHOMOP - ECH_LURED

*  Purpose:
*     LU decomposition of matrix.

*  Language:
*     Starlink Fortran 77

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (Starlink, UCL)
*     {add_further_authors_here}

*  History:
*     30-JUL-1981 (JRG):
*       IUEDR Vn. 1.0
*     28-OCT-1988 (PCTR):
*       IUEDR Vn. 2.0
*     08-AUG-1996 (MJC):
*       Lifted code from IUEDR for ECHOMOP.
*     {add_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER M                    ! Declared size of A-matrix and B-vector.
      INTEGER N                    ! Used size of A-matrix and B-vector.

*  Arguments Given and Returned:
      DOUBLE PRECISION A( M, M )   ! Matrix operator.

*  Local Variables:
      INTEGER I                    ! Block index.
      INTEGER IP1                  ! Number of lines to be reduced.
      INTEGER K                    ! Line index.
      INTEGER J                    ! Column index.
      INTEGER NM1                  ! Number of blocks to be reduced.

      DOUBLE PRECISION FACT        ! Normalisation factor.
*.

      IF ( N .GT. 1 ) THEN
         NM1 = N - 1
         DO I = 1, NM1
            IP1 = I + 1
            DO K = IP1, N
               FACT = A( K, I ) / A( I, I )
               DO J = IP1, N
                  A( K, J ) = A( K, J ) - A( I, J ) * FACT
               END DO
            END DO
         END DO
      END IF

      END


      SUBROUTINE ECH_RESLV( M, N, A, B )
*+
*  Name:
*     ECHOMOP - ECH_RESLV

*  Purpose:
*     Resolve linear system of equations from LU reduced matrix.

*  Language:
*     Starlink Fortran 77

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (Starlink, UCL)
*     {add_further_authors_here}

*  History:
*     30-JUL-1981 (JRG):
*       IUEDR Vn. 1.0
*     28-OCT-1988 (PCTR):
*       IUEDR Vn. 2.0
*     08-AUG-1996 (MJC):
*       Lifted code from IUEDR for ECHOMOP.
*     {add_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER M                    ! Declared size of A-matrix and B-vector.
      INTEGER N                    ! Used size of A-matrix and B-vector.

      DOUBLE PRECISION A( M, M )   ! Matrix operator.

*  Arguments Given and Returned:
      DOUBLE PRECISION B( M )      ! RHS and result.

*  Local Variables:
      INTEGER IP1                  ! Loop start.
      INTEGER I                    ! Block index.
      INTEGER J                    ! Loop index.
      INTEGER K                    ! Loop index.
      INTEGER L                    ! Loop index.
      INTEGER NM1                  ! Loop end.
*.

      IF ( N .EQ. 1 ) THEN
         B( N ) = B( N ) / A( N, N )

      ELSE
         NM1 = N - 1
         DO I = 1, NM1
            IP1 = I + 1
            DO J = IP1, N
               B( J ) = B( J ) - B( I ) * A( J, I ) / A( I, I )
            END DO
         END DO
         B( N ) = B( N ) / A( N, N )
         DO I = 1, NM1
            K = N - I
            L = K + 1
            DO J = L, N
               B( K ) = B( K ) - B( J ) * A( K, J )
            END DO
            B( K ) = B( K ) / A( K, K )
         END DO
      END IF

      END


      SUBROUTINE ECH_LUSLV( M, N, A, B )
*+
*  Name:
*     ECHOMOP - ECH_LUSLV

*  Purpose:
*     Solution of linear equations.

*  Language:
*     Starlink Fortran 77

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (Starlink, UCL)
*     {add_further_authors_here}

*  History:
*     30-JUL-1981 (JRG):
*       IUEDR Vn. 1.0
*     28-OCT-1988 (PCTR):
*       IUEDR Vn. 2.0
*     08-AUG-1996 (MJC):
*       Lifted code from IUEDR for ECHOMOP.
*     {add_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER M                    ! Declared size of A-matrix and B-vector.
      INTEGER N                    ! Used size of A-matrix and B-vector.

*  Arguments Given and Returned:
      DOUBLE PRECISION A( M, M )   ! Matrix operator.
      DOUBLE PRECISION B( M )      ! RHS and result.
*.

*  LU reduce A-matrix.
      CALL ECH_LURED( M, N, A )

*  Resolve for B-vector.
      CALL ECH_RESLV( M, N, A, B )

      END
