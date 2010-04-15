************************************************************************

	SUBROUTINE SVD_COMP( MATRIX, DIM_X, DIM_Y,
     :                       SIZE_X, SIZE_Y, W, V, STATUS )

*+
*  Name :
*     SVD_COMP
*
*  Purpose :
*     {routine_purpose}...
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL SVD_COMP(MATRIX, DIM_X, DIM_Y, SIZE_X, SIZE_Y, W, V, STATUS )
*
*  Description :
*     Given an arbitrary matrix A, computes its singular value decomposition
*     A = U W Vt; the matrix U replaces A on output.  The diagonal matrix
*     W is output as a vector, and the matrix V (not its transpose Vt) is
*     calculated.  size_X must be greater or equal to dim_Y; if it is smaller,
*     then A should be filled up to square with zero rows.
*
*  Arguments :
*     {arguement_description}...
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     KM: Koji Mukai (Oxford University)
*     AA: Alasdair Allan (Starlink, Keele University)
*     {enter_new_authors_here}
*
*  History :
*     08-FEB-1999
*        Cut and hack for Starlink
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-


*  Type Definitions:
	IMPLICIT NONE

*  Global Constants:
        INCLUDE 'SAE_PAR'         ! Standard SAE constants

	INTEGER BUFF_SIZE
	PARAMETER( BUFF_SIZE = 32 )	! Maximum anticipated value of size_Y

	INTEGER DIM_X, DIM_Y		! Declared size of the matrix
	REAL MATRIX( DIM_X, DIM_Y )	! Matrix to be decomposed / U
	INTEGER SIZE_X, SIZE_Y		! Actual size of the matrix
	REAL W( DIM_Y )			! Diagonal matrix
	REAL V( DIM_Y, DIM_Y )		! V as in A = U.W.Vt

	REAL RV1( BUFF_SIZE )
	REAL SCALE
	REAL NORM
	REAL C, F, G, H, S, X, Y, Z
	INTEGER I, J, K, L, LOOP, N

        INTEGER STATUS

	IF ( STATUS .NE. SAI__OK ) RETURN

	IF( SIZE_X .LT. SIZE_Y ) THEN
            STATUS  =  SAI__ERROR
            CALL ERR_REP( 'ERR_SVD_TOLARGE',
     :        'SVD: Unacceptable size', STATUS )
            GOTO 99
        ELSE IF ( SIZE_Y .GT. BUFF_SIZE )THEN
            STATUS  =  SAI__ERROR
            CALL ERR_REP( 'ERR_SVD_TOBIG',
     :        'SVD: matrix too big', STATUS )
            GOTO 99
        ENDIF

*    Householder reduction to bidiagonal form

	G = 0.0
	SCALE = 0.0
	NORM = 0.0
	DO I = 1, SIZE_Y
	  L = I + 1
	  RV1( I ) = SCALE * G
	  G = 0.0
	  S = 0.0
	  SCALE = 0.0
	  DO K = I, SIZE_X
	    SCALE = SCALE + ABS( MATRIX( K, I ) )
	  END DO
	  IF( SCALE .NE. 0.0 ) THEN
	    DO K = I, SIZE_X
	      MATRIX( K, I ) = MATRIX( K, I ) / SCALE
	      S = S + MATRIX( K, I ) * MATRIX( K, I )
	    END DO
	    F = MATRIX( I, I )
	    G = -SIGN( SQRT( S ), F )
	    H = F * G - S
	    MATRIX( I, I ) = F - G
	    IF( I .NE. SIZE_Y ) THEN
	      DO J = L, SIZE_Y
		S = 0.0
		DO K = I, SIZE_X
		  S = S + MATRIX( K, I ) * MATRIX( K, J )
		END DO
		F = S / H
		DO K = I, SIZE_X
		  MATRIX( K, J ) = MATRIX( K, J ) + F * MATRIX( K, I )
		END DO
	      END DO
	    END IF
	    DO K = I, SIZE_X
	      MATRIX( K, I ) = SCALE * MATRIX( K, I )
	    END DO
	  END IF
	  W( I ) = SCALE * G
	  G = 0.0
	  S = 0.0
	  SCALE = 0.0
	  IF( I .NE. SIZE_Y ) THEN
	    DO K = L, SIZE_Y
	      SCALE = SCALE + ABS( MATRIX( I, K ) )
	    END DO
	    IF( SCALE .NE. 0.0 ) THEN
	      DO K = L, SIZE_Y
		MATRIX( I, K ) = MATRIX( I, K ) / SCALE
		S = S + MATRIX( I, K ) * MATRIX( I, K )
	      END DO
	      F = MATRIX( I, L )
	      G = -SIGN( SQRT( S ), F )
	      H = F * G - S
	      MATRIX( I, L ) = F - G
	      DO K = L, SIZE_Y
		RV1( K ) = MATRIX( I, K ) / H
	      END DO
	      IF( I .NE. SIZE_X ) THEN
		DO J = L, SIZE_X
		  S = 0.0
		  DO K = L, SIZE_Y
		    S = S + MATRIX( J, K ) * MATRIX( I, K )
		  END DO
		  DO K = L, SIZE_Y
		    MATRIX( J, K ) = MATRIX( J, K ) + S * RV1( K )
		  END DO
		END DO
	      END IF
	      DO K = L, SIZE_Y
		MATRIX( I, K ) = SCALE * MATRIX( I, K )
	      END DO
	    END IF
	  END IF
	  NORM = MAX( NORM, ( ABS( W( I ) ) + ABS( RV1( I ) ) ) )
	END DO

*    Accumulation of right-hand transformations

	DO I = SIZE_Y, 1, -1
	  IF( I .LT. SIZE_Y ) THEN
	    IF( G .NE. 0.0 ) THEN
	      DO J = L, SIZE_Y	! Double division to avoid possible underflow
		V( J, I ) = ( MATRIX( I, J ) / MATRIX( I, L ) ) / G
	      END DO
	      DO J = L, SIZE_Y
		S = 0.0
		DO K = L, SIZE_Y
		  S = S + MATRIX( I, K ) * V( K, J )
		END DO
		DO K = L, SIZE_Y
		  V( K, J ) = V( K, J ) + S * V( K, I )
		END DO
	      END DO
	    END IF
	    DO J = L, SIZE_Y
	      V( I, J ) = 0.0
	      V( J, I ) = 0.0
	    END DO
	  END IF
	  V( I, I ) = 1.0
	  G = RV1( I )
	  L = I
	END DO

*    Accumulation of left-hand transformations

	DO I = SIZE_Y, 1, -1
	  L = I + 1
	  G = W( I )
	  IF( I .LT. SIZE_Y ) THEN
	    DO J = L, SIZE_Y
	      MATRIX( I, J ) = 0.0
	    END DO
	  END IF
	  IF( G .NE. 0.0 ) THEN
	    G = 1.0 / G
	    IF( I .NE. SIZE_Y ) THEN
	      DO J = L, SIZE_Y
		S = 0.0
		DO K = L, SIZE_X
		  S = S + MATRIX( K, I ) * MATRIX( K, J )
		END DO
		F = ( S / MATRIX( I, I ) ) * G
		DO K = I, SIZE_X
		  MATRIX( K, J ) = MATRIX( K, J ) + F * MATRIX( K, I )
		END DO
	      END DO
	    END IF
	    DO J = I, SIZE_X
	      MATRIX( J, I ) = MATRIX( J, I ) * G
	    END DO
	  ELSE
	    DO J = I, SIZE_X
	      MATRIX( J, I ) = 0.0
	    END DO
	  END IF
	  MATRIX( I, I ) = MATRIX( I, I ) + 1.0
	END DO

*    Diagonalization of the bidiagonal form

	DO K = SIZE_Y, 1, -1
	  DO LOOP = 1, 30
	    DO L = K, 1, -1
	      N = L - 1
	      IF( ( ABS( RV1( L ) ) + NORM ) .EQ. NORM ) GOTO 200
	      IF( ( ABS( W( N ) ) + NORM ) .EQ. NORM ) GOTO 100
	    END DO
100	   CONTINUE
	    C = 0.0
	    S = 1.0
	    DO I = L, K
	      F = S * RV1( I )
	      IF( ( ABS( F ) + NORM ) .NE. NORM ) THEN
		G = W( I )
		H = SQRT( F * F + G * G )
		W( I ) = H
		H = 1.0 / H
		C = G * H
		S = -F * H
		DO J = 1, SIZE_X
		  Y = MATRIX( J, N )
		  Z = MATRIX( J, I )
		  MATRIX( J, N ) = Y * C + Z * S
		  MATRIX( J, I ) = -Y * S + Z * C
		END DO
	      END IF
	    END DO
200	   CONTINUE
	    Z = W( K )
	    IF( L .EQ. K ) THEN		! Convergence
	      IF( Z .LT. 0.0 ) THEN	! Singular value is made nonnegative
		W( K ) = -Z
		DO J = 1, SIZE_Y
		  V( J, K ) = -V( J, K )
		END DO
	      END IF
	      GOTO 300
	    END IF
	    IF( LOOP .EQ. 30 ) THEN
                STATUS  =  SAI__ERROR
                CALL ERR_REP( 'ERR_SVD_CONVERGENCE',
     :             'SVD: No convergence in 30 iterations ', STATUS )
                GOTO 99
            ENDIF

	    X = W( L ) 		! Shift from bottom 2-by-2 minor
	    N = K - 1
	    Y = W( N )
	    G = RV1( N )
	    H = RV1( K )
	    F = ( ( Y - Z ) * ( Y + Z ) + ( G - H ) * ( G + H ) )
     1							/ ( 2.0 * H * Y )
	    G = SQRT( F * F + 1.0 )
	    F = ( ( X - Z ) * ( X + Z )
     1			+ H * ( Y / ( F + SIGN( G, F ) ) - H ) ) / X

*     Next QR transformation

	    C = 1.0
	    S = 1.0
	    DO J = L, N
	      I = J + 1
	      G = RV1( I )
	      Y = W( I )
	      H = S * G
	      G = C * G
	      Z = SQRT( F * F + H * H )
	      RV1( J ) = Z
	      C = F / Z
	      S = H / Z
	      F = X * C + G * S
	      G = -X * S + G * C
	      H = Y * S
	      Y = Y * C
	      DO N = 1, SIZE_Y
		X = V( N, J )
		Z = V( N, I )
		V( N, J ) = X * C + Z * S
		V( N, I ) = -X * S + Z * C
	      END DO
	      Z = SQRT( F * F + H * H )
	      W( J ) = Z		! Rotation can be arbitrary if Z=0
	      IF( Z .NE. 0.0 ) THEN
		Z = 1.0 / Z
		C = F * Z
		S = H * Z
	      END IF
	      F = C * G + S * Y
	      X = -S * G + C * Y
	      DO N = 1, SIZE_X
		Y = MATRIX( N, J )
		Z = MATRIX( N, I )
		MATRIX( N, J ) = Y * C + Z * S
		MATRIX( N, I ) = -Y * S + Z * C
	      END DO
	    END DO
	    RV1( L ) = 0.0
	    RV1( K ) = F
	    W( K ) = X
	  END DO
300	 CONTINUE
	END DO

99      CONTINUE

	END

************************************************************************

	SUBROUTINE SV_BCSB( U, W, V, DIM_X, DIM_Y, SIZE_X, SIZE_Y, B )

*+
*  Name :
*     SV_BCSB
*
*  Purpose :
*     {routine_purpose}...
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL SV_BCSB( U, W, V, DIM_X, DIM_Y, SIZE_X, SIZE_Y, B )
*
*  Description :
*     {routine_description}...
*
*  Arguments :
*     {arguement_description}...
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     KM: Koji Mukai (Oxford University)
*     AA: Alasdair Allan (Starlink, Keele University)
*     {enter_new_authors_here}
*
*  History :
*     08-FEB-1999
*        Cut and hack for Starlink
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

	IMPLICIT NONE

	INTEGER BUFF_SIZE
	PARAMETER( BUFF_SIZE = 32 )

	INTEGER DIM_X, DIM_Y	! Declared sizes of the arrays
	REAL U( DIM_X, DIM_Y )	! U, W, V as returned by SVD_COMP
	REAL W( DIM_Y )
	REAL V( DIM_Y, DIM_Y )
	INTEGER SIZE_X, SIZE_Y	! Actual sizes used
	REAL B( DIM_X )		! INPUT: B as in A.X=B (first size_X elements)
				! OUTPUT: X (first size_Y elements)

	REAL SUM, WORK( BUFF_SIZE ), MAX_W, MIN_W
	INTEGER I, J, K

*    ZERO the small W's

	MAX_W = W( 1 )
	DO J = 2, SIZE_Y
	  MAX_W = MAX( MAX_W, W( J ) )
	END DO
	MIN_W = MAX_W * 1.0E-06
	DO J = 1, SIZE_Y
	  IF( W( J ) .LT. MIN_W ) W( J ) = 0.0
	END DO

	DO J = 1, SIZE_Y
	  SUM = 0.0
	  IF( W( J ) .NE. 0.0 ) THEN
	    DO I = 1, SIZE_X
	      SUM = SUM + U( I, J ) * B( I )
	    END DO
	    SUM = SUM /  W( J )
	  END IF
	  WORK( J ) = SUM
	END DO

	DO J = 1, SIZE_Y
	  SUM = 0.0
	  DO K = 1, SIZE_Y
	    SUM = SUM + V( J, K ) * WORK( K )
	  END DO
	  B( J ) = SUM
	END DO

	END

************************************************************************

	SUBROUTINE SVD_REC( U, W, V, DIM_X, DIM_Y, SIZE_X, SIZE_Y )

*+
*  Name :
*     SVD_REC
*
*  Purpose :
*     {routine_purpose}...
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL SVD_REC( U, W, V, DIM_X, DIM_Y, SIZE_X, SIZE_Y )
*
*  Description :
*     {routine_description}...
*
*  Arguments :
*     {arguement_description}...
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     KM: Koji Mukai (Oxford University)
*     AA: Alasdair Allan (Starlink, Keele University)
*     {enter_new_authors_here}
*
*  History :
*     08-FEB-1999
*        Cut and hack for Starlink
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

	IMPLICIT NONE

	INTEGER BUFF_SIZE
	PARAMETER( BUFF_SIZE = 32 )

	INTEGER DIM_X, DIM_Y	! Declared sizes of the arrays
	REAL U( DIM_X, DIM_Y )	! U, W, V as returned by SVD_COMP
	REAL W( DIM_Y )		! U is replaced by the product Ut.W-1.Vt
	REAL V( DIM_Y, DIM_Y )
	INTEGER SIZE_X, SIZE_Y	! Actual sizes used

	REAL WORK( BUFF_SIZE, BUFF_SIZE ), MAX_W, MIN_W
	INTEGER I, J, K

*    Handle the small W's

    	MAX_W = W( 1 )
    	DO J = 2, SIZE_Y
    	  MAX_W = MAX( MAX_W, W( J ) )
    	END DO
    	MIN_W = MAX_W * 1.0E-06
    	DO J = 1, SIZE_Y
    	  W( J ) = 1.0 / MAX( W( J ), MIN_W )
    	END DO

    	DO I = 1, SIZE_X
    	  DO J = 1, SIZE_Y
    	    WORK( I, J ) = 0.0
    	    DO K = 1, SIZE_Y
    	      WORK( I, J ) =
     :           WORK( I, J ) + V( J, K ) * W( K ) * U( I, K )
    	    END DO
    	  END DO
    	END DO

    	DO I = 1, SIZE_X
    	  DO J = 1, SIZE_Y
    	    U( I, J ) = WORK( I, J )
    	  END DO
    	END DO

*    Now answer(1...dimy)=sum(u(i,j)*question(i)),i=1...dimx

	END
