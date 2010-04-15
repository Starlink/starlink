************************************************************************
          SUBROUTINE MARQUARDT( X, Y, W, POINTS, LAMBDA, A, DA,
     1	             COVAR, DIM_C, CHI, N_PAR, N_TERM, FUNCTN, FDERIV )

*+
*  Name :
*     MARQUART
*
*  Purpose :
*     {routine_purpose}...
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL MARQUARDT( X, Y, W, POINTS, LAMBDA, A, DA,
*    1		     COVAR, DIM_C, CHI, N_PAR, N_TERM, FUNCTN, FDERIV )
*
*  Description :
*     {routine_description}...
*
*  Arguments :
*     POINTS = INTEGER
*       number of data points
*     X( POINTS ) = REAL
*       x-data
*     Y( POINTS ) = REAL
*       y-data
*     W( POINTS ) = REAL
*       weight for data
*     LAMBDA = REAL
*       fit control variable
*     N_PAR = INTEGER
*       Number of paras if model
*     A( N_PAR ) = REAL
*       parameters
*     DA( N_PAR ) = REAL
*       increments of paras
*     DIM_C = INTEGER
*       covar dimension
*     CHI = REAL
*       chi squared
*     N_TERM = INTEGER
*       Number of free paras
*     FUNCTN, FDERIV = EXTERNAL
*       User supplied model function and its derivative
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

	SAVE	! All local variables MUST be saved between calls

	INTEGER M_PAR
	PARAMETER( M_PAR = 32 )		! cf. mrq_cof
	INTEGER POINTS			! number of data points
	REAL X( POINTS )		! x-data
	REAL Y( POINTS )		! y-data
	REAL W( POINTS )		! weight for data
	REAL LAMBDA			! fit control variable
	INTEGER N_PAR			! # of paras if model
	REAL A( N_PAR )			! parameters
	REAL DA( N_PAR )		! increments of paras
	INTEGER DIM_C			! covar dimension
	REAL COVAR( DIM_C, DIM_C )	! covariance matrix
	REAL CHI			! chi squared
	INTEGER N_TERM			! # of free paras
	REAL FUNCTN			! model function
	EXTERNAL FUNCTN, FDERIV		! and its derivative

	REAL ALPHA( M_PAR, M_PAR )	! curvature matrix
	REAL TRY_A( M_PAR )		! trial paras
	REAL BETA( M_PAR )
	REAL INCR_A( M_PAR )
	REAL SVD_U( M_PAR, M_PAR )
	REAL SVD_W( M_PAR )
	REAL SVD_V( M_PAR, M_PAR )
	INTEGER I, J, K, FLAG
	REAL O_CHI
	INTEGER INDEX( M_PAR )
	INTEGER TABLE_FREE( M_PAR )
	REAL TAME_FACTOR
	LOGICAL DEBUG
	COMMON / FREE_LIST / INDEX, TABLE_FREE
	COMMON / SCALE_DOWN / TAME_FACTOR
	COMMON / BUG / DEBUG

*    First call: Initialize and return

	IF( LAMBDA .LT. 0.0 ) THEN
	  LAMBDA = 0.001	! Initialize Lambda

	  CALL MRQ_COF( X, Y, W, POINTS, A, DA, N_PAR, N_TERM,
     1			ALPHA, BETA, M_PAR, CHI, FUNCTN, FDERIV )

	  O_CHI = CHI
	  DO K = 1, N_PAR
	    TRY_A( K ) = A( K )
	  END DO
	  RETURN
	END IF

*    Change diagonal elements

	Do I = 1, n_Term
	  DO J = 1, N_TERM
	    COVAR( I, J ) = ALPHA( I, J )
	  END DO
	  COVAR( I, I ) = ALPHA( I, I ) * ( 1.0 + LAMBDA )
	  INCR_A( I ) = BETA( I )
	END DO
	CALL GAUSS_JORDAN( COVAR, DIM_C, N_TERM, INCR_A, FLAG )

	IF( LAMBDA .EQ. 0.0 ) THEN	! Final call...error calc only
	  IF( FLAG .NE. 0 ) THEN
	    DO I = 1, N_TERM		! Recalculate COVAR
	      DO J = 1, N_TERM
		SVD_U( I, J ) = ALPHA( I, J )
	      END DO
	      SVD_U( I, I ) = ALPHA( I, I ) * ( 1.0 + LAMBDA )
	      INCR_A( I ) = BETA( I )
	    END DO
	    CALL SVD_COMP( SVD_U, M_PAR, M_PAR, N_TERM, N_TERM, SVD_W,
     :                     SVD_V )
	    CALL SVD_REC( SVD_U, SVD_W, SVD_V, M_PAR, M_PAR, N_TERM,
     :                    N_TERM )
	    DO I = 1, N_TERM
	      DO J = 1, N_TERM
		COVAR( I, J ) = SVD_U( I, J )
	      END DO
	    END DO
	  END IF
	  CALL COV_SORT( COVAR, DIM_C, N_PAR, N_TERM )
	  RETURN
	END IF

	IF( FLAG .NE. 0 ) THEN		! Singular Matrix
	  DO I = 1, N_TERM		! Recalculate COVAR
	    DO J = 1, N_TERM
	      SVD_U( I, J ) = ALPHA( I, J )
	    END DO
	    SVD_U( I, I ) = ALPHA( I, I ) * ( 1.0 + LAMBDA )
	    INCR_A( I ) = BETA( I )
	  END DO
	  CALL SVD_COMP( SVD_U, M_PAR, M_PAR, N_TERM, N_TERM, SVD_W,
     :                   SVD_V )
	  CALL SV_BCSB
     1		( SVD_U, SVD_W, SVD_V, M_PAR, M_PAR, N_TERM, N_TERM,
     :            INCR_A )
	END IF

	DO I = 1, N_TERM
	  INCR_A( I ) = INCR_A( I ) * TAME_FACTOR
	END DO

	CALL UPDATE_PARAM( TRY_A, INCR_A, N_PAR, N_TERM )

	CALL MRQ_COF( X, Y, W, POINTS, TRY_A, DA, N_PAR, N_TERM,
     1			COVAR, INCR_A, DIM_C, CHI, FUNCTN, FDERIV )

	IF( CHI .LT. O_CHI ) THEN	! Success
	  LAMBDA = LAMBDA * 0.1
	  O_CHI = CHI
	  DO I = 1, N_TERM
	    DO J = 1, N_TERM
	      ALPHA( I, J ) = COVAR( I, J )
	    END DO
	    BETA( I ) = INCR_A( I )
	  END DO
	  DO J = 1, N_PAR
	    A( J ) = TRY_A( J )
	  END DO
	ELSE				! Failure
	  LAMBDA = LAMBDA * 100.0
	  CHI = O_CHI
	  DO I = 1, N_TERM
	    J = INDEX( I )
	    TRY_A( J ) = A( J )
	  END DO
	END IF

	END
