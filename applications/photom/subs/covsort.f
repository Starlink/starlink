************************************************************************
	SUBROUTINE COV_SORT( COVAR, DIM_C, N_PAR, N_TERM )

*+
*  Name :
*     COV_SORT
*
*  Purpose :
*     {routine_purpose}...
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL COV_SORT( COVAR, DIM_C, N_PAR, N_TERM )
*
*  Description :
*     Repacks the covariance matrix into convenient form
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

	INTEGER M_PAR
	PARAMETER( M_PAR = 32 )

	INTEGER DIM_C
	REAL COVAR( DIM_C, DIM_C )
	INTEGER N_PAR
	INTEGER N_TERM

	REAL WORK
	INTEGER I, J, K, L

	INTEGER INDEX( M_PAR )
	INTEGER TABLE_FREE( M_PAR )
	COMMON / FREE_LIST / INDEX, TABLE_FREE

*     Zero all elemsnts below diagonal

	DO J = 1, N_PAR - 1
	  DO I = J + 1, N_PAR
	    COVAR( I, J ) = 0.0
	  END DO
	END DO

*    Repack off-diagnoal to correct location

	DO I = 1, N_TERM - 1
	  DO J = I + 1, N_TERM
	    K = INDEX( I )
	    L = INDEX( J )
	    IF( L .GT. K ) THEN
	      COVAR( L, K ) = COVAR( I, J )
	    ELSE
	      COVAR( K, L ) = COVAR( I, J )
	    END IF
	  END DO
	END DO

*    Swap diagnoals using the top row

	WORK = COVAR( 1, 1 )
	DO J = 1, N_PAR
	  COVAR( 1, J ) = COVAR( J, J )
	  COVAR( J, J ) = 0.0
	END DO
	K = INDEX( 1 )
	COVAR( K, K ) = WORK
	DO J = 2, N_TERM
	  K = INDEX( J )
	  COVAR( K, K ) = COVAR( 1, J )
	END DO

*    Finally, fill in above diagonals

	DO J = 2, N_PAR
	  DO I = 1, J - 1
	    COVAR( I, J ) = COVAR( J, I )
	  END DO
	END DO

	END
