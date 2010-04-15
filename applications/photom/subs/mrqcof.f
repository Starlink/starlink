************************************************************************

	SUBROUTINE MRQ_COF( X, Y, W, POINTS, A, DA, N_PAR, N_TERM,
     1			    ALPHA, BETA, DIM_A, CHI, FUNCTN, FDERIV )

*+
*  Name :
*     MRQ_COF
*
*  Purpose :
*     {routine_purpose}...
*
*  Language :
*     FORTRAN
*
*  Invocation :
*      CALL MRQ_COF( X, Y, W, POINTS, A, DA, N_PAR, N_TERM,
*     1		     ALPHA, BETA, DIM_A, CHI, FUNCTN, FDERIV )
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
*     08-JUL-1987 (KM):
*        Original version
*     10-FEB-1999 (AA):
*        Cut and hack for Starlink
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

	IMPLICIT NONE

	INTEGER M_PAR
	PARAMETER( M_PAR = 32 )

	INTEGER POINTS
	REAL X( POINTS )
	REAL Y( POINTS )
	REAL W( POINTS )
	INTEGER N_PAR
	REAL A( N_PAR )
	REAL DA( N_PAR )
	INTEGER N_TERM
	INTEGER DIM_A
	REAL ALPHA( DIM_A, DIM_A )
	REAL BETA( N_PAR )
	REAL CHI
	REAL FUNCTN
	EXTERNAL FUNCTN, FDERIV

	REAL DF_DA( M_PAR )
	REAL DY, WORK
	INTEGER I, J, K

	INTEGER INDEX( M_PAR )
	INTEGER TABLE_FREE( M_PAR )
	INTEGER L_INDEX( M_PAR )
	REAL L_OFFSET( M_PAR )
	LOGICAL CYCLIC_DATA
	REAL CYCLE_DATA
	COMMON / FREE_LIST / INDEX, TABLE_FREE
	COMMON / LINK_STATUS / L_INDEX, L_OFFSET
	COMMON / DATA_CYCLIC / CYCLIC_DATA, CYCLE_DATA

*    Initialize (symmetric) Alpha, Beta

	DO I = 1, N_TERM
	  DO J = 1, I
	    ALPHA( I, J ) = 0.0
	  END DO
	  BETA( I ) = 0.0
	END DO
	CHI = 0.0

*    Summation loop over all data

	DO K = 1, POINTS
	  IF( W( K ) .GT. 0.0 ) THEN
	    DY = Y( K ) - FUNCTN( X( K ), A, N_PAR )
	    IF( CYCLIC_DATA ) THEN
	      DY = DY - NINT( DY / CYCLE_DATA ) * CYCLE_DATA
	    END IF
	    CALL FDERIV( X( K ), A, DA, N_PAR, DF_DA )
	    DO I = 1, N_PAR
	      IF( TABLE_FREE( I ) .EQ. -1 ) THEN
		J = L_INDEX( I )
		DF_DA( J ) = DF_DA( J ) + DF_DA( I )
	      ELSE IF( TABLE_FREE( I ) .EQ. -2 ) THEN
		J = -L_INDEX( I )
		DF_DA( J ) = DF_DA( J ) + DF_DA( I ) * L_OFFSET( I )
	      END IF
	    END DO
	    DO I = 1, N_TERM
	      WORK = DF_DA( INDEX( I ) ) * W( K )
	      DO J = 1, I
		ALPHA( I, J ) = ALPHA( I, J ) + WORK * DF_DA( INDEX( J ) )
	      END DO
	      BETA( I ) = BETA( I ) + DY * WORK
	    END DO
	    CHI = CHI + DY * DY * W( K )
	  END IF
	END DO

*    Fill in the symmetric side

	DO J = 2, N_TERM
	  DO I = 1, J - 1
	    ALPHA( I, J ) = ALPHA( J, I )
	  END DO
	END DO

	END
