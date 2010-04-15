************************************************************************
	SUBROUTINE GAUSS_JORDAN( MATRIX, DIM, ORDER, VECTOR, FLAG )
*+
*  Name :
*     GAUSS_JORDAN
*
*  Purpose :
*     {routine_purpose}...
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL GAUSS_JORDAN( MATRIX, DIM, ORDER, VECTOR, FLAG )
*
*  Description :
*     Solves the linear equation Matrix.Answer=Vector
*     and replaces Vector by Answer, and Matrix by its inverse.
*     Matrix is declared as Dim by Dim, but only Order by Order
*     is used. Vector is of size Order.
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
*     07-JUL-1987 (KM):
*         Original Version
*     01-AUG-1989 (KM):
*        Heavy modifications
*     12-FEB-1999 (AA):
*        Cut and hack for Starlink
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

	IMPLICIT NONE

	INTEGER BUFF_SIZE	! Maximum size of matrix expected
	PARAMETER( BUFF_SIZE = 32 )

	INTEGER DIM			! Declared size
	INTEGER ORDER			! Used size
	REAL MATRIX( DIM, DIM )		! Matrix of interest
	REAL VECTOR( DIM )		! Vector of interest
	Integer Flag			! -1 if singular

	INTEGER PIVOT( BUFF_SIZE )	! Bookkeeping
	INTEGER R_INDEX( BUFF_SIZE )    ! variables for
	INTEGER C_INDEX( BUFF_SIZE )	! pivoting

	REAL BIG, INV_PIVOT, WORK
	INTEGER ROW, COL, I, J, K, L, M

	IF( ORDER .GT. BUFF_SIZE )
     1		STOP 'CRUSH gaussj.f > Array too large in GAUSS_JORDAN'
	FLAG = 0

	DO J = 1, ORDER
	  PIVOT( J ) = 0
	END DO

*    Main loop over the columns to be reduced

	DO I = 1, ORDER
	  BIG = 0.0

*    Outer loop to search for a pivot element

	  DO J = 1, ORDER
	    IF( PIVOT( J ) .NE. 1 ) THEN
	      DO K = 1, ORDER
		IF( PIVOT( K ) .EQ. 0 ) THEN
		  IF( ABS( MATRIX( J, K ) ) .GE. BIG ) THEN
		    BIG = ABS( MATRIX( J, K ) )
		    ROW = J
		    COL = K
		  END IF
		ELSE IF( PIVOT( K ) .GT. 1 ) THEN
		  FLAG = -1
		  RETURN
		END IF
	      END DO
	    END IF
	  END DO
	  PIVOT( COL ) = PIVOT( COL ) + 1


*    We now have the pivot element, so exchange rows to put the
*    pivot element on the diagonal.  Changes are labeled using
*    *_Index arrays (not actually interchanged).

	  IF( ROW .NE. COL ) THen
	    DO L = 1, ORDER
	      WORK = MATRIX( ROW, L )
	      MATRIX( ROW, L ) = MATRIX( COL, L )
	      MATRIX( COL, L ) = WORK
	    END DO
	    WORK = VECTOR( ROW )
	    VECTOR( ROW ) = VECTOR( COL )
	    VECTOR( COL ) = WORK
	  END IF
	  R_INDEX( I ) = ROW    ! Now ready to divide the pivot row
	  C_INDEX( I ) = COL	! by the pivot element
	  IF( MATRIX( COL, COL ) .LT. 2.0E-38 ) THEN	!New - protect
	    FLAG = -1
	    RETURN
	  END IF
	  INV_PIVOT = 1.0 / MATRIX( COL, COL )
	  MATRIX( COL, COL ) = 1.0
	  DO L = 1, ORDER
	    MATRIX( COL, L ) = MATRIX( COL, L ) * INV_PIVOT
	  END DO
	  VECTOR( COL ) = VECTOR( COL ) * INV_PIVOT

*    Next, reduce the rows

	  DO M = 1, ORDER
	    IF( M .NE. COL ) THEN	! ...except for the pivot one
	      WORK = MATRIX( M, COL )
	      MATRIX( M, COL ) = 0.0
	      DO L = 1, ORDER
		MATRIX( M, L )
     1			= MATRIX( M, L ) - MATRIX( COL, L ) * WORK
	      END DO
	      VECTOR( M ) = VECTOR( M ) - VECTOR( COL ) * WORK
	    END IF
	  END DO
	END DO		! Endof the main loop over columns

*    Now unscramble the solution

	DO L = ORDER, 1, -1
	  IF( R_INDEX( L ) .NE. C_INDEX( L ) ) THEN
	    DO K = 1, ORDER
	      WORK = MATRIX( K, R_INDEX( L ) )
	      MATRIX( K, R_INDEX( L ) ) = MATRIX( K, C_INDEX( L ) )
	      MATRIX( K, C_INDEX( L ) ) = WORK
	    END DO
	  END IF
	END DO

	END
