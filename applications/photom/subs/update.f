************************************************************************

	SUBROUTINE UPDATE_PARAM( TRY_A, INCR_A, N_PAR, N_TERM )

*+
*  Name :
*     UPDATE_PARAM
*
*  Purpose :
*     {routine_purpose}...
*
*  Language :
*     FORTRAN
*
*  Invocation :
*      CALL UPDATE_PARAM( TRY_A, INCR_A, N_PAR, N_TERM )
*
*  Description :
*      Update trial parameters within FIT2D. Checks for boundaries, and
*      scales INCR_A down if necessary to keep TRY_A within a sensible range.
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
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}
*
*  History :
*     13-JUL-1987 (KM):
*        Original version
*     23-DEC-1987 (KM):
*        Revised
*     08-FEB-1999 (AA):
*        Cut and hack for Starlink
*     21-FEB-2008 (PWD):
*        Stop using internal writes to copy constant strings. Initialise
*        STATUS so that messages are output. Increase output buffer size.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :

	IMPLICIT NONE

*  Include files:
        INCLUDE 'SAE_PAR'
        INCLUDE 'MSG_PAR'

	INTEGER M_PAR
	PARAMETER( M_PAR = 32 )

*			arguments
	INTEGER N_PAR		! number of parameters
	INTEGER N_TERM		! number of free parameters
	REAL TRY_A( N_PAR )	! the current parameter values
	REAL INCR_A( N_PAR )	! to be changed by...

	REAL RATIO, M_RATIO, SCALE
	REAL LABOUR
	INTEGER J, K, L
	INTEGER R_INDEX( M_PAR )

	INTEGER INDEX( M_PAR )
	INTEGER TABLE_FREE( M_PAR )
	INTEGER L_INDEX( M_PAR )
	REAL L_OFFSET( M_PAR )
	REAL LO_A( M_PAR ), HI_A( M_PAR )
	INTEGER LIST_CYCLIC( M_PAR )
	REAL TAB_PERIOD( M_PAR )
	REAL TAB_BOTTOM( M_PAR )
	LOGICAL DEBUG
	COMMON / FREE_LIST / INDEX, TABLE_FREE
	COMMON / LINK_STATUS / L_INDEX, L_OFFSET
	COMMON / BOUNDARY_PAR / LO_A, HI_A
	COMMON / CYCLIC_LIST / LIST_CYCLIC, TAB_PERIOD, TAB_BOTTOM
	COMMON / BUG / DEBUG

	CHARACTER TEXT * ( MSG__SZMSG )

	INTEGER STATUS


*   STATUS needs initialising if we are to see any reports.
        STATUS = SAI__OK

*   Is any parameter going off-boundary?

	M_RATIO = 0.0
	DO K = 1, N_TERM		! Un-linked parameters first
	  J = INDEX( K )
	  R_INDEX( J ) = K
	  IF( INCR_A( K ) .GT. 0.0 ) THEN
	    IF( HI_A( J ) .LE. TRY_A( J ) ) THEN
	      IF( DEBUG ) THEN

	         WRITE(TEXT, '(''@ Parameter #'', I2, '' was about'
     1           // ' to go off-limit'')') J
	         CALL MSG_OUT(' ', TEXT, STATUS)
     		 WRITE(TEXT, '(''  NOT UPDATING PARAMETERS DURING'
     1           // ' THIS ITERATION'')' )
	         CALL MSG_OUT(' ', TEXT, STATUS)

	      END IF
	      RETURN
	    END IF
	    RATIO = INCR_A( K ) / ( HI_A( J ) - TRY_A( J ) )
	  ELSE IF( INCR_A( K ) .EQ. 0.0 ) THEN
	    RATIO = 0.0
	  ELSE
	    IF( LO_A( J ) .GE. TRY_A( J ) ) THEN
	      IF( DEBUG ) THEN

	         WRITE(TEXT, '(''@ Parameter #'', I2, '' was about'
     1           // ' to go off-limit'')') J
	         CALL MSG_OUT(' ', TEXT, STATUS)
     		 TEXT =
     1           '  NOT UPDATING PARAMETERS DURING THIS ITERATION'
	         CALL MSG_OUT(' ', TEXT, STATUS)

	      END IF
	      RETURN
	    END IF
	    RATIO = INCR_A( K ) / ( LO_A( J ) - TRY_A( J ) )
	  END IF
	  IF( RATIO .GE. 1.0 ) THEN
	    M_RATIO = MAX( M_RATIO, RATIO )

	    IF( DEBUG ) THEN
	         WRITE(TEXT, '(''@ Parameter #'', I2, '' was about'
     1           // ' to go off-limit'')') J
	         CALL MSG_OUT(' ', TEXT, STATUS)

	    END IF
	  END IF
	END DO
	DO K = 1, N_PAR				! Linked parameters next
	  IF( TABLE_FREE( K ) .LT. 0 ) THEN	! Linked
	    IF( TABLE_FREE( K ) .EQ. -1 ) THEN	! Mode = 0
	      LABOUR = INCR_A( R_INDEX( L_INDEX( K ) ) )
	    ELSE				! Mode = 1
	      LABOUR = INCR_A( R_INDEX( -L_INDEX( K ) ) ) * L_OFFSET( K )
	    END IF
	    IF( LABOUR .GT. 0.0 ) THEN
	      RATIO = LABOUR / ( HI_A( K ) - TRY_A( K ) )
	    ELSE IF( LABOUR .EQ. 0.0 ) THEN
	      RATIO = 0.0
	    ELSE
	      RATIO = LABOUR / ( LO_A( K ) - TRY_A( K ) )
	    END IF
	    IF( RATIO .GE. 1.0 ) THEN
	      IF( DEBUG ) THEN
	         WRITE(TEXT, '(''@ Parameter #'', I2, '' was about'
     1           // ' to go off-limit'')') K
	         CALL MSG_OUT(' ', TEXT, STATUS)
	      END IF
	      M_RATIO = MAX( M_RATIO, RATIO )
	    END IF
	  END IF
	END DO

*   Rescale INCR_A if necessary

	IF( M_RATIO .GE. 1.0 ) THEN
	  SCALE = 0.95 / M_RATIO
	  IF( DEBUG ) THEN

	         WRITE(TEXT, '(''@ Changes in trial parameters are'
     1           // ' scaled down by '', E10.3)') SCALE
	         CALL MSG_OUT(' ', TEXT, STATUS)

	  End If
	ELSE
	  SCALE = 1.0
	END IF

*   Now calculate the new trial parameters

	DO K = 1, N_TERM
	  J = INDEX( K )
	  TRY_A( J ) = TRY_A( J ) + INCR_A( K ) * SCALE
	END DO

*   Linked changes

	DO K = 1, N_PAR
	  IF( TABLE_FREE( K ) .EQ. -1 ) THEN
	    L = L_INDEX( K )
	    TRY_A( K ) = TRY_A( L ) + L_OFFSET( K )
	  ELSE  IF( TABLE_FREE( K ) .EQ. -2 ) THEN
	    L = -L_INDEX( K )
	    TRY_A( K ) = TRY_A( L ) * L_OFFSET( K )
	  END IF
	END DO

*   Actively cyclic

	DO K = 1, N_PAR
	  IF( LIST_CYCLIC( K ) .EQ. 1 ) THEN
	    IF( TRY_A( K ) .LT. TAB_BOTTOM( K ) ) THEN
	      TRY_A( K ) = TAB_BOTTOM( K ) + TAB_PERIOD( K )
     1	       + MOD( TRY_A( K ) - TAB_BOTTOM( K ), TAB_PERIOD( K ) )
	    ELSE
	      TRY_A( K ) = TAB_BOTTOM( K )
     1	      + MOD( TRY_A( K ) - TAB_BOTTOM( K ), TAB_PERIOD( K ) )
	    END IF
	  END IF
	END DO

	END
