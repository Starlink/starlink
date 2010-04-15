************************************************************************

	INTEGER FUNCTION LIMIT_PAR( N, LO, HI )
*+
*  Name :
*     LIMIT_PAR
*
*  Purpose :
*     {routine_purpose}...
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL LIMIT_PAR( N, LO, HI )
*
*  Description :
*     Set up the limit within which parameter A( N ) must change
*     It does NOT check if the current value of A( N ) is within the bound.
*     The default values are +/-1.0E+36.
*
*  Arguments :
*     N = INTEGER (Given)
*         Parameter of Interest
*     LO = REAL (Given)
*         Lower Boundary
*     HI = REAL (Given)
*         Upper Boundary
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
*     08-FEB-1999
*        Cut and hack for Starlink
*     21-FEB-2008 (PWD):
*        Stop using internal writes to copy constant strings. Initialise
*        STATUS so that messages are output.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER M_PAR
	PARAMETER( M_PAR = 32 )

	INTEGER N	! Parameter of interest
        REAL LO         ! Lower boundary
	REAL HI		! Upper boundary

	INTEGER K

	REAL LO_A( M_PAR ), HI_A( M_PAR )
	INTEGER LIST_CYCLIC( M_PAR )
	REAL TAB_PERIOD( M_PAR )
	REAL TAB_BOTTOM( M_PAR )
	INTEGER L_INDEX( M_PAR )
	REAL L_OFFSET( M_PAR )
	COMMON / BOUNDARY_PAR / LO_A, HI_A
	COMMON / CYCLIC_LIST / LIST_CYCLIC, TAB_PERIOD, TAB_BOTTOM
	COMMON / LINK_STATUS / L_INDEX, L_OFFSET

        CHARACTER TEXT * 72
        INTEGER STATUS

	STATUS = SAI__OK

	IF( LO .GE. HI ) THEN

	  TEXT = 'ERROR > Upper limit is smaller than lower limit'
	  CALL MSG_OUT(' ', TEXT, STATUS)
 	  TEXT = '        This call to LIMIT_PAR is ignored'
	  CALL MSG_OUT(' ', TEXT, STATUS)

	  LIMIT_PAR = -1
	ELSE IF( N .GE. 1 .AND. N .LE. M_PAR ) THEN
	  IF( LIST_CYCLIC( N ) .NE. 0 ) THEN	! Cyclic
	    IF( L_INDEX( N ) .NE. N ) THEN

	      TEXT = 'ERROR > LIMIT_PAR says you can not do this!'
	      CALL MSG_OUT(' ', TEXT, STATUS)

	      LIMIT_PAR = -1
	      RETURN
	    ELSE IF( LO .GE. TAB_BOTTOM( N )
     1		.AND. HI .LT. TAB_BOTTOM( N ) + TAB_PERIOD( N ) ) THEN

	      TEXT = 'ERROR > LIMITing a cyclic parameter'
	      CALL MSG_OUT(' ', TEXT, STATUS)

	      LO_A( N ) = LO
	      HI_A( N ) = HI
	      LIST_CYCLIC( N ) = -1
	      LIMIT_PAR = 0
	    ELSE
	      IF( HI - LO .LT. TAB_PERIOD( N ) ) THEN
		TEXT = 'ERROR > LIMITing a cyclic parameter'
	        CALL MSG_OUT(' ', TEXT, STATUS)
		LO_A( N ) = LO
		HI_A( N ) = HI
		LIST_CYCLIC( N ) = -1
	        LIMIT_PAR = 0
	      ELSE

		TEXT =  'ERROR > Illegal attempt to LIMIT a '//
     :                  'cyclic parameter'
	        CALL MSG_OUT(' ', TEXT, STATUS)

		LIMIT_PAR = -1
	      END IF
	    END IF
	    DO K = 1, N
	      IF( L_INDEX( K ) .EQ. N .AND. K .NE. N ) THEN
		LO_A( K ) = LO_A( N ) + L_OFFSET( K )
		HI_A( K ) = HI_A( N ) + L_OFFSET( K )
	      END IF
	    END DO
	  ELSE
	    DO K = 1, N
	      IF( L_INDEX( K ) .EQ. -N ) THEN
		LO_A( K ) = LO_A( N ) * L_OFFSET( K )
		HI_A( K ) = HI_A( N ) * L_OFFSET( K )
	      ELSE IF( L_INDEX( K ) .EQ. N .AND. K .NE. N ) THEN
		LO_A( K ) = LO_A( N ) + L_OFFSET( K )
		HI_A( K ) = HI_A( N ) + L_OFFSET( K )
	      END IF
	    END DO
	    LO_A( N ) = LO
	    HI_A( N ) = HI
	    LIMIT_PAR = 1
	  END IF
	ELSE
	  WRITE(TEXT, '(''ERROR > No such parameter #'', I2)') N
	  CALL MSG_OUT(' ', TEXT, STATUS)
 	  TEXT = '        This call to LIMIT_PAR is ignored'
	  CALL MSG_OUT(' ', TEXT, STATUS)

	  LIMIT_PAR = -1
	END IF

	END


************************************************************************

	INTEGER FUNCTION NOLIMIT_PAR( N )

*+
*  Name :
*     NOLIMIT_PAR
*
*  Purpose :
*     {routine_purpose}...
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL NOLIMIT_PAR(N)
*
*  Description :
*     {routine_description}...
*
*  Arguments :
*     N = INTEGER (Given)
*         Parameter of Interest
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

	INCLUDE 'SAE_PAR'

	INTEGER M_PAR
	REAL N_LARGE, P_LARGE
	PARAMETER( M_PAR = 32 )
	PARAMETER( N_LARGE = -1.0E+36 )
	PARAMETER( P_LARGE = +1.0E+36 )

	INTEGER N	! Parameter of interest

        INTEGER K

        REAL LO_A( M_PAR ), HI_A( M_PAR )
        INTEGER LIST_CYCLIC( M_PAR )
        REAL TAB_PERIOD( M_PAR )
        REAL TAB_BOTTOM( M_PAR )
        INTEGER L_INDEX( M_PAR )
        REAL L_OFFSET( M_PAR )
        COMMON / BOUNDARY_PAR / LO_A, HI_A
        COMMON / CYCLIC_LIST / LIST_CYCLIC, TAB_PERIOD, TAB_BOTTOM
        COMMON / LINK_STATUS / L_INDEX, L_OFFSET

        CHARACTER TEXT * 72
        INTEGER STATUS

	STATUS = SAI__OK

        IF( N .GE. 1 .AND. N .LE. M_PAR ) THEN
          IF( LIST_CYCLIC( N ) .EQ. 1 ) THEN
 	    WRITE(TEXT, '(''ERROR >  No limit has been set '
     1		 //   'for cyclic parameter'', I3)') N
	    CALL MSG_OUT(' ', TEXT, STATUS)

	    NOLIMIT_PAR = -1
	  ELSE IF( LIST_CYCLIC( N ) .EQ. 0 ) THEN
	    LO_A( N ) = N_LARGE
	    HI_A( N ) = P_LARGE
	    DO K = 1, M_PAR
	      IF( L_INDEX( K ) .EQ. N .AND. K .NE. N ) THEN
		LO_A( K ) = LO_A( N ) + L_OFFSET( K )
		HI_A( K ) = HI_A( N ) + L_OFFSET( K )
	      ELSE IF( L_INDEX( K ) .EQ. -N ) THEN
		LO_A( K ) = LO_A( N ) * L_OFFSET( K )
		HI_A( K ) = HI_A( N ) * L_OFFSET( K )
	      END IF
	    END DO
	    NOLIMIT_PAR = 1
	  ELSE		! Limited cyclic parameter
	    IF( L_INDEX( N ) .EQ. N ) THEN
	      LO_A( N ) = TAB_BOTTOM( N ) - TAB_PERIOD( N ) * 0.25
	      HI_A( N ) = TAB_BOTTOM( N ) + TAB_PERIOD( N ) * 1.25
	      DO K = 1, M_PAR
		IF( L_INDEX( K ) .EQ. N .AND. K .NE. N ) THEN
		  LO_A( K ) = LO_A( N ) + L_OFFSET( K )
		  HI_A( K ) = HI_A( N ) + L_OFFSET( K )
		END IF
	      END DO
	    ELSE
 	      TEXT = 'ERROR >  Illegal attempt on a cyclic'//
     :               ' slave parameter'
	      CALL MSG_OUT(' ', TEXT, STATUS)


	      NOLIMIT_PAR = -1
	      RETURN
	    END IF
	    LIST_CYCLIC( N ) = 1
	    NOLIMIT_PAR = 1
	  END IF
	ELSE IF( N .EQ. 0 ) THEN
	  NOLIMIT_PAR = 0
	  DO K = 1, M_PAR
	    IF( LIST_CYCLIC( K ) .EQ. 1 ) THEN
 	      WRITE(TEXT, '(''ERROR >  No limit has been set '//
     1		          'for cyclic parameter'', I3)') K
	      CALL MSG_OUT(' ', TEXT, STATUS)

	      NOLIMIT_PAR = -1
	    ELSE IF( LIST_CYCLIC( K ) .EQ. 0 ) THEN
	      IF( L_INDEX( K ) .EQ. K ) THEN
		LO_A( K ) = N_LARGE
		HI_A( K ) = P_LARGE
	      ELSE IF( L_INDEX( K ) .GT. 0 ) THEN
		LO_A( K ) = LO_A( L_INDEX( K ) ) + L_OFFSET( K )
		HI_A( K ) = HI_A( L_INDEX( K ) ) + L_OFFSET( K )
	      ELSE
		LO_A( K ) = LO_A( -L_INDEX( K ) ) * L_OFFSET( K )
		HI_A( K ) = HI_A( -L_INDEX( K ) ) * L_OFFSET( K )
	      END IF
	    ELSE
	      IF( L_INDEX( K ) .EQ. K ) THEN
		LO_A( K ) = TAB_BOTTOM( K ) - TAB_PERIOD( K ) * 0.25
		HI_A( K ) = TAB_BOTTOM( K ) + TAB_PERIOD( K ) * 1.25
	      ELSE
		LO_A( K ) = TAB_BOTTOM( L_INDEX( K ) )
     1			- TAB_PERIOD( L_INDEX( K ) ) * 0.25 + L_OFFSET(K)
		HI_A( K ) = TAB_BOTTOM( L_INDEX( K ) )
     1			+ TAB_PERIOD( L_INDEX( K ) ) * 1.25 + L_OFFSET(K)
	      END IF
	      LIST_CYCLIC( K ) = 1
	    END IF
	  END DO
	ELSE
 	  WRITE(TEXT, '(''ERROR > No such parameter # '', I3)') N
	  CALL MSG_OUT(' ', TEXT, STATUS)
 	  TEXT = '        This call to NOLIMIT_PAR is ignored'
	  CALL MSG_OUT(' ', TEXT, STATUS)

	  NOLIMIT_PAR = -1
	END IF

	END
