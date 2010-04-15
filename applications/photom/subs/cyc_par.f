************************************************************************

	INTEGER FUNCTION CYC_PAR( N, LO, HI )
*+
*  Name :
*     CYC_PAR
*
*  Purpose :
*     {routine_purpose}...
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL CYC_PAR( N, LO, HI )
*
*  Description :
*     {routine_description}
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
*        STATUS so that messages are output. Increase output buffer size
*        to stop overwrites.
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
	PARAMETER( N_LARGE = -1.0E+36, P_LARGE = 1.0E+36 )

	INTEGER N	! Select the relevant parameter
	REAL LO, HI	! Specify the range for cyclic behaviour

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

        CHARACTER TEXT * 80
        INTEGER STATUS

	STATUS = SAI__OK

	IF( HI .LE. LO ) THEN
	  TEXT = 'ERROR > Range of cyclic behaviour is not positive'
	  CALL MSG_OUT(' ', TEXT, STATUS)
	  CYC_PAR = -1
	ELSE IF( N .LE. 0 .OR. N .GT. M_PAR ) THEN
 	  TEXT = 'ERROR > A non-existant parameter has been specified'
	  CALL MSG_OUT(' ', TEXT, STATUS)
          CYC_PAR = -1
        ELSE IF( LIST_CYCLIC( N ) .NE. 0 ) THEN
 	  WRITE(TEXT,'(''ERROR > Parameter'', I2,
     1	        ''  is already declared cyclic'')') N
	  CALL MSG_OUT(' ', TEXT, STATUS)
	  CYC_PAR = -1
	ELSE IF( L_INDEX( N ) .NE. N ) THEN
 	  WRITE(TEXT,'(''ERROR > Parameter'', I2,''  is linked'')') N
	  CALL MSG_OUT(' ', TEXT, STATUS)
	  CYC_PAR = -1
	ELSE IF( LO_A( N ) .NE. N_LARGE .OR. HI_A( N ) .NE. P_LARGE )
     1  THEN
	  IF( HI_A( N ) - LO_A( N ) .GE. HI - LO ) THEN
	    TEXT = 'ERROR > Conflict between LIMIT_PAR and CYC_PAR'
	    CALL MSG_OUT(' ', TEXT, STATUS)
	    CYC_PAR = -1
	  ELSE
	    DO K = 1, M_Par
	      IF( L_INDEX( K ) .eq. -N ) Then
 	        TEXT = 'ERROR > Illegal attempt to use CYC_PAR'
     1              // ' on a LINKed parameter'
	        CALL MSG_OUT(' ', TEXT, STATUS)
		CYC_PAR = -1
		RETURN
	      ELSE IF( L_INDEX( K ) .EQ. N .AND. K .NE. N ) THEN
		LIST_CYCLIC( K ) = -1
		TAB_BOTTOM( K ) = LO
		TAB_PERIOD( K ) = HI - LO
	      END IF
	    END DO
 	    TEXT = 'ERROR > This parameter has already been LIMITed'
	    CALL MSG_OUT(' ', TEXT, STATUS)
	    LIST_CYCLIC( N ) = -1
	    TAB_BOTTOM( N ) = LO
	    TAB_PERIOD( N ) = HI - LO
	    CYC_PAR = 0
	  END IF
	ELSE
	  DO K = 1, M_PAR
	    IF( L_INDEX( K ) .EQ. -N ) THEN
 	      TEXT = 'ERROR > This parameter has already been LIMITed'
	      CALL MSG_OUT(' ', TEXT, STATUS)
	      CYC_PAR = -1
	      RETURN
	    ELSE IF( L_INDEX( K ) .EQ. N .AND. K .NE. N ) THEN
	      LIST_CYCLIC( K ) = -1
	      TAB_BOTTOM( K ) = LO
	      TAB_PERIOD( K ) = HI - LO
	      LO_A( K ) = LO - TAB_PERIOD( K ) * 0.25 + L_OFFSET( K )
	      HI_A( K ) = LO + TAB_PERIOD( K ) * 1.25 + L_OFFSET( K )
	    END IF
	  END DO
	  LIST_CYCLIC( N ) = 1
	  TAB_BOTTOM( N ) = LO
	  TAB_PERIOD( N ) = HI - LO
	  LO_A( N ) = LO - TAB_PERIOD( N ) * 0.25
	  HI_A( N ) = LO + TAB_PERIOD( N ) * 1.25
	  CYC_PAR = 1
	END IF

	END
