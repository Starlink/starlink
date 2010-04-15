************************************************************************
      INTEGER FUNCTION FIX_PAR(N, NPAR, NTERM)

*+
*  Name :
*     FIX_PAR
*
*  Purpose :
*     {routine_purpose}...
*
*  Language :
*     FORTRAN
*
*  Invocation :
*      JFLAG = FIX_PAR(N, N_PAR, N_TERM)
*
*  Description :
*     {routine_description}...
*
*  Arguments :
*     N = INTEGER (Given)
*         Slave parameter
*     N_PAR = INTEGER (Given)
*         Total number of parameters
*     N_TERM = INTEGER (Given)
*         Total number of free parameters
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
*     28-JUL-1987
*        Original version written by Koji Mukai
*     04-JAN-1999
*        Cut and hack for Starlink
*     21-FEB-2008 (PWD):
*        Stop using internal writes to copy constant strings. Initialise
*        STATUS so that messages are output.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'

*  Arguments Given :

      INTEGER N
      INTEGER NPAR
      INTEGER NTERM

*  Arguments Given and Returned :

*  Arguments Returned :

*  Local Variables :

      INTEGER M_PAR
      PARAMETER( M_PAR = 32 )

      INTEGER I, J, K
      INTEGER INDEX( M_PAR )
      INTEGER TABLE_FREE( M_PAR )
      INTEGER L_INDEX( M_PAR )

      REAL L_OFFSET( M_PAR )

      COMMON / FREE_LIST / INDEX, TABLE_FREE
      COMMON / LINK_STATUS / L_INDEX, L_OFFSET
      CHARACTER TEXT * 72
      INTEGER STATUS

*.
      STATUS = SAI__OK

*   First, make sure it's not a linked parameter

      IF( L_INDEX( N ) .NE. N ) THEN

	  WRITE(TEXT, '(''ERROR > Parameter #'', I2, '' is '
     1               // ' linked'')') N
	  CALL MSG_OUT(' ', TEXT, STATUS)
 	  TEXT = '        This call to FIX_PAR is ignored'
	  CALL MSG_OUT(' ', TEXT, STATUS)

*   Error status

	  FIX_PAR = -1
	  RETURN

      END IF

      DO K = 1, NPAR

         IF( ABS( L_INDEX( K ) ) .EQ. N .AND. K .NE. N ) THEN

	    WRITE(TEXT, '(''ERROR > Parameter #'', I2, '' is '
     1               // ' linked'')') N
	    CALL MSG_OUT(' ', TEXT, STATUS)
 	    TEXT = '        This call to FIX_PAR is ignored'
	    CALL MSG_OUT(' ', TEXT, STATUS)

*   Error status

            FIX_PAR = -1
	    RETURN
         END IF

      END DO

*   Is it already fixed?

      IF( TABLE_FREE( N ) .EQ. 1 ) THEN

	  WRITE(TEXT, '(''ERROR > Parameter #'', I2, '' is '
     1               // ' already fixed'')') N
	  CALL MSG_OUT(' ', TEXT, STATUS)
 	  TEXT = '        This call to FIX_PAR is ignored'
	  CALL MSG_OUT(' ', TEXT, STATUS)

*   Warning status

          FIX_PAR = 0
          RETURN
      END IF

      TABLE_FREE( N ) = 1
      I = 1

      DO WHILE( I .LE. NTERM .AND. INDEX( I ) .NE. N )
         I = I + 1
      END DO

*   One less free parameter

      NTERM = NTERM - 1
      DO K = I, NTERM
         INDEX( K ) = INDEX( K + 1 )
      END DO

*   OK status

      FIX_PAR = 1
*

      END

************************************************************************

	INTEGER FUNCTION FREE_PAR( N, N_PAR, N_TERM )

*+
*  Name :
*     FREE_PAR
*
*  Purpose :
*      Parameter N (>0) becomes free again. Or if N=0, all parameters
*      will become free
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     JFLAG = FREE_PAR(N, N_PAR, N_TERM)
*
*  Description :
*     {routine_description}...
*
*  Arguments :
*     N = INTEGER (Given)
*         Slave parameter
*     N_PAR = INTEGER (Given)
*         Total number of parameters
*     N_TERM = INTEGER (Given)
*         Total number of free parameters
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
*        Added starlink header
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
	IMPLICIT NONE

        INCLUDE 'SAE_PAR'


	INTEGER M_PAR
	PARAMETER( M_PAR = 32 )
*
	INTEGER N
	INTEGER N_PAR
	INTEGER N_TERM

	INTEGER K

	INTEGER INDEX( M_PAR )
	INTEGER TABLE_FREE( M_PAR )
	INTEGER L_INDEX( M_PAR )
	REAL L_OFFSET( M_PAR )
	COMMON / FREE_LIST / INDEX, TABLE_FREE
	COMMON / LINK_STATUS / L_INDEX, L_OFFSET

        CHARACTER TEXT * 72
        INTEGER STATUS

*.
        STATUS = SAI__OK

	IF( N .GE. 1 .AND. N .LE. N_PAR ) THEN	! Single mode
	  IF( L_INDEX( N ) .NE. N ) THEN

	    WRITE(TEXT, '(''ERROR > Parameter #'', I2, '' is '
     1                // ' linked'')') N
	    CALL MSG_OUT(' ', TEXT, STATUS)
	    TEXT = '        Use SEPARATE_PAR to separate'
	    CALL MSG_OUT(' ', TEXT, STATUS)

	    FREE_PAR = -1
	    RETURN
	  ELSE IF( TABLE_FREE( N ) .EQ. 0 ) THEN

            WRITE(TEXT, '(''ERROR > Parameter #'', I2, '' is '
     1                // ' free'')') N
	    CALL MSG_OUT(' ', TEXT, STATUS)
	    TEXT = '        This call to FREE_PAR is ignored'
	    CALL MSG_OUT(' ', TEXT, STATUS)

	    FREE_PAR = 0
	    RETURN
	  END IF
	  N_TERM = N_TERM + 1	! One more free parameter
	  TABLE_FREE( N ) = 0
	  INDEX( N_TERM ) = N	! N must now become free
	  FREE_PAR = 1
	ELSE IF( N .EQ. 0 ) THEN		! All-clear mode
	  DO K = 1, N_PAR
	    IF( TABLE_FREE( K ) .EQ. 1 ) THEN
	      N_TERM = N_TERM + 1
	      TABLE_FREE( K ) = 0
	      INDEX( N_TERM ) = K
	    END IF
	  END DO
	  FREE_PAR = 1
	ELSE

*   Unknown mode
	    TEXT = 'ERROR > invalid arguement'
	    CALL MSG_OUT(' ', TEXT, STATUS)
	    TEXT = '        This call to FREE_PAR is ignored'
	    CALL MSG_OUT(' ', TEXT, STATUS)

	  FREE_PAR = -1
	END IF

	END
