      SUBROUTINE GET_ORDERS( RCHECK, ORDERS, STATUS )
*+
*  Name:
*     SUBROUTINE GET_ORDERS

*  Purpose:
*     Get values for the ORDERS parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GET_ORDERS( RCHECK, ORDERS, STATUS )

*  Arguments:
*     RCHECK = LOGICAL (Given)
*        Whether a check for more than one order being supplied is made.
*     ORDERS = INTEGER( 2 ) (Returned)
*        The two order limit values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     28-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'

*  Local Constants:
      INTEGER HIORD     ! Highest acceptable order number.
      INTEGER LOORD     ! Lowest acceptable order number.
      PARAMETER ( HIORD = 125, LOORD = 65 )

*  Arguments Given:
      LOGICAL RCHECK    ! Check that more than one order is given.

*  Arguments Returned:
      INTEGER ORDERS( 2 ) ! Order range.

*  Status:
      INTEGER STATUS    ! Global status.

*  Local Variables:
      INTEGER ACTVAL    ! Number of ORDER values found.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get the ORDERS parameter.
      DO WHILE ( .TRUE. )
         CALL RDPARI( 'ORDERS\\', .FALSE., 2, ORDERS, ACTVAL, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL LINE_WRITS(
     :      '%p!  ORDERS will take the value [65,125]\\' )
            CALL PRTBUF( STATUS )
            ORDERS( 1 ) = LOORD
            ORDERS( 2 ) = HIORD
            STATUS = SAI__OK
            GO TO 999

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'ORDERS\\', STATUS )
            GO TO 999

         ELSE IF ( ACTVAL .EQ. 1 ) THEN
            IF ( RCHECK ) THEN
               CALL ERRPAR( 'ORDERS\\' )
               CALL ERROUT( ': too few values\\', STATUS )

            ELSE IF ( ORDERS( 1 ).LT.LOORD .OR.
     :                ORDERS( 1 ).GT.HIORD ) THEN
               CALL ERRPAR( 'ORDERS\\' )
               CALL ERROUT( ': out of range\\', STATUS )

            ELSE
               ORDERS( 2 ) = ORDERS( 1 )
               GO TO 999
            END IF

         ELSE IF ( ABS( ORDERS( 1 ) - ORDERS( 2 ) ).LT.1 .AND.
     :             RCHECK ) THEN
            CALL ERRPAR( 'ORDERS\\' )
            CALL ERROUT( ': insufficient range\\', STATUS )

         ELSE
            IF ( ORDERS( 1 ) .GT. ORDERS( 2 ) ) THEN
               CALL MSC_ISWAP( ORDERS( 1 ), ORDERS( 2 ) )
            END IF
            IF ( ORDERS( 1 ).GE.LOORD .AND. ORDERS( 2 ).LE.HIORD ) THEN
               GO TO 999
            END IF
            CALL ERRPAR( 'ORDERS\\' )
            CALL ERROUT( ': out of range\\', STATUS )
         END IF

         CALL CNPAR( 'ORDERS\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'ORDERS\\', STATUS )
            GO TO 999
         END IF
      END DO

 999  CONTINUE

      END
