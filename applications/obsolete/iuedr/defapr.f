      SUBROUTINE DEFAPR( IRES, IAPER, STATUS )
*+
*  Name:
*     SUBROUTINE DEFAPR

*  Purpose:
*     Get selected aperture.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DEFAPR( IRES, IAPER, STATUS )

*  Arguments:
*     IRES = INTEGER (Given)
*        The current resolution.
*     IAPER = INTEGER (Returned)
*        The aperture selected.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     If IRES > 0 then it is compared to the actual resolution for a check.
*     The APERTURE parameter is read and compared to the available list.
*     The index of a selected aperture is returned.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     04-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     30-SEP-94 (MJC):
*       IUEDR Vn. 3.1-6
*     20-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER IRES       ! Resolution index.

*  Arguments Returned:
      INTEGER IAPER      ! Aperture index.

*  Status:
      INTEGER STATUS     ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'

*  Local Variables:
      BYTE APR( 16 )     ! Local aperture string.

      INTEGER ACTVAL     ! Parameter value count.
      INTEGER ICAM       ! Camera index.
      INTEGER JRES       ! Resolution index.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that there is a dataset.
      IF ( NOHEAD ) THEN
         CALL ERROUT( 'Error: DATASET undefined\\', STATUS )
         GO TO 999
      END IF

*  RESOLUTION.
      IF ( IRES .GT. 0 ) THEN
         CALL IUE_RESN( RESOL, JRES, STATUS )
         IF ( STATUS .NE. SAI__OK) THEN
            CALL ERROUT('Error: RESOLUTION undefined\\', STATUS )
            GO TO 999

         ELSE IF ( IRES .NE. JRES ) THEN
            CALL ERROUT( 'Error: RESOLUTION invalid\\', STATUS )
            GO TO 999
         END IF

      END IF

*  APERTURE.
      IF ( NAPER .LT. 1 ) THEN
         CALL ERROUT( 'Error: no apertures defined\\', STATUS )
         GO TO 999

      ELSE IF ( NAPER .EQ. 1 ) THEN
         IAPER = 1
         GO TO 999

      ELSE
         DO WHILE ( .TRUE. )
            CALL RDPARC( 'APERTURE\\', .FALSE., 16, APR, ACTVAL,
     :                   STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PARFER( 'APERTURE\\', STATUS )
               GO TO 999
            END IF

            CALL FNAPER( APR, IAPER )
            IF ( IAPER .GT. 0 ) THEN
               GO TO 100
            END IF

            CALL ERRPAR( 'APERTURE\\' )
            CALL ERROUT( ': invalid\\', STATUS )

            CALL CNPAR( 'APERTURE\\', STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PCANER( 'APERTURE\\', STATUS )
               GO TO 999
            END IF
         END DO
 100     CONTINUE
      END IF

*  CAMERA.
      CALL IUE_CAMN( CAMERA, ICAM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: CAMERA undefined\\', STATUS )
      END IF

 999  CONTINUE

      END
