      SUBROUTINE RFGEOM( STATUS )
*+
*   Name:
*      SUBROUTINE RFGEOM
*
*   Description:
*      The Goemetry Parameters are defined through the NGEOM parameter.
*
*   History:
*      Jack Giddings      01-MAY-82     AT4 version
*      Paul Rees          06-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     05-DEC-94     IUEDR Vn. 3.2
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Export:
      INTEGER STATUS     ! status return

*   Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMGEOM'

*   Local variables:
      INTEGER ACTVAL     ! parameter value count
      INTEGER NGEOM      ! polynomial order for geometric correction
*.

*   GEOMETRY
      IF ( .NOT. GEOM ) THEN
         DO WHILE ( .TRUE. )
            NGEOM = 5
            CALL RDPARI( 'NGEOM\\', .TRUE., 1, NGEOM, ACTVAL, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PARFER( 'NGEOM\\', STATUS )
               RETURN

            ELSE IF ( NGEOM.LT.2 .OR. NGEOM.GT.6 ) THEN
               CALL ERRPAR( 'NGEOM\\' )
               CALL ERROUT( ': invalid\\', STATUS )

            ELSE
               NGTERM( 1 ) = NGEOM
               NGTERM( 2 ) = NGEOM
               GO TO 100
            END IF

            CALL CNPAR( 'NGEOM\\', STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PCANER( 'NGEOM\\', STATUS )
               RETURN
            END IF

         END DO
 100     CONTINUE
      END IF

      END
