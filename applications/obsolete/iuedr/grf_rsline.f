      SUBROUTINE grf_RSLINE( STATUS )

*+
*
*   Name:
*      SUBROUTINE grf_RSLINE
*
*   Description:
*      Reset line style.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings     05-MAY-82
*         AT4 version.
*      Paul Rees         14-JAN-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*         Conversion to GKS 7.2 graphics.
*      Paul Rees         09-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*
*   Method:
*      The LINE and LINEROT parameters are read. The dynamic default for
*      LINE is obtained from the current line style index. If
*      LINEROT=TRUE, then this index will be incremented in future
*      by NXLINE.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Global constants:
      INTEGER COLOUR        ! GKS colour availability (GQCF)
      INTEGER ERR           ! error status
      INTEGER MAXNAME       ! maximum length of name
      INTEGER MONOCH        ! GKS monochrome availability (GQCF)
      PARAMETER (COLOUR=1, ERR=-3, MAXNAME=16, MONOCH=0)

*   Export:
      INTEGER STATUS        ! status return

*   Global variables:
      INCLUDE 'CMGRAF'
      INCLUDE 'CMLINR'
      INCLUDE 'CMCOLR'

*   Local:
      INTEGER ISTYLE        ! initial line-style index
      INTEGER ACTVAL        ! parameter value count

      BYTE STYLE(MAXNAME)   ! line style name

      INTEGER IPALET        ! colour index
      INTEGER COLAV         ! colour availability
      INTEGER NCOLI         ! number of colours available
      INTEGER NPCOLI        ! number of preset colours available

*   For LINEROT:
      CALL RDPARL( 'LINEROT\\', .FALSE., 1, LINROT, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'LINEROT\\', STATUS )
      ELSE

*      For LINE:
         DO WHILE ( .TRUE. )
            CALL str_MOVE( LINSTY(1, LININD), MAXNAME, STYLE )
            CALL RDPARC( 'LINE\\', .TRUE., MAXNAME, STYLE, ACTVAL,
     :                   STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PARFER( 'LINE\\', STATUS )
               GO TO 100

            ELSE
               CALL CNPAR( 'LINE\\', STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL PCANER( 'LINE\\', STATUS )
                  GO TO 100
               END IF

               CALL grf_FNLINE( STYLE, ISTYLE )
               IF ( ISTYLE .LE. 0 ) THEN
                  CALL ERRPAR( 'LINE\\' )
                  CALL ERROUT( ': invalid\\', STATUS )

               ELSE
                  LININD = ISTYLE
                  GO TO 100
               END IF
            END IF
         END DO

 100     CONTINUE

*      Check STATUS
         IF ( STATUS .EQ. SAI__OK ) THEN

*         Inquire if current device is capable of drawing colours.
            CALL GQCF( DEV, STATUS, NCOLI, COLAV, NPCOLI )

*         Only do colours if the device is a colour plotter.
            IF ( COLAV.EQ.COLOUR .AND. STATUS.EQ.SAI__OK ) THEN

*            For COLROT:
               CALL RDPARL( 'COLROT\\', .FALSE., 1, TIROT, ACTVAL,
     :                      STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL PARFER( 'COLROT\\', STATUS )
                  GO TO 200
               END IF

*            For COL:
               DO WHILE ( .TRUE. )
                  IPALET = MOD(TICUR-1, TICNT) + 1
                  CALL RDPARI( 'COL\\', .TRUE., 1, IPALET, ACTVAL,
     :                         STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL PARFER( 'COL\\', STATUS )
                     GO TO 200

                  ELSE
                     CALL CNPAR( 'COL\\', STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL PARFER( 'COL\\', STATUS )
                        GO TO 200
                     END IF

                     IF ( IPALET.LE.0 .OR. IPALET.GT.TICNT ) THEN
                        CALL ERRPAR( 'COL\\' )
                        CALL ERROUT( ': invalid\\', STATUS )

                     ELSE
                        TICUR = IPALET
                        GO TO 200
                     END IF
                  END IF
               END DO

 200           CONTINUE
            END IF
         END IF
      END IF

      END
