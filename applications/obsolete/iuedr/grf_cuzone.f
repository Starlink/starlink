      SUBROUTINE grf_CUZONE( HITS, CURMOD, HIT, X, Y, STATUS )

*+
*
*   Name:
*      SUBROUTINE grf_CUZONE
*
*   Description:
*      Zone cursor operations.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings     05-MAY-82
*         AT4 version.
*      Paul Rees         07-JAN-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*         Conversion to GKS 7.2 graphics.
*      Paul Rees         08-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*      Martin Clayton    15-SEP-94     IUEDR Vn. 3.1-3
*         SAEised
*
*   Method:
*      Get a cursor measurement from display zone.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Global constants:
      INTEGER ERR          ! error status
      PARAMETER (ERR=-3)

*   Import:
      CHARACTER HITS*(*)   ! character string of allowed hits

      INTEGER CURMOD       ! GKS cursor mode

*   Export:
      INTEGER HIT          ! returned hit key

      REAL X               ! X-axis coordinate of hit
      REAL Y               ! Y-axis coordinate of hit

      INTEGER STATUS       ! status return

*   External references:
      INTEGER CHR_LEN      ! filled character array length

      REAL snx_AGUGX       ! SNX grid-user X-axis coordinate transform
      REAL snx_AGUGY       ! SNX grid-user Y-axis coordinate transform

*   Global variables:
      INCLUDE 'CMGRAF'
      INCLUDE 'CMCOLR'

*   Local variables:
      REAL XG              ! X-axis grid coordinate
      REAL YG              ! Y-axis grid coordinate

      LOGICAL CURAV        ! SGS cursor availability

      INTEGER JINT         ! colour index
      INTEGER LOCCMOD
      INTEGER NCCHOICE

*   Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Inquire cursor availability
      CALL sgs_ICUAV( CURAV )
      IF ( .NOT. CURAV ) THEN
         CALL ERROUT( 'Error: no cursor available\\', STATUS )

      ELSE IF ( .NOT.DRAWN .AND. .NOT.IDRAWN ) THEN

*      No graph is drawn on current zone
         CALL ERROUT( 'Error: no graph drawn\\', STATUS )

      ELSE IF ( CHR_LEN(HITS) .LE. 0 ) THEN
         CALL ERROUT( 'Error: too few cursor hits\\', STATUS )

      ELSE

*      Select cursor control mode
         CALL sgs_INCHO( 1, NCCHOICE )
         IF ( NCCHOICE .LT. 2 ) THEN
           LOCCMOD = 0

         ELSE
           LOCCMOD = 1
         END IF
         CALL sgs_SELCH( LOCCMOD )

*      Set cursor hit choices
         CALL sgs_DEFCH( HITS )

*      Repeat cursor input until successful
         DO WHILE ( .TRUE. )

*         Put up cursor and get a value
            CALL grf_CURS( X, Y, HIT )

            IF ( (HIT.NE.0 .AND. LOCCMOD.EQ.0) .OR.
     :           (HIT.NE.-1 .AND. LOCCMOD.EQ.1) ) THEN

*            A successful cursor input - then get GRID values
               XG = snx_AGUGX( X )
               YG = snx_AGUGY( Y )

*            Test if cursor hit was out of range
               IF ( XG.GE.0.0 .AND. XG.LE.1.0 .AND. YG.GE.0.0
     :             .AND. YG.LE.1.0 ) THEN

*               Cursor hit was within GRID range - draw icon marker
                  CALL grf_QPALET( JINT )
                  CALL grf_PPALET( TIUSER )
                  CALL grf_GRSYMB( X, Y )
                  CALL grf_PPALET( JINT )

*               Convert return value for case of mouse input.
                  IF ( LOCCMOD .EQ. 1 ) THEN

*                   Translate keyboard keys 1, 2 and 3.
                      IF ( HIT .EQ. 18 ) THEN
                         HIT = 1
                      ELSE IF ( HIT .EQ. 19 ) THEN
                         HIT = 2
                      ELSE IF ( HIT .EQ. 20 ) THEN
                         HIT = 0

*                   Translate mouse key values.
                      ELSE IF ( HIT .EQ. 0 ) THEN
                         HIT = 2
                      ELSE IF ( HIT .EQ. -1 ) THEN
                         HIT = 0
                      END IF
                  END IF

*               Return values
                  GO TO 100
               END IF

            ELSE

*            Cursor stop requested
               GO TO 100
            END IF
         END DO

 100     CONTINUE
      END IF

      END
