      SUBROUTINE PIPAR( NAXIS1, NAXIS2, DZERO, DSCALE, DLIM, STATUS )
*+
*  Name:
*     SUBROUTINE PIPAR

*  Purpose:
*     Get parameters associated with image display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PIPAR( NAXIS1, NAXIS2, DZERO, DSCALE, DLIM, STATUS )

*  Arguments:
*     NAXIS1 = INTEGER (Given)
*        Size of x-axis (sample) in pixels.
*     NAXIS2 = INTEGER (Given)
*        Size of y-axis (line) in pixels.
*     DZERO = REAL*8 (Given)
*        Data zero point (intercept).
*     DSCALE = REAL*8 (Given)
*        Data scale factor (slope).
*     DLIM = INTEGER( 2 ) (Given)
*        Data value limits for the whole image.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     The parameters associated with picture display are obtained
*     from the environs. Suitable defaults are supplied as appropriate.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       AT4 version.
*     05-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*       Conversion to GKS 7.2 graphics.
*     09-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     27-JUL-94 (MJC):
*       IUEDR Vn. 3.1-2
*       SAE and PARised.
*     06-JAN-95 (MJC):
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

*  Global Variables:
      INCLUDE 'CMGRAF'
      INCLUDE 'CMIDEV'

*  Arguments Given:
      INTEGER NAXIS1      ! size of axis 1 (sample)
      INTEGER NAXIS2      ! size of axis 2 (line)

      REAL*8 DZERO        ! data zero point
      REAL*8 DSCALE       ! data scale factor

      INTEGER DLIM( 2 )   ! data limits (whole image)

*  Status:
      INTEGER STATUS      ! Global status.

*  Local Variables:
      INTEGER ACTVAL      ! parameter value count

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   XP - X-pixel limits, UNDEF means whole image.
      DO WHILE ( .TRUE. )
         CALL RDPARI( 'XP\\', .FALSE., 2, XP, ACTVAL, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL LINE_WRITS( '%p!  XP will take the value [0,0]\\' )
            CALL PRTBUF( STATUS )
            XP( 1 ) = 1
            XP( 2 ) = NAXIS1
            GO TO 200

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'XP\\', STATUS )
            GO TO 200

         ELSE IF ( ACTVAL .EQ. 1 ) THEN
            CALL ERRPAR( 'XP\\' )
            CALL ERROUT( ': too few values\\', STATUS )

         ELSE IF ( XP( 1 ).EQ.0 .AND. XP( 2 ).EQ.0 ) THEN
            STATUS = SAI__OK
            XP( 1 ) = 1
            XP( 2 ) = NAXIS1
            GO TO 200

         ELSE IF ( XP( 1 ).LT.1 .OR. XP( 1 ).GT.NAXIS1 ) THEN
            CALL ERRPAR( 'XP\\' )
            CALL ERROUT( ': illegal values\\', STATUS )

         ELSE IF ( XP( 2 ).LT.1 .OR. XP( 2 ).GT.NAXIS1 ) THEN
            CALL ERRPAR( 'XP\\' )
            CALL ERROUT( ': illegal values\\', STATUS )

         ELSE
            IF ( XP( 2 ) .LT. XP( 1 ) ) THEN
               CALL msc_ISWAP( XP( 1 ), XP( 2 ) )
            END IF
            GO TO 200
         END IF

         CALL CNPAR( 'XP\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'XP\\', STATUS )
            GO TO 200
         END IF
      END DO
 200  CONTINUE

*   Check STATUS.
      IF ( STATUS .EQ. SAI__OK ) THEN

*      YP - Y-pixel limits, UNDEF means whole image.
         DO WHILE ( .TRUE. )
            CALL RDPARI( 'YP\\', .FALSE., 2, YP, ACTVAL, STATUS )
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_FLUSH( STATUS )
               CALL LINE_WRITS( '%p!  YP will take the value [0,0]\\' )
               CALL PRTBUF( STATUS )
               YP( 1 ) = 1
               YP( 2 ) = NAXIS2
               GO TO 300

            ELSE IF ( STATUS .NE. SAI__OK ) THEN
               CALL PARFER( 'YP\\', STATUS )
               GO TO 300

            ELSE IF ( ACTVAL .EQ. 1 ) THEN
               CALL ERRPAR( 'YP\\' )
               CALL ERROUT( ': too few values\\', STATUS )

            ELSE IF ( YP( 1 ).EQ.0 .AND. YP( 2 ).EQ.0 ) THEN
               STATUS = SAI__OK
               YP( 1 ) = 1
               YP( 2 ) = NAXIS2
               GO TO 300

            ELSE IF ( YP( 1 ).LT.1 .OR. YP( 1 ).GT.NAXIS2 ) THEN
               CALL ERRPAR( 'YP\\' )
               CALL ERROUT( ': illegal values\\', STATUS )

            ELSE IF ( YP( 2 ).LT.1 .OR. YP( 2 ).GT.NAXIS2 ) THEN
               CALL ERRPAR( 'YP\\' )
               CALL ERROUT( ': illegal values\\', STATUS )

            ELSE
               IF ( YP( 2 ) .LT. YP( 1 ) ) THEN
                  CALL MSC_ISWAP( YP( 1 ), YP( 2 ) )
               END IF
               GO TO 300
            END IF

            CALL CNPAR( 'YP\\', STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PCANER( 'YP\\', STATUS )
               GO TO 300
            END IF
         END DO
 300     CONTINUE

*      Check STATUS.
         IF ( STATUS .EQ. SAI__OK ) THEN

*         ZL - intensity limits, UNDEF means use data defined limits.
            DO WHILE ( .TRUE. )
               CALL RDPARF('ZL\\', .FALSE., 2, ZLIM, ACTVAL, STATUS)
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_FLUSH( STATUS )
                  CALL LINE_WRITS(
     :            '%p!  ZL will take full-scale values\\' )
                  CALL PRTBUF( STATUS )
                  ZLIM( 1 ) = (DBLE(DLIM(1)) - 0.5) * DSCALE + DZERO
                  ZLIM( 2 ) = (DBLE(DLIM(2)) + 0.5) * DSCALE + DZERO
                  GO TO 400

               ELSE IF ( STATUS .NE. SAI__OK ) THEN
                  CALL PARFER( 'ZL\\', STATUS )
                  GO TO 400

               ELSE IF ( ACTVAL .EQ. 1 ) THEN
                  CALL ERRPAR('ZL\\')
                  CALL ERROUT(': too few values\\', STATUS)

               ELSE IF ( ZLIM(1).EQ.0.0 .AND. ZLIM(2).EQ.0.0 ) THEN
                  STATUS = SAI__OK
                  ZLIM( 1 ) = (REAL(DLIM(1)) - 0.5) * DSCALE + DZERO
                  ZLIM( 2 ) = (REAL(DLIM(2)) + 0.5) * DSCALE + DZERO
                  GO TO 400

               ELSE IF (ZLIM(1) .EQ. ZLIM(2)) THEN
                  CALL ERRPAR('ZL\\')
                  CALL ERROUT(': illegal values\\', STATUS)

               ELSE
                  GO TO 400
               END IF

               CALL CNPAR( 'ZL\\', STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL PCANER( 'ZL\\', STATUS )
                  GO TO 400
               END IF
            END DO
 400        CONTINUE

*         Check STATUS.
            IF ( STATUS .EQ. SAI__OK ) THEN

*            X-limits.
               XJST = .FALSE.
               IF ( XP( 1 ) .LE. XP( 2 ) ) THEN
                  XREV = 0
                  XLIM( 1 ) = REAL(XP(1)) - 0.5
                  XLIM( 2 ) = REAL(XP(2)) + 0.5

               ELSE
                  XREV = 1
                  XLIM( 1 ) = REAL(XP(1)) + 0.5
                  XLIM( 2 ) = REAL(XP(2)) - 0.5
               END IF

*            Y-limits.
               YJST = .FALSE.
               IF ( YP(1) .LE. YP(2) ) THEN
                  YREV = 0
                  YLIM( 1 ) = REAL(YP(1)) - 0.5
                  YLIM( 2 ) = REAL(YP(2)) + 0.5

               ELSE
                  YREV = 1
                  YLIM( 1 ) = REAL(YP(1)) + 0.5
                  YLIM( 2 ) = REAL(YP(2)) - 0.5
               END IF
            END IF
         END IF
      END IF

      END
