      SUBROUTINE ECH_SETUP_GRAPHICS( STATUS )
*+
*  Name:
*     ECHOMOP - ECH_SETUP_GRAPHICS

*  Purpose:
*     Set up graphics devices for use.

*  Description:
*    This routine initialises the graphics devices for soft- and
*    hard-copy plots.

*  Invocation:
*     CALL ECH_SETUP_GRAPHICS( STATUS )

*  Arguments:
*    STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     If graphics is not already set up then
*       See if image display is required
*        Yes, so we have to open the display, and also get
*        the current display parameters.
*          If NOT got imaging parameters then report problem
*          Else
*             Open image display device
*          Endif
*       Endif
*       Get zoning parameters
*       Check if hardcopy is available
*       Check for soft copy device
*       Check for special case of a NULL device
*       And if device can support a cursor
*     Endif

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL VALUE

      INTEGER ILEN
      INTEGER ISTAT
      INTEGER IXZONE
      INTEGER IYZONE
      INTEGER IDEFAULT

      CHARACTER*30 CHDEV
      CHARACTER*3 ACUR

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      INTEGER PGBEG
      INTEGER ECH_OBJ_IND
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Do nothing if graphics are already setup.
      IF ( GRAPHICS_SETUP ) THEN
         GO TO 999
      END IF

*  See if image display is required.
      CALL ECH_GET_PARAMETER( 'DISPLAY', 'LOGICAL', 0.0, IMG_DISPLAY,
     :     ' ', 0, ISTAT )
      ISTAT = 0

*  Get zoning parameters.
      IDEFAULT = DEFAULTS_INDICES(
     :           ECH_OBJ_IND( 'TUNE_XZONE' ) )
      CALL ECH_GET_PARAMETER( 'TUNE_XZONE', 'INT', VALUE,
     :     .FALSE., ' ', IDEFAULT, ISTAT )
      IXZONE = MAX( 1, INT( VALUE ) )
      IDEFAULT = DEFAULTS_INDICES(
     :           ECH_OBJ_IND( 'TUNE_XZONE' ) )
      CALL ECH_GET_PARAMETER( 'TUNE_YZONE', 'INT', VALUE,
     :     .FALSE., ' ', IDEFAULT, ISTAT )
      IYZONE = MAX( 1, INT( VALUE ) )

*  Check if hardcopy is available.
      CALL ECH_GET_PARAMETER( 'HARD', 'CHAR', 0.0,
     :     .FALSE., CHDEV, 0, ISTAT )
      HARDCOPY_DEVICE_NAME = CHDEV // ' '
      IF ( ISTAT .NE. 0 .AND. ISTAT .NE. ECH__IS_ACCESSED  ) THEN
         CALL ECH_REPORT( 0, ' No hardcopy device specified.' )
         CALL ECH_REPORT( 0,
     :        ' Use "HARD" command e.g. "HARD ps" to rectify.' )
      END IF

*  Check for soft copy device.
      CALL ECH_GET_PARAMETER( 'SOFT', 'CHAR', 0.0,
     :     .FALSE., CHDEV, 0, ISTAT )
      IF ( ISTAT .NE. 0 .AND. ISTAT .NE. ECH__IS_ACCESSED ) THEN
        CALL ECH_REPORT( 0, ' No soft-plotting device specified.' )
        CALL ECH_REPORT( 0,
     :       ' Use "SOFT" command e.g. "SOFT xw" to rectify.' )
      END IF

*  Check for special case of a NULL device.
      IF ( CHDEV( : 4 ) .EQ. 'NONE' .OR. CHDEV( : 4 ) .EQ. 'NULL'
     :     .OR. CHDEV .EQ. ' ' ) THEN
         STATUS = -1

      ELSE
         GRAPHICS_DEVICE_NAME = CHDEV // ' '
         IF ( IMG_DISPLAY ) IMG_DISPLAY_NAME = GRAPHICS_DEVICE_NAME
         IF ( IXZONE .LT. 0 ) THEN
             ISTAT = PGBEG( 0, CHDEV, 1, 1 )
             CALL PGPAP( FLOAT( ABS( IXZONE ) ), 1.0 )

         ELSE
             ISTAT = PGBEG( 0, CHDEV, IXZONE, IYZONE )
         ENDIF
         CALL PGASK( .FALSE. )
         IF ( ISTAT .EQ. 1 ) THEN
             GRAPHICS_SETUP = .TRUE.
             STATUS = 0

*         And if device can support a cursor.
             CALL PGQINF( 'CURSOR', ACUR, ILEN )
             IF ( ACUR .EQ. 'YES' ) THEN
                IPGCUR = 1

             ELSE
                IPGCUR = 0
             ENDIF

          ELSE
             STATUS = ISTAT
          ENDIF
      ENDIF

  999 CONTINUE

      END
