      SUBROUTINE OUTRK( FD, STATUS )
*+
*  Name:
*     SUBROUTINE OUTRK

*  Purpose:
*     Write a binary TRAK file on open file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL OUTRK( FD, STATUS )

*  Arguments:
*     FD = INTEGER (Given)
*        I/O unit number for output to open file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     The contents of CMSAVE are written in formatted TRAK format
*     to the file described by FD.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     MBT: Mark Taylor (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     05-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     09-JUL-99 (MBT):
*       Modified for list-type carriage control.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMSAVE'

*  Arguments Given:
      INTEGER FD      ! File descriptor.

*  Status:
      INTEGER STATUS  ! Global status.

*  External References:
      INTEGER DQ_AND  ! Data quality AND.

*  Local Variables:
      REAL*8 SCALE    ! NET scaling factor.

      LOGICAL DONE    ! Whether found max/min value.

      BYTE LINE( 80 ) ! Text line temporary.

      INTEGER BUF( 1200, 4 ) ! Output buffer for TRAK.
      INTEGER I       ! Loop index.
      INTEGER IAPR    ! Aperture index.
      INTEGER ICAM    ! Camera index.
      INTEGER IORD    ! Order index.
      INTEGER IREC    ! Record count.
      INTEGER IRES    ! Resolution index.
      INTEGER ISEC    ! Exposure time.
      INTEGER J       ! Loop index.
      INTEGER POS     ! Character position.
      INTEGER IOSTAT  ! Local status.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Camera,Image.
      POS = 0
      CALL STR_WRITS( '%p%s\\', CAMERA, 80, LINE, POS )
      CALL STR_WRITI( '%i\\', IMAGE, 80, LINE, POS )
      CALL STR_WRITS( ': %s\\', TITLE, 80, LINE, POS )
      CALL STR_WCONT( '%41p\\', 80, LINE, POS )
      CALL STR_CLEAN( LINE )

*  Camera index.
      CALL IUE_CAMN( CAMERA, ICAM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: camera invalid\\', STATUS )
         GO TO 999
      END IF

*  Aperture index.
      CALL IUE_APRN( APER, IAPR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: aperture invalid\\', STATUS )
         GO TO 999
      END IF

*  Resolution index.
      CALL IUE_RESN( RESOL, IRES, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: resolution invalid\\', STATUS )
         GO TO 999
      END IF

*  Determine global scale factor for NET.
      DONE = .FALSE.
      SCALE = 0.0

      DO IORD = 1, NORDER
         DO I = 1, NWAVS( IORD )
            IF ( DQ_AND( QNETS( I, IORD ), 1 ) .EQ. 0 ) THEN
               IF ( DONE ) THEN
                  SCALE = MAX( SCALE, ABS( SNETS( I, IORD ) ) )

               ELSE
                  SCALE = ABS( SNETS( I, IORD ) )
                  DONE = .TRUE.
               END IF
            END IF
         END DO
      END DO

*  If not DONE then cannot continue.
      IF ( .NOT. DONE ) THEN
         CALL ERROUT( 'Error: there are no valid data points\\',
     :                STATUS )
         GO TO 999

      ELSE IF ( SCALE .LE. 0.0 ) THEN
         CALL ERROUT( 'Error: spectrum is all zero\\', STATUS )
         GO TO 999

      ELSE
         SCALE = 32000.0 / SCALE
      END IF

*  Exposure time.
      ISEC = NINT( REAL( TSECS( 1 ) ) )

*  Flag line.
      WRITE ( FD, 100, IOSTAT = IOSTAT )
 100  FORMAT ( ' ' )
      WRITE ( FD, 200, IOSTAT = IOSTAT )
 200  FORMAT ( 'BEGIN DATA FROM IUEDR' )

*  Write first header line.
      WRITE ( FD, 300, IOSTAT = IOSTAT ) ( LINE( I ), I = 1, 40 ),
     :        ICAM, IMAGE, IAPR, IRES, ISEC, NORDER, SCALE
 300  FORMAT ( 40A1, 6I5, F10.6 )
      IF ( IOSTAT .NE. 0 ) THEN
         CALL ERROUT( 'Error: writing 1st text line\\', STATUS )
         GO TO 999
      END IF

*  Order information.
      WRITE ( FD, 400, IOSTAT = IOSTAT )
     :        ( ORDERS( I ), NWAVS( I ), WAV1S( I ), WAV2S( I ),
     :        I = 1, NORDER )
 400  FORMAT ( 3( 2I4, 2F8.2 ) )
      IF ( IOSTAT .NE. 0 ) THEN
         CALL ERROUT( 'Error: writing summary of orders\\', STATUS )
         GO TO 999
      END IF

*  Write each order.
      DO IORD = 1, NORDER

*     Record counter.
         IREC = ( IORD - 1 ) * 4

*     Fill output buffers.
         DO I = 1, NWAVS( IORD )

*        QUAL.
            IF ( QNETS( I, IORD ) .EQ. 0 ) THEN
               BUF( I, 1 ) = 0

            ELSE
               BUF( I, 1 ) = -1
            END IF

*        SUM.
            BUF( I, 2 ) = 1

*        GROSS.
            IF ( DQ_AND( QNETS( I, IORD ), 1 ) .EQ. 0 ) THEN
               BUF( I, 3 ) = MAX( -9999,
     :                       MIN( 32000,
     :                            NINT( REAL( SNETS( I, IORD ) * SCALE
     :                       ) ) ) )

            ELSE
               BUF( I, 3 ) = 0
            END IF

*        BKG.
            BUF( I, 4 ) = 0
         END DO

*     Write buffers.
         DO J = 1, 4
            WRITE ( FD, 500, IOSTAT = IOSTAT ) ORDERS( IORD ), IREC + J,
     :              NWAVS( IORD ), 0.0, SCALE,
     :              ( BUF( I, J ), I = 1, NWAVS( IORD ) )
 500        FORMAT ( 3I5, 1P2G16.7/( 16I5 ) )
            IF ( IOSTAT .NE. 0 ) THEN
               GO TO 600
            END IF
         END DO
      END DO
 600  CONTINUE

*  Status check for all that.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: writing spectrum arrays\\', STATUS )
         GO TO 999
      END IF

*  A final word.
      WRITE ( FD, 700, IOSTAT = IOSTAT )
 700  FORMAT ( ' END DATA' )
      IF ( IOSTAT .NE. 0 ) THEN
         CALL ERROUT( 'Error: writing final TRAK file line\\',
     :                STATUS )
         GO TO 999
      END IF

 999  CONTINUE

      END
