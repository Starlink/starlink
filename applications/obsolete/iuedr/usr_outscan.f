      SUBROUTINE USR_OUTSCAN( STATUS )
*+
*   Name:
*      SUBROUTINE USR_OUTSCAN

*   Description:
*      The current scan is output in a file suitable for input to the
*      SPECTRUM program.
*      The default OUTFILE parameter is composed from CAMERA/IMAGE.
*      SPECTYPE specifies the type of SPECTRUM file produced.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_OUTSCAN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*   History:
*     01-AUG-82 (JRG):
*       IUEDR Vn. 1.0
*     06-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     10-JUL-94 (MJC):
*       IUEDR Vn. 3.1-1
*     18-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Deifnitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS     ! Global status.

*  Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMSCAN'

*  Local Variables:
      REAL*8 X( 4096 )   ! Evaluated radial distances.
      REAL*8 Y( 4096 )   ! Modified fluxes (zero means blank).

      BYTE FILE( 81 )    ! File name.
      BYTE LABEL1( 79 )  ! 1st label.
      BYTE LABEL2( 79 )  ! 2nd label.

      INTEGER ACTVAL     ! Parameter value count.
      INTEGER I          ! Loop index.
      INTEGER POS        ! Character position.
      INTEGER SPTYPE     ! SPECTRUM file sptype (0 -> 2).
      INTEGER FD         ! I/O unit number.
      INTEGER FIOSTAT
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get Calibration.
      CALL DASSOC( '\\', '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999

      ELSE IF ( NOSCAN ) THEN
         CALL ERROUT( 'Error: no scan\\', STATUS )
         GO TO 999
      END IF

*   Copy into a temporary array, with zeros for bad points.
      DO I = 1, NSCAN
         X( I ) = DBLE( I - 1 ) * DUSCAN + U1SCAN
         IF ( QSCAN( I ) .EQ. 0 ) THEN
            Y( I ) = FSCAN( I )

         ELSE
            Y( I ) = 0.0
         END IF
      END DO

*   1st label (CAMERA, IMAGE, LABELS).
      POS = 0
      CALL STR_WRITS( '%p %s\\', CAMERA, 79, LABEL1, POS )
      CALL STR_WRITI( '%i\\', IMAGE, 79, LABEL1, POS )
      CALL STR_WRITF( ', SCAN, WAV=%.3f\\', V1SCAN, 79, LABEL1, POS )
      CALL STR_WRITF( ', FWHM=%.1f.\\', DVSCAN, 79, LABEL1, POS )

*   Create 2nd text line.
      POS = 0
      CALL STR_WRITS( '%p OBJECT=''%S''\\', TITLE, 79, LABEL2, POS )

*   SPECTYPE.
      CALL GET_SPTYPE( SPTYPE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

*   OUTFILE.
      POS = 0
      CALL STR_WRITS( '%p%s\\', CAMERA, 81, FILE, POS )
      CALL STR_WRITI( '%i\\', IMAGE, 81, FILE, POS )
      CALL STR_WCONT( 'P\\', 81, FILE, POS )

*   Add sdf extension if type is zero else DAT extension.
      IF ( SPTYPE .EQ. 0 ) THEN
         CALL STR_WCONT( '.sdf\\', 81, FILE, POS )

      ELSE
         CALL STR_WCONT( '.DAT\\', 81, FILE, POS )
      END IF

      CALL RDPARC( 'OUTFILE\\', .TRUE., 81, FILE, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'OUTFILE\\', STATUS )
         GO TO 999

      ELSE
         CALL CNPAR( 'OUTFILE\\', STATUS )
         IF ( STATUS .NE. SAI__OK) THEN
            CALL PCANER( 'OUTFILE\\', STATUS )
            GO TO 999
         END IF
      END IF

*   Create SPECTRUM file.
      CALL FIO_GUNIT( FD, STATUS )
      CALL OUTSPC( FD, FILE, SPTYPE, NSCAN, X, Y, LABEL1, LABEL2,
     :             STATUS )
      FIOSTAT = SAI__OK
      CALL FIO_PUNIT( FD, FIOSTAT )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Could not create SPECTRUM file\\', STATUS )
         GO TO 999
      END IF

 999  CONTINUE

      END
