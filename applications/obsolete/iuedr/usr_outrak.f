       SUBROUTINE USR_OUTRAK( STATUS )
*+
*  Name:
*     SUBROUTINE USR_OUTRAK

*  Description:
*     The raw spectrum is output to a formatted file that is (roughly)
*     compatible with TRAK output.
*     The default OUTFILE parameter is composed from CAMERA/IMAGE.
*     The default file type is ".TRK".

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_OUTRAK( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     MBT: Mark Taylor (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-AUG-82 (JRG):
*       IUEDR Vn. 1.0
*     05-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     03-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*       FIO I/O unit support.
*     18-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     09-JUL-99 (MBT):
*       Removed non-standard OPEN CARRIAGECONTROL keyword.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS      ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMSAVE'

*  Local Variables:
      BYTE FILE( 81 )     ! File name.

      CHARACTER*81 CFILE  ! Fortran 77 filename.

      INTEGER ACTVAL      ! Parameter value count.
      INTEGER FD          ! File descriptor.
      INTEGER ISTAT       ! Status.
      INTEGER NCHAR       ! Character count.
      INTEGER POS         ! Character position.
      INTEGER FIOSTAT
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get Calibration and Spectrum (raw).
      CALL DASSOC( 'S\\', 'T\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999

      ELSE IF ( NOSPEC .OR. NORDER.LT.1 ) THEN
         CALL ERROUT( 'Error: no spectrum\\', STATUS )
         GO TO 999
      END IF

*   FILE.
      POS = 0
      CALL STR_WRITS( '%p%s\\', CAMERA, 81, FILE, POS )
      CALL STR_WRITI( '%i.TRK\\', IMAGE, 81, FILE, POS )

      CALL RDPARC( 'OUTFILE\\', .TRUE., 81, FILE, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'OUTFILE\\', STATUS )
         GO TO 999

      ELSE
         CALL CNPAR( 'OUTFILE\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'OUTFILE\\', STATUS )
            GO TO 999
         END IF
      END IF

*   Open file.
      CALL GEN_STOC( FILE, 81, CFILE, NCHAR )
      CALL FIO_GUNIT( FD, STATUS )
      OPEN ( UNIT = FD, NAME = CFILE( :NCHAR ), ACCESS='SEQUENTIAL',
     :       STATUS = 'UNKNOWN', IOSTAT = STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERRSTR( FILE )
         CALL ERROUT( ': file open error\\', STATUS )
         GO TO 998
      END IF

*   Print information.
      CALL LINE_WRITS( '%p Writing Formatted TRAK File (%s).\\', FILE )
      CALL PRTBUF( STATUS )

*   Write spectrum.
      CALL OUTRK( FD, STATUS )
      IF ( STATUS .NE. SAI__OK )
     :    CALL ERROUT( 'Error: writing spectrum to file\\', STATUS )

      CLOSE ( UNIT = FD, IOSTAT = ISTAT )
      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL ERRSTR( FILE )
         CALL ERROUT( ': file close error\\', STATUS )
         GO TO 998
      END IF

 998  CONTINUE
      FIOSTAT = SAI__OK
      CALL FIO_PUNIT( FD, FIOSTAT )

 999  CONTINUE

      END
