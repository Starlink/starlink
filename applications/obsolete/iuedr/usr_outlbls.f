      SUBROUTINE USR_OUTLBLS( STATUS )
*+
*  Name:
*     SUBROUTINE USR_OUTLBLS

*  Purpose:
*     Output current LBLS array to a file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_OUTLBLS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     The default OUTFILE parameter is composed from CAMERA/IMAGE.
*     SPECTYPE specifies the type of file produced.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-AUG-82 (JRG):
*       IUEDR Vn. 1.0
*     05-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     30-SEP-94 (MJC):
*       IUEDR Vn. 3.1-6
*     20-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Problems:
*     At present, only SPECTYPE=0 (binary) is implemented.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS     ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMLBLS'
      INCLUDE 'CMPAN'

*  Local Variables:
      BYTE FILE( 81 )    ! File name.
      BYTE LABEL1( 79 )  ! 1st label.
      BYTE LABEL2( 79 )  ! 2nd label.

      INTEGER ACTVAL     ! Parameter value count.
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

      ELSE IF ( NOLBLS ) THEN
         CALL ERROUT( 'Error: no LBLS\\', STATUS )
         GO TO 999
      END IF

*   1st label (CAMERA,IMAGE,RESOLUTION,labels).
      POS = 0
      CALL STR_WRITS( '%p %s\\', CAMERA, 79, LABEL1, POS )
      CALL STR_WRITI( '%i\\', IMAGE, 79, LABEL1, POS )
      CALL STR_WRITS( ',%s\\', RESOL, 79, LABEL1, POS )
      CALL STR_WRITS( ',LBLS,%s\\', ULAB, 79, LABEL1, POS )
      CALL STR_WRITS( '%s\\', UUNT, 79, LABEL1, POS )
      CALL STR_WRITS( ',%s\\', VLAB, 79, LABEL1, POS )
      CALL STR_WRITS( '%s\\', VUNT, 79, LABEL1, POS )

*   Create 2nd text line.
      POS = 0
      CALL STR_WRITS( '%p OBJECT=''%S''\\', TITLE, 79, LABEL2, POS )

*   OUTFILE.
      POS = 0
      CALL STR_WRITS( '%p%s\\', CAMERA, 81, FILE, POS )
      CALL STR_WRITI( '%i\\', IMAGE, 81, FILE, POS )
      CALL STR_WCONT( 'R.DAT\\', 81, FILE, POS )
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

*   SPTYPE is fixed at zero as this is only type supported.
      SPTYPE = 0

*   Create LBLS file.
      CALL FIO_GUNIT( FD, STATUS )
      CALL OUTPAN( FD, FILE, SPTYPE, 120, 1200, FPAN, QPAN, LABEL1,
     :             LABEL2, STATUS )
      FIOSTAT = SAI__OK
      CALL FIO_PUNIT( FD, FIOSTAT )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Could not create SPECTRUM file\\', STATUS )
         GO TO 999
      END IF

 999  CONTINUE

      END
