      SUBROUTINE USR_OUTNET( STATUS )
*+
*  Name:
*     SUBROUTINE USR_OUTNET

*  Description:
*     The current NET spectrum/order is output in a file suitable
*     for input to the SPECTRUM program.
*     The default OUTFILE parameter is composed from CAMERA/IMAGE/APERTURE
*     for LORES, and CAMERA/IMAGE.ORDER for HIRES.
*     SPECTYPE specifies the type of SPECTRUM file produced.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_OUTNET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     08-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     18-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS     ! Global status.

*  External References:
      LOGICAL STR_SIMLR  ! Caseless string equality.

      INTEGER DQ_AND     ! Data quality AND operation.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMNET'
      INCLUDE 'CMWAV'
      INCLUDE 'CMSPEC'

*  Local Variables:
      REAL*8 Y( 1200 )   ! Modified fluxes (zero means blank).

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

*   DATASET.
      CALL RDSPEC( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: accessing spectrum\\', STATUS )
         GO TO 999

      ELSE IF ( NONET ) THEN
         CALL ERROUT( 'Error: no net spectrum\\', STATUS )
         GO TO 999
      END IF

*   Copy into a temporary array, with zeros for bad points.
      DO I = 1, NWAV
         IF ( DQ_AND( QNET( I ), 3 ) .EQ. 0 ) THEN
            Y( I ) = SNET( I )

         ELSE
            Y( I ) = 0.0
         END IF
      END DO

*   1st label (CAMERA,IMAGE,APERTURE/ORDER,labels).
      POS = 0
      CALL STR_WRITS( '%p %s\\', CAMERA, 79, LABEL1, POS )
      CALL STR_WRITI( '%i\\', IMAGE, 79, LABEL1, POS )
      CALL STR_WRITS( ', %s\\', APER, 79, LABEL1, POS )
      IF ( STR_SIMLR( 'LORES\\', RESOL ) ) THEN
         CALL STR_WRITS( ', %s\\', APERS( 1, ORDER ), 79, LABEL1, POS )

      ELSE
         CALL STR_WRITI( ', ORDER=%i\\', ORDER, 79, LABEL1, POS )
      END IF
      CALL STR_WRITS( ', %s.\\', NLABEL, 79, LABEL1, POS )

*   2nd label (OBJECT).
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
      IF ( SPTYPE .EQ. 0 ) THEN
         CALL STR_WCONT( '_\\', 81, FILE, POS )

      ELSE
         CALL STR_WCONT( '.\\', 81, FILE, POS )
      END IF

      IF ( .NOT. STR_SIMLR( 'LORES\\', RESOL ) ) THEN
         CALL STR_WRITI( '%i\\', ORDER, 81, FILE, POS )

      ELSE IF ( STR_SIMLR( 'SAP\\', APERS( 1, ORDER ) ) ) THEN
         CALL STR_WCONT( 'SAP\\', 81, FILE, POS )

      ELSE IF ( STR_SIMLR( 'LAP\\', APERS( 1, ORDER ) ) ) THEN
         CALL STR_WCONT( 'LAP\\', 81, FILE, POS )

      ELSE
         CALL STR_WRITI( '%i\\', ORDER, 81, FILE, POS )
      END IF

*   Add sdf extension if type is zero.
      IF ( SPTYPE .EQ. 0 ) THEN
         CALL STR_WCONT( '.sdf\\', 81, FILE, POS )
      END IF

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

*   Create SPECTRUM file.
      CALL FIO_GUNIT( FD, STATUS )
      CALL OUTSPC( FD, FILE, SPTYPE, NWAV, WAVAIR, Y, LABEL1, LABEL2,
     :             STATUS )
      FIOSTAT = SAI__OK
      CALL FIO_PUNIT( FD, FIOSTAT )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Could not create SPECTRUM file\\', STATUS )
         GO TO 999
      END IF

 999  CONTINUE

      END
