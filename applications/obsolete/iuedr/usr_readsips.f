      SUBROUTINE USR_READSIPS( STATUS )
*+
*  Name:
*     SUBROUTINE USR_READSIPS

*  Purpose:
*     Read a MELO or MEHI file (extracted spectrum) from an IUESIPS
*     tape or file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_READSIPS( STATUS )

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
*       AT4 version
*     07-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     30-SEP-94 (MJC):
*       IUEDR Vn. 3.1-6
*     20-JAN-95 (MJC):
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
      INTEGER STATUS    ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMISAF'
      INCLUDE 'CMSPC'
      INCLUDE 'CMFILE'

*  Local Variables:
      INTEGER ACTVAL    ! Parameter value count.
      INTEGER FDN
      INTEGER I         ! Loop index.
      INTEGER ISTAT     ! Status.
      INTEGER NBYTE     ! Number of bytes per VICAR record.
      INTEGER NLINE     ! Number of header lines to be read.
      INTEGER NREC      ! Number of VICAR records.
      INTEGER NWORD     ! Number of words.
      INTEGER NCHAR
      INTEGER TP        ! Tape descriptor.

      CHARACTER*8 TEMPNAME

      BYTE DSNEW( 81 )  ! Name of file to contain image.
      BYTE BTEMP( 8 )
      BYTE BDUM
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the source of data.
      CALL IUE_DEVICE( 'OPEN', TP, FDN, BDUM, 1, I, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 1000
      END IF

*  Print tape position.
      IF ( .NOT. ISAFILE ) THEN
         CALL TAPE_SHOP( TP, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: printing tape position\\', STATUS )
            GO TO 999
         END IF
      END IF

*  NLINE parameter.
      CALL RDPARI( 'NLINE\\', .FALSE., 1, NLINE, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'NLINE\\', STATUS )
         GO TO 999
      END IF

*  Read VICAR header.
      CALL VIC_TRHD( TP, NLINE, NREC, NBYTE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading IUE header\\', STATUS )
         GO TO 999
      END IF

*  Print image size.
      CALL LINE_WRITI( '%p VICAR Image is %i records,\\', NREC )
      CALL LINE_WRITI( '  each consisting %i bytes\\', NBYTE )
      CALL PRTBUF( STATUS )

*  Flush out any existing dataset.
      CALL FRDSN( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: writing previous dataset\\', STATUS )
         GO TO 999
      END IF

*  Cancel the dataset.
      CALL CNDSN
      CALL CNPAR( 'DATASET\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PCANER( 'DATASET\\', STATUS )
         GO TO 999
      END IF

*  Cancel ORDER.
      CALL CNPAR( 'ORDER\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PCANER( 'ORDER\\', STATUS )
         GO TO 999
      END IF

*  Cancel APERTURE.
      CALL CNPAR( 'APERTURE\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PCANER( 'APERTURE\\', STATUS )
         GO TO 999
      END IF

*  Check image dimensions.
      NWORD = NBYTE / 2
      IF ( NBYTE.NE.1204 .AND. NBYTE.NE.2048 ) THEN
         CALL ERROUT( 'Error: this is not an IUE spectrum\\', STATUS )
         GO TO 999

      ELSE
         IF ( NREC .LE. 7 ) THEN
            CALL LINE_WCONT( '%p This is a Low Resolution Spectrum\\' )
            CALL STR_MOVE( 'LORES\\', 16, RESOL )
            CALL STR_MOVE( 'MELO\\', 16, TYPE )

         ELSE IF ( NREC .LE. 200 ) THEN
            CALL ERROUT( 'This is an LBLS file, not acceptible.\\',
     :                  STATUS )
            GO TO 999

         ELSE
            CALL LINE_WCONT( '%p This is a High Resolution Spectrum\\' )
            CALL STR_MOVE( 'HIRES\\', 16, RESOL )
            CALL STR_MOVE( 'MEHI\\', 16, TYPE )
         END IF

         IF ( NBYTE .EQ. 1204 ) THEN
            CALL LINE_WCONT( ' from IUESIPS#1\\' )
            SWVER = 1
            PHOT = .TRUE.
            GEOM = .TRUE.

         ELSE
            CALL LINE_WCONT( ' from IUESIPS#2\\' )
            PHOT = .TRUE.
            GEOM = .FALSE.
            SWVER = 2
         END IF

         CALL PRTBUF( STATUS )
      END IF

*  Header is there now.
      NOHEAD = .FALSE.

*  DATASET parameter.
      CALL STR_COPY( ISACAMERA, 1, 3, 1, 81, DSNEW )
      WRITE ( TEMPNAME, '(I5.5)' ) ISASEQ
      CALL GEN_CTOS( TEMPNAME, 6, BTEMP, NCHAR )
      CALL STR_TERM( NCHAR, 6, BTEMP )
      CALL STR_COPY( BTEMP, 1, 5, 4, 9, DSNEW )

      CALL RDPARC( 'DATASET\\', .TRUE., 81, DSNEW, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'DATASET\\', STATUS )
         GO TO 999
      END IF

*  Release previous DATA_VM and QUAL_VM, get temporary DATA_VM.
      CALL MWSPEC( NWORD, NREC, DATA_VM, QUAL_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: getting VM for dataset\\', STATUS )
         GO TO 999
      END IF

*  Read spectrum from tape.
      CALL LINE_WCONT( '%p Reading Extracted Spectrum from Tape.\\' )
      CALL PRTBUF( STATUS )
      CALL RDMESP( TP, NWORD, NREC, %VAL( DATA_VM ), STATUS )
      CALL DLADR( DATA_VM, ISTAT )

*  Check image read OK.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT('Error: reading spectrum\\', STATUS)
         GO TO 999
      END IF

*  Prompt for parameters not available from tape.
      CALL PARSPC( DSNEW, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading calibration information\\',
     :                STATUS )
         GO TO 999
      END IF

*  Output image to file.
      CALL CRSPEC( DSNEW, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: writing dataset to file\\', STATUS )
         GO TO 999
      END IF

 999  CONTINUE

      CALL IUE_DEVICE( 'CLOSE', TP, FDN, BDUM, 1, I, STATUS )

 1000 CONTINUE

      END
