      SUBROUTINE USR_READIUE( STATUS )
*+
*  Name:
*     SUBROUTINE USR_READIUE

*  Purpose:
*     Read the RAW, PHOT or GPHOT image from an IUE tape.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_READIUE( STATUS )

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
*     03-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     30-SEP-94 (MJC):
*       IUEDR Vn. 3.1-6
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*       Assume all non-raw datasets from 1984 onwards are PHOTs.
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
      INCLUDE 'CMISAF'
      INCLUDE 'CMFILE'
      INCLUDE 'CMDATA'
      INCLUDE 'CMTRUN'
      INCLUDE 'CMITFC'

*  Local Constants:
      INTEGER ERR       ! Error status.
      INTEGER FILENAMESIZE ! Maximum file name size.
      INTEGER MAXL      ! Maximum number of IUE lines.
      INTEGER MAXNAME   ! Maximum name length.
      INTEGER MAXS      ! Maximum number of IUE samples.
      PARAMETER ( ERR = -3, FILENAMESIZE = 81, MAXL = 768,
     :            MAXNAME = 16, MAXS = 768 )

      INTEGER MAXDEVN
      PARAMETER ( MAXDEVN=255 )

*  Status:
      INTEGER STATUS    ! Global status.

*  External References:
      LOGICAL STR_SIMLR ! Caseless string equality.

*  Local Variables:
      LOGICAL NEED      ! Whether need for ITF correction.

      BYTE DSNEW( FILENAMESIZE ) ! Name of file to contain image.
      BYTE BTEMP( 8 )
      BYTE BDUM

      CHARACTER*8 TEMPNAME

      INTEGER ACTVAL    ! Parameter value count.
      INTEGER ISTAT     ! Status.
      INTEGER NBYTE     ! Number of bytes per VICAR record.
      INTEGER NLINE     ! Number of VICAR header lines to be read.
      INTEGER NREC      ! Number of VICAR records.
      INTEGER NCHAR
      INTEGER TP        ! Tape descriptor.
      INTEGER I
      INTEGER FDN
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
         CALL PCANER( 'ORDER\\', STATUS)
         GO TO 999
      END IF

*  Cancel APERTURE.
      CALL CNPAR( 'APERTURE\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PCANER( 'APERTURE\\', STATUS )
         GO TO 999
      END IF

*  Check image dimensions.
      NS = MAXS
      NL = MAXL

      IF ( NREC .NE. MAXL ) THEN
         CALL ERROUT( 'Error: this is not an IUE image\\', STATUS )
         GO TO 999

      ELSE IF ( NBYTE .EQ. MAXS ) THEN
         CALL LINE_WCONT( '%p This is a RAW IUE Image\\' )
         CALL PRTBUF( STATUS )
         GEOM = .FALSE.
         PHOT = .FALSE.
         CALL STR_MOVE( 'RAW\\', MAXNAME, TYPE )

      ELSE IF ( NBYTE .EQ. ( MAXS * 2 ) ) THEN
         PHOT = .TRUE.
         CALL LINE_WCONT(
     :             '%p This is either a PHOT or a GPHOT Image\\' )
         CALL PRTBUF( STATUS )

*     TYPE parameter.
         DO WHILE ( .TRUE. )

*        Later files are all PHOTs.
            IF ( ISAYEAR .GE. 1984 ) THEN
               CALL STR_MOVE( 'PHOT\\', MAXNAME, TYPE )

            ELSE
               CALL RDPARC( 'TYPE\\', .FALSE., MAXNAME, TYPE, ACTVAL,
     :                      STATUS )
               CALL CNPAR( 'TYPE\\', ISTAT )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL PARFER( 'TYPE\\', STATUS )
                  GO TO 999

               ELSE IF ( ISTAT .NE. SAI__OK ) THEN
                  CALL PCANER( 'TYPE\\', STATUS )
                  GO TO 999
               END IF
            END IF

            IF ( STR_SIMLR( 'GPHOT\\', TYPE ) ) THEN
               CALL LINE_WCONT( '%p Assumed to be GPHOT Image.\\' )
               CALL PRTBUF( STATUS )
               GEOM = .TRUE.
               CALL STR_MOVE( 'GPHOT\\', MAXNAME, TYPE )

*           ITFMAX parameter.
               CALL RDPARI( 'ITFMAX\\', .FALSE., 1, TNMAX, ACTVAL,
     :                      STATUS )
               CALL CNPAR( 'ITFMAX\\', ISTAT )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL PARFER( 'ITFMAX\\', STATUS )
                  GO TO 999

               ELSE IF ( ISTAT .NE. SAI__OK ) THEN
                  CALL PCANER( 'ITFMAX\\', STATUS )
                  GO TO 999
               END IF

               GO TO 100

            ELSE IF ( STR_SIMLR( 'PHOT\\', TYPE ) ) THEN
               CALL LINE_WCONT( '%p Assumed to be PHOT Image.\\' )
               CALL PRTBUF( STATUS )
               GEOM = .FALSE.
               CALL STR_MOVE( 'PHOT\\', MAXNAME, TYPE )
               GO TO 100

            ELSE
               CALL ERRPAR( 'TYPE\\' )
               CALL ERROUT( ': invalid\\', STATUS )
            END IF
         END DO

      ELSE
         CALL ERROUT( 'Error: this is not an IUE image\\', STATUS )
         GO TO 999
      END IF

 100  CONTINUE

*  Set DATA type.
      CALL STR_MOVE( 'SHORT\\', MAXNAME, DATATP )
      NOHEAD = .FALSE.

*  DATASET parameter.
      DSNEW(1) = 0
      CALL STR_COPY( ISACAMERA, 1, 3, 1, 81, DSNEW )
      WRITE ( TEMPNAME, '(I5.5)' ) ISASEQ
      CALL GEN_CTOS( TEMPNAME, 6, BTEMP, NCHAR )
      CALL STR_TERM( NCHAR, 6, BTEMP )
      CALL STR_COPY( BTEMP, 1, 5, 4, 9, DSNEW )

      CALL RDPARC( 'DATASET\\', .TRUE., FILENAMESIZE, DSNEW, ACTVAL,
     :             STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'DATASET\\', STATUS )
         GO TO 999
      END IF

*  Get VM for Data and Qual.
      CALL MWDATA( NS, NL, DATA_VM, QUAL_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: getting VM for image\\', STATUS )
         GO TO 999
      END IF

*  Associated parameters and calibration data.
      CALL PARIUE( DSNEW, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT(
     :        'Error: reading parameters or calibration information\\',
     :                STATUS )
         GO TO 999
      END IF

*  BADITF parameter (formerly ITFCOR - changed rnh/ucl/21/11/84).
      IF ( STR_SIMLR( 'LORES\\', RESOL ) .AND. PHOT ) THEN
         CALL RDPARL( 'BADITF\\', .FALSE., 1, NEED, ACTVAL, STATUS )
         CALL CNPAR( 'BADITF\\', ISTAT )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'BADITF\\', STATUS )
            GO TO 999

         ELSE IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'BADITF\\', STATUS )
            GO TO 999

         ELSE
            NOITFC = .NOT.NEED
            IF ( NEED ) THEN
               CALL STR_MOVE( 'FUDGE\\', MAXNAME, ITFCTP )
            END IF
         END IF

      ELSE
         NOITFC = .TRUE.
      END IF

*  Read image from tape or file.
      IF ( ISAFILE ) THEN
         CALL LINE_WCONT( '%p Reading Image from File.\\' )

      ELSE
         CALL LINE_WCONT( '%p Reading Image from Tape.\\' )
      END IF
      CALL PRTBUF( STATUS )

      IF ( GEOM .AND. PHOT ) THEN
         CALL GPHOT( TP, TNMAX, %VAL( DATA_VM ), %VAL( QUAL_VM ),
     :               STATUS )
         DBLANK = -32768
         DZERO = 0.0
         DSCALE = 1.0

      ELSE IF ( .NOT.GEOM .AND. PHOT ) THEN
         CALL PHOTM( TP, %VAL( DATA_VM ), %VAL( QUAL_VM ), STATUS )
         DBLANK = -32768
         DZERO = 0.0
         DSCALE = 2.0

      ELSE IF ( .NOT.GEOM .AND. .NOT.PHOT ) THEN
         CALL RAW( TP, %VAL( DATA_VM ), %VAL( QUAL_VM ), STATUS )
         DBLANK = -1
         DZERO = 0.0
         DSCALE = 1.0
      END IF

*  Check image read OK.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading image\\', STATUS )
         GO TO 999
      END IF

*  Say that there IS an image.
      NOIMA = .FALSE.

*  Define data limits on image due to face-plate etc.
      CALL FACE( NS, NL, %VAL( DATA_VM ), %VAL( QUAL_VM ), STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: defining face plate boundary\\', STATUS )
         GO TO 999
      END IF

*  Move fiducials to account for THDA variations.
      CALL MVFIDT( 0, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: moving fiducials with THDA\\', STATUS )
         GO TO 999
      END IF

*  Mark pixels affected by fiducials.
      CALL MKFIDS( 0, .TRUE., NS, NL, %VAL( DATA_VM ), %VAL( QUAL_VM ),
     :             STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: marking fiducials\\', STATUS )
         GO TO 999
      END IF

*  Create representation for geometric distortion.
      CALL GEOMF( 0, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: representing geometry\\', STATUS )
         GO TO 999
      END IF

      NODATA = .FALSE.

*  Output image to file.
      CALL CRDATA( DSNEW, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: writing image to file\\', STATUS )
         GO TO 999
      END IF

 999  CONTINUE

      CALL IUE_DEVICE( 'CLOSE', TP, FDN, BDUM, 1, I, STATUS )

 1000 CONTINUE

      END
