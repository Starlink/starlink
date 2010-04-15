*+  RED4_LIST_INDEX_SCREEN - List the contents of the CGS4 index file to screen
      SUBROUTINE RED4_LIST_INDEX_SCREEN( INDEX_FILE, OFORMAT, STATUS )
*    Description :
*     This subroutine reads the observation index file and displays
*     its contents on the terminal screen using MSG_OUT. The display may
*     be in one of two selected formats.
*     Format 1 gives:
*      Observation, Quality, Type, Time, Exposure, Grating, Slit, CVF, Filters
*     Format 2 gives:
*      Observation, Quality, Type, Time, Mode, Group, Row,Col,Osmp, Cnfindex,
*      Airmass
*    Invocation :
*     CALL RED4_LIST_INDEX_SCREEN( INDEX_FILE, OFORMAT, STATUS )
*    Parameters :
*     INDEX_FILE  = CHARACTER*(*)( READ )
*         The name of the index file to be read.
*     OFORMAT     = INTEGER( READ )
*         The output format required (see sbove).
*     STATUS      = INTEGER( UPDATE )
*         Global ADAM status.
*    Method :
*    Deficiencies :
*     Some of the character items from the index file are truncated
*     in order to display them on one line.
*    Bugs :
*    Authors :
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*     11-Jul-1990: Original version.                            (SMB)
*     23-Jul-1990: Output format altered to allow 4 digit
*                  observation numbers. Also, INDDIR changed
*                  to CGS4_INDEX.                               (SMB)
*     30-Jul-1990: Modified to use 4 digits for the year
*                  number instead of 2, so the software will
*                  work correctly after the year 2000.          (SMB)
*     31-Jul-1990: Specification changed back to how it was !!!!
*                  Year number changed from 4 digits back to
*                  2. Modulo used instead of subtracting 1900,
*                  so the software will still work after the
*                  year 2000. Also modified to construct year
*                  code with I6.6 format.                        (SMB)
*      4-Sep-1990: ERR_OUT replaced with ERR_REP.                (SMB)
*     23-Oct-1990: Commented out code removed.                   (SMB)
*     19-Nov-1990: Index file format changed again to include
*                  parameters required for wavelength and flux
*                  calibration. Two output formats defined,
*                  because there are more parameters than can
*                  be displayed on one line.                     (SMB)
*     12-Dec-1990: Index file format changed again! This is because
*                  it is possible for different observations to be
*                  made in NDR mode with different effective detector
*                  sizes, and the calibration frames for these must
*                  not get mixed up. Format 2 made to include
*                  detector size.                                (SMB)
*     13-Dec-1990: Split into RED4_LIST_INDEX and
*                  RED4_LIST_INDEX_SCREEN.                       (SMB)
*     17-Jan-1991: END_TIME replaced by START_TIME.              (SMB)
*     22-Feb-1993: Conform to error strategy                     (PND)
*     11-May-1993: Add call to MSG_SYNC to prevent crash         (PND)
*     28-Nov-1994: Portable version                              (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
      INCLUDE 'FIO_ERR'
*    Global variables:
      INCLUDE 'RED4_COMMON.INC'        ! RED4 common block containing pointers
*                                           to data in virtual memory.
*    Import :
      CHARACTER*(*)
     :  INDEX_FILE                     ! The name of the reduced-observation
*                                           index file, should be of form
*                                           CGS4_yymmdd.INDEX
      INTEGER
     :  OFORMAT                        ! Output format required (1 or 2)
*    Export:
*    Status :
      INTEGER STATUS
*    External references:
*    Local Constants :
*    Local variables :
      INTEGER
     :  RECNUM,                        ! Record number
     :  FD                             ! File descriptor
      CHARACTER*80
     :  LINE                           ! Line for output.
*
      RECORD /OBSREC/ OBSREC           ! An observation record
*                                      ! (structure defined in RED4_COMMON.INC)
*
*    Local data :
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*    open the index file
      CALL RIO_OPEN (INDEX_FILE, 'READ', 'UNFORMATTED', OBSRECSZ,
     : FD, STATUS)

*    Check this has worked.
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Display a title.
         CALL MSG_OUT( ' ', ' ', STATUS )
         CALL MSG_SETC( 'INDEX_FILE', INDEX_FILE )
         CALL MSG_SETI( 'OFORMAT', OFORMAT )
         CALL MSG_OUT( ' ', 'Contents of index file '/
     :    /'^INDEX_FILE (format type ^OFORMAT) :-', STATUS )
         CALL MSG_OUT( ' ', ' ', STATUS )

*      The heading displayed depends on the output format type.
         IF ( OFORMAT .EQ. 2 ) THEN

            CALL MSG_OUT( ' ', 'Observation   Qly Type     '/
     :       /'Time  Mode      Grp Row,Col,Os '/
*    :       /'Cfindx Airmass', STATUS )
     :       /'Cfindx', STATUS )
            CALL MSG_OUT( ' ', '-----------   --- ----     '/
     :       /'----  ----      --- ---------- '/
*    :       /'------ -------', STATUS )
     :       /'------', STATUS )
         ELSE

            CALL MSG_OUT( ' ', 'Observation   Qly Type     '/
     :       /'Time  Exposure Grating Slit    CVF     '/
     :       /'Filters', STATUS )
            CALL MSG_OUT( ' ', '-----------   --- ----     '/
     :       /'----  -------- ------- ----    ---     '/
     :       /'-------', STATUS )
         END IF

*       Read the first record
         RECNUM = 1
         CALL RIO_READ (FD, RECNUM, OBSRECSZ, OBSREC, STATUS)

*       Repeat until an error or end-of-file occurs
         DO WHILE ( STATUS .EQ. SAI__OK )

*          In verbose mode, write record to screen first
            IF ( VERBOSE ) THEN
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.OBSNUM' )
              CALL MSG_SETI( 'ITMVAL', OBSREC.OBSNUM )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.OBSERVATION' )
              CALL MSG_SETC( 'ITMVAL', OBSREC.OBSERVATION )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.QUALITY' )
              CALL MSG_SETC( 'ITMVAL', OBSREC.QUALITY )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.TYPE' )
              CALL MSG_SETC( 'ITMVAL', OBSREC.TYPE )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.START_TIME' )
              CALL MSG_SETR( 'ITMVAL', OBSREC.START_TIME )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.INTTYPE' )
              CALL MSG_SETC( 'ITMVAL', OBSREC.INTTYPE )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.GRPNUM' )
              CALL MSG_SETI( 'ITMVAL', OBSREC.GRPNUM )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.CNFINDEX' )
              CALL MSG_SETI( 'ITMVAL', OBSREC.CNFINDEX )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.EXPOSURE_TIME' )
              CALL MSG_SETR( 'ITMVAL', OBSREC.EXPOSURE_TIME )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.DET_NCOLUMNS' )
              CALL MSG_SETI( 'ITMVAL', OBSREC.DET_NCOLUMNS )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.DET_NROWS' )
              CALL MSG_SETI( 'ITMVAL', OBSREC.DET_NROWS )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.DET_ENC_BASE' )
              CALL MSG_SETI( 'ITMVAL', OBSREC.DET_ENC_BASE )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.DET_INCR' )
              CALL MSG_SETR( 'ITMVAL', OBSREC.DET_INCR )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.DET_NINCR' )
              CALL MSG_SETI( 'ITMVAL', OBSREC.DET_NINCR )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.GRATING_NAME' )
              CALL MSG_SETC( 'ITMVAL', OBSREC.GRATING_NAME )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.GRATING_WVLNGTH' )
              CALL MSG_SETR( 'ITMVAL', OBSREC.GRATING_WVLNGTH )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.GRATING_ANGLE' )
              CALL MSG_SETR( 'ITMVAL', OBSREC.GRATING_ANGLE )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.GRATING_ORDER' )
              CALL MSG_SETI( 'ITMVAL', OBSREC.GRATING_ORDER )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.SLIT_NAME' )
              CALL MSG_SETC( 'ITMVAL', OBSREC.SLIT_NAME )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.SLIT_ANGLE' )
              CALL MSG_SETR( 'ITMVAL', OBSREC.SLIT_ANGLE )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.CVF_NAME' )
              CALL MSG_SETC( 'ITMVAL', OBSREC.CVF_NAME )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.CVF_WAVELENGTH' )
              CALL MSG_SETR( 'ITMVAL', OBSREC.CVF_WAVELENGTH )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.FILTERS' )
              CALL MSG_SETC( 'ITMVAL', OBSREC.FILTERS )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
*             CALL MSG_SETC( 'ITMNAM', 'OBSREC.AIRMASS' )
*             CALL MSG_SETR( 'ITMVAL', OBSREC.AIRMASS )
*             CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SYNC( STATUS )
            ENDIF

*          If the quality is neither good nor bad then skip the record.
*          (NOTE - tried to test for a blank observation name but this
*          doesn't seem to work at all?!)
            IF (OBSREC.QUALITY(1:1) .EQ. 'G' .OR.
     :       OBSREC.QUALITY(1:1) .EQ. 'B') THEN

*             Write out the contents of the record (ignoring
*             any format conversion errors), in the required
*             output format.
               CALL CHR_FILL( ' ', LINE )
               IF ( OFORMAT .EQ. 2 ) THEN
                  WRITE( LINE, 2)
     :             OBSREC.OBSERVATION(1:14),
     :             OBSREC.QUALITY(1:1),
     :             OBSREC.TYPE(1:6),
     :             OBSREC.START_TIME,
     :             OBSREC.INTTYPE(1:9),
     :             OBSREC.GRPNUM,
     :             OBSREC.DET_NROWS,
     :             OBSREC.DET_NCOLUMNS,
     :             OBSREC.DET_NINCR,
     :             OBSREC.CNFINDEX
*    :             OBSREC.AIRMASS
 2                FORMAT( A14, A1, 3X, A6, 1X, F7.3, 1X,
     :             A9, I4, 1X, I3, ',', I3, ',', I2,
*    :             1X, I6, 1X, F7.3 )
     :             1X, I6, 1X )
               ELSE

                  WRITE( LINE, 1)
     :             OBSREC.OBSERVATION(1:14),
     :             OBSREC.QUALITY(1:1),
     :             OBSREC.TYPE(1:6),
     :             OBSREC.START_TIME,
     :             OBSREC.EXPOSURE_TIME,
     :             OBSREC.GRATING_NAME(1:7),
     :             OBSREC.SLIT_NAME(1:7),
     :             OBSREC.CVF_NAME(1:7),
     :             OBSREC.FILTERS(1:7)
 1                FORMAT( A14, A1, 3X, A6, 1X, F7.3, 1X,
     :              F8.4, 4( 1X, A7 ) )
               END IF

               CALL MSG_OUT( ' ', LINE, STATUS )
               CALL MSG_SYNC( STATUS )
            END IF

*          Attempt to read the next record
            RECNUM = RECNUM + 1
            CALL RIO_READ (FD, RECNUM, OBSRECSZ, OBSREC, STATUS)

         END DO

*       Report if an error has occurred.
         IF (STATUS .NE. SAI__OK) THEN
            IF (STATUS .EQ. FIO__EOF) THEN
               CALL ERR_ANNUL (STATUS)
            ELSE IF (STATUS .EQ. FIO__OUTFL) THEN
               CALL ERR_ANNUL (STATUS)
            ELSE
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'INDEX_FILE', INDEX_FILE )
               CALL ERR_REP( ' ', 'RED4_LIST_INDEX_SCREEN: '/
     :          /'Error reading index file '/
     :          /'^INDEX_FILE', STATUS )
            END IF
         END IF

*       Close the index file (ignoring any error)
         CALL RIO_CLOSE (FD, STATUS)
      ELSE

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'INDEX_FILE', INDEX_FILE )
         CALL ERR_REP( ' ', 'RED4_LIST_INDEX_SCREEN: '/
     :    /'Error opening index file '/
     :    /'^INDEX_FILE', STATUS )
      END IF


      END
