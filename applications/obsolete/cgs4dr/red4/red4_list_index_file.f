*+  RED4_LIST_INDEX_FILE - List the contents of the CGS4 index file to file.
      SUBROUTINE RED4_LIST_INDEX_FILE( INDEX_FILE, OUTPUT_FILE,
     :  PRINTER, STATUS )
*    Description :
*     This subroutine reads the observation index file and lists its
*     contents to the specified output file. The file may be printed
*     and deleted automatically if required. The following items are
*     included in the listing :-
*      Observation, Quality, Type, Time, Exposure, Grating, Grating wavelength,
*      Slit, CVF, Filters Mode, Group, Row,Col,Osmp, Cnfindex, Airmass
*    Invocation :
*     CALL RED4_LIST_INDEX_FILE( INDEX_FILE, OUTPUT_FILE,
*     :  PRINTER, STATUS )
*    Parameters :
*     INDEX_FILE  = CHARACTER*(*)( READ )
*         The name of the index file to be read.
*     OUTPUT_FILE = CHARACTER*(*)( READ )
*         The name of the output file to be created.
*     PRINTER     = LOGICAL( READ )
*         Flag indicating if the file is to be printed automatically.
*         TRUE  - Print and delete the file automatically.
*         FALSE - Keep the file.
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
*     13-Dec-1990: Original version, copied from
*                  RED4_LIST_INDEX_SCREEN.                      (SMB)
*      2-Jan-1991: Grating wavelength added.                    (SMB)
*     17-Jan-1991: END_TIME replaced by START_TIME.             (SMB)
*     22-Feb-1993: Conform to error strategy                    (PND)
*     28-Nov-1994: Portable version                             (AB)
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
     :  INDEX_FILE,                    ! Name of index file to be listed, of
     :                                 !  the form CGS4_yymmdd.INDEX
     :  OUTPUT_FILE                    ! Name of file to be created.
      LOGICAL
     :  PRINTER                        ! T if output file is to be printed
*    Export:
*    Status :
      INTEGER STATUS
*    External references:
      INTEGER CHR_LEN                  ! Length of character string function
*    Local Constants :
*    Local variables :
      INTEGER
     :  RECNUM,                        ! Record number
     :  NTICKS,                        ! For PSX call
     :  INDEX_FD,                      ! FD for index file
     :  OUTPUT_FD,                     ! FD for output file
     :  OSTATUS,                       ! Status for output
     :  CLEN                           ! Non-blank length of character string
      CHARACTER*30
     :  CDATETIME                      ! Buffer to contain current date
      CHARACTER*132
     :  OUTPUT_TEXT                    ! Buffer for output string
*
      RECORD /OBSREC/ OBSREC           ! An observation record
*                                      ! (structure defined in RED4_COMMON.INC)
*
*    Local data :
*-

*    Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*    Open the index file.
      CALL RIO_OPEN (INDEX_FILE, 'READ', 'UNFORMATTED', OBSRECSZ,
     : INDEX_FD, STATUS)

*    Check this has worked.
      IF ( STATUS .EQ. SAI__OK ) THEN

*       Open the output text file.
         CALL FIO_OPEN (OUTPUT_FILE, 'WRITE', 'LIST', 132, OUTPUT_FD,
     :    STATUS)

*       Check this has worked.
         IF ( STATUS .EQ. SAI__OK ) THEN

*          Write a title to the output file. Note that all
*          I/O errors while writing to the output file are
*          ignored.
            CALL FIO_WRITE( OUTPUT_FD, ' ', STATUS)
            CALL PSX_TIME (NTICKS, STATUS)
            CALL PSX_CTIME (NTICKS, CDATETIME, STATUS)

            CLEN = MAX( 1, CHR_LEN( INDEX_FILE ) )
            CALL FIO_WRITE( OUTPUT_FD,
     :       '            ****** Contents of index file '/
     :       /INDEX_FILE(1:CLEN)//' at '//CDATETIME//' ******',
     :       STATUS)
            CALL FIO_WRITE( OUTPUT_FD, ' ', STATUS)

*          Display a heading.
            CALL FIO_WRITE( OUTPUT_FD,
     :       'Observation    Qly Type     '/
     :       /'Time  Exposure Grating Glambda Slit    '/
     :       /'CVF     '/
     :       /'Filters Mode      Grp Row,Col,Os '/
*    :       /'Cfindx Airmass', STATUS)
     :       /'Cfindx', STATUS)
            CALL FIO_WRITE( OUTPUT_FD,
     :       '-----------    --- ----     '/
     :       /'----  -------- ------- ------- ----    '/
     :       /'---     '/
     :       /'------- ----      --- ---------- '/
*    :       /'------ -------', STATUS)
     :       /'------', STATUS)

*          Read the first record
            RECNUM = 1
            CALL RIO_READ( INDEX_FD, RECNUM, OBSRECSZ, OBSREC, STATUS)

*          Set output status
            OSTATUS = SAI__OK
*          Repeat until an error or end-of-file occurs
            DO WHILE ( STATUS .EQ. SAI__OK )

*             Only write if obs. quality is there
               IF (OBSREC.QUALITY(1:1) .EQ. 'G' .OR.
     :          OBSREC.QUALITY(1:1) .EQ. 'B') THEN

*                Write out the contents of the record (ignoring
*                any format conversion errors).
                  WRITE( OUTPUT_TEXT, 1)
     :             OBSREC.OBSERVATION(1:14),
     :             OBSREC.QUALITY(1:1),
     :             OBSREC.TYPE(1:6),
     :             OBSREC.START_TIME,
     :             OBSREC.EXPOSURE_TIME,
     :             OBSREC.GRATING_NAME(1:7),
     :             OBSREC.GRATING_WVLNGTH,
     :             OBSREC.SLIT_NAME(1:7),
     :             OBSREC.CVF_NAME(1:7),
     :             OBSREC.FILTERS(1:7),
     :             OBSREC.INTTYPE(1:9),
     :             OBSREC.GRPNUM,
     :             OBSREC.DET_NROWS,
     :             OBSREC.DET_NCOLUMNS,
     :             OBSREC.DET_NINCR,
     :             OBSREC.CNFINDEX
*    :             OBSREC.AIRMASS
 1                FORMAT( 1X, A14, 1X, A1, 3X, A6, 1X, F7.3, 1X,
     :             F8.4, 1X, A7, 1X, F7.3, 3( 1X, A7 ), 1X,
     :             A9, I4, 1X, I3, ',', I3, ',', I2,
*    :             1X, I6, 1X, F7.3 )
     :             1X, I6, 1X )
                  CALL FIO_WRITE (OUTPUT_FD, OUTPUT_TEXT, OSTATUS)
                  IF (OSTATUS .NE. SAI__OK) THEN
                     OSTATUS = SAI__ERROR
                     CALL MSG_SETC ('OF', OUTPUT_FILE)
                     CALL ERR_REP (' ', 'Error writing to output '/
     :                /'^OF', OSTATUS)
                     CALL ERR_ANNUL (OSTATUS)
                  END IF
               END IF

*             Attempt to read the next record
               RECNUM = RECNUM + 1
               CALL RIO_READ( INDEX_FD, RECNUM, OBSRECSZ, OBSREC, STATUS)
            END DO

*          Report if an error has occurred
            IF (STATUS .NE. SAI__OK) THEN
               IF (STATUS .EQ. FIO__EOF) THEN
                  CALL ERR_ANNUL (STATUS)
               ELSE IF (STATUS .EQ. FIO__OUTFL) THEN
                  CALL ERR_ANNUL (STATUS)
               ELSE
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'INDEX_FILE', INDEX_FILE )
                  CALL ERR_REP( ' ', 'RED4_LIST_INDEX_FILE: '/
     :             /'Error reading index file '/
     :             /'^INDEX_FILE', STATUS )
               END IF
            END IF

*          Close the output file, printing and deleting it
*          if required.
!             IF ( PRINTER ) THEN

!                CLOSE( UNIT=OUTPUT_LU, DISPOSE='PRINT/DELETE',
!     :            IOSTAT=IOS )

!                  IF ( IOS .EQ. FOR_OK ) THEN

!                     CALL MSG_OUT( ' ', 'Listing submitted to '/
!     :                 /'printer.', STATUS )
!                  END IF
!               ELSE

!                  CLOSE( UNIT=OUTPUT_LU, IOSTAT=IOS )
!               END IF

            CALL FIO_CLOSE (OUTPUT_FD, STATUS)

*          Issue a warning if an error occurred while closing
*          the output file (but carry on).
            IF ( STATUS .NE. SAI__OK ) THEN

               CALL MSG_SETC( 'OUTPUT_FILE', OUTPUT_FILE )
               CALL MSG_OUT( ' ', 'WARNING: Error occurred '/
     :          /'while closing output file ^OUTPUT_FILE', STATUS )

            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'OUTPUT_FILE', OUTPUT_FILE )
            CALL ERR_REP( ' ', 'RED4_LIST_INDEX_FILE: '/
     :        /'Error opening output file '/
     :        /'^OUTPUT_FILE (reason follows)', STATUS )
         END IF

*       Close the index file (ignoring any error)
         CALL RIO_CLOSE( INDEX_FD, STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'INDEX_FILE', INDEX_FILE )
         CALL ERR_REP( ' ', 'RED4_LIST_INDEX_FILE: '/
     :    /'Error opening index file '/
     :    /'^INDEX_FILE', STATUS )
      END IF


      END
