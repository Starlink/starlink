*+  RED4_LIST_INDEX - List the contents of the CGS4 index file.
      SUBROUTINE RED4_LIST_INDEX( STATUS )
*    Description :
*     This subroutine reads the observation index file and displays
*     its contents. The output may be directed to the screen or written
*     to a file, which may optionally be printed. The screen display may
*     be in one of two selected formats.
*     Format 1 gives:
*      Observation, Quality, Type, Time, Exposure, Grating, Slit, CVF, Filters
*     Format 2 gives:
*      Observation, Quality, Type, Time, Mode, Group, Row,Col,Osmp, Cnfindex,
*      Airmass
*     Writing to disk combines both the above formats together.
*    Invocation :
*     CALL RED4_LIST_INDEX( STATUS )
*    Parameters :
*     STATUS   = INTEGER( UPDATE )
*         Global ADAM status.
*    Method :
*    Deficiencies :
*     The system IDATE routine does not distinguish between years
*     before or after the year 2000. A fudge is used which assumes
*     that year numbers greater than 50 are in the 20th century
*     and year numbers less than or equal to 50 are in the 21st
*     century. (Now that only 2 digit year numbers are used this
*     fudge is hidden anyway, but may reappear if the specification
*     were to change again ...).
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
*     13-Dec-1990: Ability to send output to screen, file or
*                  printer added.                                (SMB)
*     22-Feb-1993: Conform to error strategy                     (PND)
*     28-Nov-1994: Portable version                              (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Global variables:
      INCLUDE 'RED4_COMMON.INC'        ! RED4 common block containing pointers
*                                           to data in virtual memory.
*    Import :
*    Export:
*    Status :
      INTEGER STATUS
*    External references:
      INTEGER CHR_LEN
*    Local Constants :
*    Local variables :
      INTEGER
     :  MONTH,                         ! UT month
     :  DAY,                           ! UT day
     :  YEAR,                          ! UT year
     :  NTICKS,                        ! For PSX_TIME
     :  DUMMY,                         ! Dummy
     :  CPOS,                          ! Position of colon or /
     :  UTDATE,                        ! Encoded UT date
     :  OFORMAT                        ! Output format required (1 or 2)
      LOGICAL
     :  IF_HITIME,                     ! T if system is running Hawaii time
     :  PRINTER                        ! T if output is to be printed
      CHARACTER*8
     :  DESTINATION,                    ! Destination for output (SCREEN,
*                                      !   FILE or PRINTER).
     :  SUTDATE                        ! UTDATE as a string.
      CHARACTER*80
     :  INDEX_FILE,                    ! Name of index file to be listed, of
     :                                 !   the form CGS4_yymmdd.INDEX
     :  FILE                           ! Name of output file (if any)
      CHARACTER*256 TRANSL             ! Translation of any directory string
*    Local data :
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the parameter determining whether the system is running
*   with Hawaii time.
      CALL PAR_GET0L( 'IF_HITIME', IF_HITIME, STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Obtain the current UT date.
*      If the flag IF_HITIME is TRUE, then assume the system is running
*      on Hawaii time and call the routine which converts the system date
*      in Hawaii into a UT date.
*      Otherwise assume the system date is the same as the UT date.
         IF ( IF_HITIME ) THEN

            CALL RED4_GET_UTDATE (SUTDATE, STATUS)
            SUTDATE = SUTDATE(3:8)
            CALL CHR_CTOI (SUTDATE, UTDATE, STATUS)
         ELSE

            CALL PSX_TIME (NTICKS, STATUS)
            CALL PSX_LOCALTIME (NTICKS, DUMMY, DUMMY, DUMMY, DAY, MONTH,
     :       YEAR, DUMMY, DUMMY, DUMMY, DUMMY, STATUS)
            MONTH = MONTH + 1

*         We only have a 2 digit year. Convert this into
*         a 4 digit year, assuming that year numbers greater than
*         50 are in the 20th century and others are in the 21st
*         century. This will only work between the years 1951
*         and 2050.

            IF ( YEAR .GT. 50 ) THEN

               YEAR = YEAR + 1900
            ELSE

               YEAR = YEAR + 2000
            END IF

*          Convert the UT date into a 6 digit number of the form yymmdd
            UTDATE = MOD( YEAR, 100)*10000 + MONTH*100 + DAY
            print *, utdate
         END IF

      END IF

*   Construct a default name for the index file
      INDEX_FILE = ' '
      WRITE( INDEX_FILE, 2001 ) UTDATE
 2001 FORMAT( 'cgs4_', I6.6, '.index' )

*   Now obtain the name of the index file, using the above as
*   a dynamic default.
      CALL PAR_DEF0C( 'INDEX_FILE', INDEX_FILE, STATUS )
      CALL PAR_GET0C( 'INDEX_FILE', INDEX_FILE, STATUS )

*   Look for a directory spec. given as an environmental variable. If
*   present then translate it.
      CPOS = INDEX (INDEX_FILE, ':')
      IF (CPOS .EQ. 0) CPOS = INDEX (INDEX_FILE, '/')
      IF (CPOS .NE. 0) THEN
         CALL RED4_TRANSLATE_PREFIX (INDEX_FILE(:CPOS), TRANSL, STATUS)
         INDEX_FILE = TRANSL(:CHR_LEN(TRANSL))//INDEX_FILE(CPOS+1:)
      END IF

*   Obtain the destination for the output, which may be SCREEN, FILE
*   or PRINTER, and convert it to upper case.
      CALL PAR_GET0C( 'DESTINATION', DESTINATION, STATUS )
      CALL CHR_UCASE( DESTINATION )

*   If the output is to be written to a file or to the printer,
*   obtain the name of the file to be written. Otherwise obtain
*   the screen format type required.
      IF ( ( DESTINATION .EQ. 'FILE' ) .OR.
     :     ( DESTINATION .EQ. 'PRINTER' ) ) THEN

         CALL PAR_GET0C( 'FILE', FILE, STATUS )
      ELSE

         CALL PAR_GET0I( 'OFORMAT', OFORMAT, STATUS )
      END IF

*   Check this has worked.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Set the PRINTER flag, according to whether the output
*      is to be printed.
         IF ( DESTINATION .EQ. 'PRINTER' ) THEN

            PRINTER = .TRUE.
         ELSE

            PRINTER = .FALSE.
         END IF

*      Now call the appropriate routine to do the actual listing.
         IF ( DESTINATION .EQ. 'SCREEN' ) THEN

            CALL RED4_LIST_INDEX_SCREEN( INDEX_FILE, OFORMAT, STATUS )
         ELSE

            CALL RED4_LIST_INDEX_FILE( INDEX_FILE, FILE,
     :        PRINTER, STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_LIST_INDEX: '/
     :     /'Error obtaining input parameters', STATUS )
      END IF

      END



