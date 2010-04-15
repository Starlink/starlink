*+   RED4_MAKE_OBSERVATION action to make a dummy observation
      SUBROUTINE RED4_MAKE_OBSERVATION (STATUS)
*    Description :
*     This routine makes a dummy observation file, which may be used
*     for test purposes.
*    Invocation :
*     CALL RED4_MAKE_OBSERVATION (STATUS)
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     Badly structured and insufficient documentation!
*
*     The system IDATE routine does not distinguish between years
*     before or after the year 2000. A fudge is used which assumes
*     that year numbers greater than 50 are in the 20th century
*     and year numbers less than or equal to 50 are in the 21st
*     century. (Now that only 2 digit year numbers are used this
*     fudge is hidden anyway, but may reappear if the specification
*     were to change again ...).
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     S.M.Beard   (REVAD::SMB)
*     P.N.Daly    (JACH::PND)
*    History :
*     1989:        Original version.                                (JFL)
*      2-Mar-1990: History added. SLIT_NUMBER changed to SLIT_NAME. (SMB)
*      2-Mar-1990: CVF_STATE changed to CVF_NAME.                   (SMB)
*     24-Apr-1990: Because of memory corruption problems, the
*                  code needs to be compiled with array bounds
*                  checking switched on. The Figaro dynamic
*                  memory functions (DYN_ELEMENT, DYNAMIC_MEM,
*                  DYN_INCREMENT) would not allow this. Code
*                  modified to use %val() instead.                  (SMB)
*      5-Jul-1990: Bug fix: CVF_STATE not changed to CVF_NAME
*                  properly.                                        (SMB)
*     23-Jul-1990: Character handling improved, and modified
*                  to accept 4 digit observation numbers. Also code
*                  spaced out more.                                 (SMB)
*     30-Jul-1990: Modified to use 4 digits for the year
*                  number instead of 2, so the software will
*                  work correctly after the year 2000. Also
*                  modified to use either the UT date or the
*                  local date according to the IF_HITIME
*                  parameter.                                       (SMB)
*     31-Jul-1990: Specification changed back to how it was !!!!
*                  Year number changed from 4 digits back to
*                  2. Modulo used instead of subtracting 1900,
*                  so the software will still work after the
*                  year 2000.                                       (SMB)
*     21-Aug-1990: Phase 1 of major changes: Header
*                  information in the observation and reduced
*                  observation files is now written in a .FITS
*                  structure.                                       (SMB)
*     29-Aug-1990: Typing mistakes fixed.                           (SMB)
*     22-Feb-1993: Conform to error strategy                        (PND)
*      7-Nov-1994: Make vaguely portable                   (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
      INCLUDE 'DSA_ERRORS'   ! DSA error codes
      INCLUDE 'RED4_COMMON.INC'
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER ICH_LEN              ! Figaro string length utility
      INTEGER CHR_LEN
*    Local variables :
*                                  ! Type of integration to be taken
      LOGICAL NDR                  ! T if non-destructive reads being used
      LOGICAL STATS                ! T if sector statistics enabled
      LOGICAL NT_ON                ! T if NTs being sent back
      LOGICAL WEIGHTED             ! T if weighted frames enabled
      INTEGER OBS_NUM              ! the number of this observation
      INTEGER NEXT_OBS             ! default number for next observation
      INTEGER UTDATE               ! The UT date used to make up default name
      INTEGER IIDATE               ! The integer UT date to write to observation
      INTEGER I, J                 ! temporary integer storage
      INTEGER L1                   ! length of string S1
      INTEGER DET_SIZE (2)         ! size of detector in pixels
      INTEGER ACTVAL               !
      INTEGER SECTOR (4,4)         ! array sector coordinates
      INTEGER TEMP (4)             ! temporary sector information
      INTEGER CLEN                 ! Non-black length of character string
      REAL R                       ! temporary real storage
      CHARACTER*80 OBSERVATION     ! name of observation file
      CHARACTER*80 OBJECT          ! Temporary parameter name string
      CHARACTER*80 INT_TYPE        ! the type of integration for this obs
      CHARACTER*20 MODE            ! CHOPping or STAREing
      CHARACTER*80 STRING          ! temporary string storage
      CHARACTER*20 S1              ! observation number in string form
      CHARACTER*8  DATE_STRING     ! UTdate
      CHARACTER*20 LPREFIX         ! prefix to add to file
      LOGICAL IF_HITIME            ! T if running with Hawaii time
*-

      IF (STATUS .NE. ADAM__OK) RETURN

      CALL DSA_OPEN (STATUS)

*    Get the number of the observation, construct the default name,
*    get the desired name.
      CALL PAR_GET0I ('OBS_NUM', OBS_NUM, STATUS)
      NEXT_OBS = OBS_NUM + 1
      CALL PAR_DEF0I ('OBS_NUM', NEXT_OBS, STATUS)

*   Obtain the parameter determining whether the system is running
*   with Hawaii time.
      CALL PAR_GET0L( 'IF_HITIME', IF_HITIME, STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Obtain the current UT date.
         CALL RED4_GET_UTDATE( DATE_STRING, STATUS )
         CALL CHR_CTOI( DATE_STRING, IIDATE, STATUS )
         CALL CHR_CTOI( DATE_STRING(3:ICH_LEN(DATE_STRING)), UTDATE, STATUS )
      END IF

      OBSERVATION = ' '
      CALL RED4_GET_PREFIX ('O', LPREFIX, STATUS)
      WRITE (OBSERVATION, '(A,''o'',I6.6,''_'')')
     :  LPREFIX(:CHR_LEN(LPREFIX)), UTDATE

      CALL CHR_ITOC( OBS_NUM, S1, L1 )
      OBSERVATION = OBSERVATION(:ICH_LEN(OBSERVATION))//S1(:L1)
      CALL PAR_DEF0C ('OBSFILE', OBSERVATION, STATUS)
      CALL PAR_GET0C ('OBSFILE', OBSERVATION, STATUS)

*    Create the file according to the obsfile template
      CALL DSA_NAMED_INPUT ('OBS_TEMP', OBSERVATION_TEMPLATE, STATUS)
      CALL DSA_NAMED_OUTPUT ('OBSERVATION', OBSERVATION(:
     :   ICH_LEN(OBSERVATION)), 'OBS_TEMP', 0, 0, STATUS)

*   If an error has occurred, report it
      IF (STATUS .NE. ADAM__OK) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_MAKE_OBSERVATION: '/
     :     /'Error opening template observation', STATUS )
      END IF

*    set the names of the objects in the .GENERAL extension
      CALL DSA_PUT_FITS_C( 'OBSERVATION', 'OBSERVER',
     :   'Bill, Ben and Little Weed', ' ', STATUS)
      CALL DSA_PUT_FITS_C( 'OBSERVATION', 'UTDATE',
     :   '1990:08:29', ' ', STATUS )
      CALL DSA_PUT_FITS_C( 'OBSERVATION', 'SOFTWARE',
     :   'CD4 - version 0.0', ' ', STATUS )

*    Same for .OBJECT extension
      CALL DSA_PUT_FITS_C( 'OBSERVATION', 'OBJECT',
     :   'Homunculus', ' ', STATUS )
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'EQUINOX', 1950.0, ' ',
     :   STATUS)
            CALL DSA_PUT_FITS_F( 'OBSERVATION', 'RA', 12.250015, ' ',
     :   STATUS)
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'DEC', 18.0100, ' ',
     :   STATUS)

*    For .START_END extension
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'RUTSTART', 12.000001,
     :   ' ', STATUS)
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'RUTEND', 12.350001,
     :   ' ', STATUS)
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'AMSTART', 1.05, ' ',
     :   STATUS)
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'AMEND', 1.25, ' ',
     :   STATUS)

*    For .CGS4_OBSDETAIL extension
      CALL DSA_PUT_FITS_I( 'OBSERVATION', 'GRPNUM', 1, ' ',
     :   STATUS)
      CALL DSA_PUT_FITS_I( 'OBSERVATION', 'OBSNUM', OBS_NUM, ' ',
     :   STATUS)
      CALL DSA_PUT_FITS_I( 'OBSERVATION', 'IDATE', IIDATE, ' ',
     :   STATUS)

      OBJECT = 'OBS_TYPE'
      CALL PAR_GET0C (OBJECT, STRING, STATUS)
      CLEN = MAX( 1, ICH_LEN( STRING ) )
      CALL DSA_PUT_FITS_C( 'OBSERVATION', 'OBSTYPE', STRING(1:CLEN),
     :   ' ', STATUS)

*    determine the type of integration to be used, construct the 'type' string
      CALL PAR_GET0C ('STARE_CHOP', MODE, STATUS)
      CALL PAR_GET0L ('NDR', NDR, STATUS)
      CALL PAR_GET0L ('STATS', STATS, STATUS)
      CALL PAR_GET0L ('WEIGHTED', WEIGHTED, STATUS)

*    interlock between weighted and NTs
      IF (WEIGHTED) THEN

         NT_ON = .TRUE.
      ELSE

         CALL PAR_GET0L ('NT', NT_ON, STATUS)
      ENDIF

      IF (MODE .EQ. 'STARE') THEN

         INT_TYPE = 'STARE'
      ELSE

         INT_TYPE = 'CHOP'
      ENDIF

      IF (NDR) THEN

         INT_TYPE = INT_TYPE(:ICH_LEN(INT_TYPE))//'+NDR'
      ENDIF

      IF (STATS) THEN

         INT_TYPE = INT_TYPE(:ICH_LEN(INT_TYPE))//'+SECTORS'
      ENDIF

      IF (NT_ON) THEN

         INT_TYPE = INT_TYPE(:ICH_LEN(INT_TYPE))//'+NT'
      ENDIF

      IF (WEIGHTED) THEN

         INT_TYPE = INT_TYPE(:ICH_LEN(INT_TYPE))//'+WEIGHTED'
      ENDIF

*    and write the string
      CLEN = MAX( 1, ICH_LEN( INT_TYPE ) )
      CALL DSA_PUT_FITS_C( 'OBSERVATION', 'INTTYPE', INT_TYPE(1:CLEN),
     :   ' ', STATUS)

      OBJECT = 'EXPOSURE_TIME'
      CALL PAR_GET0R (OBJECT, R, STATUS)
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'DEXPTIME', R, ' ',
     :   STATUS)

      OBJECT = 'DET_INCR'
      CALL PAR_GET0R (OBJECT, R, STATUS)
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'DETINCR', R, ' ',
     :   STATUS)

      OBJECT = 'DET_NINCR'
      CALL PAR_GET0I (OBJECT, I, STATUS)
      CALL DSA_PUT_FITS_I( 'OBSERVATION', 'DETNINCR', I, ' ',
     :   STATUS)

*    For .CGS4_OPTICAL extension
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'DFOCUS', 1314.0, ' ',
     :   STATUS)
      CALL DSA_PUT_FITS_I( 'OBSERVATION', 'DENCBASE', 1066, ' ',
     :   STATUS)
      OBJECT = 'GRATING_NAME'
      CALL PAR_GET0C (OBJECT, STRING, STATUS)
      CLEN = MAX( 1, ICH_LEN( STRING ) )
      CALL DSA_PUT_FITS_C( 'OBSERVATION', 'GRATING', STRING(1:CLEN),
     :   ' ', STATUS)

      OBJECT = 'GRATING_WVLNGTH'
      CALL PAR_GET0R (OBJECT, R, STATUS)
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'GLAMBDA', R, ' ',
     :   STATUS)
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'GANGLE', 45.0, ' ',
     :   STATUS)
      CALL DSA_PUT_FITS_I( 'OBSERVATION', 'GORDER', 1, ' ',
     :   STATUS)
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'GLPMM', 75.0, ' ',
     :   STATUS)
      CALL DSA_PUT_FITS_C( 'OBSERVATION', 'SLIT', '3m', ' ',
     :   STATUS)

      OBJECT = 'SLIT_ANGLE'
      CALL PAR_GET0R (OBJECT, R, STATUS)
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'SANGLE', R, ' ',
     :   STATUS)
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'SLENGTH', 90.0, ' ',
     :   STATUS)
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'SWIDTH', 3.0, ' ',
     :   STATUS)
      CALL DSA_PUT_FITS_C( 'OBSERVATION', 'CVF', 'CVF1', ' ',
     :   STATUS)
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'CLAMBDA', 3.0, ' ',
     :   STATUS)

      OBJECT = 'FILTERS'
      CALL PAR_GET0C (OBJECT, STRING, STATUS)
      CLEN = MAX( 1, ICH_LEN( STRING ) )
      CALL DSA_PUT_FITS_C( 'OBSERVATION', 'FILTERS', STRING(1:CLEN),
     :   ' ', STATUS)
      CALL DSA_PUT_FITS_F( 'OBSERVATION', 'IRTANGLE', 45.0, ' ',
     :   STATUS)

*    For .CGS4_IRACS extension
      CALL DSA_PUT_FITS_C( 'OBSERVATION', 'DETECTOR', 'FPA 180', ' ',
     :   STATUS)

      OBJECT = 'DET_SIZE'
      CALL PAR_GET1I( OBJECT, 2, DET_SIZE, ACTVAL, STATUS )
      CALL DSA_PUT_FITS_I( 'OBSERVATION', 'DCOLUMNS', DET_SIZE(1), ' ',
     :   STATUS)
      CALL DSA_PUT_FITS_I( 'OBSERVATION', 'DROWS', DET_SIZE(2), ' ',
     :   STATUS)

*    Sector coordinates if required
      IF ( ( STATUS .EQ. ADAM__OK ) .AND.
     :     ( INDEX(INT_TYPE,'SECTORS') .NE. 0 ) ) THEN

*       the sectors
         DO J = 1, 4
            DO I = 1, 4

               SECTOR (I,J) = 0
            END DO
         END DO

         CALL PAR_GET1I ('SECTOR1', 4, TEMP, ACTVAL, STATUS)
         IF (TEMP(1) .NE. -1) THEN

            DO I = 1, 4

               SECTOR (I,1) = TEMP (I)
            END DO
         ENDIF

         CALL PAR_GET1I ('SECTOR2', 4, TEMP, ACTVAL, STATUS)
         IF (TEMP(1) .NE. -1) THEN

            DO I = 1, 4

               SECTOR (I,2) = TEMP (I)
            END DO
         ENDIF

         CALL PAR_GET1I ('SECTOR3', 4, TEMP, ACTVAL, STATUS)
         IF (TEMP(1) .NE. -1) THEN

            DO I = 1, 4

               SECTOR (I,3) = TEMP (I)
            END DO
         ENDIF

         CALL PAR_GET1I ('SECTOR4', 4, TEMP, ACTVAL, STATUS)
         IF (TEMP(1) .NE. -1) THEN

            DO I = 1, 4

               SECTOR (I,4) = TEMP (I)

            END DO
         ENDIF

         CALL DSA_PUT_FITS_I( 'OBSERVATION', 'S1X1', SECTOR(1,1),
     :     ' ', STATUS )
         CALL DSA_PUT_FITS_I( 'OBSERVATION', 'S1X2', SECTOR(2,1),
     :     ' ', STATUS )
         CALL DSA_PUT_FITS_I( 'OBSERVATION', 'S1X3', SECTOR(3,1),
     :     ' ', STATUS )
         CALL DSA_PUT_FITS_I( 'OBSERVATION', 'S1X4', SECTOR(4,1),
     :     ' ', STATUS )
         CALL DSA_PUT_FITS_I( 'OBSERVATION', 'S2X1', SECTOR(1,2),
     :     ' ', STATUS )
         CALL DSA_PUT_FITS_I( 'OBSERVATION', 'S2X2', SECTOR(2,2),
     :     ' ', STATUS )
         CALL DSA_PUT_FITS_I( 'OBSERVATION', 'S2X3', SECTOR(3,2),
     :     ' ', STATUS )
         CALL DSA_PUT_FITS_I( 'OBSERVATION', 'S2X4', SECTOR(4,2),
     :     ' ', STATUS )
         CALL DSA_PUT_FITS_I( 'OBSERVATION', 'S3X1', SECTOR(1,3),
     :     ' ', STATUS )
         CALL DSA_PUT_FITS_I( 'OBSERVATION', 'S3X2', SECTOR(2,3),
     :     ' ', STATUS )
         CALL DSA_PUT_FITS_I( 'OBSERVATION', 'S3X3', SECTOR(3,3),
     :     ' ', STATUS )
         CALL DSA_PUT_FITS_I( 'OBSERVATION', 'S3X4', SECTOR(4,3),
     :     ' ', STATUS )
         CALL DSA_PUT_FITS_I( 'OBSERVATION', 'S4X1', SECTOR(1,4),
     :     ' ', STATUS )
         CALL DSA_PUT_FITS_I( 'OBSERVATION', 'S4X2', SECTOR(2,4),
     :     ' ', STATUS )
         CALL DSA_PUT_FITS_I( 'OBSERVATION', 'S4X3', SECTOR(3,4),
     :     ' ', STATUS )
         CALL DSA_PUT_FITS_I( 'OBSERVATION', 'S4X4', SECTOR(4,4),
     :     ' ', STATUS )

      ENDIF

*    Finished, close DSA.
 500  CONTINUE
      CALL DSA_CLOSE (STATUS)

      END
