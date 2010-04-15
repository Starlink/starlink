*+   RED4_MAKE_INTEGRATION action to make a dummy integration
      SUBROUTINE RED4_MAKE_INTEGRATION (STATUS)
*    Description :
*     This routine makes a dummy integration for test purposes.
*    Invocation :
*     CALL RED4_MAKE_INTEGRATION (STATUS)
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     Badly structured and insufficient documentation.
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
*     1989: Original version.                                 (JFL)
*     24-Apr-1990: History added.                             (SMB)
*     24-Apr-1990: Because of memory corruption problems, the
*                  code needs to be compiled with array bounds
*                  checking switched on. The Figaro dynamic
*                  memory functions (DYN_ELEMENT, DYNAMIC_MEM,
*                  DYN_INCREMENT) would not allow this. Code
*                  modified to use %val() instead.            (SMB)
*     24-Apr-1990: DYN_INCREMENT replaced by direct
*                  manipulation of address (with the aid of
*                  DSA_TYPESIZE).                             (SMB)
*      9-Jul-1990: Commented out code removed.                (SMB)
*     23-Jul-1990: Character handling improved, and modified
*                  to accept 4 digit observation numbers. Also
*                  code spaced out.                           (SMB)
*     30-Jul-1990: Modified to use 4 digits for the year
*                  number instead of 2, so the software will
*                  work correctly after the year 2000. Also
*                  modified to use either the UT date of the
*                  local date according to the IF_HITIME
*                  parameter.                                 (SMB)
*     31-Jul-1990: Specification changed back to how it was !!!!
*                  Year number changed from 4 digits back to
*                  2. Modulo used instead of subtracting 1900,
*                  so the software will still work after the
*                  year 2000.                                 (SMB)
*      3-Sep-1990: Phase 2 of major changes: Header
*                  information in the integration and reduced
*                  integration files is now written in a .FITS
*                  structure. KTC mode removed.               (SMB)
*      4-Sep-1990: Typing mistakes fixed.                     (SMB)
*      7-Sep-1990: BEAM removed.                              (SMB)
*     22-Feb-1993: Conform to error strategy                  (PND)
*     11-Jan-1994: Allow NDFs via RESHAPE_DATA                (PND)
*     17-Jan-1994: Add parameterised template files           (PND)
*      7-Nov-1994: Make vaguely portable                   (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
      INCLUDE 'RED4_COMMON.INC'
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER ICH_LEN                 ! KS string manipulation stuff
      INTEGER DSA_TYPESIZE            ! DSA type size enquiry function
      INTEGER CHR_LEN
*    Local variables :
      INTEGER FLOATSIZE               ! Bytes per element of 'FLOAT' array
      INTEGER OBS_NUM                 ! the # of the observation responsible
      INTEGER INT_NUM                 ! the # of this integration
      INTEGER NEXT_INT                ! the # of the next integration
      INTEGER UTDATE                  ! the date in DAP's format
      INTEGER DIMS(10)                ! dimensions of data array
      INTEGER NT_DIMS(1)              ! dimensions of data array (NT structure)
      INTEGER ADDRESS                 ! DSA mapping stuff
      INTEGER DATA_SLOT               !        "
      INTEGER DATA_PTR                !        "
      INTEGER NT_SLOT                 !        "
      INTEGER NT_PTR                  !        "
      INTEGER DET_INDEX               ! index of detector position
      INTEGER N_EXPOSURES             ! number of exposures in this integration
      INTEGER CLEN                    ! Non-blank lengthy of character string.
      CHARACTER*10 S1                 ! string to hold obs_number
      INTEGER L1                      ! length of S1
      CHARACTER*10 S2                 ! same for integration number
      INTEGER L2                      ! length of S2
      REAL TIME                       ! times in HH.MMSS
      REAL AIRMASS                    ! need I say more?
      CHARACTER*80 OBSFILE            ! name of observation description file
      CHARACTER*80 INTEGRATION        ! name of integration file
      CHARACTER*80 INT_TYPE           ! the type of the integration
      CHARACTER*4 COMMENT             ! Dummy comment
      CHARACTER*8 DATE_STRING         ! UTdate as string yyyymmdd
      CHARACTER*20 LPREFIX            ! prefix to add to file
      LOGICAL IF_HITIME               ! T if running with Hawaii time
*-

      IF (STATUS .NE. ADAM__OK) RETURN

      CALL DSA_OPEN (STATUS)

*    Get the number of the observation and the number of
*    the integration, construct the default name of the integration file.
      CALL PAR_GET0I ('OBS_NUM', OBS_NUM, STATUS)
      CALL PAR_GET0I ('INT_NUM', INT_NUM, STATUS)

      NEXT_INT = INT_NUM + 1

      CALL PAR_DEF0I ('INT_NUM', NEXT_INT, STATUS)

*   Obtain the parameter determining whether the system is running
*   with Hawaii time.
      CALL PAR_GET0L( 'IF_HITIME', IF_HITIME, STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Obtain the current UT date.
         CALL RED4_GET_UTDATE( DATE_STRING, STATUS )
         CALL CHR_CTOI( DATE_STRING(3:ICH_LEN(DATE_STRING)), UTDATE, STATUS )
      END IF

      INTEGRATION = ' '

*    get the prefix to be used
      CALL RED4_GET_PREFIX ('I', LPREFIX, STATUS)

      WRITE (INTEGRATION, '(A,''i'',I6.6,''_'')')
     : LPREFIX(:CHR_LEN(LPREFIX)),UTDATE

      CALL CHR_ITOC( OBS_NUM, S1, L1 )
      CALL CHR_ITOC( INT_NUM, S2, L2 )

      INTEGRATION = INTEGRATION(:ICH_LEN(INTEGRATION))//
     :   S1(:L1)//'_'//S2(:L2)

      CALL PAR_DEF0C ('INTFILE', INTEGRATION, STATUS)
      CALL PAR_GET0C ('INTFILE', INTEGRATION, STATUS)

*    Construct the name of the parent observation file
      L1 = INDEX (INTEGRATION,':')
      IF (L1 .EQ. 0) L1 = INDEX (INTEGRATION,'/')
      L2 = INDEX (INTEGRATION,'_')
      L2 = L2 + INDEX (INTEGRATION(L2+1:),'_')
      CALL RED4_GET_PREFIX ('O', LPREFIX, STATUS)
      OBSFILE = LPREFIX(:CHR_LEN(LPREFIX))//'o'//INTEGRATION(L1+2:L2-1)

*    Open the observation file and find out what type of integration this is
*    and find the size of the detector array
      CALL RED4_CHECK_INPUT( OBSFILE, STATUS )
      CALL DSA_NAMED_INPUT ('OBSERVATION', OBSFILE, STATUS)

      CALL DSA_GET_FITS_C( 'OBSERVATION', 'INTTYPE', 0, INT_TYPE,
     :  COMMENT, STATUS )

      CALL DSA_GET_FITS_I( 'OBSERVATION', 'DCOLUMNS', 0, DIMS(1),
     :  COMMENT, STATUS )

      CALL DSA_GET_FITS_I( 'OBSERVATION', 'DROWS', 0, DIMS(2),
     :  COMMENT, STATUS )

*   Abort if an error has occurred at this point. (This is done for
*   consistency with the old code. GOTOs should be removed eventually).
      IF ( STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_MAKE_INTEGRATION: '/
     :     /'Error getting FITS items', STATUS )
      END IF

*    from the integration type work out the depth of the data array
      IF (INDEX(INT_TYPE,'STARE') .NE. 0) THEN

         DIMS(3) = 1
      ELSE IF (INDEX(INT_TYPE,'CHOP') .NE. 0) THEN

         DIMS(3) = 2
      ENDIF

      IF (INDEX(INT_TYPE,'SECTORS') .NE. 0) THEN

         DIMS(3) = DIMS(3) + 1
      ENDIF

*    Create the file according to the integration template file
      CALL DSA_NAMED_INPUT ('INT_TEMP', INTEGRATION_TEMPLATE, STATUS)
      CALL DSA_NAMED_OUTPUT ('INTEGRATION', INTEGRATION(:
     :   ICH_LEN(INTEGRATION)), 'INT_TEMP', 0, 1, STATUS)
      CALL DSA_USE_QUALITY( 'INTEGRATION', STATUS )

*    Coerce the data array to be the right size
      CALL DSA_RESHAPE_DATA ('INTEGRATION', 'INTEGRATION', 3, DIMS,
     :   STATUS)

*    Get the values of the various objects and set them
*    The name of the observation file (without the prefix)
      CLEN = MAX( 1, ICH_LEN( OBSFILE ) )

      CALL DSA_PUT_FITS_C( 'INTEGRATION', 'OBSFILE', OBSFILE(6:CLEN),
     :   ' ', STATUS)

      CALL DSA_PUT_FITS_I( 'INTEGRATION', 'INTNUM', INT_NUM,
     :  ' ', STATUS )

      CALL PAR_GET0R ('START_TIME', TIME, STATUS)
      CALL DSA_PUT_FITS_F( 'INTEGRATION', 'RUTSTART', TIME,
     :  ' ', STATUS )

      CALL PAR_GET0R ('END_TIME', TIME, STATUS)
      CALL DSA_PUT_FITS_F( 'INTEGRATION', 'RUTEND', TIME,
     :  ' ', STATUS )

      CALL PAR_GET0R ('START_AIRMASS', AIRMASS, STATUS)
      CALL DSA_PUT_FITS_F( 'INTEGRATION', 'AMSTART', AIRMASS,
     :  ' ', STATUS )

      CALL PAR_GET0R ('END_AIRMASS', AIRMASS, STATUS)
      CALL DSA_PUT_FITS_F( 'INTEGRATION', 'AMEND', AIRMASS,
     :  ' ', STATUS )

      CALL PAR_GET0I ('DET_INDEX', DET_INDEX, STATUS)
      CALL DSA_PUT_FITS_I( 'INTEGRATION', 'DINDEX', DET_INDEX,
     :  ' ', STATUS )

      CALL PAR_GET0I ('N_EXPOSURES', N_EXPOSURES, STATUS)
      CALL PAR_CANCL ('N_EXPOSURES', STATUS)
      CALL PAR_DEF0I ('N_EXPOSURES', N_EXPOSURES, STATUS)
      CALL DSA_PUT_FITS_I( 'INTEGRATION', 'NEXP', N_EXPOSURES,
     :  ' ', STATUS )

*   Abort if an error has occurred at this point. (This is done for
*   consistency with the old code. GOTOs should be removed eventually).
      IF ( STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_MAKE_INTEGRATION: '/
     :     /'Error putting FITS items', STATUS )
      END IF

*    map in the data array
      CALL DSA_MAP_DATA ('INTEGRATION', 'WRITE', 'FLOAT', ADDRESS,
     :   DATA_SLOT, STATUS)
      DATA_PTR = ADDRESS

*    Obtain the number of bytes per element in a data array of
*    type 'FLOAT' from DSA
      FLOATSIZE = DSA_TYPESIZE( 'FLOAT', STATUS )

*    Set the planes of the data array as required
*    Chop or stare?
      IF (INDEX(INT_TYPE,'STARE') .NE. 0) THEN

*      STARE mode.

         CALL MSG_OUT( ' ', 'Phase A frame...', STATUS )
         CALL RED4_MAKEDATA (DIMS(1), DIMS(2), %val(DATA_PTR),
     :      STATUS)

      ELSE

*      CHOP mode.
         CALL MSG_OUT( ' ', 'Phase A frame...', STATUS )
         CALL RED4_MAKEDATA (DIMS(1), DIMS(2), %val(DATA_PTR),
     :      STATUS)

*       point to next plane of data array
         CALL MSG_OUT( ' ', 'Phase B frame...', STATUS )
         DATA_PTR = DATA_PTR + FLOATSIZE * DIMS(1) * DIMS(2)
         CALL RED4_MAKEDATA (DIMS(1), DIMS(2), %val(DATA_PTR),
     :      STATUS)

      ENDIF

*   Abort if an error has occurred at this point. (This is done for
*   consistency with the old code. GOTOs should be removed eventually).
      IF ( STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_MAKE_INTEGRATION: '/
     :     /'Error making the array', STATUS )
      END IF

*    Sector statistics? Always on the bottom plane of the data array
      IF (INDEX(INT_TYPE,'SECTORS') .NE. 0) THEN

         CALL MSG_OUT( ' ', 'Statistics frame...', STATUS )
         DATA_PTR = DATA_PTR + FLOATSIZE * DIMS(1) * DIMS(2)
         CALL RED4_MAKEDATA (DIMS(1), DIMS(2), %val(DATA_PTR),
     :      STATUS)
      ENDIF

*    The NT section
      IF (INDEX(INT_TYPE,'NT') .NE. 0) THEN

*       open the CGS4_NT structure, so that the NT data array can
*       easily be accessed by a DSA call
         CALL DSA_NAMED_INPUT ('NT',
     :      INTEGRATION(:ICH_LEN(INTEGRATION))//
     :      '.MORE.CGS4_NT', STATUS)

*       the number of exposures was not known when the template was made
*       so create the data array now
         NT_DIMS(1) = N_EXPOSURES
         CALL DSA_COERCE_DATA_ARRAY ('NT', 'INT', 1, NT_DIMS, STATUS)

*       map in the data array so created
         CALL DSA_MAP_DATA ('NT', 'WRITE', 'INT', ADDRESS, NT_SLOT,
     :      STATUS)
         NT_PTR = ADDRESS

*       and construct it
         CALL RED4_MAKENT (%val(NT_PTR), N_EXPOSURES, STATUS)

      ENDIF

*    Finished
 500  CONTINUE

*   Close DSA and tidy up.
      CALL DSA_CLOSE (STATUS)

      END
