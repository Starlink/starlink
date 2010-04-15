*+  RED4_SEEK_OBSERVATION - Look up a calibration observation from index file
      SUBROUTINE RED4_SEEK_OBSERVATION ( INDEX_FILE, OBSREF,
     :  TYPE_REQUIRED, SEARCH_MODE, OBS_MATCH, AMTOLER, OBS_NAME,
     :  STATUS)
*    Description :
*     This subroutine searches the observation cache and index for a reduced
*     observation that will be of use in the reduction sequence of the
*     current integration/observation. For example the observation of
*     a star will need the results of BIAS, DARK, and FLAT
*     observations in its reduction sequence, each of these will be
*     selected using this routine.
*
*     Currently the routine selects the suitable observation of the
*     desired type that was taken closest in time to the observation
*     being reduced. The SEARCH_MODE argument controls whether
*     this search is made to cover observations taken before the current
*     observation (backwards search), after the current observation
*     (forwards search), or both. 'Suitability' depends on the type of the
*     desired observation, it may require many of the array exposure and
*     optical setup parameters to match those of the current observation.
*     Some of the matching criteria are compulsory, and others are optional
*     and selectable by parameter. The matching criteria are as follows:-
*
*     Desired obs. type     Matching criteria          FITS items
*     -----------------     -----------------          ----------
*
*     BIAS                  Detector size              DROWS, DCOLUMNS
*
*     DARK                  Detector size              DROWS, DCOLUMNS
*                           On-chip exposure time      DEXPTIME
*                           Observation mode, NDR      INTTYPE
*
*     FLAT                  Detector size              DROWS, DCOLUMNS
*                           Grating name (*)           GRATING
*                           Grating angle (*)          GANGLE
*                           Grating wavelength (*)     GLAMBDA
*                           Grating order (*)          GORDER
*                           Slit name (*)              SLIT
*                           Slit angle (*)             SANGLE
*                           CVF name (*)               CVF
*                           CVF wavelength (*)         CLAMBDA
*                           Filters (*)                FILTERS
*                           Configuration index (*)    CNFINDEX
*                           Warning if not normalised  NORMALIS
*
*     CALIBRATION           Detector columns           DCOLUMNS
*                           Oversampling parameters    DENCBASE, DETNINCR
*                           Grating name (*)           GRATING
*                           Grating angle (*)          GANGLE
*                           Grating wavelength (*)     GLAMBDA
*                           Grating order (*)          GORDER
*                           Slit name (*)              SLIT
*                           Slit angle (*)             SANGLE
*                           CVF name (*)               CVF
*                           CVF wavelength (*)         CLAMBDA
*                           Filters (*)                FILTERS
*                           Configuration index (*)    CNFINDEX
*
*     STANDARD              Detector size              DROWS, DCOLUMNS
*                           Oversampling parameters    DENCBASE, DETNINCR
*                           Grating name (*)           GRATING
*                           Grating angle (*)          GANGLE
*                           Grating wavelength (*)     GLAMBDA
*                           Grating order (*)          GORDER
*                           Slit name (*)              SLIT
*                           Slit angle (*)             SANGLE
*                           CVF name (*)               CVF
*                           CVF wavelength (*)         CLAMBDA
*                           Filters (*)                FILTERS
*                           Configuration index (*)    CNFINDEX
*                           Air mass (*)               AMSTART, AMEND
*
*     (*) Optional matching criteria, specified in OBS_MATCH argument.
*
*     On entry the parent observation file must have been opened as
*     OBSREF by DSA.
*    Invocation :
*      CALL RED4_SEEK_OBSERVATION ( INDEX_FILE, OBSREF, TYPE_REQUIRED,
*     :  SEARCH_MODE, OBS_MATCH, AMTOLER, OBS_NAME, STATUS)
*    Parameters :
*    Method :
*    Deficiencies :
*     This code needs restructuring without GOTOs. It is also far more
*     complicated than it needs to be, and requires simplification.
*
*     DSA status values do not conform to the ADAM scheme. It has
*     therefore been necessary to trap these statuses before they
*     get translated into confusing messages like "exceeded quota".
*     The traps can be removed once DSA conforms.
*
*     Real values, such as the grating wavelength, are tested against
*     an absolute tolerance. A percentage difference test should
*     perhaps have been used instead (as is done with air mass).
*    Bugs :
*    Authors :
*     J. Lightfoot (REVAD::JFL)
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*     A.Bridger    (JACH::AB)
*    History :
*     18-Dec-1989: Original version.                            (JFL)
*      4-Feb-1990: Changed to read observations into common     (JFL)
*      2-Mar-1990: SLIT_NUMBER changed to SLIT_NAME.            (SMB)
*      2-Mar-1990: CVF_STATE changed to CVF_NAME.               (SMB)
*     12-Mar-1990: Bug, in which wrong length obtained for
*                  OBSREC.SLIT_NAME, fixed. Status report
*                  included in error reports.  MAXDIM and
*                  IMAXDIM parameters included.                 (SMB)
*     23-Apr-1990: FLAT_FIELD changed to FLAT.                  (SMB)
*     23-Apr-1990: There is a very subtle bug in which,
*                  under some circumstances, DTA will write
*                  a different data structure to that which
*                  it has been told to create. I suspect
*                  this is a memory corruption problem. The
*                  problem seems to occur when a DARK
*                  observation is reduced after another one
*                  which has used the same BIAS. This routine
*                  is the prime candidate for the bug. Code
*                  checked and found to make use of some
*                  uninitialised variables. This fault
*                  corrected and code spaced out. Bug still
*                  not fixed, though. Error checking improved.
*                  Status checked after LIB$GET_LUN             (SMB)
*     23-Apr-1990: I/O errors were not being tested properly.
*                  "attempt to access non-existent record"
*                  errors should be treated as an end-of-file
*                  Relevant code included.                      (SMB)
*     24-Apr-1990: Because of memory corruption problems, the
*                  code needs to be compiled with array bounds
*                  checking switched on. The Figaro dynamic
*                  memory functions (DYN_ELEMENT, DYNAMIC_MEM,
*                  DYN_INCREMENT) would not allow this. Code
*                  modified to use %val() instead.              (SMB)
*     27-Apr-1990: The "memory corruption/DTA" bug does not
*                  occur if a DARK is reduced which requires
*                  a different BIAS. Try a fudge in which a
*                  new BIAS is always mapped for a DARK, to
*                  allow the system to be used while the real
*                  bug is hunted. The fudge worked.             (SMB)
*     27-Apr-1990: The GOTO structure was found to be incorrect.
*                  If an error occurred, the routine would
*                  attempt to close a file which hadn't been
*                  opened, and free a logical unit which had
*                  not been obtained. GOTOs corrected, but they
*                  should eventually be removed!!               (SMB)
*     27-Apr-1990: Fudge to get over "memory corruption/DTA"
*                  bug removed, so the code may be debugged.    (SMB)
*     27-Apr-1990: Close scrutiny of the code revealed more
*                  bugs. The check for TYPE_REQUIRED being
*                  invalid was made in the wrong place.
*                  Also, a typing mistake was found in which
*                  LIB$FREE_VM could free a different amount
*                  of virtual memory than that allocated by
*                  LIB$GET_VM. These bugs fixed. Substantially
*                  more comments added after consultation with
*                  JFL.                                         (SMB)
*     30-Apr-1990: Fudge to get around DTA/corruption bug now
*                  implemented permenantly, since
*                  investigations have failed to track the bug
*                  down. It is logged as an outstanding bug.   (SMB)
*      5-Jul-1990: Major changes: It has been decided that the
*                  requirement that flat fields should be taken
*                  with identical instrument settings be dropped,
*                  as flat fields can now be normalised to take
*                  out large scale variations with wavelength.
*                  Flat field index pointers and dimensions are
*                  therefore no longer needed, and have been
*                  removed from the common block and are no
*                  longer initialised by this routine. The
*                  contents of the index file remains the same
*                  "for historical reasons" to maintain
*                  compatibility with existing index files.
*                  Definition of OBSREC structure moved to
*                  include file. Commented out code removed.  (SMB)
*      9-Jul-1990: It has now been decided that this routine
*                  should insist that a flat-field has the
*                  same grating name, grating order, slit name,
*                  slit angle, cvf name and filters, but not
*                  exposure time. This is because these parameters
*                  may affect the instrument vigneting along the
*                  slit, which cannot be removed by normnalisation.
*                  The grating order and slit angle checks should
*                  be commented out for the time being so as not
*                  to frustrate the engineering checks. Original
*                  code modified so that checks for equality
*                  between REAL values are not made.          (SMB)
*      9-Jul-1990: Modified so the reasons for rejecting
*                  an observation are given.                  (SMB)
*     12-Jul-1990: Routine checked to ensure all character
*                  strings are converted to upper case before
*                  comparison. Discovered SLIT_NAME was not
*                  being converted - fixed.                   (SMB)
*     12-Jul-1990: Fudge added for engineering checks. ANY
*                  flat-field will be accepted.               (SMB)
*     12-Jul-1990: Fudge removed. As it is likely that the
*                  items which need to be matched for a
*                  suitable flat-field will vary during
*                  future engineering tests, these items are
*                  now obtained via a parameter, FLAT_MATCH,
*                  which is defaulted in the interface file.  (SMB)
*     20-Jul-1990: Messages about DARK and FLAT frames having
*                  "incorrect" settings was confusing. Changed
*                  to "non-matching".                         (SMB)
*     23-Jul-1990: Character handling improved by using utility
*                  routines to convert between file names, and
*                  modified to accept 4 digit observation numbers. (SMB)
*     23-Jul-1990: INDDIR changed to CGS4_INDEX.                   (SMB)
*     24-Jul-1990: Mistake fixed. The change to the format of the
*                  index file has also changed its KEY positions.  (SMB)
*     28-Aug-1990: Phase 1 of major changes: Header
*                  information in the observation and reduced
*                  observation files is now written in a .FITS
*                  structure. Linearisation coefficients are
*                  now obtained from a file rather than being
*                  buried in the data structure. KTC mode
*                  replaced by NDR.                                (SMB)
*      3-Sep-1990: Phase 2 of major changes: Header
*                  information in the integration and reduced
*                  integration files is now written in a .FITS
*                  structure.                                      (SMB)
*      4-Sep-1990: ERR_OUT replaced with ERR_REP.                  (SMB)
*      6-Sep-1990: I think the DTA cache bug has been fixed. Code
*                  uncommented tentatively. Also modified to check
*                  the NORMALIS item in the FITS structure to find
*                  out if a FLAT has been normalised.              (SMB)
*      7-Sep-1990: All output commented out, as it was too verbose.(SMB)
*      2-Oct-1990: Made to open index file READONLY.               (SMB)
*     23-Oct-1990: Commented out code removed.                     (SMB)
*     19-Nov-1990: Index file format changed again to include
*                  parameters required for wavelength and flux
*                  calibration. Ability to search for CALIBRATION
*                  observation added. Quantity of output controlled
*                  by VERBOSE flag. "Non-matching" replaced by
*                  the more grammatical "different"                (SMB)
*     21-Nov-1990: INTREF replaced by INDEX_FILE, to try and make
*                  this routine more general-purpose. As a side
*                  effect of this, the end time is now obtained
*                  from the OBSERVATION file and not the
*                  INTEGRATION file.                               (SMB)
*     21-Nov-1990: Mistake correction: A CALIBRATION observation
*                  does not have to be opened and mapped.          (SMB)
*     29-Nov-1990: MAXDIM moved to RED4_COMMON.INC                 (SMB)
*     12-Dec-1990: Index file format changed again! This is because
*                  it is possible for different observations to be
*                  made in NDR mode with different effective detector
*                  sizes, and the calibration frames for these must
*                  not get mixed up. Description improved.
*                  Bug fix. The TIME will no longer wrap at 24.0,
*                  so remove the midnight adjustment.              (SMB)
*     18-Dec-1990: Typing mistakes fixed.                          (SMB)
*     17-Jan-1991: END_TIME replaced by START_TIME.                (SMB)
*      7-Feb-1991: Bug in calculation of mean air mass fixed.      (SMB)
*      8-Feb-1991: Observation mode (integration type) matched for
*                  DARK observations.                              (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed, which would
*                  have made this routine fail under ADAM V1.9.    (SMB)
*     24-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.           (SMB)
*     27-Feb-1991: Match tolerance reduced slightly. (It would
*                  probably be better if real values were matched
*                  by percentage, as with air mass).               (SMB)
*     20-May-1991: Fix bug with GLAMBDA which was being compared
*                  with GANGLE                                     (PND)
*      7-Oct-1991: Major change to incorporate DRIMP/5.1 and 5.2.
*                  Parts of this routine have been transferred to
*                  RED4_GET_OBSERVATION, which now calls this
*                  routine. Ability to search either forwards or
*                  backwards in time, or both, added. Note that
*                  FLAT_MATCH, CALIB_MATCH and STANDARD_MATCH
*                  have been replaced by OBS_MATCH.                (SMB)
*      8-Oct-1991: "STANDARD observation" message changed to
*                  "STANDARD group".                               (SMB)
*     20-Oct-1991: Add some verbose messages about which files
*                  are acceptable etc                              (PND)
*     13-May-1992: Fix bug reported on 5 May 1992. The routine was
*                  always finding the earliest suitable observation
*                  rather than the nearest to the observation being
*                  reduced. The cause was MIN_ADIFF being set to
*                  the minimum value of DIFF rather than ADIFF (i.e.
*                  the sign was not being removed from the time
*                  difference).                                    (SMB)
*     23-Feb-1993: Conform to error strategy                       (PND)
*     30-Jun-1993: Remove DSA_WRUSER calls etc                     (PND)
*      6-Dec-1993: Update for IRCAM                                (PND)
*     11-Nov-1994: More portable version                           (AB)
*     28-Nov-1994: Major changes to searching - using direct access
*                  file i/o.                                       (AB)
*     22-May-1995: Add oversampled FLATs                           (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'
*    Global variables:
      INCLUDE 'RED4_COMMON.INC'        ! RED4 common block containing pointers
*                                          to data in virtual memory.
*    Import :
      CHARACTER*(*) INDEX_FILE         ! The name of the index file to be
*                                          searched. should be of form
*                                          CGS4_890818.INDEX
      CHARACTER*(*) OBSREF             ! The DSA reference name for the
*                                          observation file.
      CHARACTER*(*) TYPE_REQUIRED      ! the type of the reduced observation that
*                                          is required
      CHARACTER*(*) SEARCH_MODE        ! The search mode required:
*                                      !   'BACKWARDS' - backwards in time
*                                      !   'FORWARDS'  - forwards in time
*                                      !   'BOTH'      - backwards and forwards
      CHARACTER*(*) OBS_MATCH          ! String containing the names of the
*                                      !   items which must match for the
*                                      !   observation to be considered
*                                      !   suitable. This string will have
*                                      !   come from either the FLAT_MATCH,
*                                      !   CALIB_MATCH or STANDARD_MATCH
*                                      !   parameters.
      REAL AMTOLER                     ! The air mass % tolerance
*    Export:
      CHARACTER*(*) OBS_NAME           ! the name of the file containing the
*                                           desired reduced observation
*    Status :
      INTEGER STATUS
*    External references:
      INTEGER CHR_LEN                  ! Length of character string function
*    Local Constants :
      REAL TOLER                       ! The difference at which two real
      PARAMETER ( TOLER = 1.0E-4 )     ! values are said to be different.
      REAL AMTHRESH                    ! Threshold airmass below which test
                                       ! becomes absolute rather than %age
      PARAMETER ( AMTHRESH = 0.2 )
*    Local variables :
*
*           --- general variables ---
      LOGICAL LOOPING                  ! T if looping through entries in index
*                                           file
      LOGICAL FOUND                    ! T if a suitable entry is found
      LOGICAL FLAT_OK                  ! Flag indicating if a FLAT is suitable
      LOGICAL CALIB_OK                 ! Flag indicating if a CALIBRATION is suitable
      LOGICAL STANDARD_OK              ! Flag indicating if a STANDARD is suitable
      INTEGER DSA_STATUS               ! DSA status value
      INTEGER FD                       ! File descriptor
      INTEGER NDRPOS1                  ! Position of 'NDR' string in character
      INTEGER NDRPOS2                  ! Position of 'NDR' string in character
      INTEGER CLEN                     ! Non-blank length of character string
      INTEGER RECNUM                   ! Record number in file
      INTEGER INC                      ! Amount to increment record number by
      REAL AMDIFF                      ! The air mass % difference
      CHARACTER*20 LPREFIX             ! To hold directory prefix
      CHARACTER*4 COMMENT              ! Dummy comment
*                                           I/O error that occurs
      CHARACTER*132 DIFFERENCES        ! The differences between the
*                                      !    required and the actual
*                                      !    configuration of an observation.
*
*           --- variables holding description of observation being reduced ---
      CHARACTER*20 INTTYPE             ! Observation mode.
      INTEGER GRATING_ORDER            ! The order in which the grating is
*                                            working
      INTEGER OBSNUM                   ! Observation number
      CHARACTER*20 INSTRUMENT         ! The instrument
      CHARACTER*10 SLIT_NAME           ! The name of the slit in use
      INTEGER DET_NROWS                ! The number of detector rows
      INTEGER DET_NCOLUMNS             ! The number of detector columns
      INTEGER DET_ENC_BASE             ! The base position of the detector
*                                           translation mechanism for this
*                                           observation
      INTEGER DET_NINCR                ! The number of detector positions
*                                           measured in this observation
      INTEGER CNFINDEX                 ! The instrument configuration index.
      REAL TIME                        ! The time at which this observation
*                                           ended, e.g. 23.4856 (hh.mmss)
      REAL DIFF                        ! The difference in hours between
*                                           this observation ending and the
*                                           end of a particular observation of
*                                           the desired type
      REAL ADIFF                       ! The absolute value of DIFF.
      REAL MIN_ADIFF                   ! The minimum value of ADIFF
      REAL EXPOSURE                    ! The on-chip exposure time used
*                                           during the observation currently
*                                           being examined on the index
      REAL GRATING_WVLNGTH             ! The wavelength to which the grating
*                                           set during the observation
      REAL GRATING_ANGLE               ! The angle of incidence of the input
*                                           beam on the grating
      REAL SLIT_ANGLE                  ! The angle of the slit during the
*                                           observation
      REAL CVF_WAVELENGTH              ! The wavelength to which the CVF was set
*                                           during the observation
      REAL AMSTART                     ! Air mass at start of observation
      REAL AMEND                       ! Air mass at end of observation
      REAL AIRMASS                     ! The mean air mass of the observation
      CHARACTER*80 GRATING_NAME        ! The name of the grating used for the
*                                           observation to be reduced
      CHARACTER*80 CVF_NAME            ! The name of the CVF used during the
*                                           observation to be reduced
      CHARACTER*80 FILTERS             ! The filter combination used during
*                                           the observation to be reduced
*
      RECORD /OBSREC/ OBSREC           ! An observation record
*                                      ! (structure defined in RED4_COMMON.INC)
*
*    Local data :
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialise OBS_NAME to ' ', indicating file not yet found.
      OBS_NAME = ' '

*    Report the name of the index file and the type of observation
*    being searched for.
*    Index files are found in the directory whose logical name is
*    CGS4_INDEX.
      IF ( VERBOSE ) THEN

         CALL MSG_SETC( 'INDEX_FILE', INDEX_FILE )
         CALL MSG_SETC( 'TYPE_REQUIRED', TYPE_REQUIRED )
         CALL MSG_OUT( ' ', 'Searching ^INDEX_FILE for a reduced '/
     :     /'^TYPE_REQUIRED observation', STATUS )
      END IF

*    Get the time at which the observation to be reduced started.
      DSA_STATUS = STATUS
      CALL DSA_GET_FITS_F( OBSREF, 'RUTSTART', 0, TIME, COMMENT,
     :  DSA_STATUS )

*    And get the observation number
      CALL DSA_GET_FITS_I( OBSREF, 'OBSNUM', 0, OBSNUM, COMMENT,
     :  DSA_STATUS )

      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_SEEK_OBSERVATION: '/
     :     /'First error getting FITS item', STATUS )
      END IF

*   Abort if an error has occurred at this point. (This is done for
*   consistency with the old code. GOTOs should be removed eventually).
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_SEEK_OBSERVATION: '/
     :     /'First ADAM error has been detected', STATUS )
      END IF

*    All observation types require a match with the detector size,
*    so obtain the size of the detector in the observation being
*    reduced.
      CALL DSA_GET_FITS_I( OBSREF, 'DCOLUMNS', 0, DET_NCOLUMNS,
     :  COMMENT, DSA_STATUS )
      CALL DSA_GET_FITS_I( OBSREF, 'DROWS', 0, DET_NROWS,
     :  COMMENT, DSA_STATUS )

*    If the type of observation required is a DARK then we need to
*    know the exposure time also, since a DARK frame can
*    only be used to reduce data when the exposure times match.
*    The observation mode (integration type) also needs to be checked,
*    to ensure that a DARK observation with the appropriate destructive
*    or non-destructive read mode is found.
      IF (TYPE_REQUIRED .EQ. 'DARK') THEN

         CALL DSA_GET_FITS_F( OBSREF, 'DEXPTIME', 0, EXPOSURE,
     :     COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_C( OBSREF, 'INTTYPE', 0, INTTYPE,
     :     COMMENT, DSA_STATUS )
      ENDIF

*    If the type required is a FLAT, CALIBRATION or STANDARD then we must
*    obtain the parameters describing the  optical configuration of the
*    instrument. (Note that the actual parameters which are tested are
*    selectable via a parameter). The character parameters are converted
*    to upper case.
      IF ( ( TYPE_REQUIRED .EQ. 'FLAT' ) .OR.
     :     ( TYPE_REQUIRED .EQ. 'CALIBRATION' ) .OR.
     :     ( TYPE_REQUIRED .EQ. 'STANDARD' ) ) THEN

*      Instrument
         CALL DSA_GET_FITS_C( OBSREF, 'INSTRUME', 0, INSTRUMENT,
     :     COMMENT, DSA_STATUS )
         CALL CHR_RMBLK( INSTRUMENT )
         CALL CHR_UCASE( INSTRUMENT )

         IF ( INDEX( INSTRUMENT, 'IRCAM' ) .EQ. 0  .AND.
     :        INDEX( INSTRUMENT, 'ALICE' ) .EQ. 0 ) THEN

*         Grating name
            CALL DSA_GET_FITS_C( OBSREF, 'GRATING', 0, GRATING_NAME,
     :        COMMENT, DSA_STATUS )
            CALL CHR_UCASE( GRATING_NAME )

*         Grating angle
            CALL DSA_GET_FITS_F( OBSREF, 'GANGLE', 0, GRATING_ANGLE,
     :        COMMENT, DSA_STATUS )

*         Grating order
            CALL DSA_GET_FITS_I( OBSREF, 'GORDER', 0, GRATING_ORDER,
     :        COMMENT, DSA_STATUS )

*         Grating wavelength
            CALL DSA_GET_FITS_F( OBSREF, 'GLAMBDA', 0, GRATING_WVLNGTH,
     :        COMMENT, DSA_STATUS )

*         Slit name
            CALL DSA_GET_FITS_C( OBSREF, 'SLIT', 0, SLIT_NAME,
     :        COMMENT, DSA_STATUS )
            CALL CHR_UCASE( SLIT_NAME )

*         Slit angle
            CALL DSA_GET_FITS_F( OBSREF, 'SANGLE', 0, SLIT_ANGLE,
     :        COMMENT, DSA_STATUS )

*         CVF name
            CALL DSA_GET_FITS_C( OBSREF, 'CVF', 0, CVF_NAME,
     :        COMMENT, DSA_STATUS )
            CALL CHR_UCASE( CVF_NAME )

*         CVF wavelength
            CALL DSA_GET_FITS_F( OBSREF, 'CLAMBDA', 0, CVF_WAVELENGTH,
     :        COMMENT, DSA_STATUS )

*         Filters
            CALL DSA_GET_FITS_C( OBSREF, 'FILTERS', 0, FILTERS,
     :        COMMENT, DSA_STATUS )
            CALL CHR_UCASE( FILTERS )

*        The overall instrument configuration index. (This index combines
*        all the factors above. Matching this is the severest test which
*        can be imposed).
            CALL DSA_GET_FITS_I( OBSREF, 'CNFINDEX', 0, CNFINDEX,
     :        COMMENT, DSA_STATUS )
         ELSE

*         Filters
            CALL DSA_GET_FITS_C( OBSREF, 'FILTER', 0, FILTERS,
     :        COMMENT, DSA_STATUS )
            CALL CHR_UCASE( FILTERS )

            GRATING_NAME    = '(NONE)'
            GRATING_WVLNGTH = 0.0
            GRATING_ANGLE   = 0.0
            GRATING_ORDER   = 0
            SLIT_NAME       = '(NONE)'
            SLIT_ANGLE      = 0.0
            CVF_NAME        = '(NONE)'
            CVF_WAVELENGTH  = 0.0
            CNFINDEX        = 0
         ENDIF
      ENDIF

*   If the type required is a FLAT, CALIBRATION or STANDARD, there must be a match
*   in the oversampling parameters, so obtain these.
      IF ( ( TYPE_REQUIRED .EQ. 'FLAT' ) .OR.
     :     ( TYPE_REQUIRED .EQ. 'CALIBRATION' ) .OR.
     :     ( TYPE_REQUIRED .EQ. 'STANDARD' ) ) THEN

         CALL DSA_GET_FITS_I( OBSREF, 'DENCBASE', 0, DET_ENC_BASE,
     :     COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_I( OBSREF, 'DETNINCR', 0, DET_NINCR,
     :     COMMENT, DSA_STATUS )
      ENDIF

*   If the type required is a STANDARD, then the mean air mass may also be
*   checked. Assume the mean air mass is mid way between the air mass at
*   the start and at the end.
      IF ( TYPE_REQUIRED .EQ. 'STANDARD' ) THEN

         CALL DSA_GET_FITS_F( OBSREF, 'AMSTART', 0, AMSTART,
     :     COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_F( OBSREF, 'AMEND', 0, AMEND,
     :     COMMENT, DSA_STATUS )
         AIRMASS = (AMSTART + AMEND) / 2.0
      END IF

*   Abort if an error has occurred at this point. (This is done for
*   consistency with the old code. GOTOs should be removed eventually).
      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_SEEK_OBSERVATION: '/
     :     /'Second error getting FITS items', STATUS )
      END IF

      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_SEEK_OBSERVATION: '/
     :     /'Second ADAM error has been detected', STATUS )
      END IF

*   Open the index file
      CLEN = CHR_LEN( INDEX_FILE )

      CALL RIO_OPEN (INDEX_FILE, 'READ', 'UNFORMATTED', OBSRECSZ, FD,
     : STATUS)

      IF (STATUS .NE. SAI__OK) THEN

*       File open error
         CALL MSG_SETC( 'INDEX', INDEX_FILE )
         CALL ERR_OUT( ' ', 'Unable to open index file ^INDEX', STATUS )
      ELSE

*       Arbitrarily set the minimum time different to 24000 hours (1000
*       days), as it is unlikely that any data reduction session will last
*       this long (unless the VAX is very very busy??).
         MIN_ADIFF = 24000.0

*       Search for the first observation of the correct type.

*       Starting record is that of the observation being reduced
*       Increment +ve if searching forwards, -ve if searching backwards.
*       If searching both then do backwards first.
         RECNUM = OBSNUM
         IF (SEARCH_MODE .EQ. 'BACKWARDS') THEN
            INC = -1
         ELSE IF (SEARCH_MODE .EQ. 'FORWARDS') THEN
            INC = 1
         ELSE IF (SEARCH_MODE .EQ. 'BOTH') THEN
            INC = -1
         END IF
         LOOPING = .TRUE.
         FOUND = .FALSE.

*       Search loop
         DO WHILE (LOOPING .AND. STATUS .EQ. SAI__OK)

*          Increment the record number
            RECNUM = RECNUM + INC
            IF (RECNUM .LT. 1) THEN

*          this means searching backwards has found no match and we are at the
*          end of the records (record 0). If searching in 'BOTH' then reverse
*          the direction of search and start again. Otherwise quit.
               IF (SEARCH_MODE .EQ. 'BOTH') THEN
                  INC = 1
                  RECNUM = OBSNUM + 1
               ELSE
                  LOOPING = .FALSE.
               END IF
            END IF

*          read the record
            CALL RIO_READ (FD, RECNUM, OBSRECSZ, OBSREC, STATUS)
            IF (STATUS .NE. SAI__OK) THEN
*             on end of file if going backwards then simply annul it - it
*             could be that the observation before the one we are reducing
*             has not been filed - only abort a backwards search when reach
*             record 0, or a more serious error.
               IF (STATUS .EQ. FIO__EOF) THEN
                  CALL ERR_ANNUL (STATUS)
                  IF (INC .GT. 0) LOOPING = .FALSE.
               ELSE IF (STATUS .EQ. FIO__OUTFL) THEN
                  CALL ERR_ANNUL (STATUS)
                  IF (INC .GT. 0) LOOPING = .FALSE.
               ELSE
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_SEEK_OBSERVATION: '/
     :             /'Index file I/O error', STATUS )
               END IF
            ELSE

*             Convert the character items just read to upper case, so they
*             may be tested more easily.
               CALL CHR_UCASE( OBSREC.QUALITY )
               CALL CHR_UCASE( OBSREC.TYPE )
               CALL CHR_UCASE( OBSREC.INTTYPE )
               CALL CHR_UCASE( OBSREC.GRATING_NAME )
               CALL CHR_UCASE( OBSREC.SLIT_NAME )
               CALL CHR_UCASE( OBSREC.CVF_NAME )
               CALL CHR_UCASE( OBSREC.FILTERS )

*             check that this observation is good quality and the type is
*             correct
               IF (OBSREC.QUALITY .EQ. 'GOOD' .AND. OBSREC.TYPE .EQ.
     :          TYPE_REQUIRED) THEN

*                The checks used to determine whether an observation is
*                suitable depends on the type of observation required
                  IF (TYPE_REQUIRED .EQ. 'BIAS') THEN

*                   BIAS required. A BIAS must have been taken with the same
*                   detector size.
*                   A suitable BIAS taken most closely in time to the current
*                   observation is used.
                     IF ( (OBSREC.DET_NROWS .EQ. DET_NROWS) .AND.
     :                (OBSREC.DET_NCOLUMNS .EQ. DET_NCOLUMNS) ) THEN

                        FOUND = .TRUE.

*                      A suitable BIAS has been found.
*                      If searching in only one direction then just assume it
*                      is the closest in time   in that direction. Logically it
*                      must be. If searching in both directions then if this
*                      is the first direction (backwards) accept this obs. and
*                      start searching forwards. If searching forwards then
*                      compare time differences to get the best one and then
*                      quit the loop.

                        IF ( SEARCH_MODE .EQ. 'BOTH' ) THEN

                           DIFF = OBSREC.START_TIME - TIME
                           ADIFF = ABS( DIFF )

                           IF ( ADIFF .LT. MIN_ADIFF ) THEN
                              CALL RED4_OBSTOROBS( OBSREC.OBSERVATION,
     :                         OBS_NAME, STATUS )
                              MIN_ADIFF = ADIFF
                           END IF

*                         if that was -ve direction then reverse direction and
*                         start from obsnum + 1 (the + 1 gets done at the loop
*                         start) if +ve direction then finished.
                           IF (INC .LT. 0) THEN
                              INC = 1
                              RECNUM = OBSNUM
                           ELSE
                              LOOPING = .FALSE.
                           END IF

                        ELSE IF (SEARCH_MODE .EQ. 'BACKWARDS') THEN

                           CALL RED4_OBSTOROBS( OBSREC.OBSERVATION,
     :                      OBS_NAME, STATUS )
                           LOOPING = .FALSE.

                        ELSE IF (SEARCH_MODE .EQ. 'FORWARDS' ) THEN

                           CALL RED4_OBSTOROBS( OBSREC.OBSERVATION,
     :                      OBS_NAME, STATUS )
                           LOOPING = .FALSE.

                        ENDIF
                     ELSE

*                      If required, issue a message saying why this BIAS is
*                      being rejected.
                        IF ( VERBOSE ) THEN

                           CALL MSG_SETC( 'NAME', OBSREC.OBSERVATION )
                           CALL MSG_OUT( ' ', '(BIAS observation '/
     :                      /'r^NAME has a different detector size)',
     :                      STATUS )
                        END IF
                     END IF

                  ELSE IF (TYPE_REQUIRED .EQ. 'DARK') THEN

*                   DARK required. The DARK must have been taken
*                   with the same detector size, on-chip exposure time
*                   and similar observing mode.
*                   A suitable DARK taken most closely in time to the current
*                   observation is used.
*                   Check the detector size.
                     IF ( (OBSREC.DET_NROWS .EQ. DET_NROWS) .AND.
     :                (OBSREC.DET_NCOLUMNS .EQ. DET_NCOLUMNS) ) THEN

*                      Check the on-chip exposure time.
                        IF ( ABS( OBSREC.EXPOSURE_TIME - EXPOSURE )
     :                   .LT. TOLER ) THEN

*                         Check the observing mode (integration type).
*                         The modes must either both be NDR or both not be NDR.
                           NDRPOS1 = INDEX( OBSREC.INTTYPE, 'NDR' )
                           NDRPOS2 = INDEX( INTTYPE, 'NDR' )

                           IF ( ( ( NDRPOS1 .EQ. 0 ) .AND.
     :                      ( NDRPOS2 .EQ. 0 ) ) .OR.
     :                      ( ( NDRPOS1 .NE. 0 ) .AND.
     :                      ( NDRPOS2 .NE. 0 ) ) ) THEN

                              FOUND = .TRUE.

*                         A suitable DARK has been found.
                              IF ( SEARCH_MODE .EQ. 'BOTH' ) THEN

                                 DIFF = OBSREC.START_TIME - TIME
                                 ADIFF = ABS( DIFF )

                                 IF ( ADIFF .LT. MIN_ADIFF ) THEN
                                    CALL RED4_OBSTOROBS(
     :                               OBSREC.OBSERVATION, OBS_NAME,
     :                               STATUS )
                                    MIN_ADIFF = ADIFF
                                 END IF

                                 IF (INC .LT. 0) THEN
                                    INC = 1
                                    RECNUM = OBSNUM
                                 ELSE
                                   LOOPING = .FALSE.
                                 END IF

                              ELSE IF (SEARCH_MODE .EQ. 'BACKWARDS') THEN

                                 CALL RED4_OBSTOROBS(
     :                            OBSREC.OBSERVATION, OBS_NAME, STATUS )
                                LOOPING = .FALSE.

                              ELSE IF (SEARCH_MODE .EQ. 'FORWARDS' ) THEN

                                 CALL RED4_OBSTOROBS( OBSREC.OBSERVATION,
     :                            OBS_NAME, STATUS )
                                 LOOPING = .FALSE.

                              ENDIF

                           ELSE

*                            If required, issue a message indicating why this
*                            DARK is being rejected.
                              IF ( VERBOSE ) THEN

                                 CALL MSG_SETC( 'NAME',
     :                            OBSREC.OBSERVATION )
                                 CALL MSG_OUT( ' ', '(DARK observation '/
     :                            /'r^NAME has a different observing mode)',
     :                            STATUS )
                              END IF
                           END IF
                        ELSE

*                         If required, issue a message indicating why this
*                         DARK is being rejected.
                           IF ( VERBOSE ) THEN

                              CALL MSG_SETC( 'NAME', OBSREC.OBSERVATION )
                              CALL MSG_OUT( ' ', '(DARK observation '/
     :                         /'r^NAME has a different on-chip '/
     :                         /'exposure time)', STATUS )
                           END IF
                        END IF
                     ELSE

*                      If required, issue a message indicating why this DARK is
*                      being rejected.
                        IF ( VERBOSE ) THEN

                           CALL MSG_SETC( 'NAME', OBSREC.OBSERVATION )
                           CALL MSG_OUT( ' ', '(DARK observation r^NAME '/
     :                      /'has a different detector size)', STATUS )
                        END IF
                     ENDIF

                  ELSE IF (TYPE_REQUIRED .EQ. 'FLAT') THEN

*                   FLAT required. The FLAT must have been taken with the
*                   same detector size. A match is also checked between
*                   the instrument configuration configuration parameters
*                   specified in the FLAT_MATCH parameter (OBS_MATCH argument).
*                   A suitable FLAT taken most closely in time to the current
*                   observation is used.
*                   First check the FLAT has the same detector size.
*                   This check is compulsory.
                     IF ( (OBSREC.DET_NROWS .EQ. DET_NROWS) .AND.
     :                (OBSREC.DET_NCOLUMNS .EQ. DET_NCOLUMNS) ) THEN

*                Check oversampling parameters (compulsory)
*                 IF ( (OBSREC.DET_ENC_BASE .EQ. DET_ENC_BASE) .AND.
*    :                 (OBSREC.DET_NINCR .EQ. DET_NINCR) ) THEN


*                      Now check the optional parameters, included in OBS_MATCH
*                      Initialise the OK flag and the differences list and
*                      check the parameters one by one.
                        FLAT_OK = .TRUE.
                        DIFFERENCES = ' '

*                      Grating name ?
                        IF ( INDEX(OBS_MATCH, 'GRATING') .NE. 0 ) THEN

                           IF ( OBSREC.GRATING_NAME .NE. GRATING_NAME ) THEN

                              FLAT_OK = .FALSE.
                              CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                              DIFFERENCES = DIFFERENCES(1:CLEN)
     :                         // 'GRATING '
                           END IF
                        END IF

*                      Grating angle ?
                        IF ( INDEX(OBS_MATCH, 'GANGLE') .NE. 0 ) THEN

                           IF ( ABS ( OBSREC.GRATING_ANGLE -
     :                      GRATING_ANGLE ) .GT. TOLER ) THEN

                              FLAT_OK = .FALSE.
                              CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                              DIFFERENCES = DIFFERENCES(1:CLEN+1)
     :                         // 'GANGLE '
                           END IF
                        END IF

*                      Grating order ?
                        IF ( INDEX(OBS_MATCH, 'GORDER') .NE. 0 ) THEN

                           IF ( OBSREC.GRATING_ORDER .NE. GRATING_ORDER ) THEN

                              FLAT_OK = .FALSE.
                              CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                              DIFFERENCES = DIFFERENCES(1:CLEN+1)
     :                         // 'GORDER '
                           END IF
                        END IF

*                      Grating wavelength ?
                        IF ( INDEX(OBS_MATCH, 'GLAMBDA') .NE. 0 ) THEN

                           IF ( ABS ( OBSREC.GRATING_WVLNGTH -
     :                      GRATING_WVLNGTH )
     :                      .GT. TOLER ) THEN

                              FLAT_OK = .FALSE.
                              CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                              DIFFERENCES = DIFFERENCES(1:CLEN+1)
     :                         // 'GLAMBDA '
                           END IF
                        END IF

*                      Slit name ?
                        IF ( INDEX(OBS_MATCH, 'SLIT') .NE. 0 ) THEN

                           IF ( OBSREC.SLIT_NAME .NE. SLIT_NAME ) THEN

                              FLAT_OK = .FALSE.
                              CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                              DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'SLIT '
                           END IF
                        END IF

*                      Slit angle ?
                        IF ( INDEX(OBS_MATCH, 'SANGLE') .NE. 0 ) THEN

                           IF ( ABS ( OBSREC.SLIT_ANGLE - SLIT_ANGLE )
     :                      .GT. TOLER ) THEN

                              FLAT_OK = .FALSE.
                              CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                              DIFFERENCES = DIFFERENCES(1:CLEN+1)
     :                         // 'SANGLE '
                           END IF
                        END IF

*                      CVF name ?
                        IF ( INDEX(OBS_MATCH, 'CVF') .NE. 0 ) THEN

                           IF ( OBSREC.CVF_NAME .NE. CVF_NAME ) THEN

                              FLAT_OK = .FALSE.
                              CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                              DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'CVF '
                           END IF
                        END IF

*                      CVF wavelength ?
                        IF ( INDEX(OBS_MATCH, 'CLAMBDA') .NE. 0 ) THEN

                           IF ( ABS ( OBSREC.CVF_WAVELENGTH - CVF_WAVELENGTH )
     :                      .GT. TOLER ) THEN

                              FLAT_OK = .FALSE.
                              CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                              DIFFERENCES = DIFFERENCES(1:CLEN+1)
     :                         // 'CLAMBDA '
                           END IF
                        END IF

*                      Filter combination name ?
                        IF ( INDEX(OBS_MATCH, 'FILTERS') .NE. 0 ) THEN

                           IF ( OBSREC.FILTERS .NE. FILTERS ) THEN

                              FLAT_OK = .FALSE.
                              CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                              DIFFERENCES = DIFFERENCES(1:CLEN+1) //
     :                         'FILTERS '
                           END IF
                        END IF

*                      Instrument configuration index ?
                        IF ( INDEX(OBS_MATCH, 'CNFINDEX') .NE. 0 ) THEN

                           IF ( OBSREC.CNFINDEX .NE. CNFINDEX ) THEN

                              FLAT_OK = .FALSE.
                              CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                              DIFFERENCES = DIFFERENCES(1:CLEN+1) //
     :                         'CNFINDEX '
                           END IF
                        END IF

*                      Check to see if the FLAT has passed all the above tests.
                        IF ( FLAT_OK ) THEN

                           FOUND = .TRUE.
*                         A suitable FLAT has been found.

                           IF ( SEARCH_MODE .EQ. 'BOTH' ) THEN

                              DIFF = OBSREC.START_TIME - TIME
                              ADIFF = ABS( DIFF )

                              IF ( ADIFF .LT. MIN_ADIFF ) THEN
                                 CALL RED4_OBSTOROBS(OBSREC.OBSERVATION,
     :                            OBS_NAME, STATUS )
                                 MIN_ADIFF = ADIFF
                              END IF

*                            if that was -ve direction then reverse direction
*                            and start from obsnum+1 (the +1 gets done at
*                            the loop start) if +ve direction then finished.
                              IF (INC .LT. 0) THEN
                                 INC = 1
                                 RECNUM = OBSNUM
                              ELSE
                                 LOOPING = .FALSE.
                              END IF

                           ELSE IF (SEARCH_MODE .EQ. 'BACKWARDS') THEN

                              CALL RED4_OBSTOROBS( OBSREC.OBSERVATION,
     :                         OBS_NAME, STATUS )
                              LOOPING = .FALSE.

                           ELSE IF (SEARCH_MODE .EQ. 'FORWARDS' ) THEN

                              CALL RED4_OBSTOROBS( OBSREC.OBSERVATION,
     :                         OBS_NAME, STATUS )
                              LOOPING = .FALSE.

                           ENDIF

                           IF ( VERBOSE ) THEN
                              CALL MSG_SETC( 'NAME', OBSREC.OBSERVATION )
                              CALL MSG_OUT( ' ', 'RED4_SEEK_OBSERVATION: '/
     :                         /'Reduced FLAT ^NAME is OK', STATUS )
                           ENDIF

                        ELSE

*                         If required, issue a message indicating why this FLAT
*                         is being rejected (splitting the message over several
*                         lines if very long).
                           IF ( VERBOSE ) THEN

                              CALL MSG_SETC( 'NAME', OBSREC.OBSERVATION )
                              CALL MSG_OUT( ' ', '(FLAT observation ^NAME '/
     :                         /'differs by the following settings:',
     :                         STATUS )

                              CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )

                              CALL MSG_SETC( 'DIFFERENCES',
     :                         DIFFERENCES(1:72) )
                              CALL MSG_OUT( ' ', ' - ^DIFFERENCES', STATUS )

                              IF ( CLEN .GT. 72 ) THEN

                                 CALL MSG_SETC( 'DIFFERENCES2',
     :                            DIFFERENCES(73:) )
                                 CALL MSG_OUT( ' ', ' - ^DIFFERENCES2',
     :                            STATUS )
                              END IF
                           END IF
                        END IF
                     ELSE
*                 ELSE
*                    IF ( VERBOSE ) THEN

*                       CALL MSG_SETC( 'NAME', OBSREC.OBSERVATION )
*                       CALL MSG_OUT( ' ', '(FLAT observation '/
*    :                    /'R^NAME has different oversampling '/
*    :                    /'parameters)', STATUS )
*                    END IF
*                 ENDIF

*                      If required, issue a message indicating why this FLAT is
*                      being rejected.
                        IF ( VERBOSE ) THEN

                           CALL MSG_SETC( 'NAME', OBSREC.OBSERVATION )
                           CALL MSG_OUT( ' ', '(FLAT observation r^NAME '/
     :                      /'has a different detector size)', STATUS )
                        END IF
                     ENDIF

                  ELSE IF (TYPE_REQUIRED .EQ. 'CALIBRATION' ) THEN

*                   CALIBRATION required. The CALIBRATION must have been taken
*                   with the same detector columns and oversampling parameters.
*                   A match is also checked between the instrument
*                   configuration parameters specified in the CALIB_MATCH
*                   parameter (OBS_MATCH argument).
*                   A suitable CALIBRATION taken most closely in time to the
*                   current observation is used.
*                   First check the CALIBRATION has the same number of detector
*                   columns. (The number of rows doesn't matter, as only the
*                   X axis is used). This check is compulsory.
                     IF ( OBSREC.DET_NCOLUMNS .EQ. DET_NCOLUMNS ) THEN

*                      Now check the CALIBRATION has the same oversampling
*                      parameters. This check is also compulsory.
                        IF ( (OBSREC.DET_ENC_BASE .EQ. DET_ENC_BASE) .AND.
     :                   (OBSREC.DET_NINCR .EQ. DET_NINCR) ) THEN

*                         Now check the optional parameters, included in
*                         OBS_MATCH. Initialise the OK flag and the
*                         differences list, check the parameters one by one.
                           CALIB_OK = .TRUE.
                           DIFFERENCES = ' '

*                         Grating name ?
                           IF ( INDEX(OBS_MATCH, 'GRATING') .NE. 0 ) THEN

                              IF ( OBSREC.GRATING_NAME .NE.
     :                         GRATING_NAME ) THEN

                                 CALIB_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN)
     :                            // 'GRATING '
                              END IF
                           END IF

*                         Grating angle ?
                           IF ( INDEX(OBS_MATCH, 'GANGLE') .NE. 0 ) THEN

                              IF ( ABS ( OBSREC.GRATING_ANGLE -
     :                         GRATING_ANGLE )
     :                         .GT. TOLER ) THEN

                                 CALIB_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1)
     :                            // 'GANGLE '
                              END IF
                           END IF

*                         Grating order ?
                           IF ( INDEX(OBS_MATCH, 'GORDER') .NE. 0 ) THEN

                              IF ( OBSREC.GRATING_ORDER .NE.
     :                         GRATING_ORDER ) THEN

                                 CALIB_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1)
     :                            // 'GORDER '
                              END IF
                           END IF

*                         Grating wavelength ?
                           IF ( INDEX(OBS_MATCH, 'GLAMBDA') .NE. 0 ) THEN

                              IF ( ABS ( OBSREC.GRATING_WVLNGTH -
     :                         GRATING_WVLNGTH )
     :                         .GT. TOLER ) THEN

                                 CALIB_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1)
     :                            // 'GLAMBDA '
                              END IF
                           END IF

*                         Slit name ?
                           IF ( INDEX(OBS_MATCH, 'SLIT') .NE. 0 ) THEN

                              IF ( OBSREC.SLIT_NAME .NE. SLIT_NAME ) THEN

                                 CALIB_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1) //
     :                            'SLIT '
                              END IF
                           END IF

*                         Slit angle ?
                           IF ( INDEX(OBS_MATCH, 'SANGLE') .NE. 0 ) THEN

                              IF ( ABS ( OBSREC.SLIT_ANGLE - SLIT_ANGLE )
     :                         .GT. TOLER ) THEN

                                 CALIB_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1)
     :                            // 'SANGLE '
                              END IF
                           END IF

*                         CVF name ?
                           IF ( INDEX(OBS_MATCH, 'CVF') .NE. 0 ) THEN

                              IF ( OBSREC.CVF_NAME .NE. CVF_NAME ) THEN

                                 CALIB_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'CVF '
                              END IF
                           END IF

*                         CVF wavelength ?
                           IF ( INDEX(OBS_MATCH, 'CLAMBDA') .NE. 0 ) THEN

                              IF ( ABS ( OBSREC.CVF_WAVELENGTH -
     :                         CVF_WAVELENGTH )
     :                         .GT. TOLER ) THEN

                                 CALIB_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1)
     :                            // 'CLAMBDA '
                              END IF
                           END IF

*                         Filter combination name ?
                           IF ( INDEX(OBS_MATCH, 'FILTERS') .NE. 0 ) THEN

                              IF ( OBSREC.FILTERS .NE. FILTERS ) THEN

                                 CALIB_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1) //
     :                            'FILTERS '
                              END IF
                           END IF

*                         Instrument configuration index ?
                           IF ( INDEX(OBS_MATCH, 'CNFINDEX') .NE. 0 ) THEN

                              IF ( OBSREC.CNFINDEX .NE. CNFINDEX ) THEN

                                 CALIB_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1) //
     :                            'CNFINDEX '
                              END IF
                           END IF

*                         Check to see if the CALIBRATION has passed the tests.
                           IF ( CALIB_OK ) THEN

                              FOUND = .TRUE.

*                            A suitable CALIBRATION has been found.

                              IF ( SEARCH_MODE .EQ. 'BOTH' ) THEN

                                 DIFF = OBSREC.START_TIME - TIME
                                 ADIFF = ABS( DIFF )

                                 IF ( ADIFF .LT. MIN_ADIFF ) THEN
                                    CALL RED4_OBSTOCAL(OBSREC.OBSERVATION,
     :                               OBS_NAME, STATUS )
                                    MIN_ADIFF = ADIFF
                                 END IF

                                 IF (INC .LT. 0) THEN
                                    INC = 1
                                    RECNUM = OBSNUM
                                 ELSE
                                    LOOPING = .FALSE.
                                 END IF

                              ELSE IF (SEARCH_MODE .EQ. 'BACKWARDS') THEN

                                 CALL RED4_OBSTOCAL( OBSREC.OBSERVATION,
     :                            OBS_NAME, STATUS )
                                 LOOPING = .FALSE.

                              ELSE IF (SEARCH_MODE .EQ. 'FORWARDS' ) THEN

                                 CALL RED4_OBSTOCAL( OBSREC.OBSERVATION,
     :                            OBS_NAME, STATUS )
                                 LOOPING = .FALSE.

                              ENDIF

                              IF ( VERBOSE ) THEN
                                 CALL MSG_SETC( 'NAME', OBSREC.OBSERVATION )
                                 CALL MSG_OUT( ' ', 'RED4_SEEK_OBSERVATION: '/
     :                            /'Reduced CALIBRATION ^NAME is OK',
     :                            STATUS )
                              ENDIF
                           ELSE

                              IF ( VERBOSE ) THEN

                                 CALL MSG_SETC( 'NAME', OBSREC.OBSERVATION )
                                 CALL MSG_OUT( ' ', '(CALIBRATION '/
     :                            /'observation r^NAME differs by the '/
     :                            /'following settings:', STATUS )

                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )

                                 CALL MSG_SETC( 'DIFFERENCES',
     :                            DIFFERENCES(1:72) )
                                 CALL MSG_OUT( ' ', ' - ^DIFFERENCES',
     :                            STATUS )

                                 IF ( CLEN .GT. 72 ) THEN

                                    CALL MSG_SETC( 'DIFFERENCES2',
     :                               DIFFERENCES(73:) )
                                    CALL MSG_OUT( ' ', ' - ^DIFFERENCES2',
     :                               STATUS )
                                 END IF
                              END IF
                           END IF
                        ELSE

                           IF ( VERBOSE ) THEN

                              CALL MSG_SETC( 'NAME', OBSREC.OBSERVATION )
                              CALL MSG_OUT( ' ', '(CALIBRATION observation '/
     :                         /'r^NAME has different oversampling '/
     :                         /'parameters)', STATUS )
                           END IF
                        ENDIF
                     ELSE

                        IF ( VERBOSE ) THEN

                           CALL MSG_SETC( 'NAME', OBSREC.OBSERVATION )
                           CALL MSG_OUT( ' ', '(CALIBRATION observation '/
     :                      /'r^NAME has a different no. of detector '/
     :                      /'columns)', STATUS )
                        END IF
                     ENDIF

                  ELSE IF (TYPE_REQUIRED .EQ. 'STANDARD' ) THEN

*                   STANDARD required. The STANDARD must have been taken
*                   with the same detector size and oversampling parameters.
*                   A match is also checked between the instrument
*                   configuration parameters specified in the STANDARD_MATCH
*                   parameter (OBS_MATCH argument).
*                   A suitable STANDARD taken most closely in time to the
*                   current observation is used.
*                   First check the STANDARD has the same detector size.
*                   This check is compulsory.
                     IF ( (OBSREC.DET_NROWS .EQ. DET_NROWS) .AND.
     :                (OBSREC.DET_NCOLUMNS .EQ. DET_NCOLUMNS) ) THEN

*                      Now check the STANDARD has the same oversampling
*                      parameters. This check is also compulsory.
                        IF ( (OBSREC.DET_ENC_BASE .EQ. DET_ENC_BASE) .AND.
     :                   (OBSREC.DET_NINCR .EQ. DET_NINCR) ) THEN

*                         Now check the optional parameters, included in
*                         OBS_MATCH. Initialise the OK flag and the
*                         differences list and check the parameters one by one.
                           STANDARD_OK = .TRUE.
                           DIFFERENCES = ' '

*                         Grating name ?
                           IF ( INDEX(OBS_MATCH, 'GRATING') .NE. 0 ) THEN

                              IF ( OBSREC.GRATING_NAME .NE.
     :                         GRATING_NAME ) THEN

                                 STANDARD_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN)
     :                            // 'GRATING '
                              END IF
                           END IF

*                         Grating angle ?
                           IF ( INDEX(OBS_MATCH, 'GANGLE') .NE. 0 ) THEN

                              IF ( ABS ( OBSREC.GRATING_ANGLE -
     :                         GRATING_ANGLE )
     :                         .GT. TOLER ) THEN

                                 STANDARD_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1)
     :                            // 'GANGLE '
                              END IF
                           END IF

*                         Grating order ?
                           IF ( INDEX(OBS_MATCH, 'GORDER') .NE. 0 ) THEN

                              IF ( OBSREC.GRATING_ORDER .NE.
     :                         GRATING_ORDER ) THEN

                                 STANDARD_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1)
     :                            // 'GORDER '
                              END IF
                           END IF

*                         Grating wavelength ?
                           IF ( INDEX(OBS_MATCH, 'GLAMBDA') .NE. 0 ) THEN

                              IF ( ABS ( OBSREC.GRATING_WVLNGTH -
     :                         GRATING_WVLNGTH )
     :                         .GT. TOLER ) THEN

                                 STANDARD_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1)
     :                            // 'GLAMBDA '
                              END IF
                           END IF

*                         Slit name ?
                           IF ( INDEX(OBS_MATCH, 'SLIT') .NE. 0 ) THEN

                              IF ( OBSREC.SLIT_NAME .NE. SLIT_NAME ) THEN

                                 STANDARD_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1) //
     :                            'SLIT '
                              END IF
                           END IF

*                         Slit angle ?
                           IF ( INDEX(OBS_MATCH, 'SANGLE') .NE. 0 ) THEN

                              IF ( ABS ( OBSREC.SLIT_ANGLE - SLIT_ANGLE )
     :                         .GT. TOLER ) THEN

                                 STANDARD_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1)
     :                            // 'SANGLE '
                              END IF
                           END IF

*                         CVF name ?
                           IF ( INDEX(OBS_MATCH, 'CVF') .NE. 0 ) THEN

                              IF ( OBSREC.CVF_NAME .NE. CVF_NAME ) THEN

                                 STANDARD_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'CVF '
                              END IF
                           END IF

*                         CVF wavelength ?
                           IF ( INDEX(OBS_MATCH, 'CLAMBDA') .NE. 0 ) THEN

                              IF ( ABS ( OBSREC.CVF_WAVELENGTH -
     :                         CVF_WAVELENGTH )
     :                         .GT. TOLER ) THEN

                                 STANDARD_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1)
     :                            // 'CLAMBDA '
                              END IF
                           END IF

*                         Filter combination name ?
                           IF ( INDEX(OBS_MATCH, 'FILTERS') .NE. 0 ) THEN

                              IF ( OBSREC.FILTERS .NE. FILTERS ) THEN

                                 STANDARD_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1) //
     :                            'FILTERS '
                              END IF
                           END IF

*                         Instrument configuration index ?
                           IF ( INDEX(OBS_MATCH, 'CNFINDEX')
     :                     .NE. 0 ) THEN

                              IF ( OBSREC.CNFINDEX .NE. CNFINDEX ) THEN

                                 STANDARD_OK = .FALSE.
                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                 DIFFERENCES = DIFFERENCES(1:CLEN+1) //
     :                            'CNFINDEX '
                              END IF
                           END IF

*                         Air mass ?
                           IF ( INDEX(OBS_MATCH, 'AIRMASS') .NE. 0 ) THEN

*                            Use a percentage difference for air masses greater
*                            than AMTHRESH, and a fixed absolute difference
*                            below this level.
                              IF ( AIRMASS .GT. AMTHRESH ) THEN

                                 AMDIFF = 100.0 * ABS( OBSREC.AIRMASS -
     :                            AIRMASS )
     :                            / AIRMASS

                                 IF ( AMDIFF .GT. AMTOLER ) THEN

                                    STANDARD_OK = .FALSE.
                                    CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                    DIFFERENCES = DIFFERENCES(1:CLEN+1) //
     :                               'AIRMASS '
                                 END IF
                              ELSE

                                 AMDIFF = ABS( OBSREC.AIRMASS - AIRMASS )

                                 IF ( AMDIFF .GT.
     :                            (AMTOLER*AMTHRESH/100.0) ) THEN

                                    STANDARD_OK = .FALSE.
                                    CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                                    DIFFERENCES = DIFFERENCES(1:CLEN+1) //
     :                               'AIRMASS '
                                 END IF
                              END IF
                           END IF

*                         Check to see if the STANDARD has passed the tests.
                           IF ( STANDARD_OK ) THEN

                              FOUND = .TRUE.

*                            A suitable STANDARD has been found.

*                            Get the directory prefix
                              CALL RED4_GET_PREFIX ('RG', LPREFIX, STATUS)

                              IF ( SEARCH_MODE .EQ. 'BOTH' ) THEN

                                 DIFF = OBSREC.START_TIME - TIME
                                 ADIFF = ABS( DIFF )

                                 IF ( ADIFF .LT. MIN_ADIFF ) THEN
                                    OBS_NAME = LPREFIX(:CHR_LEN(LPREFIX))/
     :                               /OBSREC.OBSERVATION
                                    MIN_ADIFF = ADIFF
                                 END IF

                                 IF (INC .LT. 0) THEN
                                    INC = 1
                                    RECNUM = OBSNUM
                                 ELSE
                                    LOOPING = .FALSE.
                                 END IF

                              ELSE IF (SEARCH_MODE .EQ. 'BACKWARDS') THEN

                                 OBS_NAME = LPREFIX(:CHR_LEN(LPREFIX))/
     :                            /OBSREC.OBSERVATION
                                 LOOPING = .FALSE.

                              ELSE IF (SEARCH_MODE .EQ. 'FORWARDS' ) THEN

                                 OBS_NAME = LPREFIX(:CHR_LEN(LPREFIX))/
     :                            /OBSREC.OBSERVATION
                                 LOOPING = .FALSE.

                              ENDIF

                              IF ( VERBOSE ) THEN
                                 CALL MSG_SETC( 'NAME', OBSREC.OBSERVATION )
                                 CALL MSG_OUT( ' ', 'RED4_SEEK_OBSERVATION: '/
     :                            /'Reduced STANDARD ^NAME is OK', STATUS )
                              ENDIF
                           ELSE

                              IF ( VERBOSE ) THEN

                                 CALL MSG_SETC( 'NAME', OBSREC.OBSERVATION )
                                 CALL MSG_OUT( ' ', '(STANDARD '/
     :                            /'group ^NAME differs by the '/
     :                            /'following settings:', STATUS )

                                 CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )

                                 CALL MSG_SETC( 'DIFFERENCES',
     :                            DIFFERENCES(1:72) )
                                 CALL MSG_OUT( ' ', ' - ^DIFFERENCES',
     :                            STATUS )

                                 IF ( CLEN .GT. 72 ) THEN

                                    CALL MSG_SETC( 'DIFFERENCES2',
     :                               DIFFERENCES(73:) )
                                    CALL MSG_OUT( ' ', ' - ^DIFFERENCES2',
     :                               STATUS )
                                 END IF
                              END IF
                           END IF
                        ELSE

                           IF ( VERBOSE ) THEN

                              CALL MSG_SETC( 'NAME', OBSREC.OBSERVATION )
                              CALL MSG_OUT( ' ', '(STANDARD group '/
     :                        /'^NAME has different oversampling '/
     :                        /'parameters)', STATUS )
                           END IF
                        ENDIF
                     ELSE

                        IF ( VERBOSE ) THEN

                           CALL MSG_SETC( 'NAME', OBSREC.OBSERVATION )
                           CALL MSG_OUT( ' ', '(STANDARD group '/
     :                      /'^NAME has a different detector size)',
     :                      STATUS )
                        END IF
                     ENDIF
                  END IF
               ENDIF

            END IF
         END DO

*       Finished looping. Close file ...
         CALL RIO_CLOSE (FD, STATUS)

      END IF


      END
