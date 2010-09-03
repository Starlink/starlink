      SUBROUTINE SURF_SCAN_RLB (STATUS)
*+
*  Name:
*     SCAN_RLB

*  Purpose:
*     Remove the linear baseline from SCAN/MAP data

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SURF_SCAN_RLB( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*     This routine removes a linear baseline from each scan.
*     It does this by fitting a straight line to the scan ends and then
*     removing this from the entire scan.

*  Usage:
*     scan_rlb in out

*  ADAM Parameters:
*     CHOP = INTEGER (Read)
*        The basline fit is calculated over regions CHOP arcseconds from the
*        scan ends. This region should be as large as possible but should
*        only include baseline regions -- any scan that includes a source
*        detection within CHOP arcseconds of the scan ends will be rendered
*        useless. Only used for METHOD=LINEAR.
*        The default value is the chop throw.
*     IN = NDF (Read)
*        The name of the input file containing demodulated SCUBA data.
*     METHOD = CHAR (Read)
*        Baseline removal method. Options are:
*           LINEAR:  Remove a linear baseline using the ends of the scan
*           MEDIAN:  Remove a DC level calculated from the median of each
*                    scan
*           MEAN:    Remove the mean level from each scan (data further
*                    than 3 sigma from the mean are ignored and the MEAN
*                    recalculated)
*           SECTION: Use a SCUBA section to specify regions of each
*                    integration that are thought to be flux free.
*                    Remove the median of the specified section
*                    from the associated integration.
*     MSG_FILTER = CHAR (Read)
*        The messaging level. Default is NORM. There are no verbose messages.
*     OUT = NDF (Write)
*        The name of the output file to contain the processed data.
*     RLB = INTEGER (Read)
*        This parameter governs whether the baseline fit is removed from the
*        input data or stored instead of the data. If RLB is .TRUE. the
*        corrected data are returned. If RLB is .FALSE. the fit is returned.
*     SECTION() = CHAR (Read)
*         This array parameter can be used to specify SCUBA sections
*         to be used for baseline calculation. It is requested when
*         METHOD=SECTION. In general the SCUBA section should
*         include scan (exposure) or position (p) specifiers which
*         will be applied to each bolometer and integration. It is
*         possible to be more specific and to provide multiple sections
*         singling out certain bolometers or integrations. If entire
*         integrations are selected no baseline removal will occur
*         on unselected integrations (this will be stated).
*         The median of the section supplied for each integration
*         is subtracted from every exposure in that integration (remember
*         that if no integration is specified, all integrations are assumed).

*         Curly brackets must still be given. Since this is an array
*         parameter square brackets must be used to specify more than
*         one component:
*
*             SECTION > [ {e1} , {e4;b2} ]
*
*         would select exposure one from each integration along with
*         exposure 4 for bolometer 2.
*         be used if the square brackets are not used.

*         Care must also be taken when using commas in SCUBA sections -
*         the parameter system will split multiple entries on commas
*         unless the entire section is quoted:
*
*             SECTION > "{e1,4}"
*
*         If necessary the negation character should come after a
*         section (ie after the closing curly bracket) and that
*         negation applies to the combined section and not just the string
*         containing the negation character:
*
*             SECTION > {e3}-
*
*         implies that the section consists of everything except exposure 3.


*  Examples:
*     scan_rlb infile \
*        Remove linear baslines from each scan using basline regions the
*        same size as the chop. Write the results to the default output file.
*     scan_rlb infile method=section section='"{e1,3}"' \
*        Use exposures 1 and 3 to calculate baseline region for each
*        integration.


*  Authors:
*     JFL: John Lightfoot (jfl@roe.ac.uk)
*     TIMJ: Tim Jenness (timj@jach.hawaii.edu)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     21-SEP-1995: original version.
*     $Log$
*     Revision 1.14  2004/09/08 02:03:34  timj
*     Add CNF_PVAL where appropriate
*
*     Revision 1.13  2001/10/30 03:02:03  timj
*     POLMAP is a MAP observation
*
*     Revision 1.12  1999/08/03 20:01:40  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     Revision 1.11  1999/06/16 21:08:18  timj
*     Add SECTIONing
*
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'                 ! SSE global definitions
      INCLUDE 'DAT_PAR'                 ! for DAT__SZLOC
      INCLUDE 'SURF_PAR'                ! REDS constants
      INCLUDE 'MSG_PAR'                 ! for MSG__ constants
      INCLUDE 'CNF_PAR'                 ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN

*  Local Constants:
      INTEGER MAXDIM
      PARAMETER (MAXDIM = 4)
      CHARACTER * 10 TSKNAME            ! Name of task
      PARAMETER (TSKNAME = 'SCAN_RLB')
      CHARACTER * 1    NEGCHAR  ! Character used to negate a section
      PARAMETER (NEGCHAR = '-') !


*  Local variables:
      LOGICAL      ABORTED              ! .TRUE. if observation was
                                        ! aborted
      BYTE         BADBIT               ! Bad bit mask
      CHARACTER*15 CHOP_COORDS          ! coordinate system of chop
      CHARACTER*15 CHOP_FUN             ! chop mode used in observation
      INTEGER      CHOP_SIZE            ! Chop in pixels
      REAL         CHOP_THROW           ! chopper throw (arcsec)
      CHARACTER*128 DATA_SPEC(SCUBA__MAX_SECT) ! Array of section specs
      INTEGER      DIM (MAXDIM)         ! the dimensions of an array
      LOGICAL      DORLB                ! Perform the subtraction
      LOGICAL      EXTINCTION           ! .TRUE. if the EXTINCTION application
                                        ! has already been run on the
                                        ! input file
      CHARACTER*80 FITS (SCUBA__MAX_FITS)
                                        ! array of FITS keyword lines
      CHARACTER*132 FNAME               ! Input filename
      INTEGER      I                    ! DO loop variable
      INTEGER      INDF                 ! NDF identifier of input file
      INTEGER      ITEMP                ! scratch integer
      INTEGER      IN_DATA_PTR          ! pointer to data array of input file
      INTEGER      IN_DEM_PNTR_PTR      ! pointer to input .SCUBA.DEM_PNTR
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC
                                        ! locator to FITS extension in input
                                        ! file
      INTEGER      IN_QUALITY_PTR       ! pointer to quality array in input
                                        ! file
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC
                                        ! locator to SCUBA extension in input
                                        ! file
      INTEGER      IN_VARIANCE_PTR      ! pointer to variance array in input
                                        ! file
      INTEGER      LAST_EXP             ! exposure where abort occurred
      INTEGER      LAST_INT             ! integration where abort occurred
      INTEGER      LAST_MEAS            ! measurement where abort occurred
      CHARACTER*10 METHOD               ! Baseline removal method
      INTEGER      NDIM                 ! the number of dimensions in an array
      INTEGER      NEGPOS               ! Position of NEGCHAR
      INTEGER      NREC                 ! number of history records in file
      INTEGER      N_BOL                ! number of bolometers measured
      INTEGER      N_EXPOSURES          ! number of exposures per integration
      INTEGER      N_FITS               ! number of FITS lines read from file
      INTEGER      N_INTEGRATIONS       ! number of integrations per measurement
      INTEGER      N_MEASUREMENTS       ! number of measurements in the file
      INTEGER      N_POS                ! the total number of posns measured
      INTEGER      N_SPEC               ! Number of section specifications
      CHARACTER*30 OBJECT               ! name of object observed
      CHARACTER*15 OBSERVING_MODE       ! type of observation
      CHARACTER*132 OUTFILE             ! Output filename
      INTEGER      OUTNDF               ! NDF identifier of output file
      INTEGER      OUT_DATA_PTR         ! pointer to data array in output
      INTEGER      OUT_QUALITY_PTR      ! pointer to quality array in output
      INTEGER      OUT_VARIANCE_PTR     ! pointer to variance array in output
      LOGICAL      REDUCE_SWITCH        ! .TRUE. if REDUCE_SWITCH has been run
      INTEGER      RUN_NUMBER           ! run number of observation
      REAL         SAMPLE_DX            ! sample spacing along scans
      CHARACTER*15 SAMPLE_MODE          ! SAMPLE_MODE of observation
      CHARACTER*80 STATE                ! 'state' of SCUCD at end of
                                        ! observation
      CHARACTER*80 STEMP                ! scratch string
      CHARACTER * (10) SUFFIX_STRINGS(SCUBA__N_SUFFIX) ! Suffix for OUT
      LOGICAL      USE_SECT             ! Am I using the section or inverse?

*  Local Data:
      DATA SUFFIX_STRINGS /'!_rlb','b','_rlb'/
*.

      IF (STATUS .NE. SAI__OK) RETURN




*  start up the NDF system and read in the demodulated data file

      CALL NDF_BEGIN

      CALL NDF_ASSOC ('IN', 'READ', INDF, STATUS)

*     Get the name of the filename associated with 'IN'

      CALL SCULIB_GET_FILENAME('IN', FNAME, STATUS)

* Read in badbit mask
      CALL NDF_BB(INDF, BADBIT, STATUS)

*  get some general descriptive parameters of the observation

      CALL NDF_XLOC (INDF, 'FITS', 'READ', IN_FITSX_LOC, STATUS)
      CALL NDF_XLOC (INDF, 'SCUBA', 'READ', IN_SCUBAX_LOC, STATUS)

      CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: input file '//
     :        'contains too many FITS items', STATUS)
         END IF
      END IF
      CALL DAT_GET1C (IN_FITSX_LOC, SCUBA__MAX_FITS, FITS, N_FITS,
     :  STATUS)
      CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'RUN',
     :  RUN_NUMBER, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'OBJECT',
     :  OBJECT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'MODE',
     :  OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :  'SAM_MODE', SAMPLE_MODE, STATUS)
      CALL CHR_UCASE (SAMPLE_MODE)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :  'CHOP_CRD', CHOP_COORDS, STATUS)
      CALL CHR_UCASE (CHOP_COORDS)

*  check that the observation was suitable for RESTORE

      IF (STATUS .EQ. SAI__OK) THEN
         IF (OBSERVING_MODE .NE. 'MAP' .AND.
     :        OBSERVING_MODE .NE. 'POLMAP') THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: file does not contain '//
     :        'data for a MAP observation', STATUS)
         ELSE IF (SAMPLE_MODE .NE. 'RASTER') THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: map was not obtained '//
     :        'with RASTER sampling', STATUS)
         END IF
      END IF

      CALL MSG_SETC ('OBJECT', OBJECT)
      CALL MSG_SETI ('RUN', RUN_NUMBER)
      CALL MSG_SETC ('PKG', PACKAGE)
      CALL MSG_OUTIF (MSG__NORM, ' ',
     :     '^PKG: run ^RUN was a MAP observation '//
     :     'of object ^OBJECT', STATUS)

*  check that the history of the input file is OK

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_HNREC (INDF, NREC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            NREC = 0
         END IF

         REDUCE_SWITCH = .FALSE.
         EXTINCTION = .FALSE.

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (INDF, 'APPLICATION', I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP(:13) .EQ. 'REDUCE_SWITCH') THEN
                  REDUCE_SWITCH = .TRUE.
               ELSE IF (STEMP(:10) .EQ. 'EXTINCTION') THEN
                  EXTINCTION = .TRUE.
               END IF
            END DO
         END IF

         IF (STATUS .EQ. SAI__OK) THEN
            IF (.NOT. REDUCE_SWITCH) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: the '//
     :           'REDUCE_SWITCH application has not been run '//
     :           'on the input file', STATUS)
            END IF

*            IF (EXTINCTION) THEN
*               STATUS = SAI__ERROR
*               CALL MSG_SETC('TASK', TSKNAME)
*               CALL ERR_REP (' ', '^TASK: the '//
*     :           'EXTINCTION application has already been run '//
*     :           'on the input file. RESTORE should be run before '//
*     :           'EXTINCTION', STATUS)
*            END IF
         END IF
      END IF

*  get some other FITS items needed for this stage of reduction

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'SAM_DX',
     :  SAMPLE_DX, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'CHOP_FUN',
     :  CHOP_FUN, STATUS)
      CALL CHR_UCASE (CHOP_FUN)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'CHOP_THR',
     :  CHOP_THROW, STATUS)

*  find out if the observation was aborted

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'STATE',
     :  STATE, STATUS)
      CALL CHR_UCASE (STATE)
      ABORTED = .FALSE.
      IF (INDEX(STATE,'ABORTING') .NE. 0) THEN
         ABORTED = .TRUE.
      END IF

*  check the data dimensions

      CALL NDF_DIM (INDF, MAXDIM, DIM, NDIM, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((NDIM .NE. 2) .OR.
     :       (DIM(1) .LT. 1) .OR.
     :       (DIM(2) .LT. 1)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETI ('DIM1', DIM(1))
            CALL MSG_SETI ('DIM2', DIM(2))
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: main data '//
     :        'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2', STATUS)
         END IF
      END IF

      N_BOL = DIM (1)
      N_POS = DIM (2)

*     map the input arrays

      CALL NDF_MAP (INDF, 'QUALITY', '_UBYTE', 'READ',
     :     IN_QUALITY_PTR, ITEMP, STATUS)
      CALL NDF_MAP (INDF, 'DATA', '_REAL', 'READ', IN_DATA_PTR,
     :     ITEMP, STATUS)
      CALL NDF_MAP (INDF, 'VARIANCE', '_REAL', 'READ', IN_VARIANCE_PTR,
     :     ITEMP, STATUS)


*  map the DEM_PNTR array and check its dimensions

      CALL SCULIB_GET_DEM_PNTR(3, IN_SCUBAX_LOC,
     :     IN_DEM_PNTR_PTR, ITEMP, N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS, STATUS)

      CALL MSG_SETI ('N_E', N_EXPOSURES)
      CALL MSG_SETI ('N_I', N_INTEGRATIONS)
      CALL MSG_SETI ('N_M', N_MEASUREMENTS)
      CALL MSG_SETC ('PKG', PACKAGE)

      IF (.NOT. ABORTED) THEN
         CALL MSG_OUTIF (MSG__NORM, ' ',
     :        '^PKG: file contains data for ^N_E '//
     :        'exposure(s) in ^N_I integration(s) in '//
     :        '^N_M measurement(s)', STATUS)
      ELSE

*  get the exposure, integration, measurement numbers at which the
*  abort occurred

         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'EXP_NO', LAST_EXP, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'INT_NO', LAST_INT, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'MEAS_NO', LAST_MEAS, STATUS)

         CALL MSG_OUTIF (MSG__NORM, ' ',
     :        '^PKG: the observation should have '//
     :        'had ^N_E exposure(s) in ^N_I integrations in ^N_M '//
     :        'measurement(s)', STATUS)
         CALL MSG_SETI ('N_E', LAST_EXP)
         CALL MSG_SETI ('N_I', LAST_INT)
         CALL MSG_SETI ('N_M', LAST_MEAS)
         CALL MSG_OUTIF (MSG__NORM,' ',
     :        ' - However, the observation was '//
     :        'ABORTED during exposure ^N_E of integration ^N_I '//
     :        'of measurement ^N_M', STATUS)
      END IF

*     Generate a default name for the output file
      CALL SCULIB_CONSTRUCT_OUT(FNAME, SUFFIX_ENV, SCUBA__N_SUFFIX,
     :     SUFFIX_OPTIONS, SUFFIX_STRINGS, OUTFILE, STATUS)

*     set the default
      CALL PAR_DEF0C('OUT', OUTFILE, STATUS)

*  now open the output NDF, propagating it from the input file

      CALL NDF_PROP (INDF, 'Units,Axis', 'OUT', OUTNDF, STATUS)

*  map the various components of the output data array

      CALL NDF_MAP (OUTNDF, 'QUALITY', '_UBYTE', 'WRITE',
     :  OUT_QUALITY_PTR, ITEMP, STATUS)
      CALL NDF_MAP (OUTNDF, 'DATA', '_REAL', 'WRITE',
     :  OUT_DATA_PTR, ITEMP, STATUS)
      CALL NDF_MAP (OUTNDF, 'VARIANCE', '_REAL', 'WRITE',
     :  OUT_VARIANCE_PTR, ITEMP, STATUS)

* Bad bit mask
      CALL NDF_SBB(BADBIT, OUTNDF, STATUS)

*     Ask for the baseline removal mode
      CALL PAR_CHOIC('METHOD', 'LINEAR','LINEAR,MEDIAN,MEAN,SECTION',
     :     .TRUE., METHOD, STATUS)

*     Ask for the chop throw (only needed for METHOD=LINEAR)

      IF (METHOD .EQ. 'LINEAR') THEN

         CALL PAR_DEF0R('CHOP', CHOP_THROW, STATUS)
         CALL PAR_GET0R('CHOP', CHOP_THROW, STATUS)

      END IF



*     Allow for the fit to be stored rather than simply the
*     data with the fit subtracted. Gives you more flexibility later
*     since can use the kAPPA command SUB to do the subtraction if
*     required.

      CALL PAR_GET0L('RLB', DORLB, STATUS)


*  now go through the various exposures in the observation

      IF (STATUS .EQ. SAI__OK) THEN

*     Method = linear

         IF (METHOD .EQ. 'LINEAR') THEN

            CHOP_SIZE = INT(CHOP_THROW / SAMPLE_DX)


*     Remove a linear baseline

            CALL SCULIB_REMOVE_LINEAR_BASELINE(DORLB, N_EXPOSURES,
     :           N_INTEGRATIONS, N_MEASUREMENTS,
     :           %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)), N_BOL, N_POS,
     :           %VAL(CNF_PVAL(IN_DATA_PTR)),
     :           %VAL(CNF_PVAL(IN_VARIANCE_PTR)),
     :           %VAL(CNF_PVAL(IN_QUALITY_PTR)), CHOP_SIZE, CHOP_SIZE,
     :           %VAL(CNF_PVAL(OUT_DATA_PTR)),
     :           %VAL(CNF_PVAL(OUT_VARIANCE_PTR)),
     :           %VAL(CNF_PVAL(OUT_QUALITY_PTR)), BADBIT, STATUS)

         ELSE IF (METHOD .EQ. 'MEAN' .OR. METHOD .EQ. 'MEDIAN') THEN

            CALL SURFLIB_REMOVE_DC_FROM_EXP(DORLB, N_EXPOSURES,
     :           N_INTEGRATIONS, N_MEASUREMENTS, METHOD,
     :           %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)), N_BOL, N_POS,
     :           %VAL(CNF_PVAL(IN_DATA_PTR)),
     :           %VAL(CNF_PVAL(IN_VARIANCE_PTR)),
     :           %VAL(CNF_PVAL(IN_QUALITY_PTR)),
     :           %VAL(CNF_PVAL(OUT_DATA_PTR)),
     :           %VAL(CNF_PVAL(OUT_VARIANCE_PTR)),
     :           %VAL(CNF_PVAL(OUT_QUALITY_PTR)), BADBIT, STATUS)

         ELSE IF (METHOD .EQ. 'SECTION') THEN

*     Get the section
            CALL PAR_GET1C('SECTION', SCUBA__MAX_SECT, DATA_SPEC,
     :           N_SPEC, STATUS)


*     Look for inverse section specified
            USE_SECT = .TRUE.

            DO I = 1, N_SPEC

               NEGPOS = 1
               CALL CHR_FIND(DATA_SPEC(I), NEGCHAR, .TRUE., NEGPOS)

*     If the string contains the character remove it and set USE_SECT
               IF (NEGPOS .LE. CHR_LEN(DATA_SPEC(I))) THEN
                  USE_SECT = .FALSE.
                  CALL CHR_RMCHR(NEGCHAR, DATA_SPEC(I))
               END IF

            END DO

*     Report that we are using an inverted section if necessary
            IF (.NOT.USE_SECT) THEN

               CALL MSG_SETC('TASK', TSKNAME)
               CALL MSG_OUTIF(MSG__NORM,' ','^TASK: The inverse '//
     :              'section has been selected', STATUS)

            END IF

*     Do the removal
            CALL SURFLIB_REMOVE_DC_VIA_SECT(DORLB, N_SPEC, DATA_SPEC,
     :           USE_SECT, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :           %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)), N_BOL, N_POS,
     :           %VAL(CNF_PVAL(IN_DATA_PTR)),
     :           %VAL(CNF_PVAL(IN_VARIANCE_PTR)),
     :           %VAL(CNF_PVAL(IN_QUALITY_PTR)),
     :           %VAL(CNF_PVAL(OUT_DATA_PTR)),
     :           %VAL(CNF_PVAL(OUT_VARIANCE_PTR)),
     :           %VAL(CNF_PVAL(OUT_QUALITY_PTR)), BADBIT, STATUS)

         ELSE

            STATUS = SAI__ERROR
            CALL MSG_SETC('TSK',TSKNAME)
            CALL MSG_SETC('MTD',METHOD)
            CALL ERR_REP(' ','^TSK: Method ^MTD not recognised',
     :           STATUS)

         END IF


      END IF

*  tidy up


      CALL NDF_UNMAP (OUTNDF, '*', STATUS)
      CALL NDF_UNMAP (INDF, '*', STATUS)

      CALL CMP_UNMAP (IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)

      CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)

      CALL NDF_ANNUL (INDF, STATUS)
      CALL NDF_ANNUL (OUTNDF, STATUS)

      CALL NDF_END (STATUS)

      END
