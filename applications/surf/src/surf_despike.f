      SUBROUTINE SURF_DESPIKE (STATUS)
*+
*  Name:
*     DESPIKE2

*  Purpose:
*     Remove spikes from SCAN/MAP observations

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL SURF_DESPIKE( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     This routine removes spikes from SCAN/MAP observations.
*     The scan map differential despiking algorithm uses 2 criteria
*     to decide which points are spikes.
*
*     First, for each bolometer used a pass is made through each
*     scan calculating for each point:-
*
*       diff(i) = point(i) - (point(i-1) + point(i+1))
*                            -------------------------
*                                       2.0 
*
*
*     Values of 'diff' for the first and last points in the scan are
*     calculated in a similar way but subtracting the mean of points
*     2 and 3 and points n-1 and n-2 respectively.
*
*     The mean and standard deviation of 'diff' are calculated by
*     coadding the 10 points at each end of the scan where,
*     hopefully, there is no source emission. Spikes in these
*     regions are handled by removing points from the coadd that lie
*     further than 3 sigma from the mean, then redoing the
*     calculation recursively until no further points need be
*     removed.
*
*     The first criterion for a spike is that it's 'diff' value
*     should be further from the mean of 'diff' by NSIGMA times the
*     sigma derived from the endpoints.
*     
*     The problem with this simple approach is that bright sources
*     in the scan themselves lead to excursions in 'diff' that can
*     be wrongly identified as spikes. To prevent this happening a
*     second criterion is used. In this the scan values are
*     convolved with a 3 sample wide box so that each 'box' point is
*     the average of the point itself and the points on either side of
*     it. 'Box' is expected to increase faster for real sources than
*     for spikes because in them the increase will be spread over
*     all 3 averaged points rather than just 1.
* 
*     The second criterion for a spike is met, therefore, if a
*     point's 'diff' is further from the 'diff' mean than the value
*     of 'box' at that point.
*
*     Fixed-up values for points that have identified as spikes are
*     calculated by interpolating between the closest healthy points
*     on either side.
*
*     The second spike criterion also means unfortunately that the
*     technique is less sensitive to spikes on bright sources than
*     elsewhere. In addition, it is still possible to clip bright
*     sources if too low a value for NSIGMA is used. It is
*     recommended to run despike several times with different values
*     of NSIGMA. Begin with NSIGMA=5, look at the result to see how
*     effective despiking has been, then repeat the process with
*     NSIGMA=4.5, 4.0 etc. until you start to clip source
*     information.



*  Usage:
*     restore in out nsigma

*  ADAM Parameters:
*     IN = NDF (Read)
*        The name of the input file containing demodulated SCUBA data.
*     MSG_FILTER = CHAR (Read)
*        Message filter level. Default is NORM. In verbose mode the location
*        of each spike is reported.
*     NSIGMA = REAL (Read)
*        Nsigma from mean at which 'spikes' begin.
*     OUT = NDF (Write)
*        The name of the output file to contain the processed data.
*        A default output name is suggested that is derived from the
*        input.

*  Examples:
*     restore o37 o37_des 5.0
*       Despike o37.sdf at 5.0 sigma.
*     restore o37 \
*       Despike using the default sigma level and writing to the
*       default output file.

*  Notes:
*     - Care must be taken when despiking bright sources.
*     - If there are 3 spikes in a row and the middle spike is approximately
*       the mean of the spikes either side, the middle spike will not be
*       treated as a spike (and will not be changed)

*  Related Applications:
*     SURF: DESPIKE, SCUCLIP, SIGCLIP, RESTORE

*  Authors:
*     JFL: John Lightfoot (jfl@roe.ac.uk)
*     TIMJ: T. Jenness (t.jenness@jach.hawaii.edu)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*    History:
*     $Log$
*     Revision 1.11  2005/03/18 06:26:21  timj
*     + Initialise some variables
*     + Protect some calls with STATUS checks
*
*     Revision 1.10  2004/09/08 02:03:33  timj
*     Add CNF_PVAL where appropriate
*
*     Revision 1.9  2001/10/30 03:02:03  timj
*     POLMAP is a MAP observation
*
*     Revision 1.8  1999/08/03 20:36:40  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     Revision 1.7  1999/05/15 01:48:39  timj
*     Finalise support for POLMAP/POLPHOT observing modes.
*     Only check first few characters of history app name
*     now that we are writing version number to this string.
*     POLPHOT is synonym for PHOTOM.
*
*     Revision 1.6  1997/12/23 21:44:16  timj
*     Set badbit mask for despiking.
*
*     Revision 1.5  1997/12/22 23:57:06  timj
*     Add note to documentation. Remove locator to SCUCD extension. Map input data
*     after checking the data dimensions.
*
*     Revision 1.4  1997/12/19 03:17:05  timj
*     Write out total number of spikes removed.
*
*     Revision 1.3  1997/11/30 01:40:01  timj
*     Add some documentation.
*
*     Revision 1.2  1997/11/06 23:22:45  timj
*     Add the verbose suffix option.
*
*     Revision 1.1  1997/09/04 18:43:36  timj
*     Initial revision
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
      INCLUDE 'SURF_PAR'                ! SURF constants
      INCLUDE 'MSG_PAR'                 ! for MSG__ constants
      INCLUDE 'CNF_PAR'                 ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External References:
      BYTE    SCULIB_BITON              ! Turn on a bit

*  Local Constants:
      INTEGER MAXDIM
      PARAMETER (MAXDIM = 4)
      CHARACTER * 10 TSKNAME            ! Name of task
      PARAMETER (TSKNAME = 'DESPIKE2')

*  Local variables:
      LOGICAL      ABORTED              ! .TRUE. if observation was
                                        ! aborted
      BYTE         BADBIT               ! Bad bit mask
      INTEGER      DIM (MAXDIM)         ! the dimensions of an array
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
      INTEGER      NDIM                 ! the number of dimensions in an array
      INTEGER      NREC                 ! number of history records in file
      REAL         NSIGMA               ! sigma cut-off for spike
                                        ! detection
      INTEGER      NSPIKES              ! Total number of spikes detected
      INTEGER      N_BOL                ! number of bolometers measured 
      INTEGER      N_EXPOSURES          ! number of exposures per integration
      INTEGER      N_FITS               ! number of FITS lines read from file
      INTEGER      N_INTEGRATIONS       ! number of integrations per measurement
      INTEGER      N_MEASUREMENTS       ! number of measurements in the file
      INTEGER      N_POS                ! the total number of positions measured
      CHARACTER*30 OBJECT               ! name of object observed
      CHARACTER*15 OBSERVING_MODE       ! type of observation
      CHARACTER*132 OUTFILE             ! Output filename
      INTEGER      OUTNDF               ! NDF identifier of output file
      INTEGER      OUT_DATA_PTR         ! pointer to data array in output
      INTEGER      OUT_QUALITY_PTR      ! pointer to quality array in output 
      INTEGER      OUT_VARIANCE_PTR     ! pointer to variance array in output
      LOGICAL      REDUCE_SWITCH        ! .TRUE. if REDUCE_SWITCH has been run
      INTEGER      RUN_NUMBER           ! run number of observation
      CHARACTER*80 SAMPLE_MODE          ! sample mode of observation
      CHARACTER*80 STATE                ! 'state' of SCUCD at end of
                                        ! observation
      CHARACTER*80 STEMP                ! scratch string
      CHARACTER * (10) SUFFIX_STRINGS(SCUBA__N_SUFFIX) ! Suffix for OUT

*  Local Data:
      DATA SUFFIX_STRINGS /'!_des','d','_des'/

*.

      IF (STATUS .NE. SAI__OK) RETURN





*  start up the NDF system and read in the demodulated data file

      CALL NDF_BEGIN

      CALL NDF_ASSOC ('IN', 'READ', INDF, STATUS)
*     Get the name of the filename associated with 'IN'

      CALL SCULIB_GET_FILENAME('IN', FNAME, STATUS)

*  Read in badbit mask

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

*  check that the observation was suitable for DESPIKE

      IF (STATUS .EQ. SAI__OK) THEN
         IF (OBSERVING_MODE .NE. 'MAP' 
     :        .AND. OBSERVING_MODE .NE. 'POLMAP') THEN
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

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (INDF, 'APPLICATION', I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP(:13) .EQ. 'REDUCE_SWITCH') THEN
                  REDUCE_SWITCH = .TRUE.
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
         END IF
      END IF

*  find out if the observation was aborted

      STATE = ' '
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'STATE',
     :  STATE, STATUS)
      IF (STATUS .EQ. SAI__OK) CALL CHR_UCASE (STATE)
      ABORTED = .FALSE.
      IF (INDEX(STATE,'ABORTING') .NE. 0) THEN
         ABORTED = .TRUE.
      END IF

*     check the data dimensions 

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

*     Map the input data

      CALL NDF_MAP (INDF, 'QUALITY', '_UBYTE', 'READ',
     :  IN_QUALITY_PTR, ITEMP, STATUS)
      CALL NDF_MAP (INDF, 'DATA', '_REAL', 'READ', IN_DATA_PTR,
     :  ITEMP, STATUS)
      CALL NDF_MAP (INDF, 'VARIANCE', '_REAL', 'READ', IN_VARIANCE_PTR,
     :  ITEMP, STATUS)


*  map the DEM_PNTR array and check its dimensions

      N_EXPOSURES = 0
      N_INTEGRATIONS = 0
      N_MEASUREMENTS = 0
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
 
*  read the NSIGMA for the spike cut-off

      CALL PAR_GET0R ('NSIGMA', NSIGMA, STATUS)

*     Generate a default name for the output file
      CALL SCULIB_CONSTRUCT_OUT(FNAME, SUFFIX_ENV, SCUBA__N_SUFFIX,
     :     SUFFIX_OPTIONS, SUFFIX_STRINGS, OUTFILE, STATUS)

*     set the default
      CALL PAR_DEF0C('OUT', OUTFILE, STATUS)

*  now open the output NDF, propagating it from the input file

      CALL NDF_PROP (INDF, 'Axis,Units', 'OUT', OUTNDF, STATUS)

*  map the various components of the output data array

      CALL NDF_MAP (OUTNDF, 'QUALITY', '_UBYTE', 'WRITE',
     :  OUT_QUALITY_PTR, ITEMP, STATUS)
      CALL NDF_MAP (OUTNDF, 'DATA', '_REAL', 'WRITE', 
     :  OUT_DATA_PTR, ITEMP, STATUS)
      CALL NDF_MAP (OUTNDF, 'VARIANCE', '_REAL', 'WRITE',
     :  OUT_VARIANCE_PTR, ITEMP, STATUS)

*     Set the output bad bit mask
*     There is a chance that we can set bit 4 (need to put bit numbers
*     in include file

      BADBIT = SCULIB_BITON(BADBIT, 4)
      CALL NDF_SBB(BADBIT, OUTNDF, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN

*  despike the data

         CALL SURFLIB_DIFF_DESPIKE (N_EXPOSURES,
     :     N_INTEGRATIONS, N_MEASUREMENTS, 
     :     %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)), N_BOL, N_POS,
     :     %VAL(CNF_PVAL(IN_DATA_PTR)), %VAL(CNF_PVAL(IN_VARIANCE_PTR)),
     :     %VAL(CNF_PVAL(IN_QUALITY_PTR)), BADBIT, NSIGMA,
     :     %VAL(CNF_PVAL(OUT_DATA_PTR)), 
     :     %VAL(CNF_PVAL(OUT_VARIANCE_PTR)),
     :     %VAL(CNF_PVAL(OUT_QUALITY_PTR)), NSPIKES, STATUS)

*     unmap the main data array

         CALL NDF_UNMAP (OUTNDF, '*', STATUS)

*     Report the total number of spikes if greater than 0
         IF (NSPIKES .GT. 1) THEN
            CALL MSG_SETI('NS', NSPIKES)
            CALL MSG_SETC('TSK', TSKNAME)
            CALL MSG_OUTIF(MSG__NORM, ' ', '^TSK: ^NS spikes '//
     :           'detected.', STATUS)

         ELSE IF (NSPIKES .EQ. 1) THEN
            CALL MSG_SETI('NS', NSPIKES)
            CALL MSG_SETC('TSK', TSKNAME)
            CALL MSG_OUTIF(MSG__NORM, ' ', '^TSK: ^NS spike '//
     :           'detected.', STATUS)

         ELSE
            CALL MSG_SETC('TSK', TSKNAME)
            CALL MSG_OUTIF(MSG__NORM, ' ', '^TSK: No spikes '//
     :           'detected.', STATUS)

         END IF

      END IF

*  tidy up

      CALL CMP_UNMAP (IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)

      CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)

      CALL NDF_ANNUL (INDF, STATUS)
      CALL NDF_ANNUL (OUTNDF, STATUS)

      CALL NDF_END (STATUS)

      END
