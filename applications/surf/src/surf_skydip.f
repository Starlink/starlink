      SUBROUTINE SURF_SKYDIP (STATUS)
*+
*  Name:
*     SKYDIP

*  Purpose:
*     calculate sky properties from SCUBA skydip data

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SURF_SKYDIP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*     This application takes raw SKYDIP data and calculates tau, eta_tel
*     and B by fitting. Sky brightness temperatures are calculated for
*     different airmasses and then fitted with a model of the sky.
*     This application accepts raw data or data processed by REDUCE_SWITCH.

*  Usage:
*     skydip in sub_instrument t_cold eta_tel b_fit out model_out

*  ADAM Parameters:
*     IN = NDF (Read)
*        The name of the raw skydip data file or of the file processed
*        by REDUCE_SWITCH.
*     B_ERR = REAL (Write)
*        The error on the fitted value of B_VAL
*     B_FIT = REAL (Write)
*        The fitted value of the B parameter.
*     B_VAL = REAL (Read)
*        The B parameter (filter transmission). This efficiency factor
*        must be between 0 and 1. A negative value allows this parameter
*        to be free.
*     CVAR = LOGICAL (Read)
*        This parameter governs whether the points are fitted with
*        a constant variance for all points (true) or the variance
*        derived from the scatter in the individual integrations (false).
*        The value used for the fixed variance is the mean of all the
*        calculated variances.
*     ETA_ERR = REAL (Write)
*        The error on the fitted value of ETA_TEL
*     ETA_TEL = REAL (Read)
*        The telescope efficiency. If available the current telescope value
*        is used as the default.  Values must be between 0 and 1.0.
*        A negative value allows this parameter to be free.
*        For data taken before 26 April 2000, the default values supplied
*        for 850 and 450 are provided by the routine rather than being
*        read from the FITS header.
*     ETA_TEL_FIT = REAL (Write)
*        The fitted value of ETA_TEL.
*     GOODFIT = LOGICAL (Write)
*        Flag to indicate whether the fit was good (TRUE) or bad (FALSE).
*     MODEL_OUT = CHAR (Write)
*        The name of the output file that contains the fitted sky
*        temperatures.
*     MSG_FILTER = CHAR (Read)
*        Message filter level. Default is NORM.
*     OUT = CHAR (Write)
*        The name of the output file that contains the measured
*        sky temperatures.
*     RESIDUAL = DOUBLE (Write)
*        Absolute difference between the model and the data in Kelvin.
*        i.e. Sum ( Abs(Data - model) )
*     SIGMA = DOUBLE (Write)
*        Standard deviation of the difference between the fit
*        and the input data.
*     SUB_INSTRUMENT = CHAR (Read)
*        The name of the sub-instrument whose data are to be
*        selected from the input file and fitted. Permitted
*        values are SHORT, LONG, P1100, P1350 and P2000
*     TAUZ_ERR = REAL (Write)
*        The error on the fitted value of TAUZ
*     TAUZ_FIT = REAL (Write)
*        The fitted sky opacity for the selected sub instrument.
*     T_COLD = REAL (Read)
*        Temperature of the cold load. The default value is
*        taken from the input file. This parameter is ignored if the
*        REDUCE_SWITCH'ed data is supplied.
*     T_HOT = REAL (Read)
*        Temperature of the hot load. This parameter is ignored if the
*        REDUCE_SWITCH'ed data is supplied.
*     WAVELENGTH = REAL (Write)
*        The wavelength of the fitted data.
*     XISQ = REAL (Write)
*        The reduced chi square of the fit.

*  Examples:
*     skydip jun10_dem_0002 short \
*        Process the short sub-instrument using the default value
*        for T_COLD and allowing ETA_TEL and B to be free parameters.
*        No output files are written.
*     skydip 19970610_dem_0003 long eta_tel=0.9 out=sky model_out=model b_val=-1
*        Process the long wave sub-instrument with ETA_TEL fixed at 0.9
*        and B free. Write the sky temperature to sky.sdf and the fitted
*        model to model.sdf.

*  Notes:
*     If the input file is not found in the current directory, the directory
*     specified by the DATADIR environment variable is searched. This means
*     that the raw data does not have to be in the working directory. In
*     addition 'IN' accepts a number. This number is converted to a demodulated
*     data filename by prepending it with information specified in
*     the SCUBA_PREFIX environment variable. This filename expansion only works
*     for demodulated data (ie data containing '_dem_'). The '_dem_' is
*     assumed and should not be present in $SCUBA_PREFIX.
*

*  Related Applications:
*     SURF: EXTINCTION, SDIP, REDUCE_SWITCH

*  Algorithm:
*     If status is good on entry, the routine reads in some general information
*     about the run and checks the HISTORY record to make sure this is a SKYDIP
*     observation.
*     The sub-instrument information is then read and checked against the
*     requested sub-instrument.
*     Bolometer information is then read and matched to the requested sub-
*     instrument.
*     If everything is okay, the DATA is mapped and the dimensions checked.
*     This is followed by a check of DEM_PNTR.
*     The MAX and MIN elevation are then read from the FITS data.
*     The AIRMASS steps are then calculated and fitting paramters requested.

*     If we have been given the raw data we calculate the sky temperatures
*     for each measurement with SCULIB_CALC_SKYDIP_TEMPS. If we are processing
*     the output from reduce_switch the skydip temperatures have already
*     been calculated and we just have to average the data from each
*     integration into a measurement.
*
*     The data is then fitted with SCULIB_FIT_SKYDIP. Both the measured and
*     model data are then written to NDFs for plotting in KAPPA-LINPLOT. (The
*     model data is calculated with SCULIB_J_THEORETICAL.

*  Authors:
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 1995-2006 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     $Log$
*     Revision 1.4  2006/07/05 21:29:40  timj
*     fix copyright year
*
*     Revision 1.3  2006/07/05 21:28:46  timj
*     sculib_airmass now has double precision args
*
*     Revision 1.2  2005/08/15 08:40:03  timj
*     check for %LOC vs LOC in surf_skydip.F
*
*     Revision 1.1  2005/08/15 07:36:24  timj
*     renamed from surf_skydip.f in order to check for %LOC
*
*     Revision 1.37  2004/09/08 02:03:35  timj
*     Add CNF_PVAL where appropriate
*
*     Revision 1.36  2004/09/08 00:57:56  timj
*     Register %LOC pointer with CNF
*
*     Revision 1.35  2001/02/22 02:49:56  timj
*     Support raster skydips in new TCS
*
*     Revision 1.34  2000/06/16 01:25:16  timj
*     Use new-format SCULIB_GET_MJD
*
*     Revision 1.33  2000/06/03 03:14:28  timj
*     Default ETA_TEL correctly
*
*     Revision 1.32  2000/05/11 19:59:03  timj
*     Add RESIDUAL parameter
*
*     Revision 1.31  1999/08/03 20:01:43  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     Revision 1.30  1999/07/15 07:59:19  timj
*     Correct types for some MSG_ calls
*
*     Revision 1.29  1999/05/15 01:48:43  timj
*     Finalise support for POLMAP/POLPHOT observing modes.
*     Only check first few characters of history app name
*     now that we are writing version number to this string.
*     POLPHOT is synonym for PHOTOM.
*
*     Revision 1.28  1999/03/24 00:28:55  timj
*     Initial release of RASTER skydip mode.
*
*     Revision 1.27  1998/06/06 03:23:24  timj
*     Add errors - store as parameters
*
*     Revision 1.26  1998/01/23 04:01:33  timj
*     Fix some typos in the header.
*
*     Revision 1.25  1998/01/15 01:59:33  timj
*     Check for aborted skydips.
*     Always write the fit parameters (even when status is bad)
*
*     Revision 1.24  1998/01/12 21:01:43  timj
*     Add output parameters and update header to reflect change.
*
*     Revision 1.23  1998/01/07 20:21:35  timj
*     Make sure that Model airmass range matches the airmass range of the
*     data.
*
*     Revision 1.22  1998/01/07 00:31:35  timj
*     Add the CVAR parameter for controlling whether a constant variance is used.
*
*     Revision 1.21  1997/07/28 20:58:23  timj
*     Add support for the presence of ETATEL in the FITS header.
*
*     Revision 1.20  1997/07/19 02:41:47  timj
*     Add header information to describe SCUBA_PREFIX.
*
*     Revision 1.19  1997/07/19 00:26:49  timj
*     Change SEARCH_DATADIR so that it passes PACKAGE.
*
*     Revision 1.18  1997/07/19 00:23:22  timj
*     Now can read REDUCE_SWITCH output.
*     Uses SCULIB_CALC_SKYDIP_TEMPS
*
*     Revision 1.17  1997/06/27 23:23:22  timj
*     Slight changes to header.
*     Make sure I am not setting status when status is already bad.
*
*     Revision 1.16  1997/06/20 21:46:10  timj
*     Add printout of the LST of the observation since this is useful for
*     EXTINCTION.
*
*     Revision 1.15  1997/06/13 00:19:45  timj
*     Use MSG_OUTIF
*     Change name to SURF
*     Doc updates.
*
*     Revision 1.14  1997/06/05 23:14:28  timj
*     Initialise pointer.
*
*     Revision 1.13  1997/05/27 22:19:55  timj
*     Use %LOC instead of LOC - for Alpha compiler
*
*     Revision 1.12  1997/05/22 03:27:03  timj
*     Fix problem with setting status to bad early on.
*     Now allows remote access (DATADIR) of input files.
*
*     Revision 1.11  1997/05/22 01:16:07  timj
*     Check that this is SKYDIP data.
*
*     Revision 1.10  1997/04/15 18:30:30  timj
*     Remove debugging print statement.
*
*     Revision 1.9  1997/04/03 20:13:31  timj
*     Add SCULIB_GET_SUB_INST and SCULIB_GET_DEM_PNTR.
*     Write both output files inside a loop to reduce amount of repeating code.
*     Add PKG and TASK names for MSG_OUT.
*
*     Revision 1.8  1997/03/07 02:33:30  timj
*     Add support for PAR__NULL.
*     Add support for START_EL and END_EL.
*     Minor tweak in call name for non-NAG.
*
c Revision 1.7  1996/12/12  21:05:28  timj
c Fix Starlink header.
c Replace COPYR with VEC
c Use PAR_CHOIC for SUB_INSTRUMENT.
c
c Revision 1.6  1996/08/28  03:07:57  timj
c Added BADBIT
c
c Revision 1.5  1996/08/27  03:42:44  timj
c Fix UBYTES for QUALITY
c
c Revision 1.4  1996/08/26  19:36:40  timj
c Quality array was being mapped as REAL instead of UBYTE.
c
c Revision 1.3  1996/08/26  19:31:53  timj
c Remove LTEMP variable (left over from BAD_PIXEL experiment)
c
c Revision 1.2  1996/08/26  19:27:43  timj
c Use SCULIB_COPYX to copy data to mapped arrays.
c Fix bug when writing out T_COLD
c
c Revision 1.1  1996/08/16  15:26:31  timj
c Initial revision
c
*     2011-08-18 (TIMJ):
*        Factor out model/data writing to separate routine which simplifies
*        the code a lot and saves us from using %LOC.
*     {enter_further_changes_here}

*  Bugs:
*     Aborted datasets have an incorrect Y-axis scale.
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'                 ! SSE global definitions
      INCLUDE 'DAT_PAR'                 ! for DAT__SZLOC
      INCLUDE 'MSG_PAR'                 ! MSG__ constants
      INCLUDE 'PAR_ERR'                 ! for PAR__ constants
      INCLUDE 'PRM_PAR'                 ! for VAL__ constants
      INCLUDE 'SURF_PAR'                ! SURF  constants
      INCLUDE 'CNF_PAR'                 ! For CNF_PREG and CNF_PVAL

*  Arguments Given:

*  Arguments Given & Returned:

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN

*  Global variables:

*  Local Constants:
      REAL    ARCSEC                    ! 1 arcsec in radians
      PARAMETER (ARCSEC = 4.8481368E-6)
      BYTE    BADBIT                    ! Bad bit mask
      PARAMETER (BADBIT = 1)
      REAL    DEG2RAD                   ! Convert degrees to radians
      PARAMETER (DEG2RAD = 0.017453292)
      INTEGER MAXDIM
      PARAMETER (MAXDIM = 4)
      INTEGER N_MODEL                   ! Number of points in output model
      PARAMETER (N_MODEL = 100)
      CHARACTER * 8 TSKNAME             ! Name of task
      PARAMETER (TSKNAME = 'SKYDIP')
      REAL A_MAX                        ! Max acceleration of telescope
      PARAMETER (A_MAX = 0.15 * DEG2RAD) ! 0.15/deg/sec/sec
      REAL V_MAX                        ! Max velocity of telescope
      PARAMETER (V_MAX = 0.7 * DEG2RAD) ! 0.7 deg/sec
      INTEGER MAX_FIT_DATA              ! Max number of points
      PARAMETER (MAX_FIT_DATA = SCUBA__MAX_MEAS) ! allowed in input data

*    Local variables:
      LOGICAL ABORTED                   ! Was the observation aborted
      REAL    AIR_MODEL(N_MODEL)        ! Airmass values for MODEL
      REAL    AIRMASS(MAX_FIT_DATA)     ! Array of AIRMASS data
      REAL    AIRMASST(MAX_FIT_DATA)    ! First Array of AIRMASS data
      DOUBLE PRECISION AIRMASS_START    ! Start airmass
      DOUBLE PRECISION AIRMASS_END      ! End airmass
      REAL    AIRMASS_TVAR(MAX_FIT_DATA)! First Array of AIRMASS variance
      REAL    AIRMASS_VAR(MAX_FIT_DATA) ! Array of AIRMASS variance
      REAL    AIRSTEP                   ! AIRMASS increment for DO loop
      REAL    AV_DATA(SCUBA__MAX_SUB, SCUBA__MAX_MEAS)
                                        ! Average skydip
      BYTE    AV_QUAL(SCUBA__MAX_SUB, SCUBA__MAX_MEAS)
                                        ! Quality of AV_QUAL
      REAL    AV_VAR(SCUBA__MAX_SUB, SCUBA__MAX_MEAS)
                                        ! Var for average skydip
      REAL    B                         ! requested B
      REAL    B_ERROR                   ! Error in B
      REAL    B_FIT                     ! B parameter
      INTEGER BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! A/D numbers of bolometers measured in
                                        ! input file
      INTEGER BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! channel numbers of bolometers
                                        ! measured in input file
      CHARACTER*20 BOL_TYPE (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                        ! bolometer types
      REAL             BOL_DU3 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                        ! dU3 Nasmyth coord of bolometers
      REAL             BOL_DU4 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                        ! dU4 Nasmyth coord of bolometers
      LOGICAL CVAR                      ! Use constant variance?
      DOUBLE PRECISION DAIRMASST(MAX_FIT_DATA)
                                        ! Tmp DBL airmass array for RASTER
      DOUBLE PRECISION DAIRMASS_TVAR(MAX_FIT_DATA)
                                        ! DBL airmass var array for RASTER
      INTEGER DIM (MAXDIM)              ! the dimensions of an array
      REAL    DEFAULT_ETA_TEL           ! Eta tel read from FITS header
      INTEGER DUM_DATA_PTR_END          ! Scratch space end
      INTEGER DUM_DATA_PTR              ! Scratch space
      INTEGER DUM_VAR_PTR_END           ! Scratch space 2end
      INTEGER DUM_VAR_PTR               ! Scratch space 2
      INTEGER DUM_QUAL_PTR_END          ! Scratch space 3 end
      INTEGER DUM_QUAL_PTR              ! Scratch space 3
      INTEGER DUMMY_PTR_END             ! Scratch space 4 end
      INTEGER DUMMY_PTR                 ! Scratch space 4
      DOUBLE PRECISION DTEMP            ! Temp double
      REAL    EL                        ! Elevation in radians used for loop
      REAL    EL0                       ! Elevation at start of measurement
      REAL    ELEV_STEP                 ! Elevation increment
      REAL    END_EL                    ! End elevation of skydip
      REAL    EPOCH                     ! Julian epoch of observation
      REAL    ETA_ERROR                 ! Error in ETA_TEL
      REAL    ETA_TEL                   ! Telescope efficiency
      REAL    ETA_TEL_FIT               ! Fitted eta_tel
      INTEGER EXP_END                   ! end index of data for an exposure
      INTEGER EXP_START                 ! start index of data for an exposure
      CHARACTER*15 FILT                 ! Selected filter
      LOGICAL FITFAIL                   ! Status of the Model fit
      CHARACTER*80 FITS (SCUBA__MAX_FITS)
                                        ! array of FITS keyword lines
      CHARACTER*(DAT__SZLOC) FITS_LOC   ! HDS locator to FITS structure
      REAL    FSIGN                     ! Used to store sign information +/-
      INTEGER GOOD                      ! dummy status for PAR_PUT/GOODFIT
      INTEGER I                         ! DO loop index
      INTEGER ITEMP                     ! scratch integer
      BYTE    IN_BADBIT                 ! input Bad bit mask
      INTEGER IN_DEM_PNTR_PTR           ! pointer to input .SCUBA.DEM_PNTR array
      INTEGER IN_DATA_PTR               ! pointer to data array of input file
      INTEGER IN_NDF                    ! NDF identifier of input file
      INTEGER IN_QUAL_PTR               ! pointer to qual array of input file
      REAL    JSKY (SCUBA__MAX_MEAS)    ! Average SKY data for used SUB-INS
      REAL    JSKY_VAR (SCUBA__MAX_MEAS)! Variance of JSKY
      REAL    J_THEORETICAL (N_MODEL)   ! Array of model sky data
      INTEGER      LAST_INT             ! the last integration number in
                                        ! an aborted observation
      INTEGER      LAST_MEAS            ! the last measurement number in
                                        ! an aborted observation
      DOUBLE PRECISION MEAN             ! Mean of measurement
      INTEGER MEASUREMENT               ! Measurement loop counter
      DOUBLE PRECISION MEDIAN           ! Median of measurement
      DOUBLE PRECISION MJD              ! MJD of observation
      LOGICAL MODIFIED                  ! ETA_TEL default has been modified
      INTEGER NDIM                      ! the number of dimensions in an array
      INTEGER NGOOD                     ! Number of good points from stats
      INTEGER NKEPT                     ! Number of good measurements
      INTEGER NREC                      ! number of history records in file
      INTEGER N_BOLS                    ! number of bolometers measured
      INTEGER N_EXPOSURES               ! number of exposures per integration
      INTEGER N_FITS                    ! number of FITS lines read from file
      INTEGER N_INTEGRATIONS            ! number of integrations per measurement
      INTEGER N_MEASUREMENTS            ! number of measurements in the file
      INTEGER N_POS                     ! the total number of positions measured
      INTEGER N_SUB                     ! number of sub-instruments used
      INTEGER N_SWITCHES                ! number of switches per exposure
      CHARACTER*15 OBSERVING_MODE       ! type of observation
      INTEGER OFFSET                    ! Offset in array
      REAL    QSORT(SCUBA__MAX_INT)     ! Scratch space for SCULIB_STATR
      LOGICAL RASTER                    ! True if this is on-the-fly skydip
      DOUBLE PRECISION RESIDUAL         ! Residual of fit
      LOGICAL RESW                      ! Was the data reduce_switched
      REAL    REXISQ                    ! Reduced chi square
      REAL    RTEMP                     ! Temp real
      INTEGER RUN_NUMBER                ! run number of observation
      REAL    SAM_DX                    ! Length of sample point for raster
      DOUBLE PRECISION SIGMA            ! Sigma of difference between model/dat
      LOGICAL SKYDIP                    ! .TRUE. if not RAW data
      INTEGER SLICE_PTR                 ! Pointer to start of slice
      INTEGER SLICE_PTR_END             ! Pointer to end of slice
      CHARACTER*80 STEMP                ! scratch string
      CHARACTER*80 STATE                ! string describing the 'state'
                                        ! of SCUCD when the observation
                                        ! finished
      REAL    START_EL                  ! Start elevation of skydip
      CHARACTER * 15 SUB_INST           ! Name of selected SUB instrument
      INTEGER SUB_POINTER               ! index of SUB_REQUIRED in sub-
                                        ! instruments observed
      DOUBLE PRECISION SUM              ! Sum from SCULIB_STATR
      DOUBLE PRECISION SUMSQ            ! Sum of squares from SCULIB_STATR
      DOUBLE PRECISION STDEV            ! Standard deviation of measurement
      CHARACTER*15 SWITCH_MODE          ! switch mode used
      REAL    TAU_ERROR                 ! Error in tau
      REAL    TAUZ_FIT                  ! Fitted TAU
      REAL    T_AMB                     ! Temperature of ambient load
      REAL    T_COLD                    ! Temperature of cold load
      REAL    T_TEL                     ! Temperature of telescope
      REAL    WAVE                      ! Selectred wavelength
      DOUBLE PRECISION ZD               ! Zenith distance
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC ! Locator to EXTENSIONS

*  External functions:
      INCLUDE 'NDF_FUNC'
*.

      IF (STATUS .NE. SAI__OK) RETURN

*  Initialisations for compiler
      RESW = .FALSE.

* Start up the NDF system and read in some data

      CALL NDF_BEGIN

      CALL SCULIB_SEARCH_DATADIR(PACKAGE, 'IN', IN_NDF, STATUS)

*  get some general descriptive parameters of the observation

      CALL NDF_XLOC (IN_NDF, 'FITS', 'READ', FITS_LOC, STATUS)
      CALL DAT_SIZE (FITS_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK',TSKNAME)
            CALL ERR_REP (' ', '^TASK: input file '//
     :        'contains too many FITS items', STATUS)
         END IF
      END IF

      CALL DAT_GET1C (FITS_LOC, SCUBA__MAX_FITS, FITS, N_FITS, STATUS)

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'RUN',
     :  RUN_NUMBER, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'MODE',
     :  OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)

      CALL MSG_SETC ('MODE', OBSERVING_MODE)
      CALL MSG_SETI ('RUN', RUN_NUMBER)
      CALL MSG_SETC ('PKG', PACKAGE)
      CALL MSG_OUTIF (MSG__NORM, ' ',
     :     '^PKG: run ^RUN was a ^MODE observation ',
     :     STATUS)

*     Check that this is a SKYDIP observation

      IF (OBSERVING_MODE .NE. 'SKYDIP') THEN
         CALL MSG_SETC('TASK', TSKNAME)

         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP(' ', '^TASK: This is not a SKYDIP observation',
     :           STATUS)
         END IF
      END IF


*  check that the history of the input file is OK

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_HNREC (IN_NDF, NREC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            NREC = 0
         END IF

         SKYDIP = .FALSE.
         RESW = .FALSE.

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (IN_NDF, 'APPLICATION', I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP(:6) .EQ. 'SKYDIP') THEN
                  SKYDIP = .TRUE.
               ELSE IF (STEMP(:13) .EQ. 'REDUCE_SWITCH') THEN
                  RESW = .TRUE.
               END IF
            END DO
         ENDIF

         IF (SKYDIP) THEN
            IF (STATUS .EQ. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK',TSKNAME)
               CALL ERR_REP (' ', '^TASK: SKYDIP '//
     :           'has already been run on the input data', STATUS)
            END IF
         END IF
      END IF

*     Get Sidereal time start and end.
*     (Should probably merge this with the identical code
*     found in EXTINCTION)

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'STSTART',
     :  STEMP, STATUS)
      DO I = 1, 2
         ITEMP = INDEX (STEMP,':')
         IF (ITEMP .NE. 0) THEN
            STEMP (ITEMP:ITEMP) = ' '
         END IF
      END DO
      ITEMP = INDEX (STEMP, '.') ! Remove the decimal places
      STEMP = STEMP(:ITEMP-1)

      CALL MSG_SETC ('START_LST', STEMP)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'STEND',
     :  STEMP, STATUS)
      DO I = 1, 2
         ITEMP = INDEX (STEMP,':')
         IF (ITEMP .NE. 0) THEN
            STEMP (ITEMP:ITEMP) = ' '
         END IF
      END DO
      ITEMP = INDEX (STEMP, '.') ! Remove the decimal places
      STEMP = STEMP(:ITEMP-1)

      CALL MSG_SETC ('END_LST', STEMP)
      CALL MSG_SETC ('PKG', PACKAGE)
      CALL MSG_OUTIF (MSG__NORM, ' ',
     :     '^PKG: observation started at sidereal '//
     :     'time ^START_LST and ended at ^END_LST', STATUS)

*  find and report the sub instruments used and filters for this observation
*  and ask for the sub-instrument of interest.

      CALL SCULIB_GET_SUB_INST(PACKAGE, N_FITS, FITS, 'SUB_INSTRUMENT',
     :     N_SUB, SUB_POINTER, WAVE, SUB_INST, FILT, STATUS)

*     Store the wavelength
      CALL PAR_PUT0R('WAVELENGTH', WAVE, STATUS)

* Add _DC suffix to data

      IF (STATUS .EQ. SAI__OK) THEN
         ITEMP = CHR_LEN(SUB_INST)
         CALL CHR_APPND('_DC', SUB_INST, ITEMP)
      END IF

*  get the number of bolometers measured

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'N_BOLS',
     :  N_BOLS, STATUS)


* Find the SCUBA extension

      CALL NDF_XLOC (IN_NDF, 'SCUBA', 'READ', IN_SCUBAX_LOC, STATUS)

* Get the bolometer description arrays (NB BOL_DU3 and DU4 are not used)

      CALL SCULIB_GET_BOL_DESC(IN_SCUBAX_LOC, SCUBA__NUM_CHAN,
     :     SCUBA__NUM_ADC, N_BOLS, BOL_TYPE, BOL_DU3,
     :     BOL_DU4, BOL_ADC, BOL_CHAN, STATUS)

*     Check the dimensions of the input data array

      CALL NDF_DIM (IN_NDF, MAXDIM, DIM, NDIM, STATUS)

*     We have to distinguish the SKYDIP raw data from the output
*     of reduce_switch

      IF (STATUS .EQ. SAI__OK) THEN

         IF (RESW) THEN

            IF ((NDIM .NE. 2)        .OR.
     :           (DIM(1) .NE. N_BOLS)      .OR.
     :           (DIM(2) .LT. 1)) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETC ('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: main data '//
     :              'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2',
     :              STATUS)
            END IF

            N_POS = DIM(2)

         ELSE
            IF ((NDIM .NE. 3)        .OR.
     :           (DIM(1) .NE. SCUBA__N_TEMPS)      .OR.
     :           (DIM(2) .NE. N_BOLS) .OR.
     :           (DIM(3) .LT. 1))     THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL MSG_SETC ('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: main data '//
     :              'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2 '//
     :              '^DIM3', STATUS)
            END IF

            N_POS = DIM (3)


         END IF

      END IF

*     Read the SAM_MODE flag from the header

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'SAM_MODE',
     :     STEMP, STATUS)

      IF (STEMP .EQ. 'RASTER') THEN
         RASTER = .TRUE.
      ELSE
         RASTER = .FALSE.
      END IF

*     I should use automatic quality masking but the SCULIB_STATR
*     routine uses a quality mask so it is easier to just map it
*     rather than trying to change an existing routine
*     We do use automatic quality masking for RASTER mode data
*     since that does not use SCULIB_STATR

      IF (RESW .AND. .NOT. RASTER) THEN

         CALL NDF_MAP (IN_NDF, 'QUALITY', '_UBYTE', 'READ', IN_QUAL_PTR,
     :        ITEMP, STATUS)

*     and the BADBIT mask...

         CALL NDF_BB(IN_NDF, IN_BADBIT, STATUS)

      END IF

*     Finally map the data array

      CALL NDF_MAP (IN_NDF, 'DATA', '_REAL', 'READ', IN_DATA_PTR,
     :  ITEMP, STATUS)

*     We do not use the variance array since the variance is derived
*     from the error of the mean (for REDUCE_SWITCH'ed data)
*     and at present the output from SKYDIP REDUCE_SWITCH contains
*     no errors for the skydip temperatures.



*  map the DEM_PNTR array and check its dimensions
*     Note that the processed data no longer has switches.

      IF (RESW) THEN
         NDIM = 3
      ELSE
         NDIM = 4
      END IF

      CALL SCULIB_GET_DEM_PNTR(NDIM, IN_SCUBAX_LOC,
     :     IN_DEM_PNTR_PTR, N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS, STATUS)

      IF (N_MEASUREMENTS .LE. 1) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ','^TASK: Not enough measurements for fit',
     :           STATUS)
         END IF
      END IF


*  check that the number of switches matches the SWITCH_MODE
*  SKYDIP does not switch and the REDUCE_SWITCHed data doesn't either
*  This is not really necessary!

      IF (STATUS .EQ. SAI__OK) THEN
         IF (N_SWITCHES .GT. 1) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('SWITCH_MODE', SWITCH_MODE)
            CALL MSG_SETI ('N_S', N_SWITCHES)
            CALL MSG_SETC('TASK',TSKNAME)
            CALL ERR_REP (' ', '^TASK: number '//
     :        'of switches in .SCUBA.DEM_PNTR (^N_S) does not '//
     :        'match SWITCH_MODE (SKYDIP)', STATUS)
         END IF
      END IF

*     see if the observation completed normally or was aborted

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'STATE',
     :  STATE, STATUS)
      CALL CHR_UCASE (STATE)
      ABORTED = .FALSE.
      IF (INDEX(STATE,'ABORTING') .NE. 0) THEN
         ABORTED = .TRUE.
      END IF


*     Report general information about the SKYDIP

      CALL MSG_SETI ('N_I', N_INTEGRATIONS)
      CALL MSG_SETI ('N_M', N_MEASUREMENTS)
      CALL MSG_SETC ('PKG', PACKAGE)

      IF (.NOT. ABORTED) THEN

         CALL MSG_OUTIF (MSG__NORM, ' ', '^PKG: file contains data '//
     :        'for ^N_I integration(s) in ^N_M measurement(s)', STATUS)


      ELSE

*  get the exposure, integration, measurement numbers at which the
*  abort occurred

         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'INT_NO', LAST_INT, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'MEAS_NO', LAST_MEAS, STATUS)

         CALL MSG_OUTIF (MSG__NORM, ' ',
     :        '^PKG: the observation should have '//
     :        'contained data for ^N_I '//
     :        'integration(s) in ^N_M measurement(s)', STATUS)
         CALL MSG_SETI ('N_I', LAST_INT)
         CALL MSG_SETI ('N_M', LAST_MEAS)
         CALL MSG_OUTIF (MSG__NORM, ' ',
     :        ' - However, the observation was '//
     :        'ABORTED during integration ^N_I '//
     :        'of measurement ^N_M', STATUS)


      END IF

      IF (RASTER) THEN
         CALL MSG_SETC ('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ',
     :        '^PKG: Skydip was taken in RASTER mode', STATUS)
      END IF

*     Check that we haven't exceeded array bounds
*     For RASTER, N_POS should not exceed MAX_FIT_DATA

      IF (STATUS .EQ. SAI__OK) THEN

         IF (RASTER) THEN
            IF (N_POS .GT. MAX_FIT_DATA) THEN
               CALL MSG_SETI('MAX',MAX_FIT_DATA)
               CALL MSG_SETI('ACT', N_POS)
               CALL MSG_SETC('TSK',TSKNAME)
               STATUS = SAI__ERROR
               CALL ERR_REP(' ', '^TSK: Too many data points. ^ACT'//
     :              ' exceeds maximum (^MAX)', STATUS)
            END IF
         ELSE
            IF (N_MEASUREMENTS .GT. SCUBA__MAX_MEAS) THEN
               CALL MSG_SETI('MEAS', SCUBA__MAX_MEAS)
               CALL MSG_SETI('ACT', N_MEASUREMENTS)
               CALL MSG_SETC('TSK',TSKNAME)
               STATUS = SAI__ERROR
               CALL ERR_REP(' ', '^TSK: Too many data points. ^ACT'//
     :              ' measurements exceeds maximum (^MAX)', STATUS)
            END IF
         END IF
      END IF


* Read in the elevation data from the FITS header
* Read from START and END EL first

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'START_EL',START_EL, STATUS)
         CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'END_EL', END_EL, STATUS)

* Doesnt have START_EL so look for MAX_EL
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL(STATUS)
            CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :           'MAX_EL',START_EL, STATUS)
            CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :           'MIN_EL', END_EL, STATUS)
         ENDIF
      ENDIF

*     Calculate the AIRMASS array
*     Two ways to do this - depends on RASTER or DISCRETE skydips

      IF (STATUS .EQ. SAI__OK) THEN

         IF (RASTER) THEN

*     For raster skydips the observation is split into N_MEASUREMENT
*     equal chunks in ELEVATION. Each integration is taken at an
*     elevation related to the start elevation and the sample
*     size until the number of points are processed. The next measurement
*     starts at the correct place and not from a location related to the
*     end of the first measurement. This is bacuase the telescope may
*     overrun slightly in order to record an integer number of integrations.

*     Retrieve the PIXEL size from the header
            CALL SCULIB_GET_FITS_R(SCUBA__MAX_FITS, N_FITS,
     :           FITS, 'SAM_DX', SAM_DX, STATUS)

*     Convert it to radians
            SAM_DX = SAM_DX / R2AS

*     Convert start and end to radians
            START_EL = START_EL * DEG2RAD
            END_EL   = END_EL   * DEG2RAD

*     Find the elevation increment for each measurement
            ELEV_STEP = ( END_EL - START_EL ) / N_MEASUREMENTS
            FSIGN = 1.0
            IF (ELEV_STEP .LT. 0.0) FSIGN = -1.0

*     Loop over each measurement calculating the correct
*     positions

            DO MEASUREMENT = 1, N_MEASUREMENTS

*     Calculate start elevation
               EL0 = START_EL + (MEASUREMENT - 1) * ELEV_STEP

*     Set offset into data array
               OFFSET = (MEASUREMENT - 1) * N_INTEGRATIONS

*     Loop over each integration
               DO I = 1, N_INTEGRATIONS

*     Calculate the elevation of this integration
*     Assume the middle of the sample is the relevant elevation
*     for the read out
                  EL = EL0 + ( (REAL(I-1)+0.5) * SAM_DX * FSIGN )

*     Convert elevation to airmass
                  ZD = (PI/2.0D0) - DBLE(EL)
                  CALL SCULIB_AIRMASS (ZD, DTEMP, STATUS)

*     For information
                  CALL MSG_SETI('M', MEASUREMENT)
                  CALL MSG_SETI('INT', I)
                  CALL MSG_SETR('EL',EL/DEG2RAD)
                  CALL MSG_SETR('AM',REAL(DTEMP))
                  CALL MSG_SETI('N', OFFSET+I)
                  CALL MSG_OUTIF(MSG__VERB, ' ',
     :                 ' Meas ^M, Int ^INT, Offset ^N El: '//
     :                 '^EL deg Airmass: ^AM',
     :                 STATUS)


*     Store it in the airmass array
                  IF (STATUS .EQ. SAI__OK) THEN
                     DAIRMASST(OFFSET+I) = DTEMP
                     DAIRMASS_TVAR(OFFSET+I) = 0.0D0
                  END IF

               END DO

            END DO

         ELSE

*     Assume constant space between airmasses

*     Start airmass
            ZD = (PI/2.0) - DBLE( START_EL * DEG2RAD )
            CALL SCULIB_AIRMASS (ZD, AIRMASS_START, STATUS)

*     End airmass
            ZD = (PI/2.0) - DBLE( END_EL * DEG2RAD )
            CALL SCULIB_AIRMASS (ZD, AIRMASS_END, STATUS)

            AIRSTEP = REAL(AIRMASS_END - AIRMASS_START) /
     :           REAL(N_MEASUREMENTS - 1)
            DO I = 1, N_MEASUREMENTS
               AIRMASST(I) = REAL(AIRMASS_START) + AIRSTEP * REAL(I-1)

*  Calculate the error Delta A = (TAN / SIN) Delta EL
*     TAN = 1/SQRT((AIR-1)(AIR+1))
               AIRMASS_TVAR(I) = 2.0 * ARCSEC * AIRMASST(I) / ! 2 arcsec error
     :              SQRT((AIRMASST(I)-1) * (AIRMASST(I)+1))
            END DO
         END IF
      END IF


*     Now I need to convert he input data array into an array
*     of data - one average temperature (var + qual) for each
*     measurement

*     If the data has been through REDUCE_SWITCH I simply have to
*     average the data from each integration (DISCRETE) or
*     simply copy the data (RASTER)

      IF (RESW) THEN

*     For RASTER skydips we simply copy the data for this
*     bolometer to the output array (removing bad pixels)
*     For DISCRETE skydips we have to coadd the integations

         IF (RASTER) THEN

*     A little bit of repetition here....
*     Get some memory and extract the selected bolometer

            DUM_DATA_PTR = 0
            DUM_DATA_PTR_END = 0
            DUM_QUAL_PTR = 0
            DUM_QUAL_PTR_END = 0

            CALL SCULIB_MALLOC(N_POS * VAL__NBR, DUM_DATA_PTR,
     :           DUM_DATA_PTR_END, STATUS)
            CALL SCULIB_MALLOC(N_POS * VAL__NBUB, DUM_QUAL_PTR,
     :           DUM_QUAL_PTR_END, STATUS)

            CALL SCULIB_EXTRACT_BOL(SUB_POINTER, N_BOLS, N_POS,
     :           %VAL(CNF_PVAL(IN_DATA_PTR)),
     :           %VAL(CNF_PVAL(DUM_QUAL_PTR)),
     :           %VAL(CNF_PVAL(DUM_DATA_PTR)),
     :           %VAL(CNF_PVAL(DUM_QUAL_PTR)), STATUS)

*     Get dummy variance memory and fill with zeroes
            DUM_VAR_PTR = 0
            DUM_VAR_PTR_END = 0
            CALL SCULIB_MALLOC(N_POS * VAL__NBR, DUM_VAR_PTR,
     :           DUM_VAR_PTR_END, STATUS)
            IF (STATUS .EQ. SAI__OK) THEN
               CALL SCULIB_CFILLR(N_POS, 0.0,
     :                            %VAL(CNF_PVAL(DUM_VAR_PTR)))
            END IF

*     Note that AIRMASST and AIRMASS_TVAR need to be copied
*     from DOUBLE PRECISION ARRAYS  - this is a kluge since
*     I am too lazy to wirte a version of SCULIB_COPY_GOOD
*     that can go from real to real rather than DBLE to REAL

*     Copy from input array to ouput array, removing bad pixels
*     assumes the data were mapped with automatic quality masking
            CALL SCULIB_COPY_GOOD(N_POS, %VAL(CNF_PVAL(DUM_DATA_PTR)),
     :           %VAL(CNF_PVAL(DUM_VAR_PTR)), DAIRMASST, DAIRMASS_TVAR,
     :           NKEPT, JSKY, JSKY_VAR, AIRMASS, AIRMASS_VAR,
     :           STATUS)

*     Free the variance memory
            CALL SCULIB_FREE('DumVarScratch', DUM_VAR_PTR,
     :           DUM_VAR_PTR_END, STATUS)

         ELSE

*     This is for reducing discrete skydips
*     Need to average the integrations to derive the value for
*     a single measurement.

*     First we need to extract the one bolometer out of the input data
*     we'll make use of the dummy variables used to calculate the
*     skydips. So we need some memory

            DUM_DATA_PTR = 0
            DUM_DATA_PTR_END = 0
            DUM_QUAL_PTR = 0
            DUM_QUAL_PTR_END = 0

            CALL SCULIB_MALLOC(N_POS * VAL__NBR, DUM_DATA_PTR,
     :           DUM_DATA_PTR_END, STATUS)
            CALL SCULIB_MALLOC(N_POS * VAL__NBUB, DUM_QUAL_PTR,
     :           DUM_QUAL_PTR_END, STATUS)


            CALL SCULIB_EXTRACT_BOL(SUB_POINTER, N_BOLS, N_POS,
     :           %VAL(CNF_PVAL(IN_DATA_PTR)),
     :           %VAL(CNF_PVAL(IN_QUAL_PTR)),
     :           %VAL(CNF_PVAL(DUM_DATA_PTR)),
     :           %VAL(CNF_PVAL(DUM_QUAL_PTR)), STATUS)

*     Setup a counter before the loop so that we can remove
*     bad data

            NKEPT = 0


*     We need to loop over each measurement and
*     find the start and end of each measurement

            DO I = 1, N_MEASUREMENTS

*  find where the exposure starts and finishes in the data array

               CALL SCULIB_FIND_SWITCH (%VAL(CNF_PVAL(IN_DEM_PNTR_PTR)),
     :              1, 1, N_INTEGRATIONS, N_MEASUREMENTS,
     :              N_POS, 1, 1, 1, I,
     :              EXP_START, EXP_END, STATUS)

*     I already know that this data has been processed such
*     that there is one data point per integration so I am
*     only interested in EXP_START.

*     Now calculate the mean of the data

               CALL SCULIB_STATR(N_INTEGRATIONS, -1.0,
     :   %VAL(CNF_PVAL(DUM_DATA_PTR) + (EXP_START - 1) * VAL__NBR),
     :   %VAL(CNF_PVAL(DUM_QUAL_PTR) + (EXP_START - 1) * VAL__NBUB),
     :              IN_BADBIT, NGOOD, MEAN, MEDIAN, SUM, SUMSQ,
     :              STDEV, QSORT, STATUS)

*     Copy data to the measurement array
*     Note that STATR returns bad for MEAN if neceesary.
*     Note that SCULIB_FIT_SKYDIP does not have quality and does not
*     know about VAL__BADR so we have to remove the points ourself
*     Check for bad value and division by zero
*     Dont forget to copy the airmass as well

               IF (MEAN .NE. VAL__BADD .AND. NGOOD .GT. 0) THEN

                  NKEPT = NKEPT + 1

                  JSKY(NKEPT) = REAL(MEAN)
                  JSKY_VAR(NKEPT) = REAL(STDEV * STDEV) / NGOOD
                  AIRMASS(NKEPT) = AIRMASST(I)
                  AIRMASS_VAR(NKEPT) = AIRMASS_TVAR(I)

               END IF

            END DO

         END IF

*     Tidy up

         CALL SCULIB_FREE ('SCRATCHd' , DUM_DATA_PTR, DUM_DATA_PTR_END,
     :        STATUS)
         CALL SCULIB_FREE ('SCRATCHq' , DUM_QUAL_PTR,
     :        DUM_QUAL_PTR_END, STATUS)



      ELSE

*     If this data has not been through reduce_switch then we
*     need to convert the raw data to temperatures. This is done
*     in the same way as for REDUCE_SWITCH except that we are only
*     interested in one sub instrument. Note that we process all the
*     data but the overhead is not very large and computers are
*     very fast :-)
*     Trying to select a sub-instrument will just make the subroutine
*     more complicated than it needs to be.


*     Allocate some memory for slice and SKYDIP TEMPERATURES
         SLICE_PTR = 0
         SLICE_PTR_END = 0

         CALL SCULIB_MALLOC (SCUBA__N_TEMPS * N_BOLS * N_INTEGRATIONS *
     :        VAL__NBR, SLICE_PTR, SLICE_PTR_END, STATUS)

*     Allocate some dummy memory for all those temperatures that
*     have not been averaged yet. These are going to be the temperatures
*     before averaging and the modified DEM_PNTR array that are necessary
*     for REDUCE_SWITCH (which shares SCULIB_CALC_SKYDIP_TEMPS).

         DUM_DATA_PTR = 0
         DUM_DATA_PTR_END = 0
         DUM_VAR_PTR = 0
         DUM_VAR_PTR_END = 0
         DUM_QUAL_PTR = 0
         DUM_QUAL_PTR_END = 0
         DUMMY_PTR = 0
         DUMMY_PTR_END = 0

         CALL SCULIB_MALLOC(N_BOLS * N_POS * VAL__NBR, DUM_DATA_PTR,
     :        DUM_DATA_PTR_END, STATUS)
         CALL SCULIB_MALLOC(N_BOLS * N_POS * VAL__NBR, DUM_VAR_PTR,
     :        DUM_VAR_PTR_END, STATUS)
         CALL SCULIB_MALLOC(N_BOLS * N_POS * VAL__NBUB, DUM_QUAL_PTR,
     :        DUM_QUAL_PTR_END, STATUS)
         CALL SCULIB_MALLOC(N_INTEGRATIONS * N_MEASUREMENTS * VAL__NBR,
     :        DUMMY_PTR, DUMMY_PTR_END, STATUS)


*     Now calculate the skydip temperatures

         CALL SCULIB_CALC_SKYDIP_TEMPS(SCUBA__N_TEMPS, SUB_POINTER,
     :        N_FITS, FITS,
     :        'T_COLD', N_INTEGRATIONS, N_MEASUREMENTS, N_POS,
     :        N_BOLS, SCUBA__MAX_SUB, SCUBA__NUM_CHAN, SCUBA__NUM_ADC,
     :        BOL_CHAN, BOL_TYPE, BOL_ADC,
     :        %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)),
     :        %VAL(CNF_PVAL(IN_DATA_PTR)), %VAL(CNF_PVAL(DUM_DATA_PTR)),
     :        %VAL(CNF_PVAL(DUM_VAR_PTR)), %VAL(CNF_PVAL(DUM_QUAL_PTR)),
     :        %VAL(CNF_PVAL(DUMMY_PTR)), AV_DATA, AV_VAR, AV_QUAL,
     :        %VAL(CNF_PVAL(SLICE_PTR)), STATUS)

*     Now extract the necesary data from the average data
*     Keep track of how many bad points we have removed
*     Dont forget to copy over the AIRMASS data as well

*     For DISCRETE skydips we are interested in the average
*     data values returned by CALC_SKYDIP_TEMPS.
*     For RASTER skydips we are actually interested in all
*     the data values (stored in the DUM_DATA_PTR arrays)

         NKEPT = 0
         IN_BADBIT = VAL__BADUB   ! Any value is bad

         IF (RASTER) THEN

*     RASTER mode we have to extract the bolometer of choice
*     as for the RESW data. Note that we extract into the same
*     memory.

*     ...data and quality
            CALL SCULIB_EXTRACT_BOL(SUB_POINTER, N_BOLS, N_POS,
     :           %VAL(CNF_PVAL(DUM_DATA_PTR)),
     :           %VAL(CNF_PVAL(DUM_QUAL_PTR)),
     :           %VAL(CNF_PVAL(DUM_DATA_PTR)),
     :           %VAL(CNF_PVAL(DUM_QUAL_PTR)), STATUS)

*     ....variance
            CALL SCULIB_EXTRACT_2DIM_R(SUB_POINTER, N_BOLS,
     :           N_POS, %VAL(CNF_PVAL(DUM_VAR_PTR)),
     :           %VAL(CNF_PVAL(DUM_VAR_PTR)),
     :           STATUS)

*     Now copy the good data (ignoring quality for now)
            CALL SCULIB_COPY_GOOD(N_POS, %VAL(CNF_PVAL(DUM_DATA_PTR)),
     :           %VAL(CNF_PVAL(DUM_VAR_PTR)), DAIRMASST, DAIRMASS_TVAR,
     :           NKEPT, JSKY, JSKY_VAR, AIRMASS, AIRMASS_VAR,
     :           STATUS)

         ELSE
            DO I = 1, N_MEASUREMENTS

               IF ((AV_DATA(SUB_POINTER, I) .NE. VAL__BADR).AND.
     :              (NDF_QMASK(AV_QUAL(SUB_POINTER,I), IN_BADBIT))) THEN

                  NKEPT = NKEPT + 1

                  JSKY(NKEPT) = AV_DATA(SUB_POINTER, I)
                  JSKY_VAR(NKEPT) = AV_VAR(SUB_POINTER, I)
                  AIRMASS(NKEPT) = AIRMASST(I)
                  AIRMASS_VAR(NKEPT) = AIRMASS_TVAR(I)

               END IF

            END DO

         END IF

*     Have now finished with input data

         CALL SCULIB_FREE ('SLICE' , SLICE_PTR, SLICE_PTR_END, STATUS)
         CALL SCULIB_FREE ('SCRATCHd' , DUM_DATA_PTR, DUM_DATA_PTR_END,
     :        STATUS)
         CALL SCULIB_FREE ('SCRATCHv' , DUM_VAR_PTR,DUM_VAR_PTR_END,
     :        STATUS)
         CALL SCULIB_FREE ('SCRATCHq' , DUM_QUAL_PTR,
     :        DUM_QUAL_PTR_END, STATUS)
         CALL SCULIB_FREE ('SCRATCHdp' , DUMMY_PTR, DUMMY_PTR_END,
     :        STATUS)

      END IF


*     Have now finished with input data

      CALL CMP_UNMAP(IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)
      CALL DAT_ANNUL(IN_SCUBAX_LOC, STATUS)
      CALL NDF_ANNUL (IN_NDF, STATUS)

*     If NKEPT is zero then we may as well set status to bad since
*     we arent going to make any progress

      IF (NKEPT .EQ. 0 .AND. STATUS .EQ. SAI__OK) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC('PKG',PACKAGE)
         CALL ERR_REP(' ','^PKG: All data are bad. No fit possible',
     :        STATUS)
      END IF

*     Get the temperatures from the FITS extension

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'T_AMB',
     :  T_AMB, STATUS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'T_TEL',
     :  T_TEL, STATUS)

*     As of 19970728 Skydip data contains the suggested ETATEL values
*     in the FITS header. We should provide these as the default if it is
*     there. Have to make sure that we have good status before proceeding.

      IF (STATUS .EQ. SAI__OK) THEN
         STEMP = 'ETATEL_'
         ITEMP = 7
         CALL CHR_PUTI(SUB_POINTER, STEMP, ITEMP)
         CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, STEMP,
     :        DEFAULT_ETA_TEL, STATUS)

*     Check to see that the entry was found okay.
*     If so we will set the dynamic default
*     Else we do nothing and allow the default to be used from the IFL file
         IF (STATUS .EQ. SAI__OK) THEN

*     As of 26 April 2000 we think we have a better handle of what the
*     default eta_tel should be for 450 and 850 microns (independent
*     of the filter profile). In that case, we suggest a default that
*     is different from the value stored in the header.
*     Need to get MJD of observation (April 26 2000 is MJD 51660.0)
            CALL SCULIB_GET_MJD(N_FITS, FITS, -1.0D0, MJD, EPOCH,
     :           RTEMP, STATUS)

            IF (MJD .LT. 51660.0D0) THEN

               MODIFIED = .FALSE.
               IF (FILT .EQ. '850' .AND.
     :              DEFAULT_ETA_TEL .NE. 0.85) THEN
                  DEFAULT_ETA_TEL = 0.85
                  MODIFIED = .TRUE.
               ELSE IF (FILT .EQ. '450' .AND.
     :                 DEFAULT_ETA_TEL .NE. 0.75) THEN
                  DEFAULT_ETA_TEL = 0.75
                  MODIFIED = .TRUE.
               END IF
*     Let people know what is happening
               IF (MODIFIED) THEN
                  CALL MSG_SETC ('PKG', PACKAGE)
                  CALL MSG_OUTIF(MSG__NORM, ' ',
     :               '^PKG: Overriding default ETA_TEL found in header',
     :                 STATUS)
               END IF
            END IF

            CALL PAR_DEF0R('ETA_TEL', DEFAULT_ETA_TEL, STATUS)
         ELSE
            CALL ERR_ANNUL(STATUS)
         END IF

      END IF

*     Get the fit parameters

      CALL PAR_GET0R ('ETA_TEL', ETA_TEL, STATUS )
      CALL PAR_GET0R ('B_VAL', B, STATUS )

*     Ask whether we are using a fixed variance or the actual variance.

      CALL PAR_GET0L ('CVAR', CVAR, STATUS)

*     The number of measurements is now actually the number of points
*     that were kept after removing bad data


*     Send to fit skydip

      FITFAIL = .TRUE.
      B_FIT = VAL__BADR
      TAUZ_FIT = VAL__BADR
      REXISQ = VAL__BADR
      ETA_TEL_FIT = VAL__BADR
      B_ERROR = VAL__BADR
      TAU_ERROR = VAL__BADR
      ETA_ERROR = VAL__BADR
      SIGMA = VAL__BADD
      RESIDUAL = VAL__BADD

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_FIT_SKYDIP (CVAR, NKEPT, AIRMASS, JSKY,JSKY_VAR,
     :        WAVE, SUB_INST, FILT, T_TEL, T_AMB, ETA_TEL,B,ETA_TEL_FIT,
     :        B_FIT, TAUZ_FIT, REXISQ, TAU_ERROR, ETA_ERROR, B_ERROR,
     :        RESIDUAL, SIGMA, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            FITFAIL = .FALSE.
         ELSE
            CALL ERR_FLUSH(STATUS)
            FITFAIL = .TRUE.
         ENDIF
      END IF

*     Store the fit parameters even if status was bad before the fit
*     (In which case the parameters are bad)
*     Want to make sure that the GOODFIT parameter is set to false if
*     we have had any problems at all. Just make sure it is written
*     with a good status

*     Check parameters for non-physical ranges
      IF (TAUZ_FIT .LT. 0.0) FITFAIL = .TRUE.
      IF (ETA_TEL_FIT .LT. 0.0) FITFAIL = .TRUE.
      IF (B_FIT .LT. 0.0) FITFAIL = .TRUE.

      GOOD = SAI__OK
      CALL PAR_PUT0L('GOODFIT', .NOT.FITFAIL, GOOD)
      CALL PAR_PUT0R('TAUZ_FIT', TAUZ_FIT, GOOD)
      CALL PAR_PUT0R('TAUZ_ERR', TAU_ERROR, GOOD)
      CALL PAR_PUT0R('B_FIT', B_FIT, GOOD)
      CALL PAR_PUT0R('B_ERR', B_ERROR, GOOD)
      CALL PAR_PUT0R('ETA_TEL_FIT', ETA_TEL_FIT, GOOD)
      CALL PAR_PUT0R('ETA_TEL_ERR', ETA_ERROR, GOOD)
      CALL PAR_PUT0R('XISQ', REXISQ, GOOD)
      CALL PAR_PUT0D('SIGMA', SIGMA, GOOD)
      CALL PAR_PUT0D('RESIDUAL', RESIDUAL, GOOD)

*     Retrieve the Cold temperature from the FITS header

      STEMP = 'T_COLD_'
      ITEMP = 7
      CALL CHR_PUTI(SUB_POINTER, STEMP, ITEMP)
      CALL SCULIB_GET_FITS_R (N_FITS, N_FITS, FITS, STEMP,
     :     T_COLD, STATUS)

*     Now create output files

*     The data we fitted
      CALL SCULIB_WRITE_SKYDIP_DATAMODEL( 'OUT', N_FITS, FITS,
     :     'Jsky', 'Skydip', 'K', 'Airmass', NKEPT,
     :     JSKY, .TRUE., JSKY_VAR, AIRMASS, .TRUE., AIRMASS_VAR,
     :     SUB_INST, FILT, WAVE, T_COLD,
     :     VAL__BADR, VAL__BADR, VAL__BADR, STATUS )

*     Only create model if we fitted okay
      IF ( .NOT. FITFAIL ) THEN

         AIRSTEP = (AIRMASS(NKEPT) - AIRMASS(1)) /
     :        (N_MODEL - 1)

         DO I = 1, N_MODEL
            AIR_MODEL(I) = AIRMASS(1) + AIRSTEP * (I - 1)
            CALL SCULIB_J_THEORETICAL (TAUZ_FIT, AIR_MODEL(I), T_TEL,
     :           T_AMB, WAVE, ETA_TEL_FIT, B_FIT, J_THEORETICAL(I),
     :           STATUS)
         END DO

         CALL SCULIB_WRITE_SKYDIP_DATAMODEL( 'MODEL_OUT', N_FITS, FITS,
     :        'Jsky', 'Skydip (model)', 'K', 'Airmass', N_MODEL,
     :        J_THEORETICAL, .FALSE., J_THEORETICAL,
     :        AIR_MODEL, .FALSE., AIR_MODEL,
     :        SUB_INST, FILT, WAVE, T_COLD,
     :        ETA_TEL_FIT, B_FIT, TAUZ_FIT, STATUS )

      END IF

* Shut down the NDF system

      CALL NDF_END (STATUS)

      END
