      SUBROUTINE SURF_SKYDIP2 (STATUS)
*+
*  Name:
*     SKYDIP2

*  Purpose:
*     calculate sky properties given generic skydip data

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SURF_SKYDIP2( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*     This application takes sky brightness temperatures (K) with associated
*     airmass values and calculated tau, eta_l and B by fitting to a model
*     of the sky.
*
*     This application will not take the output of REDUCE_SWITCH nor will
*     it accept raw SCUBA data. Use the SKYDIP application for these.

*  Usage:
*     skydip in eta_tel b_val out model_out

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
*        The telescope efficiency (also called eta_l). If available the
*        current telescope value from the ETAL header is used as the default.
*        Values must be between 0 and 1.0. A negative value allows this
*        parameter to be free.
*     ETA_TEL_FIT = REAL (Write)
*        The fitted value of ETA_TEL.
*     GOODFIT = LOGICAL (Write)
*        Flag to indicate whether the fit was good (TRUE) or bad (FALSE).
*     OUT = CHAR (Write)
*        The name of the output file that contains the fitted sky
*        temperatures. Can be null (!) if no output data are required.
*     RESIDUAL = DOUBLE (Write)
*        Absolute difference between the model and the data in Kelvin.
*        i.e. Sum ( Abs(Data - model) )
*     SIGMA = DOUBLE (Write)
*        Standard deviation of the difference between the fit
*        and the input data.
*     TAUZ_ERR = REAL (Write)
*        The error on the fitted value of TAUZ
*     TAUZ_FIT = REAL (Write)
*        The fitted sky opacity for the selected sub instrument.
*     WAVELENGTH = REAL (Write)
*        The wavelength of the fitted data.
*     XISQ = REAL (Write)
*        The reduced chi square of the fit.

*  Examples:
*     skydip indata \
*        Process the skydip data using the default value
*        for T_COLD and allowing ETA_TEL and B to be free parameters.
*        No output files are written.
*     skydip indata eta_tel=0.9 out=sky model_out=model b_val=-1
*        Process the skydip data with ETA_TEL fixed at 0.9
*        and B free. Write the sky temperature to sky.sdf and the fitted
*        model to model.sdf.

*  Notes:
*     The input data must be a 1-D NDF with AXIS values of airmass and
*     data values the sky brightness in K. A suitable output file is written
*     via the OUT parameter of the standard SKYDIP command.

*  Related Applications:
*     SURF: SKYDIP

*  Authors:
*     TIMJ: T. Jenness (t.jenness@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     Copyright (C) 1995-2006 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     2011-08-17 (TIMJ):
*        Initial version copied from surf_skydip.F
*     {enter_further_changes_here}

*  Bugs:
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
      INCLUDE 'CNF_PAR'                 ! For CNF_PVAL
      INCLUDE 'GRP_PAR'                 ! GRP__ constants
      INCLUDE 'AST_PAR'                 ! AST functions

*  Arguments Given:

*  Arguments Given & Returned:

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN

*  Global variables:

*  Local Constants:
      INTEGER MAXDIM
      PARAMETER (MAXDIM = 4)
      INTEGER N_MODEL                   ! Number of points in output model
      PARAMETER (N_MODEL = 100)
      CHARACTER * 8 TSKNAME             ! Name of task
      PARAMETER (TSKNAME = 'SKYDIP2')
      INTEGER MAX_FIT_DATA              ! Max number of points
      PARAMETER (MAX_FIT_DATA = 2048)   ! allowed in input data
      REAL DEFVAR                       ! Default variance if no variance
      PARAMETER (DEFVAR = 1.0)

*    Local variables:
      REAL    AIR_MODEL(N_MODEL)        ! Airmass values for MODEL
      REAL    AIRMASS(MAX_FIT_DATA)     ! Array of AIRMASS data
      REAL    AIRSTEP                   ! AIRMASS increment for DO loop
      REAL    B                         ! requested B
      REAL    B_ERROR                   ! Error in B
      LOGICAL BADPIX                    ! are there bad pixels
      REAL    B_FIT                     ! B parameter
      LOGICAL CVAR                      ! Use constant variance?
      INTEGER DIM (MAXDIM)              ! the dimensions of an array
      DOUBLE PRECISION DEFAULT_ETA_TEL           ! Eta tel read from FITS header
      INTEGER DUM_VAR_PTR_END           ! Scratch space end for temp VAR
      LOGICAL DUMMY
      CHARACTER *(15) ETALKEY           ! Key for ETATEL FITS header
      REAL    ETA_ERROR                 ! Error in ETA_TEL
      REAL    ETA_TEL                   ! Telescope efficiency
      REAL    ETA_TEL_FIT               ! Fitted eta_tel
      INTEGER FCHAN                     ! AST FitsChan
      CHARACTER*15 FILT                 ! Selected filter
      CHARACTER *(15) FILTKEY           ! Key for FILTER FITS header
      LOGICAL FITFAIL                   ! Status of the Model fit
      CHARACTER*80 FITS (SCUBA__MAX_FITS)
                                        ! array of FITS keyword lines
      CHARACTER*(DAT__SZLOC) FITS_LOC   ! HDS locator to FITS structure
      INTEGER GOOD                      ! dummy status for PAR_PUT/GOODFIT
      LOGICAL HASAX                     ! Do we have AXIS?
      LOGICAL HASVAR                    ! Do we have VARIANCE?
      INTEGER I                         ! DO loop index
      INTEGER IERR                      ! For VEC_
      INTEGER IGRP                      ! Input file group
      CHARACTER *(15) INSTRUME          ! Instrument name
      INTEGER ITEMP                     ! scratch integer
      INTEGER IN_AX_PTR                 ! Pointer to AXIS array
      INTEGER IN_DATA_PTR               ! pointer to data array of input file
      INTEGER IN_NDF                    ! NDF identifier of input file
      INTEGER IN_VAR_PTR                ! Input variance pointer
      REAL    JSKY (MAX_FIT_DATA)    ! Average SKY data for used SUB-INS
      REAL    JSKY_VAR (MAX_FIT_DATA)! Variance of JSKY
      REAL    J_THEORETICAL (N_MODEL)   ! Array of model sky data
      CHARACTER * 15 LABEL              ! File label
      INTEGER LBND (MAXDIM)             ! lower bounds of array
      CHARACTER*(DAT__SZLOC) LOC1       ! Dummy locator
      INTEGER NDIM                      ! the number of dimensions in an array
      INTEGER NERR                      ! For VEC_
      INTEGER NKEPT                     ! Number of good measurements
      INTEGER NPTS                      ! Number of output data points
      INTEGER N_FITS                    ! number of FITS lines read from file
      INTEGER N_POS                     ! the total number of positions measured
      INTEGER OUT_AXIS_PTR              ! pointer to axis of observed output file
      INTEGER OUT_DATA_PTR              ! pointer to data array of output file
      CHARACTER *80 OUT_NAME            ! Name of output NDF
      INTEGER OUT_NDF                   ! NDF identifier of output file
      INTEGER PLACE                     ! A placeholder
      DOUBLE PRECISION RESIDUAL         ! Residual of fit
      REAL    REXISQ                    ! Reduced chi square
      DOUBLE PRECISION SIGMA            ! Sigma of difference between model/dat
      INTEGER SIZE                      ! Size of input group
      CHARACTER * 15 SUBNAME            ! Name of selected SUB instrument
      CHARACTER *(15) SUBNAMEKEY        ! Key for sub-instrument FITS header
      CHARACTER *(15) TAMBKEY           ! Key for T_AMB FITS header
      REAL    TAU_ERROR                 ! Error in tau
      REAL    TAUZ_FIT                  ! Fitted TAU
      CHARACTER * 15 TITLE              ! File title
      REAL TOFFSET                      ! Offset to add on to temperature headers
      CHARACTER *(15) TTELKEY           ! Key for T_TEL FITS header
      DOUBLE PRECISION T_AMB            ! Temperature of ambient load
      DOUBLE PRECISION   T_TEL          ! Temperature of telescope
      INTEGER UBND (MAXDIM)             ! upper bounds of array
      DOUBLE PRECISION    WAVE          ! Selected wavelength
      CHARACTER *(15) WAVEKEY           ! Key for wavelength FITS header
      DOUBLE PRECISION WAVE2MICRONS     ! How to convert WAVE header to microns

*     External functions:
      INCLUDE 'NDF_FUNC'

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Initialisations for compiler
      IGRP = GRP__NOID
      SIZE = 0

*     Start up the NDF system and read in some data

      CALL NDF_BEGIN

      CALL KPG1_RGNDF( 'IN', 1, 1,'More', IGRP, SIZE, STATUS )

      CALL NDG_NDFAS( IGRP, 1, 'READ', IN_NDF, STATUS )

*     get some general descriptive parameters of the observation

      CALL NDF_XLOC (IN_NDF, 'FITS', 'READ', FITS_LOC, STATUS)
      CALL DAT_SIZE (FITS_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK',TSKNAME)
            CALL ERR_REP (' ', '^TASK: input file '//
     :           'contains too many FITS items', STATUS)
         END IF
      END IF

      CALL DAT_GET1C (FITS_LOC, SCUBA__MAX_FITS, FITS, N_FITS, STATUS)
      FCHAN = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )
      DO I = 1, N_FITS
         CALL AST_PUTFITS( FCHAN, FITS(I), .FALSE.,  STATUS )
      END DO
      CALL DAT_ANNUL( FITS_LOC, STATUS )

      INSTRUME = ' '
      DUMMY = AST_GETFITSS( FCHAN, 'INSTRUME', INSTRUME, STATUS )

      IF (INSTRUME .EQ. 'SCUBA') THEN
         SUBNAMEKEY = 'SUB_1'
         WAVEKEY    = 'WAVE_1'
         ETALKEY    = 'ETATEL_1'
         FILTKEY    = 'FILT_1'
         TAMBKEY    = 'T_AMB'
         TTELKEY    = 'T_TEL'
         WAVE2MICRONS = 1.0D0   ! Convert WAVE to microns
         TOFFSET    = 0.0       ! Convert header to kelvin
      ELSE
         SUBNAMEKEY = 'SUBARRAY'
         WAVEKEY    = 'WAVELEN'
         FILTKEY    = 'FILTER'
         ETALKEY    = 'ETA_L'
C     Do not average the start/end value yet
C     This could be done in the routine that forms the input file
         TAMBKEY    = 'ATSTART'
         TTELKEY    = 'FRLEGST'
         WAVE2MICRONS = 1.0D6   ! Convert WAVE in m to microns
         TOFFSET    = 273.15    ! Convert header to kelvin
      END IF

      SUBNAME = ' '
      DUMMY = AST_GETFITSS( FCHAN, SUBNAMEKEY, SUBNAME, STATUS )
      FILT = ' '
      DUMMY = AST_GETFITSS( FCHAN, FILTKEY, FILT, STATUS )

      CALL MSG_SETC( 'S', SUBNAME )
      CALL MSG_SETC( 'I', INSTRUME )
      CALL MSG_SETC( 'F', FILT )
      CALL MSG_OUT( ' ', 'Processing skydip data from instrument ^I'/
     :     /' subsystem ^S, filter ^F', STATUS )

C     Get the wavelength from the FITS header. Needed in microns
      WAVE = VAL__BADD
      IF (AST_GETFITSF( FCHAN, WAVEKEY, WAVE, STATUS )) THEN
         WAVE = WAVE * WAVE2MICRONS
      END IF

*     Store the wavelength
      CALL PAR_PUT0D('WAVELENGTH', WAVE, STATUS)

*     Check the dimensions of the input data array

      CALL NDF_DIM (IN_NDF, MAXDIM, DIM, NDIM, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (NDIM .NE. 1) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME )
            CALL MSG_SETI( 'NDIM', NDIM )
            CALL ERR_REP( ' ', '^TASK: main data '//
     :           ' array should be 1-D not ^NDIM-D',
     :           STATUS )
         END IF
      END IF
      N_POS = DIM(1)

*     Finally map the data array

      IN_DATA_PTR = 0
      CALL NDF_MAP (IN_NDF, 'DATA', '_REAL', 'READ', IN_DATA_PTR,
     :     ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (ITEMP .NE. N_POS) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Internal error since we mapped '//
     :           'a different number of data points than expected',
     :           STATUS )
         END IF
      END IF

*     Map variance if we have it
      CALL NDF_STATE( IN_NDF, 'VARIANCE', HASVAR, STATUS )

      IN_VAR_PTR = 0
      DUM_VAR_PTR_END = 0
      IF (HASVAR) THEN
         CALL NDF_MAP( IN_NDF, 'VARIANCE', '_REAL', 'READ',
     :        IN_VAR_PTR, ITEMP, STATUS )
      ELSE
*     Get dummy variance memory and fill with default error
         CALL SCULIB_MALLOC(N_POS * VAL__NBR, IN_VAR_PTR,
     :        DUM_VAR_PTR_END, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
            CALL SCULIB_CFILLR(N_POS, DEFVAR,
     :           %VAL(CNF_PVAL(IN_VAR_PTR)))
         END IF
      END IF

*     AXIS information
      IN_AX_PTR = 0
      CALL NDF_ASTAT( IN_NDF, 'CENTRE', 1, HASAX, STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         IF (.NOT.HASAX) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ','Airmass values are required in the AXIS'/
     :           /' component of the NDF', STATUS )
         END IF

         CALL NDF_AMAP( IN_NDF, 'CENTRE', 1, '_REAL', 'READ',
     :        IN_AX_PTR, ITEMP, STATUS )
      END IF

*     Copy to fitting arrays checking for bad values
      IF (N_POS .GT. MAX_FIT_DATA) THEN
         CALL MSG_SETI( 'NMAX', MAX_FIT_DATA )
         CALL MSG_SETI( 'N', N_POS )
         CALL MSG_OUTIF( MSG__QUIET, ' ',
     :        'Number of data points to fit (^N) exceeds array '//
     :        'dimensions. Truncating to first ^NMAX.',
     :        STATUS )
         N_POS = MAX_FIT_DATA
      END IF

      CALL SCULIB__COPY_DATA( N_POS, %VAL(CNF_PVAL(IN_DATA_PTR)),
     :     %VAL(CNF_PVAL(IN_VAR_PTR)), %VAL(CNF_PVAL(IN_AX_PTR)),
     :     NKEPT, JSKY, JSKY_VAR, AIRMASS, STATUS )

*     Have now finished with input data

      CALL NDF_ANNUL (IN_NDF, STATUS)
      IF (DUM_VAR_PTR_END .NE. 0) THEN
         CALL SCULIB_FREE( 'SCRATCHVAR', IN_VAR_PTR,
     :        DUM_VAR_PTR_END, STATUS )
      END IF

*     If NKEPT is zero then we may as well set status to bad since
*     we arent going to make any progress

      IF (NKEPT .EQ. 0 .AND. STATUS .EQ. SAI__OK) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC('PKG',PACKAGE)
         CALL ERR_REP(' ','^PKG: All data are bad. No fit possible',
     :        STATUS)
      END IF

*     Get the temperatures from the FITS extension

      DUMMY = AST_GETFITSF( FCHAN, TAMBKEY, T_AMB, STATUS )
      DUMMY = AST_GETFITSF( FCHAN, TTELKEY, T_TEL, STATUS )
      T_TEL = T_TEL + TOFFSET
      T_AMB = T_AMB + TOFFSET

*     ETA TEL from the header
      DEFAULT_ETA_TEL = -1.0D0
      DUMMY = AST_GETFITSF( FCHAN, ETALKEY, DEFAULT_ETA_TEL, STATUS )
      CALL PAR_DEF0D('ETA_TEL', DEFAULT_ETA_TEL, STATUS)

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

         CALL SCULIB_FIT_SKYDIP (CVAR, NKEPT, AIRMASS, JSKY, JSKY_VAR,
     :        REAL(WAVE), SUBNAME, FILT, REAL(T_TEL), REAL(T_AMB),
     :        ETA_TEL, B, ETA_TEL_FIT,
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

*     Only create model if we fitted okay
      IF (.NOT. FITFAIL .AND. STATUS .EQ. SAI__OK ) THEN

         AIRSTEP = (AIRMASS(NKEPT) - AIRMASS(1)) /
     :        (N_MODEL - 1)

         DO I = 1, N_MODEL
            AIR_MODEL(I) = AIRMASS(1) + AIRSTEP * (I - 1)
            CALL SCULIB_J_THEORETICAL (TAUZ_FIT, AIR_MODEL(I),
     :            REAL(T_TEL), REAL(T_AMB), REAL(WAVE),
     :           ETA_TEL_FIT, B_FIT, J_THEORETICAL(I),
     :           STATUS)
         END DO

         CALL SCULIB_WRITE_SKYDIP_DATAMODEL( 'OUT', N_FITS, FITS,
     :        'Jsky', 'Skydip (model)', 'K', 'Airmass', N_MODEL,
     :        J_THEORETICAL, .FALSE., J_THEORETICAL,
     :        AIR_MODEL, .FALSE., AIR_MODEL,
     :        SUBNAME, FILT, REAL(WAVE), VAL__BADR,
     :        ETA_TEL_FIT, B_FIT, TAUZ_FIT, STATUS )

      END IF

      CALL GRP_DELET( IGRP, STATUS )

*     Shut down the NDF system

      CALL NDF_END (STATUS)

      END

      SUBROUTINE SCULIB__COPY_DATA( N_POS, INDATA, INVAR, INAXIS,
     :     NKEPT, OUTDATA, OUTVAR, OUTAXIS, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER N_POS
      REAL INDATA(N_POS)
      REAL INVAR(N_POS)
      REAL INAXIS(N_POS)
      INTEGER NKEPT
      REAL OUTDATA(N_POS)
      REAL OUTVAR(N_POS)
      REAL OUTAXIS(N_POS)
      INTEGER STATUS

      INTEGER I

      NKEPT = 0
      IF (STATUS .NE. SAI__OK) RETURN

      DO I = 1, N_POS
         IF (INDATA(I) .NE. VAL__BADR .AND.
     :        INVAR(I) .NE. VAL__BADR .AND.
     :        INAXIS(I) .NE. VAL__BADR ) THEN
            NKEPT = NKEPT + 1
            OUTDATA(I) = INDATA(I)
            OUTVAR(I) = INVAR(I)
            OUTAXIS(I) = INAXIS(I)
         END IF
      END DO

      END
