      SUBROUTINE SURF_REMSKY (STATUS)
*+
*  Name:
*     REMSKY

*  Purpose:
*     Remove sky noise and constant offsets from SCUBA jiggle data

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL SURF_REMSKY( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description :
*     This task removes sky noise and constant offsets from SCUBA jiggle
*     data. It does this by requesting `sky' bolometers, calculating some
*     average value for each jiggle and then subtracts this off the
*     jiggle. Each jiggle is analysed in turn. The average value can be
*     calculated in two ways: either MEDIAN or MEAN.
*
*     After the calculation, the mean value removed from each jiggle
*     can be added back onto the data -- this should protect against removing
*     flux from MAP data.

*  Usage:
*     remsky in out

*  ADAM Parameters:
*     ADD = LOGICAL (Read)
*        If true the mean of the `sky' level that was removed from every 
*        frame is added back onto the data after sky removal. This step should
*        make sure that flux is not removed from the data. The default is
*        for ADD to be true for MAPs and false for other modes (the assumption
*        being that sky bolometers in PHOTOM observations are guaranteed to
*        be on sky)
*     BOLOMETERS = CHAR (Read)
*        List of sky bolometers (either by number in the data file, or
*        by id (eg H7,G3)), or by ring number (r0,r1, etc) or even
*        'all' for all bolometers. Any bolometer can be removed by
*        prefixing the id with a minus sign.
*        For example:
*            [all,-r4,-r1,h8]  would select all the bolometers then
*                              remove bolometers from rings 4 and 1
*                              and add h8.
*            [17,18,19,20]     Bolometers 17, 18, 19 and 20
*            [h6,h7,h8,h9]     Bolometers H6, H7, H8, H9 
*            [all]             Whole array 
*            [r0]              Ring zero (central pixel)
*            [r0,-19]          No bolometers (bol 19 of LONG is R0/H7)
*            [h7,r1]           inner ring and H7
*            [r1,-h8]          inner ring without H8
*            [r1,-18]          inner ring without bolometer 18
*            [all,-r1,-h7]     all pixels except the inner ring and H7
*            [all,-r3,g1]      all pixels except ring 3 but with
*                                    G1 (which happens to be in r3)
*            [all,-r1,-r2,-r3,-r4,-r5]        Selects the central pixel 
*     IN = NDF (Read)
*        This is the name of the input demodulated data file.
*     ITER_SIGMA = REAL (Read)
*        When using MEAN to calculate the average, this is the sigma clipping
*        level used. This is an iterative value - points will be removed
*        from the mean until the spread of data points is smaller than
*        this value. Supplying a negative value  will turn off clipping.
*     MODE = CHAR (Read)
*        Method to be used for calculating the average sky. There are
*        two methods available:
*        - Median - the median value for all the sky bolometers is taken
*                   from each bolomoter signal.
*        - Mean   - the mean of the sky bolometers is used as the average.
*                   This mean value is iterative - ie The mean and standard
*                   deviation are calculated, any points greater than the
*                   given distance from the mean are removed and the mean
*                   and standard deviation are calculated.  This process
*                   is repeated until no bolometers are dropped from the
*                   mean.
*     MSG_FILTER = CHAR (Read)
*        Message output level. Default is NORM. In verbose mode the
*        selected bolometers are listed and the mean value removed from
*        each frame.
*     OUT = NDF (Write)
*        Output data file

*  Examples:
*     remsky ndf sky_removed bolometers='[g1,g2,g3,g4,g5]' mode=median \
*        Use the median of bolometers g1,g2,g3,g4,g5 (not necessarily 
*        the best choice) to calculate the sky signal and write the
*        output to sky_removed.sdf.
*     remsky o12_lon_ext bolometers=[all] mode=median \
*        Use the median of all the bolometers for each jiggle and write the
*        output to the default output file (e.g. o12_lon_sky).
*     remsky o25_sho_ext bolometers=[r5] mode=mean iter_sigma=4 \
*        Use the outer ring of the short-wave array as the sky bolometers.
*        Calculate the sky contribution by using a clipped mean of each 
*        jiggle and remove any points from the
*        calculation of the mean that are more than 4 sigma from the mean.
*        Write the output to the default output file.

*  Notes:
*     - Source rotation is not accounted for so use only those bolometers
*       that always observe sky. This can be checked by using
*       SCUOVER to overlay the bolometer positions on a NAsmyth regridded
*       image (since NA shows the signal measured by each bolometer
*       throughout the observation without source rotation).
*     - For weak sources (ie sources that are not obvious in a single
*       integration) it is probably sufficient to choose BOLOMETERS=[all] and
*       MODE=median.

*  Implementation status:

*  Related Applications:
*     SURF: SCUQUICK, REBIN, SCUPHOT, SCUOVER;
 
*  Authors:
*     TIMJ: Tim Jenness (timj@jach.hawaii.edu)
*     {enter_new_authors_here}
 
*  History :
*     3 Nov 1996: TIMJ
*        Original version
*     $Log$
*     Revision 1.14  1997/11/30 01:12:15  timj
*     Change it so that ADD is true for MAP but FALSE otherwise.
*
*     Revision 1.13  1997/11/27 20:08:12  timj
*     Update documentation
*
*     Revision 1.12  1997/11/06 23:19:16  timj
*     Add the verbose suffix option.
*
*     Revision 1.11  1997/11/06 22:20:53  timj
*     Decode the bolometer strings in a subroutine.
*     Report the number and bolometer list (if verbose).
*
*     Revision 1.10  1997/11/04 23:28:10  timj
*     Remove clipping.
*
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-

*  Type Definitions :
      IMPLICIT NONE                     ! No implicit typing

*  Global constants :
      INCLUDE 'SAE_PAR'                 ! SSE global definitions
      INCLUDE 'DAT_PAR'                 ! for DAT__SZLOC
      INCLUDE 'PRM_PAR'                 ! for VAL__xxxx
      INCLUDE 'SURF_PAR'                ! REDS constants
      INCLUDE 'MSG_PAR'                 ! MSG__ constants

*  Status :
      INTEGER STATUS

*  External references :

*  Local Constants :
      INTEGER          LLEN             ! Length of output line
      PARAMETER (LLEN = 78)
      INTEGER          MAX__BOL         ! max number of bolometers
      PARAMETER (MAX__BOL = 100)        ! that can be specified
      INTEGER          MAXDIM
      PARAMETER (MAXDIM = 4)
      CHARACTER * 10   TSKNAME          ! Name of task
      PARAMETER (TSKNAME = 'REMSKY')

*  Local variables :
      LOGICAL          ADD_BACK         ! Add on the mean sky level
      BYTE             BADBIT           ! Bad bit mask
      INTEGER          BEAM             ! beam number in DO loop
      INTEGER          BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! A/D numbers of bolometers measured in
                                        ! input file
      INTEGER          BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! channel numbers of bolometers
                                        ! measured in input file
      REAL             BOL_DU3 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                        ! dU3 Nasmyth coord of bolometers
      REAL             BOL_DU4 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                        ! dU4 Nasmyth coord of bolometers
      CHARACTER*20     BOL_TYPE (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                        ! bolometer types
      CHARACTER*15     CENTRE_COORDS    ! coord system of telescope centre
      REAL             CENTRE_DU3       ! dU3 Nasmyth coord of point on focal
                                        ! plane that defines telescope axis
      REAL             CENTRE_DU4       ! dU4 Nasmyth coord of point on focal
                                        ! plane that defines telescope axis
      INTEGER          DIM (MAXDIM)     ! the dimensions of an array
      REAL             ITERCLIP         ! Number of bols to drop from mean
      LOGICAL          EXTINCTION       ! .TRUE. if EXTINCTION has been run
      CHARACTER*80     FITS (SCUBA__MAX_FITS)
                                        ! array of FITS keyword lines
      CHARACTER*132    FNAME            ! Input filename
      INTEGER          I                ! DO loop variable
      INTEGER          INDF             ! NDF identifier of input file
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC
                                        ! locator to FITS extension in input
                                        ! file
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC
                                        ! locator to SCUBA extension in input
                                        ! file
      CHARACTER*(DAT__SZLOC) IN_SCUCDX_LOC
                                        ! locator to SCUCD extension in input
                                        ! file
      INTEGER          IPOSN            ! Position in string
      INTEGER          ITEMP            ! scratch integer
      DOUBLE PRECISION LAT_RAD          ! latitude of telescope centre (radians)
      DOUBLE PRECISION LAT2_RAD         ! latitude of telescope centre at MJD2
                                        ! (radians)
      INTEGER          LBND (MAXDIM)    ! lower bounds of array
      CHARACTER*(4 * SCUBA__NUM_CHAN * SCUBA__NUM_ADC ) LINE
                                        ! Scratch string for bolometer list
      DOUBLE PRECISION LONG_RAD         ! longitude of telescope centre 
                                        ! (radians)
      DOUBLE PRECISION LONG2_RAD        ! apparent RA of telescope centre at
                                        ! MJD2 (radians)
      DOUBLE PRECISION MJD1             ! modified Julian day at which object 
                                        ! was at LAT,LONG for PLANET centre
                                        ! coordinate system
      DOUBLE PRECISION MJD2             ! modified Julian day at which object
                                        ! was at LAT2,LONG2 for PLANET centre
                                        ! coordinate system
      CHARACTER * (10) MODE             ! Method of sky removal
      INTEGER          MSG_LEV          ! Messaging level
      INTEGER          NDIM             ! the number of dimensions in an array
      INTEGER          NREC             ! number of history records in file
      INTEGER          N_BEAMS          ! number of beams for which data have
                                        ! been reduced
      INTEGER          N_BOLS           ! number of bolometers measured in
                                        ! output file
      INTEGER          N_FITS           ! number of FITS lines read from file
      INTEGER          N_GOODBOLS       ! Number of good bols in list
      INTEGER          N_POS            ! the total number of positions measured
      INTEGER          N_SKYBOLS        ! Number of skybols
      CHARACTER*30     OBJECT           ! name of object observed
      CHARACTER*15     OBSERVING_MODE   ! type of observation
      CHARACTER*132    OUTFILE          ! Default output filename
      INTEGER          OUTNDF           ! NDF identifier of output file
      INTEGER          OUT_DATA_PTR     ! pointer to data array in output file
      INTEGER          OUT_QUALITY_PTR  ! pointer to quality array in output 
      INTEGER          OUT_VARIANCE_PTR ! pointer to variance array in output
      LOGICAL          REDUCE_SWITCH    ! .TRUE. if REDUCE_SWITCH has been run
      REAL             RTEMP            ! Scratch real
      INTEGER          RUN_NUMBER       ! run number of observation
      CHARACTER*15     SAMPLE_MODE      ! SAMPLE_MODE of observation
      INTEGER          SECNDF           ! NDF id of section
      CHARACTER*5      SKYBOLC(MAX__BOL)       ! indices or names of 
                                               ! bolometers whose data are to
                                               ! be treated as sky
      INTEGER          SKYBOLS(MAX__BOL)! Indices of sky bolometers
      CHARACTER*80     STEMP            ! scratch string
      CHARACTER*10     SUB_INSTRUMENT   ! Sub instrument name
      CHARACTER * (10) SUFFIX_STRINGS(SCUBA__N_SUFFIX) ! Suffix for OUT
      INTEGER          UBND(MAXDIM)     ! Upper bounds of section

*  Local Data:
      DATA SUFFIX_STRINGS /'!_sky','s','_sky'/

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Set the MSG output level (for use with MSG_OUTIF)
      CALL MSG_IFGET('MSG_FILTER', STATUS)


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
      CALL NDF_XLOC (INDF, 'SCUCD', 'READ', IN_SCUCDX_LOC, STATUS)

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

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'RUN', 
     :  RUN_NUMBER, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'OBJECT',
     :  OBJECT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'MODE',
     :  OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'SAM_MODE',
     :  SAMPLE_MODE, STATUS)
      CALL CHR_UCASE (SAMPLE_MODE)

      CALL MSG_SETC ('OBJECT', OBJECT)
      CALL MSG_SETC ('MODE', OBSERVING_MODE)
      CALL MSG_SETC ('SAMPLE', SAMPLE_MODE)
      CALL MSG_SETI ('RUN', RUN_NUMBER)
      CALL MSG_SETC ('PKG',PACKAGE)
      CALL MSG_OUTIF (MSG__NORM, ' ', 
     :     '^PKG: run ^RUN was a ^MODE observation '//
     :     'with ^SAMPLE sampling of object ^OBJECT', STATUS)

*  get the number of history records present in the file

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_HNREC (INDF, NREC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            NREC = 0
         END IF

*  check that the history of the input file is OK

         REDUCE_SWITCH = .FALSE.
         EXTINCTION = .FALSE.

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (INDF, 'APPLICATION', I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP .EQ. 'REDUCE_SWITCH') THEN
                  REDUCE_SWITCH = .TRUE.
               ELSE IF (STEMP .EQ. 'EXTINCTION') THEN
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

            IF (.NOT.EXTINCTION) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: the '//
     :           'EXTINCTION application has not been run '//
     :           'on the input file', STATUS)
            END IF
         END IF
      END IF

*  get the sub-instrument and wavelength of the data, check for consistency
 
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SUB_1', SUB_INSTRUMENT, STATUS)
      
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'WAVE_1', RTEMP, STATUS)

*     Remsky only works (so far) for array observations

      CALL CHR_UCASE(SUB_INSTRUMENT)
      IF ((SUB_INSTRUMENT .NE. 'SHORT') .AND.
     :     (SUB_INSTRUMENT .NE. 'LONG')) THEN

         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('PKG', PACKAGE)
            CALL MSG_SETC('TSK', TSKNAME)
            CALL ERR_REP(' ','^PKG: ^TSK can only be run on '//
     :           'array data.', STATUS)

         END IF

      END IF

*  Nasmyth coords of point on focal plane that the telescope is tracking
*  on the sky

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'CNTR_DU3',
     :  CENTRE_DU3, STATUS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'CNTR_DU4',
     :  CENTRE_DU4, STATUS)

*  coordinate system and coords of telescope `centre'

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'CENT_CRD',
     :  CENTRE_COORDS, STATUS)
      CALL CHR_UCASE (CENTRE_COORDS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LAT',
     :  STEMP, STATUS)
      CALL SCULIB_DECODE_ANGLE (STEMP, LAT_RAD, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LONG',
     :  STEMP, STATUS)
      CALL SCULIB_DECODE_ANGLE (STEMP, LONG_RAD, STATUS)

      IF (CENTRE_COORDS .EQ. 'PLANET') THEN
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LAT2',
     :     STEMP, STATUS)
         CALL SCULIB_DECODE_ANGLE (STEMP, LAT2_RAD, STATUS)
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LONG2',
     :     STEMP, STATUS)
         CALL SCULIB_DECODE_ANGLE (STEMP, LONG2_RAD, STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 'MJD1',
     :     MJD1, STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 'MJD2',
     :     MJD2, STATUS)
      END IF

      IF ((CENTRE_COORDS .NE. 'AZ')  .AND.
     :    (CENTRE_COORDS .NE. 'GA')) THEN
         LONG_RAD = LONG_RAD * 15.0D0
         LONG2_RAD = LONG2_RAD * 15.0D0
      END IF


*  the number of bolometers measured

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'N_BOLS',
     :  N_BOLS, STATUS)

*     Check the dimensions of the data array

      CALL NDF_DIM (INDF, MAXDIM, DIM, NDIM, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (OBSERVING_MODE .EQ. 'PHOTOM') THEN
            IF ((NDIM .NE. 3)                  .OR.
     :          (DIM(1) .NE. N_BOLS)         .OR.
     :          (DIM(2) .LT. 1)                .OR.
     :          (DIM(3) .NE. SCUBA__MAX_BEAM)) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: main data '//
     :           'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2 '//
     :           '^DIM3', STATUS)
            END IF
         ELSE
            IF ((NDIM .NE. 2)          .OR.
     :          (DIM(1) .NE. N_BOLS) .OR.
     :          (DIM(2) .LT. 1))       THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: main data '//
     :           'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2',
     :           STATUS)
            END IF
         END IF
      END IF

      N_POS = DIM (2)

*  get the bolometer description arrays

      CALL SCULIB_GET_BOL_DESC(IN_SCUBAX_LOC, SCUBA__NUM_CHAN,
     :     SCUBA__NUM_ADC, N_BOLS, BOL_TYPE, BOL_DU3,
     :     BOL_DU4, BOL_ADC, BOL_CHAN, STATUS)

****** END CHECKING ******

*     Generate a default name for the output file
      CALL SCULIB_CONSTRUCT_OUT(FNAME, SUFFIX_ENV, SCUBA__N_SUFFIX,
     :     SUFFIX_OPTIONS, SUFFIX_STRINGS, OUTFILE, STATUS)

*     set the default
      CALL PAR_DEF0C('OUT', OUTFILE, STATUS)

*  now open the output NDF, propagating it from the input file

      CALL NDF_PROP (INDF, 'Data,Var,Qual,Axis', 'OUT', OUTNDF, STATUS)

*  get the bad bit mask

      CALL NDF_BB(OUTNDF, BADBIT, STATUS)

*  Get the list of SKY bolometers if not a photometry pixel

      IF (N_BOLS .GT. 1) THEN

         CALL PAR_GET1C ('BOLOMETERS', MAX__BOL, SKYBOLC,
     :        N_SKYBOLS, STATUS)

*     Decode the array strings into a list of bolometer numbers
         CALL SURFLIB_DECODE_REMSKY_STRING(SUB_INSTRUMENT,
     :        N_SKYBOLS, SKYBOLC, N_BOLS, BOL_ADC, BOL_CHAN,
     :        SKYBOLS, N_GOODBOLS, STATUS)


*     Raise an error if no bolometers are present

         IF (N_GOODBOLS .LE. 0 .AND. STATUS .EQ. SAI__OK) THEN

            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP(' ', '^TASK: None of the selected '//
     :           'bolometers  were present in the data',
     :           STATUS)

         END IF

*     Print a message informing the user of the number of selected bolometers
         CALL MSG_SETI('NB',N_GOODBOLS)
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ',
     :        '^PKG: Using ^NB sky bolometers', STATUS)

*     Check the message filter level. If it is VERBOSE then we can
*     construct a string of the bolometer names
*     Otherwise it is a waste of time

         CALL MSG_IFLEV(MSG_LEV)

         IF (MSG_LEV .EQ. MSG__VERB) THEN

            CALL MSG_SETC('PKG',PACKAGE)
            CALL MSG_OUTIF(MSG__VERB, ' ', 
     :           '^PKG: Selected sky bolometers:', STATUS)

*     Write the bolometers into a string 

            IF (STATUS .EQ. SAI__OK) THEN
               LINE = ' '
               IPOSN = 0
               DO I = 1, N_GOODBOLS

*     If the string is now longer than MSG__SZMSG - 28
*     then we end up with ellipsis (...).
*     In order to overcome this I will force the string onto
*     a single line and then split it myself. This means I have to
*     clear the output string occassionally.
*     (since MSG can not display a string longer than MSG__SZMSG)
*     The extra check is there to make sure that LLEN is smaller than
*     MSG__SZMSG. The '10' is there to account for the package name.

                  IF ((IPOSN .GT. (MSG__SZMSG - 10)) .OR.
     :                 (IPOSN .GT. (LLEN - 10))) THEN
                     CALL MSG_SETC('BL',LINE)
                     CALL MSG_SETC('PKG', PACKAGE)
                     CALL MSG_OUTIF(MSG__VERB, ' ',
     :                    '^PKG: ^BL', STATUS)
                     
                     IPOSN = 0
                     LINE = ' '
                  
                  END IF

*     Convert the index to a string
                  CALL CHR_ITOC(SKYBOLS(I), STEMP, ITEMP)
                  IF(IPOSN.GT.0) THEN 
                     CALL CHR_APPND(', ',LINE,IPOSN)
                     IPOSN = IPOSN + 1 ! Since len does not see last space
                  END IF
                  CALL CHR_APPND(STEMP, LINE, IPOSN)
               END DO
            END IF

*     Print more information in verbose mode
*     Have to deal with the problem of the string being longer than
*     MSG__SZMSG and not being able to display all the bolometers
            
            CALL MSG_SETC('BL',LINE)
            CALL MSG_SETC('PKG', PACKAGE)
            CALL MSG_OUTIF(MSG__VERB, ' ',
     :           '^PKG: ^BL', STATUS)


         END IF

* Which mode of sky removal
         CALL PAR_CHOIC('MODE', 'MEAN','MEAN,MEDIAN', .TRUE., MODE,
     :        STATUS)

         IF (MODE .EQ. 'MEAN') THEN
            CALL PAR_GET0R('ITER_SIGMA',ITERCLIP, STATUS)
         ELSE
            ITERCLIP = -1.0
         END IF

      END IF

*     Find out if we want to add back the constant offset
*     The default behaviour should depend on the observation 
*     mode. If this is a MAP then add it on, else (ie for PHOTOM)
*     do not add it unless asked.

      IF (OBSERVING_MODE .EQ. 'MAP') THEN
         CALL PAR_DEF0L('ADD', .TRUE., STATUS)
      ELSE
         CALL PAR_DEF0L('ADD',.FALSE., STATUS)
      END IF

      CALL PAR_GET0L('ADD', ADD_BACK, STATUS)


*  Go through the data and remove sky

* Loop through beams if this is a photometry

      IF (OBSERVING_MODE .EQ. 'PHOTOM') THEN
         N_BEAMS = SCUBA__MAX_BEAM
         NDIM = 3
      ELSE
         N_BEAMS = 1
         NDIM = 2
      END IF

*  Define the base section
      LBND(1) = 1
      LBND(2) = 1
      UBND(1) = N_BOLS
      UBND(2) = N_POS

      DO BEAM = 1, N_BEAMS

*    Get the beam as an NDF section
         UBND(3) = BEAM
         LBND(3) = BEAM

         CALL NDF_SECT(OUTNDF, NDIM, LBND, UBND, SECNDF, STATUS)

*     map the various components

         CALL NDF_MAP (SECNDF, 'QUALITY', '_UBYTE', 'UPDATE',
     :        OUT_QUALITY_PTR, ITEMP, STATUS)
         CALL NDF_MAP (SECNDF, 'DATA', '_REAL', 'UPDATE', 
     :        OUT_DATA_PTR, ITEMP, STATUS)
         CALL NDF_MAP (SECNDF, 'VARIANCE', '_REAL', 'UPDATE',
     :        OUT_VARIANCE_PTR, ITEMP, STATUS)

         IF (N_BOLS .GT. 1) THEN
            CALL SCULIB_REM_SKY(MODE, ADD_BACK, N_BOLS, N_POS, 
     :           %val(OUT_DATA_PTR),
     :           %val(OUT_VARIANCE_PTR), 
     :           %val(OUT_QUALITY_PTR),
     :           ITERCLIP, N_GOODBOLS, SKYBOLS, BADBIT, STATUS)
         END IF

*  unmap the main data array

         CALL NDF_UNMAP (SECNDF, '*', STATUS)
         CALL NDF_ANNUL(SECNDF, STATUS)
      END DO


*  set the bad bit mask

      CALL NDF_SBB(BADBIT, OUTNDF, STATUS)

*  tidy up

      CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUCDX_LOC, STATUS)

      CALL NDF_ANNUL (INDF, STATUS)
      CALL NDF_ANNUL (OUTNDF, STATUS)

      CALL NDF_END (STATUS)

      END
