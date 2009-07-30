      SUBROUTINE SURF_SCUCLIP (STATUS)
*+
*  Name:
*     SCUCLIP

*  Purpose:
*     Simple sigma clipping for each bolometer

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
*     Each bolometer is analysed independently, the mean and standard 
*     deviation are calculated, any points greater than NSIGMA sigma 
*     from the mean are treated as spikes and removed. Note that for mapping 
*     this despiking algorithm is only useful for very weak
*     sources; bright sources will be removed (since a bolometer
*     jiggles on and off bright sources). Photometry observations
*     do not suffer from this problem as the bolometers are always on 
*     source.

*  Usage:
*     scuclip in out

*  ADAM Parameters:
*     IN = NDF (Read)
*        This is the name of the input demodulated data file
*     MSG_FILTER = CHAR (Read)
*        Message output level. Default is NORM. If MSG_FILTER is set
*        to VERBOSE the number of spikes removed from each bolometer is
*        reported.
*     NSIGMA = DOUBLE (Read)
*        Number of sigma beyond which data are thought to be spikes.
*     OUT = NDF (Write)
*        Output data file.

*  Examples:
*     scuclip infile outfile nsigma=5
*       Clip any data points that are further than 5 sigma from the mean.
*       The clipping is done on a per bolometer basis.

*  Notes:
*     - The despiking routine is very primitive and should not be used
*       with jiggle map data of bright sources. It can be used
*       on PHOTOM data since the jiggle pattern never moves off source
*       (although SIGCLIP can be used once the data has been processed
*        by SCUPHOT).

*  Implementation status:
*     The despiking routine sets QUALITY bit 5 to bad. It does not affect
*     the data. The effects of despiking can be removed by using the 
*     Kappa task SETBB to unset quality bit 5.

*  Related Applications:
*     SURF: SCUQUICK, REBIN, SCUPHOT, SCUOVER, SIGCLIP, DESPIKE;
*     KAPPA: SETBB
 
*  Authors:
*     TIMJ: Tim Jenness (timj@jach.hawaii.edu)
*     {enter_new_authors_here}
 

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     3 Nov 1996: TIMJ
*        Original version (in REMSKY)
*     $Log$
*     Revision 1.6  2004/09/08 02:03:34  timj
*     Add CNF_PVAL where appropriate
*
*     Revision 1.5  1999/08/03 20:01:42  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     Revision 1.4  1999/05/15 01:48:43  timj
*     Finalise support for POLMAP/POLPHOT observing modes.
*     Only check first few characters of history app name
*     now that we are writing version number to this string.
*     POLPHOT is synonym for PHOTOM.
*
*     Revision 1.3  1997/12/01 02:00:52  timj
*     Update documentation.
*
*     Revision 1.2  1997/11/06 23:42:32  timj
*     Report total number of spikes removed.
*     Add the verbose suffix option.
*
*     Revision 1.1  1997/11/06 23:32:40  timj
*     Initial revision
*
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global constants:
      INCLUDE 'SAE_PAR'                 ! SSE global definitions
      INCLUDE 'DAT_PAR'                 ! for DAT__SZLOC
      INCLUDE 'PRM_PAR'                 ! for VAL__xxxx
      INCLUDE 'SURF_PAR'                ! REDS constants
      INCLUDE 'MSG_PAR'                 ! MSG__ constants
      INCLUDE 'CNF_PAR'                 ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External references:
      BYTE    SCULIB_BITON              ! Turn on skybit

*  Local Constants:
      INTEGER          MAX__BOL                  ! max number of bolometers
      PARAMETER (MAX__BOL = 100)                 ! that can be specified
      INTEGER          MAXDIM
      PARAMETER (MAXDIM = 4)
      CHARACTER * 10   TSKNAME          ! Name of task
      PARAMETER (TSKNAME = 'SCUCLIP')

*  Local variables:
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
      INTEGER          BOL_PTR          ! Pointer to single bolometer
      INTEGER          BOL_PTR_END      ! Pointer to end single bolometer
      INTEGER          BOL_QPTR         ! Pointer to single bolometer quality
      INTEGER          BOL_QPTR_END     ! Pointer to end single bol quality
      INTEGER          DIM (MAXDIM)     ! the dimensions of an array
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
      INTEGER          ITEMP            ! scratch integer
      INTEGER          LBND (MAXDIM)    ! lower bounds of array
      INTEGER          NDIM             ! the number of dimensions in an array
      INTEGER          NREC             ! number of history records in file
      DOUBLE PRECISION NSIGMA           ! clipping level
      INTEGER          NSPIKES          ! Number of spikes detected
      INTEGER          N_BEAMS          ! number of beams for which data have
                                        ! been reduced
      INTEGER          N_BOLS           ! number of bolometers measured in
                                        ! output file
      INTEGER          N_FITS           ! number of FITS lines read from file
      INTEGER          N_POS            ! the total number of positions measured
      CHARACTER*30     OBJECT           ! name of object observed
      CHARACTER*15     OBSERVING_MODE   ! type of observation
      CHARACTER*132    OUTFILE          ! Default output filename
      INTEGER          OUTNDF           ! NDF identifier of output file
      INTEGER          OUT_DATA_PTR     ! pointer to data array in output file
      INTEGER          OUT_QUALITY_PTR  ! pointer to quality array in output 
      INTEGER          OUT_VARIANCE_PTR ! pointer to variance array in output
      LOGICAL          REDUCE_SWITCH    ! .TRUE. if REDUCE_SWITCH has been run
      INTEGER          RUN_NUMBER       ! run number of observation
      CHARACTER*15     SAMPLE_MODE      ! SAMPLE_MODE of observation
      INTEGER          SECNDF           ! NDF id of section
      CHARACTER*80     STEMP            ! scratch string
      CHARACTER * (10) SUFFIX_STRINGS(SCUBA__N_SUFFIX) ! Suffix for OUT
      INTEGER          TOT_SPIKES       ! Total number of spikes
      INTEGER          UBND(MAXDIM)     ! Upper bounds of section

*  Local Data:
      DATA SUFFIX_STRINGS /'!_clip','c','_clip'/

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

            IF (.NOT.EXTINCTION) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: the '//
     :           'EXTINCTION application has not been run '//
     :           'on the input file', STATUS)
            END IF
         END IF
      END IF

*  the number of bolometers measured

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'N_BOLS',
     :  N_BOLS, STATUS)

*  map the various components of the data array and check the data dimensions 

      CALL NDF_DIM (INDF, MAXDIM, DIM, NDIM, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :        OBSERVING_MODE .EQ. 'POLPHOT') THEN
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

      CALL NDF_PROP (INDF, 'Data,Var,Qual,Axis,Units', 'OUT', OUTNDF, 
     :     STATUS)

*  get the bad bit mask

      CALL NDF_BB(OUTNDF, BADBIT, STATUS)

*  How many sigma for the clip?
      CALL PAR_GET0D('NSIGMA', NSIGMA, STATUS)

* Loop through beams if this is a photometry

      IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :     OBSERVING_MODE .EQ. 'POLPHOT') THEN
         N_BEAMS = SCUBA__MAX_BEAM
         NDIM = 3
      ELSE
         N_BEAMS = 1
         NDIM = 2
      END IF

*     Initialise the counter
      TOT_SPIKES = 0

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

*  Initalise Pointers
         IF (STATUS .EQ. SAI__OK) THEN
            BOL_PTR = 0
            BOL_PTR_END = 0
            BOL_QPTR = 0
            BOL_QPTR_END = 0
         END IF

*  Grab some scratch data
         CALL SCULIB_MALLOC(N_POS * VAL__NBR, BOL_PTR,
     :        BOL_PTR_END, STATUS)
         CALL SCULIB_MALLOC(N_POS * VAL__NBUB, BOL_QPTR,
     :        BOL_QPTR_END, STATUS)

         DO I = 1, N_BOLS

            CALL SCULIB_EXTRACT_BOL(I, N_BOLS, N_POS, 
     :           %VAL(CNF_PVAL(OUT_DATA_PTR)),
     :           %VAL(CNF_PVAL(OUT_QUALITY_PTR)),
     :           %VAL(CNF_PVAL(BOL_PTR)), %VAL(CNF_PVAL(BOL_QPTR)), 
     :           STATUS)

*  Despike
            CALL SCULIB_CLIP_BOL(N_POS, %VAL(CNF_PVAL(BOL_PTR)),
     :           %VAL(CNF_PVAL(BOL_QPTR)), 
     :           NSIGMA, BADBIT, NSPIKES, STATUS)

*     I note that the system will find the same spike in each
*     beam (of photometry data) so that I should only report spikes
*     when we are in the first beam.

            IF (BEAM .EQ. 1) THEN

*     Keep track of the total number of spikes
               TOT_SPIKES = TOT_SPIKES + NSPIKES
            
*     Report the number of spikes

               IF (NSPIKES .GT. 0) THEN
                  CALL MSG_SETC('TSK', TSKNAME)
                  CALL MSG_SETI('NM', NSPIKES)
                  CALL MSG_SETI('BOL', I)
                  
                  CALL MSG_OUTIF(MSG__VERB, ' ', 
     :                 '^TSK: ^NM points clipped from bolometer ^BOL',
     :                 STATUS)

               END IF

            END IF


            CALL SCULIB_INSERT_BOL(I, N_BOLS, N_POS, 
     :                             %VAL(CNF_PVAL(BOL_PTR)),
     :           %VAL(CNF_PVAL(BOL_QPTR)), %VAL(CNF_PVAL(OUT_DATA_PTR)),
     :           %VAL(CNF_PVAL(OUT_QUALITY_PTR)), STATUS)

         END DO

*     Tidy up
         BADBIT = SCULIB_BITON(BADBIT, 4)
         CALL SCULIB_FREE('BOLDATA', BOL_PTR, BOL_PTR_END, STATUS)
         CALL SCULIB_FREE('BOLQDATA', BOL_QPTR, BOL_QPTR_END, STATUS)
         
*  unmap the main data array

         CALL NDF_UNMAP (SECNDF, '*', STATUS)
         CALL NDF_ANNUL(SECNDF, STATUS)
      END DO


*  set the bad bit mask

      CALL NDF_SBB(BADBIT, OUTNDF, STATUS)

*     Report the total number of spikes

      IF (TOT_SPIKES .GT. 0) THEN
         CALL MSG_SETI('NS',TOT_SPIKES)
         CALL MSG_SETC('PKG',PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ',
     :        '^PKG: Removed ^NS spikes', STATUS)
      END IF


*  tidy up

      CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUCDX_LOC, STATUS)

      CALL NDF_ANNUL (INDF, STATUS)
      CALL NDF_ANNUL (OUTNDF, STATUS)

      CALL NDF_END (STATUS)

      END
