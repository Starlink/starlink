      SUBROUTINE SURF_SCUPHOT (STATUS)
*+
*  Name:
*     SCUPHOT

*  Purpose:
*     Reduce SCUBA PHOTOM data

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SURF_SCUPHOT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*        This routine reduces the data for a single sub-instrument from a
*     PHOTOM observation. For each bolometer used to look at the source the
*     data will be analysed as follows:-
*
*      - An ndf called <bolname>_map (e.g. h7_map) will be created in the
*        OUT file to hold the coadded data from all the integrations. If the
*        jiggle pattern points fit a 2-d rectangular pattern then these data
*        will be arranged as a 2-d map suitable for plotting as an image. A
*        2-d parabola will be fitted to the coadded image and the results
*        written in ASCII form to FILE. If an irregular jiggle pattern is
*        used the map will take the form of a 1-D strip.
*      - Second, an ndf called <bolname>_peak (e.g. h7_peak) will be created
*        in the OUT file to hold the fit results to the data for each
*        integration. The results stored are the fit peak, its variance and
*        quality and they are held as a 1-d array suitable for plotting as
*        a graph. The fit results are also written in ASCII form to FILE, as
*        is the coadd of all the individual fits to the data.
*

*  Usage:
*     scuphot analysis in out file

*  ADAM Parameters:
*     ALLBOLS = LOGICAL (Read)
*        By default only the observed bolometers are processed (i.e. if you
*        observed with H7 only H7 data will be stored). If ALLBOLS is set
*        to true then all middle beam data is processed. This is useful
*        for examining sky noise. Note that for 2 and 3 bolometer photometry
*        ALLBOLS must be false to avoid weighting problems for the
*        bolometers that were observed in the left or right beams.
*     ANALYSIS = CHAR (Read)
*        The method used to detemine peak. Allowed options are:
*         AVERAGE: Determine the average of each integration.
*         PARABOLA: Fit a parabola to each integration.
*                   Parabola is not recommended at this time.
*         SAMPLES:  Store ALL samples (ie do not combine at all)
*                   No text file is written in this case. This option is
*                   similar to AVERAGE (in that each sample is treated
*                   as a measurement of the source flux) but sometimes
*                   gives a better ERROR (same MEAN) since more data
*                   points are available. Should be used when only a few
*                   integrations are available.
*     FILE = FILENAME (Write)
*        The name of the ASCII output file. No text file is written
*        if ANALYSIS=SAMPLES
*     IN = NDF (Read)
*        The name of the input file containing demodulated (extinction
*        corrected) SCUBA data.
*     MSG_FILTER = CHAR (Read)
*        Message output level. Default is NORM.
*     OUT = CHAR (Write)
*        The name of the HDS output file to contain the NDFs described above.
*        This file will have the extension .sdf but this should not be
*        specified in the name.

*  Notes:
*     - ALLBOLS must be false for 2 and 3 bolometer photometry unless you
*       know what you are doing.
*     - SCUPHOT can process JIGGLE/MAP data. The output is the signal
*       for each integration for each bolometer. This is useful
*       for checking sky removal and should not be used for performing
*       on-source photometry on map data! This method can not be used
*       for SCAN/MAP data.

*  Implementation Status:
*     Ideally SCUPHOT should process MAP data on a per exposure basis.
*     Currently only per integration is supported.

*  Related Applications:
*     SURF: SCUCAT

*  Algorithm:
*        In more detail the routine works as follows. If status is good on
*     entry the routine opens the IN file, reads some FITS items describing
*     the observation and reports them to the user. A check is made that the
*     data does come from a PHOTOM observation. The file `history' is read
*     and a check made that the REDUCE_SWITCH and EXTINCTION
*     applications have been run on it but that PHOTOM has not. A warning is
*     issued if EXTINCTION has not been run on the data. A note is
*     taken if the SKY_ERROR application has been run to remove `sky noise'
*     error from the data.
*        Other FITS items are read, mainly so that they can be passed on to
*     the ASCII output file. These are the coordinate system and coordinates
*     of the telescope centre, the coordinate system and magnitude of the
*     offsets of the source from the centre, the date and time of the
*     observation.
*        Next, the components of the main data array are mapped and their
*     dimensions checked. All the component integrations of the observation
*     are butted end to end in this array so the `pointer' array is also
*     mapped, containing the start and finish indices of each exposure.
*        The indices of the target bolometers in each `beam' are read from
*     the input file together with the weights to be associated with the
*     data from each. A search is made for the INT_QUALITY array in the
*     REDS extension of the input file. This may have been placed there
*     by the REDS MODIFY application to specify those integrations whose
*     data are to be ignored in the reduction. If no such array is found
*     then data from all the integrations will be used.
*        The jiggle pattern used is read from the file along with the
*     coordinate system of the offsets. SCULIB_CALC_GRID is called
*     to determine whether or not the jiggle pattern conforms to a square
*     grid. If it does then the coadded integrations for each bolometer
*     wll be stored as a small map in a 2-d ndf in the OUT file, otherwise
*     they will be stored as a 1-d vector.
*        Next some scratch memory is obtained to hold the integration data
*     for each target bolometer and the relevant data copied into it from
*     the IN data array.
*        The name of the OUT file is read from the parameter and the file
*     created. The routine cycles through the 3 possible beams that can
*     be used in a PHOTOM observation and, if a bolometer was working in
*     that beam, creates 2 ndfs in the OUT file, one called <bolname>_map
*     to hold the coadded integration result and a second called <bolname>_peak
*     to hold the fit results to the data for each integration. The weight
*     to be given to this bolometer's data is stored in the REDS.BEAM_WT
*     object in the MORE structure of each ndf.
*        The routine cycles through the integrations taken and for each one,
*     if INT_QUALITY is 0, calls SCULIB_COADD to add it to the coadd result and
*     SCULIB_FIT_2D_PARABOLA to fit a parabola to it. The fit results are
*     themselves coadded. When all the integrations have been dealt with
*     SCULIB_FIT_2D_PARABOLA is called to fit a parabola to the coadded map
*     and SCULIB_UNPACK_JIGGLE_SEPARATES to store the map to the output ndf.
*     After the integration fit results have been copied to their ndf the
*     analysis is complete and SURF_WRITE_PHOTOM is called to write the ASCII
*     version of the results to the file whose name is given in parameter
*     FILE.
*        Lastly, the IN and OUT files are closed.

*  Authors:
*     JFL: John Lightfoot (ROE)
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     16-JUL-1995: Original version.
*     $Log$
*     Revision 1.32  2004/09/08 02:03:35  timj
*     Add CNF_PVAL where appropriate
*
*     Revision 1.31  2000/09/20 00:52:12  timj
*     Fix SEGV when using POLPHOT data (the if statement was incorrect)
*
*     Revision 1.30  2000/06/28 01:30:41  timj
*     Reset MEAS_2_Q each time round loop
*
*     Revision 1.29  2000/06/28 01:10:50  timj
*     Reset MEAS_Q each time round loop
*
*     Revision 1.28  1999/08/03 20:01:43  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     Revision 1.27  1999/05/16 03:48:55  timj
*     Check PHOT_BB before freeing memory
*
*     Revision 1.26  1999/05/15 04:19:49  timj
*     Add STATUS checking to SCULIB_COADD
*
*     Revision 1.25  1999/05/15 03:48:45  timj
*     Initialise MAP_N_PTR.
*
*     Revision 1.24  1999/05/15 01:51:07  timj
*     Add support for measurement propogation (POLPHOT mode)
*
*     Revision 1.23  1998/10/06 20:56:20  timj
*     Check for aborted observations.
*     Modify N_INTEGRATIONS if aborted.
*
*     Revision 1.22  1998/06/09 21:57:34  timj
*     Propogate Units to IPEAK rather than setting it.
*
*     Revision 1.21  1998/04/29 00:54:19  timj
*     Add ANALYSIS=SAMPLES to simply propogate the data. Propogate input NDF to
*     output NDFs so that FITS extension and HISTORY are copied automatically.
*
*     Revision 1.20  1997/11/06 23:43:20  timj
*     Add the verbose suffix option.
*
*     Revision 1.19  1997/09/04 19:04:03  timj
*     Automatically supply default for 'OUT'. Also fix null response to 'OUT'
*
*     Revision 1.18  1997/07/22 17:50:47  timj
*     Remove reference to IN_CENTRE_COORDS (should be CENTRE_COORDS)
*
*     Revision 1.17  1997/06/27 23:17:59  timj
*     Tweak header.
*
*     Revision 1.16  1997/06/13 00:17:24  timj
*     Check if we have performed a parabolic fit or not.
*     Update doc.
*     Change name to SURF
*
*     Revision 1.15  1997/05/28 19:17:38  timj
*     Fix declaration of MEAS_*_Q (was INTEGER, now BYTE)
*
*     Revision 1.14  1997/05/22 21:21:01  timj
*     Allow for null response to file open request.
*
*     Revision 1.13  1997/05/22 03:05:54  timj
*     Allow all bolometers to be processed - not just the targeted ones.
*
*     Revision 1.12  1997/04/30 02:39:39  timj
*     Add MSG_OUTIF
*
*     Revision 1.11  1997/04/14 23:58:07  timj
*     Add more checks for zero jiggle offsets.
*
*     Revision 1.10  1997/03/31 19:46:28  timj
*     Add SCULIB_GET_JIGGLE
*     Change PACKAGE and TSKNAME to variables.
*
*     Revision 1.9  1997/03/21 01:12:12  timj
*     Use SCULIB_GET_DEM_PNTR
*     Write FITS to output NDFs.
*
*     Revision 1.8  1997/03/20 21:41:30  timj
*     Update header.
*     Remove NDF_SCOPY and replace with NDF_NEW
*
c Revision 1.7  1996/12/10  02:12:43  timj
c Fix for non-square jiggles.
c Change 'ANALYSIS' to PAR_CHOIC.
c
c Revision 1.6  1996/11/02  01:43:25  timj
c Fix bug in history header
c
c Revision 1.5  1996/11/02  01:25:10  timj
c Change Naem: to SCUPHOT from SURF_SCUPHOT in header.
c
c Revision 1.4  1996/11/01  21:46:04  timj
c Change name to SCUPHOT
c
c Revision 1.3  1996/10/30  22:54:34  timj
c Add modern header.
c Replace SCULIB_COPY with VEC_
c Change size of OUT to 132 characters
c Probably lots of other things...
c
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'                ! SSE global definitions
      INCLUDE 'DAT_PAR'                ! Data-system constants
      INCLUDE 'MSG_PAR'                ! Add MSG__ constants
      INCLUDE 'NDF_PAR'                ! for NDF__xxxx constants
      INCLUDE 'PRM_PAR'                ! for VAL__xxxx constants
      INCLUDE 'SURF_PAR'               ! REDS constants
      INCLUDE 'PAR_ERR'                ! PAR__ constants
      INCLUDE 'CNF_PAR'                ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN                  ! CHR used-string-length function

*  Local Constants:
      INTEGER     MAX_DIM              ! max number of dims in array
      PARAMETER (MAX_DIM = 4)
      BYTE        OUTBAD               ! Output bad pixel mask
      PARAMETER (OUTBAD = 1)
      CHARACTER * 10 TSKNAME           ! Name of task
      PARAMETER (TSKNAME = 'SCUPHOT')

*  Local variables:
      LOGICAL          ABORTED         ! .TRUE. if the observation was
                                       ! aborted
      INTEGER          ALL             ! Beam to use for ALLBOLS
      LOGICAL          ALL_BOLS        ! Select all bolometers
      CHARACTER*10     ANALYSIS        ! analysis mode
      CHARACTER*10     ANALYSIS_METHOD ! analysis mode (first time)
      BYTE             BADBIT          ! bad bit mask
      INTEGER          BEAM            ! beam index in DO loop
      REAL             BEAM_WEIGHT (SCUBA__MAX_BEAM)
                                       ! the weight assigned to the measurement
                                       ! in each beam
      INTEGER          BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                       ! A/D numbers of bolometers measured in
                                       ! input file
      INTEGER          BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                       ! channel numbers of bolometers measured
                                       ! in input file
      CHARACTER*15     CENTRE_COORDS   ! coordinate system of telescope centre
      INTEGER          COUNT           ! Number of times through loop
      INTEGER          DEM_PNTR_PTR    ! array pointer to SCUBA.DEM_PNTR array
      INTEGER          DIM (MAX_DIM)   ! array dimensions
      INTEGER          END_BOL         ! Last bolometer
      REAL             EXPOSURE_TIME   ! exposure time per jiggle point
      LOGICAL          EXTINCTION      ! .TRUE. if EXTINCTION application has
                                       ! been run on input file
      INTEGER          FD              ! Text file descriptor
      CHARACTER*15     FILTER          ! the name of the filter being used
      CHARACTER*80     FITS (SCUBA__MAX_FITS)
				       ! array of FITS keywords
      CHARACTER*132    FNAME           ! Input filename
      CHARACTER*(DAT__SZNAM) HDSNAME   ! Name of the container (not the fname)
      INTEGER          I               ! DO loop index
      INTEGER          IBEAM           ! ndf identifier
      INTEGER          IERR            ! Pos of errors from VEC_
      INTEGER          INT             ! integration index in DO loop
      INTEGER          INT_D_END (SCUBA__MAX_BEAM)
                                       ! end of space holding integration data
      INTEGER          INT_D_PTR (SCUBA__MAX_BEAM)
                                       ! start of space holding integration
                                       ! data
      INTEGER          INT_OFFSET      ! offset of the start of an integration
                                       ! in the data array
      INTEGER          INT_Q_END (SCUBA__MAX_BEAM)
                                       ! end of space holding integration
                                       ! quality
      INTEGER          INT_Q_PTR (SCUBA__MAX_BEAM)
                                       ! start of space holding integration
                                       ! quality
      INTEGER          INT_V_END (SCUBA__MAX_BEAM)
                                       ! end of space holding integration
                                       ! variance
      INTEGER          INT_V_PTR (SCUBA__MAX_BEAM)
                                       ! start of space holding integration
                                       ! variance
      INTEGER          IN_D_PTR        ! pointer to input data array
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC
                                       ! locator to FITS extension in input
                                       ! file
      INTEGER          IN_OFFSET       ! offset in input array
      INTEGER          IN_NDF          ! NDF index of input file
      INTEGER          IN_Q_PTR        ! pointer to input quality array
      CHARACTER*(DAT__SZLOC) IN_REDSX_LOC
                                       ! locator to REDS extension in input
                                       ! file
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC
                                       ! locator to SCUBA extension in input
                                       ! file
      CHARACTER*(DAT__SZLOC) IN_SCUCDX_LOC
                                       ! locator to SCUCD extension in input
                                       ! file
      INTEGER          IN_V_PTR        ! pointer to input variance array
      INTEGER          IPEAK           ! ndf identifier
      INTEGER          IPOS (SCUBA__MAX_JIGGLE)
				       ! i index of jiggle measurement in
				       ! 2d map
      INTEGER          IPOSN           ! Position in string
      LOGICAL          ISBAD           ! Are there bad output pixels
      INTEGER          ITEMP           ! scratch integer
      INTEGER          JIGGLE_COUNT    ! number of jiggles in pattern
      INTEGER          JIGGLE_P_SWITCH ! number of jiggles per switch
      INTEGER          JIGGLE_REPEAT   ! number of times jiggle pattern is
                                       ! repeated in a switch
      REAL             JIGGLE_X (SCUBA__MAX_JIGGLE)
                                       ! x jiggle offsets (arcsec)
      REAL             JIGGLE_Y (SCUBA__MAX_JIGGLE)
                                       ! y jiggle offsets (arcsec)
      REAL             JIGGLE_2D_A1 (SCUBA__MAX_JIGGLE)
                                       ! x-axis of jiggle map
      REAL             JIGGLE_2D_A2 (SCUBA__MAX_JIGGLE)
                                       ! y-axis of jiggle map
      INTEGER          JPOS (SCUBA__MAX_JIGGLE)
				       ! j index of jiggle measurement in
				       ! 2d map
      INTEGER          LAST_EXP        ! the number of the exposure being
                                       ! measured when the abort occurred
      INTEGER          LAST_INT        ! the number of the integration
                                       ! being measured when the abort
                                       ! occurred
      INTEGER          LAST_MEAS       ! the number of the measurement
                                       ! being measured when the abort
                                       ! occurred
      CHARACTER*15     LAT             ! the latitude of the telescope centre
      CHARACTER*15     LAT2            ! the second latitude of the telescope
                                       ! centre for a PLANET coord system
      INTEGER          LBND (2)        ! pixel indices of bottom left corner
                                       ! of output image
      CHARACTER*15     LONG            ! the longitude of the telescope centre
      CHARACTER*15     LONG2           ! the second longitude of the telescope
                                       ! centre for a PLANET coord system
      INTEGER          MAP_D_PTR       ! pointer to output map data array
      INTEGER          MAP_Q_PTR       ! pointer to output map quality array
      INTEGER          MAP_N_PTR       ! pointer to output map scratch array
      INTEGER          MAP_N_PTR_END   ! pointer to end of scratch array
      INTEGER          MAP_V_PTR       ! pointer to output map variance array
      REAL             MAP_X           ! x offset of map centre from telescope
                                       ! centre (radians)
      REAL             MAP_Y           ! y offset of map centre from telescope
                                       ! centre (radians)
      INTEGER          MEAS            ! Measurement counter
      REAL             MEAS_D (SCUBA__MAX_JIGGLE, SCUBA__MAX_BEAM)
				       ! the coadded data for each beam
      INTEGER          MEAS_N (SCUBA__MAX_JIGGLE, SCUBA__MAX_BEAM)
                                       ! number of coadded integrations in
                                       ! MEAS_D
      BYTE             MEAS_Q (SCUBA__MAX_JIGGLE, SCUBA__MAX_BEAM)
				       ! the quality on MEAS_D
      REAL             MEAS_V (SCUBA__MAX_JIGGLE, SCUBA__MAX_BEAM)
				       ! the variance on MEAS_D
      REAL             MEAS_1_A0 (SCUBA__MAX_BEAM)
				       ! the a0 of the parabola fit to the
				       ! coadded measurement
      REAL             MEAS_1_A1 (SCUBA__MAX_BEAM)
				       ! the a1 of the parabola fit to the
				       ! coadded measurement
      REAL             MEAS_1_D (SCUBA__MAX_BEAM)
				       ! the fitted peak to the coadded
				       ! integrations
      BYTE             MEAS_1_Q (SCUBA__MAX_BEAM)
				       ! the quality on MEAS_1_D
      REAL             MEAS_1_V (SCUBA__MAX_BEAM)
				       ! the variance on MEAS_1_D
      REAL             MEAS_1_X (SCUBA__MAX_BEAM)
				       ! the x offset of the fitted peak
      REAL             MEAS_1_Y (SCUBA__MAX_BEAM)
				       ! the y offset of the fitted peak
      REAL             MEAS_2_D (SCUBA__MAX_BEAM)
				       ! the coadd of the peaks fitted to
				       ! the individual integrations for
				       ! each bolometer
      INTEGER          MEAS_2_N (SCUBA__MAX_BEAM)
                                       ! the number of integrations coadded
      BYTE             MEAS_2_Q (SCUBA__MAX_BEAM)
				       ! the quality on MEAS_2_D
      REAL             MEAS_2_V (SCUBA__MAX_BEAM)
				       ! the variance on MEAS_2_D
      DOUBLE PRECISION MJD1            ! the Julian date at which the object
                                       ! was at LAT,LONG
      DOUBLE PRECISION MJD2            ! the Julian date at which the object
                                       ! was at LAT2, LONG2
      CHARACTER*15     NDF_NAME        ! name of ndf
      INTEGER          NDIM            ! the number of dimensions in an array
      INTEGER          NELM            ! number of array elements mapped
      INTEGER          NERR            ! Number of errors from VEC_
      INTEGER          NREC            ! number of history records in input file
      INTEGER          NUM_INTS        ! Current number of integations in loop
      INTEGER          N_BITS          ! Number of integrations or samples
      INTEGER          N_BOLS          ! number of bolometers measured in input
                                       ! files
      INTEGER          N_EXPOSURES     ! number of exposures per integration
                                       ! in input file
      INTEGER          N_FITS          ! number of items in FITS array
      INTEGER          N_INTEGRATIONS  ! number of integrations per measurement
                                       ! in input file
      INTEGER          N_MEASUREMENTS  ! number of measurements in input file
      INTEGER          N_OBSDIM        ! number of dimensions in jiggle map
      INTEGER          N_POS           ! number of positions measured in input
                                       ! file
      CHARACTER*40     OBJECT          ! name of object
      CHARACTER*40     OBSERVING_MODE  ! observing mode of input file
      CHARACTER*64     ODF_NAME        ! name of observation definition file
      CHARACTER*15     OFFSET_COORDS   ! coord system of MAP_X and MAP_Y
      CHARACTER*(DAT__SZLOC) OUT_LOC   ! locator of HDS container file for
				       ! output
      CHARACTER*132    OUT             ! name of output HDS container file
      CHARACTER*132    OUTFILE         ! Default name for out
      INTEGER          OUT_OFFSET      ! offset in output array
      CHARACTER*(DAT__SZLOC) OUT_REDSX_LOC
                                       ! pointer to REDS extension in output
                                       ! NDFs
      LOGICAL          PARABOLA        ! Can I fit a parabola
      REAL             PEAK_D (SCUBA__MAX_INT, SCUBA__MAX_BEAM)
                                       ! fitted peaks
      BYTE             PEAK_Q (SCUBA__MAX_INT, SCUBA__MAX_BEAM)
                                       ! quality on fitted peaks
      REAL             PEAK_V (SCUBA__MAX_INT, SCUBA__MAX_BEAM)
                                       ! variance on fitted peaks
      REAL             PEAK_X (SCUBA__MAX_INT, SCUBA__MAX_BEAM)
                                       ! x coord of fitted peak
      REAL             PEAK_Y (SCUBA__MAX_INT, SCUBA__MAX_BEAM)
                                       ! y coord of fitted peak
      INTEGER          PEAK_D_PTR      ! pointer to PEAK_D array, holding
				       ! fitted peaks in ndf
      INTEGER          PEAK_Q_PTR      ! pointer to PEAK_Q array, holding
				       ! quality of fitted peaks in ndf
      INTEGER          PEAK_V_PTR      ! pointer to PEAK_V array, holding
				       ! variance of fitted peaks in ndf
      LOGICAL          PHOTOM          ! .TRUE. if the PHOTOM application
                                       ! has already been run on the file
      INTEGER          PHOTBB          ! Loop index
      INTEGER          PHOT_BB (SCUBA__MAX_BEAM)
                                       ! index of target bolometers in
                                       ! input data array
      INTEGER          PLACE           ! place holder for ndf in output file
      INTEGER          POS             ! measurement index in DO loop
      LOGICAL          REDUCE_SWITCH   ! .TRUE. if REDUCE_SWITCH application
                                       ! has been run on input file
      REAL             RTEMP           ! scratch real
      INTEGER          RUN_NUMBER      ! run number of input file
      CHARACTER*15     SAMPLE_COORDS   ! coordinate system of sample offsets
      CHARACTER*15     SAMPLE_MODE     ! sample mode of input file
      REAL             SAMPLE_PA       ! position angle of sample x axis
                                       ! relative to x axis of SAMPLE_COORDS
                                       ! system
      INTEGER          SEC_NDF         ! NDF identifier to section
      LOGICAL          SKY_ERROR       ! .TRUE. if SKY_ERROR application has
                                       ! been run on the data
      INTEGER          START_BOL       ! First bolometer
      CHARACTER*80     STATE           ! the state of SCUCD when the
                                       ! datafile was closed
      CHARACTER*80     STEMP           ! scratch string
      CHARACTER*15     SUB_INSTRUMENT  ! the sub-instrument used to make the
                                       ! maps
      CHARACTER * (10) SUFFIX_STRINGS(SCUBA__N_SUFFIX) ! Suffix for OUT
      INTEGER          TEMP_PTR        ! temporary array pointer
      INTEGER          UBND (2)        ! pixel indices of top right corner
                                       ! of output image
      LOGICAL          USEFILE         ! Am I writing the text file
      CHARACTER*15     UTDATE          ! date of input observation
      CHARACTER*15     UTSTART         ! UT of start of input observation
      REAL             WAVELENGTH      ! the wavelength of the map (microns)
      LOGICAL          WRITEMAP        ! Am I storing the jiggle maps
      REAL             XMAX            ! maximum x jiggle offset
      REAL             XMIN            ! minimum x jiggle offset
      REAL             XSPACE          ! spacing between x jiggle offsets
      REAL             YMAX            ! maximum y jiggle offset
      REAL             YMIN            ! minimum y jiggle offset
      REAL             YSPACE          ! spacing between y jiggle offsets

*  Local Data:
      DATA SUFFIX_STRINGS /'!_pht','p','_pht'/

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Initialisation
      MAP_N_PTR = 0
      MAP_N_PTR_END = 0

      USEFILE = .TRUE.
      PARABOLA = .TRUE.   ! Assume I can fit a parabola




*     start up the NDF system and read in the input demodulated file

      CALL NDF_BEGIN

      CALL NDF_ASSOC ('IN', 'READ', IN_NDF, STATUS)

*     Get the name of the filename associated with 'IN'

      CALL SCULIB_GET_FILENAME('IN', FNAME, STATUS)


*     Get bad bit mask
      CALL NDF_BB(IN_NDF, BADBIT, STATUS)

*     get the number of history records present in the file

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_HNREC (IN_NDF, NREC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            NREC = 0
         END IF

*     check that the mode and history of the input file are OK

         REDUCE_SWITCH = .FALSE.
         EXTINCTION = .FALSE.
         SKY_ERROR = .FALSE.
         PHOTOM = .FALSE.

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (IN_NDF, 'APPLICATION', I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP(:13) .EQ. 'REDUCE_SWITCH') THEN
                  REDUCE_SWITCH = .TRUE.
               ELSE IF (STEMP(:10) .EQ. 'EXTINCTION') THEN
                  EXTINCTION = .TRUE.
               ELSE IF (STEMP(:9) .EQ. 'SKY_ERROR' .OR.
     :                 STEMP(:6) .EQ. 'REMSKY') THEN
                  SKY_ERROR = .TRUE.
               ELSE IF (STEMP(:6) .EQ. 'PHOTOM') THEN
                  PHOTOM = .TRUE.
               END IF
            END DO
         END IF

         IF (STATUS .EQ. SAI__OK) THEN
            IF (.NOT. REDUCE_SWITCH) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: the '//
     :              'REDUCE_SWITCH application has not been run '//
     :              'on the input file', STATUS)
            END IF

            IF (.NOT. EXTINCTION) THEN
               CALL MSG_SETC('TASK', TSKNAME)
               CALL MSG_OUTIF (MSG__QUIET, ' ', '^TASK: Warning the '//
     :              'input data has not been corrected for extinction',
     :              STATUS)
            END IF

            IF (PHOTOM) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: the '//
     :              'PHOTOM application has already been run on '//
     :              'the input file', STATUS)
            END IF
         END IF
      END IF

*     get some general descriptive parameters of the observation

      CALL NDF_XLOC (IN_NDF, 'FITS', 'READ', IN_FITSX_LOC, STATUS)
      CALL NDF_XLOC (IN_NDF, 'SCUBA', 'READ', IN_SCUBAX_LOC, STATUS)
      CALL NDF_XLOC (IN_NDF, 'SCUCD', 'READ', IN_SCUCDX_LOC, STATUS)
      CALL NDF_XLOC (IN_NDF, 'REDS', 'READ', IN_REDSX_LOC, STATUS)

      CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: input file '//
     :           'contains too many FITS items', STATUS)
         END IF
      END IF
      CALL DAT_GET1C (IN_FITSX_LOC, SCUBA__MAX_FITS, FITS, N_FITS,
     :     STATUS)
      CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'OBSDEF',
     :     ODF_NAME, STATUS)
      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'RUN',
     :     RUN_NUMBER, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'OBJECT',
     :     OBJECT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'MODE',
     :     OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SAM_MODE', SAMPLE_MODE, STATUS)
      CALL CHR_UCASE (SAMPLE_MODE)

      IF (STATUS .EQ. SAI__OK) THEN

         IF (SAMPLE_MODE .NE. 'JIGGLE') THEN

            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: the file '//
     :           'does not contain data from a JIGGLE observation',
     :           STATUS)
         END IF

         IF (OBSERVING_MODE .NE. 'PHOTOM' .AND.
     :        OBSERVING_MODE .NE. 'POLPHOT') THEN
            CALL MSG_OUTIF(MSG__QUIET, ' ',
     :           'WARNING! This is not a photometry observation '//
     :           '-- please take care', STATUS)

         END IF
      END IF



*     report the run number and object of the observation

      CALL MSG_SETC ('OBJECT', OBJECT)
      CALL MSG_SETI ('RUN', RUN_NUMBER)
      CALL MSG_SETC ('MODE', OBSERVING_MODE)
      CALL MSG_SETC('PKG', PACKAGE)
      CALL MSG_OUTIF (MSG__NORM, ' ',
     :     '^PKG: run ^RUN was a ^MODE observation '//
     :     'of ^OBJECT', STATUS)

*     get the sub-instrument and filter used

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'SUB_1',
     :     SUB_INSTRUMENT, STATUS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'WAVE_1',
     :     WAVELENGTH, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'FILT_1',
     :     FILTER, STATUS)

*     get some other FITS items that will be needed

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'EXP_TIME', EXPOSURE_TIME, STATUS)
      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'N_BOLS',
     :     N_BOLS, STATUS)

*     coords of telescope centre

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'CENT_CRD', CENTRE_COORDS, STATUS)
      CALL CHR_UCASE (CENTRE_COORDS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LAT',
     :     LAT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LONG',
     :     LONG, STATUS)

      IF (CENTRE_COORDS .EQ. 'PLANET') THEN
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LAT2',
     :        LAT2, STATUS)
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LONG2',
     :        LONG2, STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 'MJD1',
     :        MJD1, STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 'MJD2',
     :        MJD2, STATUS)
      END IF

*     offset from telescope centre

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'MAP_X',
     :     MAP_X, STATUS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'MAP_Y',
     :     MAP_Y, STATUS)
      OFFSET_COORDS = 'UNKNOWN'

*     the UT of the observation expressed as modified Julian day

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'UTDATE',
     :     UTDATE, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'UTSTART',
     :     UTSTART, STATUS)

*     Check data dimensions
      CALL NDF_DIM (IN_NDF, MAX_DIM, DIM, NDIM, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :        OBSERVING_MODE .EQ. 'POLPHOT') THEN
            IF ((NDIM .NE. 3)                  .OR.
     :           (DIM(1) .NE. N_BOLS)           .OR.
     :           (DIM(2) .LT. 1)                .OR.
     :           (DIM(3) .NE. SCUBA__MAX_BEAM)) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: data array '//
     :              'has bad dimensions (^NDIM) ^DIM1, ^DIM2, ^DIM3',
     :              STATUS)
            END IF
         ELSE
            IF ((NDIM .NE. 2)                  .OR.
     :           (DIM(1) .NE. N_BOLS)           .OR.
     :           (DIM(2) .LT. 1))                THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: data array '//
     :              'has bad dimensions (^NDIM) ^DIM1, ^DIM2, ^DIM3',
     :              STATUS)
            END IF
         END IF
      END IF

      N_POS = DIM (2)


*     Map the input data
      CALL NDF_MAP (IN_NDF, 'QUALITY', '_UBYTE', 'READ', IN_Q_PTR,
     :     NELM, STATUS)
      CALL NDF_MAP (IN_NDF, 'DATA', '_REAL', 'READ', IN_D_PTR,
     :     NELM, STATUS)
      CALL NDF_MAP (IN_NDF, 'VARIANCE', '_REAL', 'READ', IN_V_PTR,
     :     NELM, STATUS)

*     map the DEM_PNTR array and check its dimensions

      CALL SCULIB_GET_DEM_PNTR(3, IN_SCUBAX_LOC,
     :     DEM_PNTR_PTR, ITEMP, N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS, STATUS)

*     unmap DEM_PNTR
      CALL CMP_UNMAP(IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)

*  see if the observation completed normally or was aborted

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'STATE',
     :  STATE, STATUS)
      CALL CHR_UCASE (STATE)
      ABORTED = .FALSE.
      IF (INDEX(STATE,'ABORTING') .NE. 0) THEN
         ABORTED = .TRUE.
      END IF

*     write out some information
      CALL MSG_SETI ('N_E', N_EXPOSURES)
      CALL MSG_SETI ('N_I', N_INTEGRATIONS)
      CALL MSG_SETI ('N_M', N_MEASUREMENTS)
      CALL MSG_SETC ('PKG', PACKAGE)


*     Everything okay
      IF (.NOT. ABORTED) THEN
         CALL MSG_OUTIF (MSG__NORM, ' ',
     :        '^PKG: file contains data for ^N_E '//
     :        'exposure(s) in ^N_I integrations(s) in ^N_M '//
     :        'measurement(s)', STATUS)

      ELSE

*  get the exposure, integration, measurement numbers at which the abort
*  occurred

         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'EXP_NO', LAST_EXP, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'INT_NO', LAST_INT, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'MEAS_NO', LAST_MEAS, STATUS)

         CALL MSG_OUTIF (MSG__NORM, ' ',
     :        '^PKG: the observation should have '//
     :        'had ^N_E exposure(s) in ^N_I integration(s) in ^N_M '//
     :        'measurement(s)', STATUS)
         CALL MSG_SETI ('N_E', LAST_EXP)
         CALL MSG_SETI ('N_I', LAST_INT)
         CALL MSG_SETI ('N_M', LAST_MEAS)
         CALL MSG_OUTIF (MSG__NORM, ' ',
     :        ' - However, the observation was '//
     :        'ABORTED during exposure ^N_E of integration ^N_I '//
     :        'of measurement ^N_M', STATUS)

*     For an aborted observation we can simply reduce the number of
*     integrations that are processed. Can do this since the data
*     are just processed in order, looping over N_INTGRATIONS
*     and N_BEAMS
         N_INTEGRATIONS = LAST_INT
         N_MEASUREMENTS = LAST_MEAS

      END IF



      IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :     OBSERVING_MODE .EQ. 'POLPHOT') THEN
*     get the target indices of the bolometers in the data array
         CALL CMP_GETVI (IN_SCUBAX_LOC, 'PHOT_BB', SCUBA__MAX_BEAM,
     :        PHOT_BB, NELM, STATUS)


*     and the associated weights

         CALL CMP_GETVR (IN_REDSX_LOC, 'BEAM_WT', SCUBA__MAX_BEAM,
     :        BEAM_WEIGHT, NELM, STATUS)

      ELSE

*     If this is a map observation then I need to use beam 1
         DO I = 1, SCUBA__MAX_BEAM
            BEAM_WEIGHT(I) = 1.0
            PHOT_BB(I) = 0.0
         END DO
         PHOT_BB(1) = 1  ! Select a beam 1 (any bolometer)

      END IF

*     get the channel and ADC numbers of the bolometers used

      CALL CMP_GET1I (IN_SCUBAX_LOC, 'BOL_CHAN',
     :     SCUBA__NUM_CHAN * SCUBA__NUM_ADC, BOL_CHAN, ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (ITEMP .NE. N_BOLS) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: dimension '//
     :           'of .SCUBA.BOL_CHAN does not match main data array',
     :           STATUS)
         END IF
      END IF

      CALL CMP_GET1I (IN_SCUBAX_LOC, 'BOL_ADC',
     :     SCUBA__NUM_CHAN * SCUBA__NUM_ADC, BOL_ADC, ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (ITEMP .NE. N_BOLS) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: dimension '//
     :           'of .SCUBA.BOL_ADC does not match main data array',
     :           STATUS)
         END IF
      END IF

*     Now read in the jiggle pattern itself

      WRITEMAP = .TRUE.

      CALL SCULIB_GET_JIGGLE(IN_SCUCDX_LOC, SCUBA__MAX_JIGGLE,
     :     N_FITS, FITS, JIGGLE_COUNT, JIGGLE_REPEAT,
     :     JIGGLE_P_SWITCH, SAMPLE_PA, SAMPLE_COORDS, JIGGLE_X,
     :     JIGGLE_Y, STATUS)

*     find out if the jiggle pattern corresponds to a rectangular grid

      IF (STATUS .EQ. SAI__OK) THEN
         LBND (1) = 1
         LBND (2) = 1

*     There seems to be a problem with MAP jiggls returning zero
*     from this routine.

         CALL SCULIB_CALC_GRID (JIGGLE_COUNT, JIGGLE_X, JIGGLE_Y,
     :        XMIN, XMAX, XSPACE, UBND(1), YMIN, YMAX, YSPACE,
     :        UBND(2), IPOS, JPOS, STATUS)


         IF (ABS(YSPACE) .LT. 1.0E-5 .AND. ABS(XSPACE) .LT. 1.0E-5) THEN
*     Zero jiggle
*     Lots of this is now done in SCULIB_CALC_GRID
            IF (STATUS .NE. SAI__OK) CALL ERR_ANNUL (STATUS)

            N_OBSDIM = 1
            UBND (1) = JIGGLE_COUNT
            UBND(2) = 1

            DO I = 1, JIGGLE_COUNT
               IPOS(I) = 0
               JPOS(I) = 0
            END DO

*     Tweak the message if not a PHOTOM observation

            CALL MSG_SETC('PKG', PACKAGE)
            IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :           OBSERVING_MODE .EQ. 'POLPHOT') THEN

               CALL MSG_OUTIF (MSG__NORM, ' ',
     :              '^PKG: No jiggle pattern was used. '//
     :              'No images will be stored', STATUS)
            ELSE
               CALL MSG_OUTIF(MSG__NORM, ' ',
     :              '^PKG: No images will be stored', STATUS)
            END IF

            WRITEMAP = .FALSE.
            PARABOLA = .FALSE.

         ELSE IF (STATUS .NE. SAI__OK .OR.
     :           (ABS(XSPACE/YSPACE - 1.0) .GT. 0.001)) THEN
            IF (STATUS .NE. SAI__OK) CALL ERR_ANNUL (STATUS)

            N_OBSDIM = 1
            UBND (1) = JIGGLE_COUNT
            UBND(2) = 1
*     Reset jiggle positions for unpack_jiggle_separates
            DO I = 1, JIGGLE_COUNT
               IPOS(I) = I
               JPOS(I) = 1
            END  DO

            CALL MSG_SETC('PKG', PACKAGE)
            CALL MSG_OUTIF (MSG__NORM, ' ',
     :           '^PKG: the jiggle pattern does '//
     :           'not fit a rectangular mesh, strip images will be '//
     :           'stored', STATUS)
         ELSE

*     construct axes for 2D map

            N_OBSDIM = 2

            DO I = 1, UBND (1)
               JIGGLE_2D_A1 (I) = XMIN + REAL (I-1) * XSPACE
            END DO
            DO I = 1, UBND (2)
               JIGGLE_2D_A2 (I) = YMIN + REAL (I-1) * YSPACE
            END DO

         END IF
      END IF

*     Ask for the reduction method
*     Do not include PARABOLA if we are not allowed to fit
*     parabolas (ie a zero offset jiggle is being used)

      STEMP = 'Average,Samples'
      IF (PARABOLA) THEN
         IPOSN = CHR_LEN(STEMP)
         CALL CHR_APPND(',Parabola', STEMP, IPOSN)
      END IF

      CALL PAR_CHOIC('ANALYSIS', 'AVERAGE', STEMP,
     :        .TRUE., ANALYSIS_METHOD, STATUS)

*     Generate a default name for the output file
      CALL SCULIB_CONSTRUCT_OUT(FNAME, SUFFIX_ENV, SCUBA__N_SUFFIX,
     :     SUFFIX_OPTIONS, SUFFIX_STRINGS, OUTFILE, STATUS)

*     set the default
      CALL PAR_DEF0C('OUT', OUTFILE, STATUS)

*     create the output file that will contain the reduced data in NDFs

      CALL PAR_GET0C ('OUT', OUT, STATUS)

      HDSNAME = OUT(1:DAT__SZNAM)
      CALL HDS_NEW (OUT, HDSNAME, 'SURF_SCUPHOT', 0, 0, OUT_LOC, STATUS)

*     Get some work space
      DO BEAM = 1, SCUBA__MAX_BEAM
         IF (PHOT_BB(BEAM) .NE. 0) THEN

*     get some scratch memory to hold the data for the target bolometers
            INT_D_PTR(BEAM) = 0
            INT_D_END(BEAM) = 0
            INT_V_PTR(BEAM) = 0
            INT_V_END(BEAM) = 0
            INT_Q_PTR(BEAM) = 0
            INT_Q_END(BEAM) = 0

            CALL SCULIB_MALLOC (N_POS * VAL__NBR, INT_D_PTR(BEAM),
     :           INT_D_END(BEAM), STATUS)
            CALL SCULIB_MALLOC (N_POS * VAL__NBR, INT_V_PTR(BEAM),
     :           INT_V_END(BEAM), STATUS)
            CALL SCULIB_MALLOC (N_POS * VAL__NBUB, INT_Q_PTR(BEAM),
     :           INT_Q_END(BEAM), STATUS)
         END IF
      END DO


*     Now fix PHOT_BB so that it gives me the bolometer I want
*     PHOT_BB contains the bolometer number in this sub instrument
*     Only 3 numbers and we only want to specify middle beam if
*     processing all of them

*     If this is is not a PHOTOM observation I have to use ALLBOLS

      ALL = 2  ! This is the beam of choice for ALLBOLS

      IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :     OBSERVING_MODE .EQ. 'POLPHOT') THEN
         CALL PAR_GET0L('ALLBOLS', ALL_BOLS, STATUS)
      ELSE
         ALL_BOLS = .TRUE.
      END IF

      IF (ALL_BOLS) THEN
         PHOT_BB(1) = 0
         PHOT_BB(3) = 0

         START_BOL = 1
         END_BOL   = N_BOLS

         CALL MSG_SETC('TASK', TSKNAME)
         CALL MSG_OUTIF(MSG__QUIET,' ','^TASK: All bolometers selected',
     :        STATUS)

         IF (OBSERVING_MODE .NE. 'PHOTOM' .AND.
     :        OBSERVING_MODE .NE. 'POLPHOT') THEN
*     Want to use the first beam
            PHOT_BB(2) = 0
            PHOT_BB(3) = 0
            ALL = 1

         END IF

      ELSE

         START_BOL = PHOT_BB(2)
         END_BOL   = PHOT_BB(2)

      END IF

*     If we are using SAMPLES analysis mode we want to keep all
*     N_POS else numbers else we want to keep N_INTEGRATIONS

      IF (ANALYSIS_METHOD .EQ. 'SAMPLES') THEN
         N_BITS = N_POS
         ANALYSIS = 'AVERAGE' ! So that bol_map works
         USEFILE = .FALSE.   ! Dont write text file
      ELSE
         ANALYSIS = ANALYSIS_METHOD
         N_BITS = N_INTEGRATIONS * N_MEASUREMENTS
      END IF


*     Now loop over the requisite number of bolometers

      COUNT = 0
      DO PHOTBB = START_BOL, END_BOL

         COUNT = COUNT + 1
         PHOT_BB(ALL) = PHOTBB

*     Extract data for each targeted bolometer

         DO BEAM = 1, SCUBA__MAX_BEAM
            IF (PHOT_BB(BEAM) .NE. 0) THEN

*     copy the integration data from the relevant bolometer into the
*     temporary space - clumsy method

               IF (STATUS .EQ. SAI__OK) THEN
                  DO POS = 1, N_POS
                     IN_OFFSET = (BEAM-1) * N_POS * N_BOLS + (POS-1) *
     :                    N_BOLS + (PHOT_BB(BEAM) - 1)
                     OUT_OFFSET = POS - 1

                     CALL VEC_RTOR(.FALSE., 1,
     :                             %VAL(CNF_PVAL(IN_D_PTR) + IN_OFFSET
     :                    * VAL__NBR),
     :   %VAL(CNF_PVAL(INT_D_PTR(BEAM)) + OUT_OFFSET
     :                    * VAL__NBR), IERR, NERR, STATUS)
                     CALL VEC_RTOR(.FALSE., 1,
     :                             %VAL(CNF_PVAL(IN_V_PTR) + IN_OFFSET
     :                    * VAL__NBR),
     :   %VAL(CNF_PVAL(INT_V_PTR(BEAM)) + OUT_OFFSET
     :                    * VAL__NBR), IERR, NERR, STATUS)
                     CALL VEC_UBTOUB(.FALSE., 1,
     :                               %VAL(CNF_PVAL(IN_Q_PTR) +
     :                    IN_OFFSET * VAL__NBUB),
     :                    %VAL(CNF_PVAL(INT_Q_PTR(BEAM)) + OUT_OFFSET *
     :                    VAL__NBUB), IERR, NERR, STATUS)

                  END DO
               END IF
            END IF
         END DO

*     cycle through the beams used in this sub-instrument

         IF (STATUS .EQ. SAI__OK) THEN

            DO BEAM = 1, SCUBA__MAX_BEAM
               MEAS_1_Q (BEAM) = 1
               MEAS_2_N (BEAM) = 0
               MEAS_2_Q (BEAM) = 0

*     Reset the coadd count and the coadd quality, else
*     SCULIB_COADD will get confused. Do not need to reset
*     MEAS_V and MEAS_D since MEAS_N=0 indicates no input data
               DO POS = 1, SCUBA__MAX_JIGGLE
                  MEAS_N (POS,BEAM) = 0
                  MEAS_Q (POS,BEAM) = 0
               END DO

               IF (PHOT_BB(BEAM) .NE. 0) THEN

*     this sub-instrument / beam combination was used, so analyse
*     and store the data

                  CALL NDF_BEGIN

*     Only if I am writing the JIGGLE map
                  IF (WRITEMAP) THEN

*     first, create the NDF to hold the map data, called <bol>_map

                     CALL SCULIB_BOLNAME (BOL_ADC(PHOT_BB(BEAM)),
     :                    BOL_CHAN(PHOT_BB(BEAM)), NDF_NAME, STATUS)
                     NDF_NAME = NDF_NAME(:CHR_LEN(NDF_NAME))//'_map'

*     Get an identifier to an NDF section of the input file
                     CALL NDF_SECT(IN_NDF, N_OBSDIM, LBND, UBND,
     :                    SEC_NDF, STATUS)

                     CALL NDF_PLACE (OUT_LOC, NDF_NAME, PLACE, STATUS)

*     Propogate from input NDF
                     CALL NDF_SCOPY(SEC_NDF,
     :                    'TITLE,UNITS,NOEXTENSION(SCUBA,SCUCD)',
     :                    PLACE, IBEAM, STATUS)

*     Unmap the section
                     CALL NDF_ANNUL(SEC_NDF, STATUS)

*     Map the output data
                     CALL NDF_MAP (IBEAM, 'QUALITY', '_UBYTE', 'WRITE',
     :                    MAP_Q_PTR, NELM, STATUS)
                     CALL NDF_MAP (IBEAM, 'DATA', '_REAL', 'WRITE/ZERO',
     :                    MAP_D_PTR, NELM, STATUS)
                     CALL NDF_MAP (IBEAM, 'VARIANCE','_REAL',
     :                    'WRITE/ZERO', MAP_V_PTR, NELM, STATUS)


*     initialise quality to bad

                     IF (STATUS .EQ. SAI__OK) THEN
                        CALL SCULIB_CFILLB (NELM, 1,
     :                                      %VAL(CNF_PVAL(MAP_Q_PTR)))
                     END IF

*     construct axes

                     IF (N_OBSDIM .EQ. 1) THEN
                        CALL NDF_AMAP (IBEAM, 'CENTRE', 1, '_REAL',
     :                       'WRITE', TEMP_PTR, NELM, STATUS)
                        IF (STATUS .EQ. SAI__OK) THEN
                           CALL SCULIB_NFILLR (JIGGLE_COUNT,
     :                          %VAL(CNF_PVAL(TEMP_PTR)))
                        END IF
                        CALL NDF_AUNMP (IBEAM, 'CENTRE', 1, STATUS)
                     ELSE IF (N_OBSDIM .EQ. 2) THEN
                        CALL NDF_AMAP (IBEAM, 'CENTRE', 1, '_REAL',
     :                       'WRITE', TEMP_PTR, NELM, STATUS)
                        CALL VEC_RTOR(.FALSE., UBND(1), JIGGLE_2D_A1,
     :                       %VAL(CNF_PVAL(TEMP_PTR)),
     :                       IERR, NERR, STATUS)
                        CALL NDF_AUNMP (IBEAM, 'CENTRE', 1, STATUS)

                        CALL NDF_AMAP (IBEAM, 'CENTRE', 2, '_REAL',
     :                       'WRITE', TEMP_PTR, NELM, STATUS)
                        CALL VEC_RTOR(.FALSE., UBND(2), JIGGLE_2D_A2,
     :                       %VAL(CNF_PVAL(TEMP_PTR)),
     :                       IERR, NERR, STATUS)
                        CALL NDF_AUNMP (IBEAM, 'CENTRE', 2, STATUS)
                     END IF

*     set the beam weight

                     IF (STATUS .EQ. SAI__OK) THEN
                        CALL NDF_XLOC (IBEAM, 'REDS', 'UPDATE',
     :                       OUT_REDSX_LOC, STATUS)
                        IF (STATUS .NE. SAI__OK) THEN
                           CALL ERR_ANNUL (STATUS)
                           CALL NDF_XNEW (IBEAM,'REDS','SURF_EXTENSION',
     :                          0, 0, OUT_REDSX_LOC, STATUS)
                        END IF
                        CALL CMP_MOD (OUT_REDSX_LOC, 'BEAM_WT', '_REAL',
     :                       0, 0, STATUS)
                        CALL CMP_PUT0R (OUT_REDSX_LOC, 'BEAM_WT',
     :                       BEAM_WEIGHT(BEAM), STATUS)
                        CALL DAT_ANNUL (OUT_REDSX_LOC, STATUS)
                     END IF

                  END IF


*     now create the NDF to hold the fitted peaks for each integration,
*     called <bol>_peak

                  CALL SCULIB_BOLNAME (BOL_ADC(PHOT_BB(BEAM)),
     :                 BOL_CHAN(PHOT_BB(BEAM)), NDF_NAME, STATUS)
                  NDF_NAME = NDF_NAME(:CHR_LEN(NDF_NAME))//'_peak'

                  CALL NDF_SECT(IN_NDF, 1, 1, N_BITS, SEC_NDF, STATUS)

                  CALL NDF_PLACE (OUT_LOC, NDF_NAME, PLACE, STATUS)

*     Propogate units
                  CALL NDF_SCOPY(SEC_NDF,
     :                 'LABEL,TITLE,UNITS,NOEXTENSION(SCUBA,SCUCD)',
     :                 PLACE, IPEAK, STATUS)

                  CALL NDF_ANNUL(SEC_NDF, STATUS)

*     Map output data
                  CALL NDF_MAP (IPEAK, 'QUALITY', '_UBYTE', 'WRITE',
     :                 PEAK_Q_PTR, NELM, STATUS)
                  CALL NDF_MAP (IPEAK, 'DATA', '_REAL', 'WRITE/ZERO',
     :                 PEAK_D_PTR, NELM, STATUS)
                  CALL NDF_MAP (IPEAK, 'VARIANCE', '_REAL','WRITE/ZERO',
     :                 PEAK_V_PTR, NELM, STATUS)

*     construct axis

                  CALL NDF_AMAP (IPEAK, 'CENTRE', 1, '_INTEGER',
     :                 'WRITE', TEMP_PTR, NELM, STATUS)
                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL SCULIB_NFILLI (N_BITS,
     :                                   %VAL(CNF_PVAL(TEMP_PTR)))
                  END IF
                  CALL NDF_AUNMP (IPEAK, 'CENTRE', 1, STATUS)

*     set the beam weight

                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL NDF_XLOC (IPEAK, 'REDS', 'UPDATE',
     :                    OUT_REDSX_LOC, STATUS)
                     IF (STATUS .NE. SAI__OK) THEN
                        CALL ERR_ANNUL (STATUS)
                        CALL NDF_XNEW (IPEAK, 'REDS', 'SURF_EXTENSION',
     :                       0, 0, OUT_REDSX_LOC, STATUS)
                     END IF
                     CALL CMP_MOD (OUT_REDSX_LOC, 'BEAM_WT', '_REAL',
     :                    0, 0, STATUS)
                     CALL CMP_PUT0R (OUT_REDSX_LOC, 'BEAM_WT',
     :                    BEAM_WEIGHT(BEAM), STATUS)
                     CALL DAT_ANNUL (OUT_REDSX_LOC, STATUS)
                  END IF

*     If samples then axes are different

                  IF (ANALYSIS_METHOD .EQ. 'SAMPLES') THEN
                     CALL NDF_ACPUT('Samples',IPEAK,'LABEL',1,STATUS)
                     CALL NDF_CPUT('Signal',IPEAK, 'LAB', STATUS)
                  ELSE
                     CALL NDF_ACPUT('Integrations',IPEAK,'LABEL',1,
     :                    STATUS)
                     CALL NDF_CPUT('Fitted peak',IPEAK, 'LAB', STATUS)
                  END IF

*     Put on some axis information and labels

                  IF (WRITEMAP) THEN
                     IF (N_OBSDIM.GT.1) THEN
                        CALL NDF_ACPUT('X-offset',IBEAM,'LABEL',1,
     :                       STATUS)
                        CALL NDF_ACPUT('Y-offset',IBEAM,'LABEL',2,
     :                       STATUS)
                        CALL NDF_ACPUT('arcsec',IBEAM,'UNITS',1,STATUS)
                        CALL NDF_ACPUT('arcsec',IBEAM,'UNITS',2,STATUS)
                     END IF
                     CALL NDF_CPUT('Coadd jiggle map',IBEAM, 'LAB',
     :                    STATUS)
                  END IF

*     initialise quality to bad

                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL SCULIB_CFILLB (NELM, 1,
     :                                   %VAL(CNF_PVAL(PEAK_Q_PTR)))
                  END IF

*     Cycle through measurements
                  DO MEAS = 1, N_MEASUREMENTS

*     cycle through the integrations coadding them as required

                     DO INT = 1, N_INTEGRATIONS

*     Calculate current integration number
                        NUM_INTS = INT + ((MEAS - 1) * N_INTEGRATIONS)

*     calculate the offset (taking measurements into account)

                        INT_OFFSET = (NUM_INTS - 1) * JIGGLE_COUNT

*     coadd the jiggle data for the integration into that for the measurement

                        CALL SCULIB_COADD (JIGGLE_COUNT,
     :                       %VAL(CNF_PVAL(INT_D_PTR(BEAM))
     :                       + INT_OFFSET*VAL__NBR),
     :                       %VAL(CNF_PVAL(INT_V_PTR(BEAM))
     :                       + INT_OFFSET*VAL__NBR),
     :                       %VAL(CNF_PVAL(INT_Q_PTR(BEAM))
     :                       + INT_OFFSET*VAL__NBUB),
     :                       MEAS_D(1,BEAM), MEAS_V(1,BEAM),
     :                       MEAS_Q(1,BEAM), MEAS_N(1,BEAM),
     :                       MEAS_D(1,BEAM), MEAS_V(1,BEAM),
     :                       MEAS_Q(1,BEAM), MEAS_N(1,BEAM), BADBIT,
     :                       .TRUE., STATUS)

*     derive the measured signal for this integration

                        CALL SCULIB_ANALYSE_PHOTOM_JIGGLE (ANALYSIS,
     :                       1, 1, JIGGLE_COUNT, JIGGLE_X, JIGGLE_Y,
     :                       %VAL(CNF_PVAL(INT_D_PTR(BEAM))
     :                       + INT_OFFSET*VAL__NBR),
     :                       %VAL(CNF_PVAL(INT_V_PTR(BEAM))
     :                       + INT_OFFSET*VAL__NBR),
     :                       %VAL(CNF_PVAL(INT_Q_PTR(BEAM))
     :                       + INT_OFFSET*VAL__NBUB),
     :                       PEAK_D(NUM_INTS,BEAM),
     :                       PEAK_V(NUM_INTS,BEAM),
     :                       PEAK_Q(NUM_INTS,BEAM), RTEMP, RTEMP,
     :                       PEAK_X(NUM_INTS,BEAM),
     :                       PEAK_Y(NUM_INTS,BEAM), BADBIT, STATUS)

*     coadd the fitted peak into the running mean

                        CALL SCULIB_COADD (1,
     :                       PEAK_D(NUM_INTS,BEAM),
     :                       PEAK_V(NUM_INTS,BEAM),
     :                       PEAK_Q(NUM_INTS,BEAM),
     :                       MEAS_2_D(BEAM), MEAS_2_V(BEAM),
     :                       MEAS_2_Q(BEAM), MEAS_2_N(BEAM),
     :                       MEAS_2_D(BEAM), MEAS_2_V(BEAM),
     :                       MEAS_2_Q(BEAM), MEAS_2_N(BEAM),
     :                       BADBIT, .TRUE., STATUS)

                     END DO
                  END DO

*     derive the measured signal from the coadded measurement

                  IF (PARABOLA) THEN
                     CALL SCULIB_ANALYSE_PHOTOM_JIGGLE ('PARABOLA',
     :                    1, 1, JIGGLE_COUNT, JIGGLE_X, JIGGLE_Y,
     :                    MEAS_D(1,BEAM), MEAS_V(1,BEAM),
     :                    MEAS_Q(1,BEAM), MEAS_1_D(BEAM),
     :                    MEAS_1_V(BEAM), MEAS_1_Q(BEAM),
     :                    MEAS_1_A0(BEAM), MEAS_1_A1(BEAM),
     :                    MEAS_1_X(BEAM), MEAS_1_Y(BEAM),
     :                    BADBIT, STATUS)
                  END IF


*     Only if I am writing the jiggle map

                  IF (WRITEMAP) THEN
*     Initialise the scratch array for UNPACK_JIGGLE_SEPARATES

                     CALL SCULIB_MALLOC(UBND(1)*UBND(2) * VAL__NBI,
     :                    MAP_N_PTR, MAP_N_PTR_END, STATUS)


*     store the coadded measurement to the output map

                     IF (STATUS .EQ. SAI__OK) THEN
                        CALL SCULIB_UNPACK_JIGGLE_SEPARATES (
     :                       JIGGLE_COUNT, 1, MEAS_D(1,BEAM),
     :                       MEAS_V(1,BEAM),
     :                       MEAS_Q(1,BEAM), 1, JIGGLE_COUNT, IPOS,
     :                       JPOS, UBND(1), UBND(2),
     :                       %VAL(CNF_PVAL(MAP_D_PTR)),
     :                       %VAL(CNF_PVAL(MAP_V_PTR)),
     :                       %VAL(CNF_PVAL(MAP_Q_PTR)),
     :                       %VAL(CNF_PVAL(MAP_N_PTR)), ITEMP, BADBIT,
     :                       STATUS)
                     END IF

                     CALL SCULIB_FREE ('MAP_N', MAP_N_PTR,MAP_N_PTR_END,
     :                    STATUS)
                  END IF

*     store the fitted peak values to the output ndf
*     If we are storing samples then just copy the whole thing

                  IF (ANALYSIS_METHOD .EQ. 'SAMPLES') THEN

                     CALL VEC_RTOR(.FALSE., N_BITS,
     :                    %VAL(CNF_PVAL(INT_D_PTR(BEAM))),
     :                    %VAL(CNF_PVAL(PEAK_D_PTR)),
     :                    IERR, NERR, STATUS)
                     CALL VEC_RTOR(.FALSE., N_BITS,
     :                    %VAL(CNF_PVAL(INT_V_PTR(BEAM))),
     :                    %VAL(CNF_PVAL(PEAK_V_PTR)),
     :                    IERR, NERR, STATUS)
                     CALL VEC_UBTOUB(.FALSE., N_BITS,
     :                    %VAL(CNF_PVAL(INT_Q_PTR(BEAM))),
     :                    %VAL(CNF_PVAL(PEAK_Q_PTR)),
     :                    IERR, NERR, STATUS)

*     Now set the bad pixel mask from the input
                     CALL NDF_SBB(BADBIT, IPEAK, STATUS)

                  ELSE
*     Store the fitted peak
                     CALL VEC_RTOR(.FALSE.,
     :                    N_INTEGRATIONS * N_MEASUREMENTS,
     :                    PEAK_D(1,BEAM), %VAL(CNF_PVAL(PEAK_D_PTR)),
     :                    IERR, NERR,
     :                    STATUS)
                     CALL VEC_RTOR(.FALSE.,
     :                    N_INTEGRATIONS * N_MEASUREMENTS,
     :                    PEAK_V(1,BEAM), %VAL(CNF_PVAL(PEAK_V_PTR)),
     :                    IERR, NERR,
     :                    STATUS)
                     CALL VEC_UBTOUB(.FALSE.,
     :                    N_INTEGRATIONS * N_MEASUREMENTS,
     :                    PEAK_Q(1,BEAM), %VAL(CNF_PVAL(PEAK_Q_PTR)),
     :                    IERR, NERR,
     :                    STATUS)

*     Write BIT MASK

                     CALL NDF_SBB(OUTBAD, IPEAK, STATUS)
                     ISBAD = .FALSE.
                     DO I = 1, N_INTEGRATIONS * N_MEASUREMENTS
                        IF (PEAK_Q(I, BEAM).NE.0) ISBAD = .TRUE.
                     END DO

                     CALL NDF_SBAD(ISBAD, IPEAK, 'Data', STATUS)



                  END IF

                  IF (WRITEMAP) CALL NDF_SBB(OUTBAD, IBEAM, STATUS)

*     close the ndfs opened in this ndf context
                  CALL NDF_UNMAP(IPEAK, '*', STATUS)
                  CALL NDF_ANNUL(IPEAK, STATUS)
                  IF (WRITEMAP) THEN
                     CALL NDF_UNMAP(IBEAM, '*', STATUS)
                     CALL NDF_ANNUL(IBEAM, STATUS)
                  END IF

                  CALL NDF_END (STATUS)

               END IF
            END DO
         END IF

*       Open a file and write header if this is first time through

         IF (USEFILE .AND. COUNT .EQ. 1 .AND. STATUS .EQ. SAI__OK) THEN

            CALL SURF_WRITE_PHOTOM_HEADER(ODF_NAME, UTDATE, UTSTART,
     :           ANALYSIS, RUN_NUMBER, OBJECT, SUB_INSTRUMENT, FILTER,
     :           CENTRE_COORDS, LAT, LONG, LAT2, LONG2, MJD1, MJD2,
     :           OFFSET_COORDS, MAP_X, MAP_Y, SAMPLE_COORDS, SAMPLE_PA,
     :           SKY_ERROR, FD, STATUS)

            IF (STATUS .EQ. PAR__NULL) THEN
               CALL ERR_ANNUL(STATUS)
               USEFILE = .FALSE.
            ELSE IF (STATUS .NE. SAI__OK) THEN
               USEFILE = .FALSE.
            END IF

         END IF

*     write the results out to an ASCII file

         IF (USEFILE) THEN
            CALL SURF_WRITE_PHOTOM (FD, PARABOLA, SCUBA__MAX_BEAM,
     :           N_BOLS, BOL_CHAN, BOL_ADC, PHOT_BB, SCUBA__MAX_INT,
     :           N_MEASUREMENTS, N_INTEGRATIONS,
     :           PEAK_D, PEAK_V, PEAK_X, PEAK_Y, PEAK_Q, BEAM_WEIGHT,
     :           MEAS_1_D, MEAS_1_V, MEAS_1_X, MEAS_1_Y, MEAS_1_Q,
     :           MEAS_2_D, MEAS_2_V, MEAS_2_Q, STATUS)
         END IF

      END DO


*     finish off

*  close the file

      IF (USEFILE) CALL FIO_CLOSE (FD, STATUS)


*     free memory

      DO BEAM = 1, SCUBA__MAX_BEAM
         IF (PHOT_BB(BEAM) .NE. 0) THEN
            CALL SCULIB_FREE ('INT_D', INT_D_PTR(BEAM), INT_D_END(BEAM),
     :           STATUS)
            CALL SCULIB_FREE ('INT_V', INT_V_PTR(BEAM), INT_V_END(BEAM),
     :           STATUS)
            CALL SCULIB_FREE ('INT_Q', INT_Q_PTR(BEAM), INT_Q_END(BEAM),
     :           STATUS)
         END IF
      END DO

*     close the input file
      CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUCDX_LOC, STATUS)
      CALL DAT_ANNUL (IN_REDSX_LOC, STATUS)

      CALL NDF_ANNUL (IN_NDF, STATUS)

*     close the output file

      CALL DAT_ANNUL (OUT_LOC, STATUS)

*     and close down NDF

      CALL NDF_END (STATUS)

      END


      SUBROUTINE SHOW_ARRAY(N, DAT, QUAL)
      IMPLICIT NONE

      INTEGER N
      REAL DAT(N)
      BYTE QUAL(N)
      INTEGER I

      DO I = 1, N
         PRINT *, I, DAT(I), QUAL(I)
      END DO

      END
