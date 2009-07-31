      SUBROUTINE POINTCRDD( STATUS )
*+
*  Name:
*     POINTCRDD

*  Purpose:
*     Detects point sources along the detector trace(s) of CRDD NDF file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POINTCRDD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application is designed to detect point sources along the
*     data traces of CRDD or CRDD-like NDF's (e.g. the outputs of
*     COADDCRDD, etc. ) files.
*
*     The application will run in two modes, in non-automatic mode it will
*     prompt the user for an expected source position, by default this will 
*     be the reference position of the scan. If the user accepts or enters 
*     a position the search for the point sources will be performed along a 
*     segment of the detector trace nearest to this expected source position. 
*     The two values SCAN_NORTH and SCAN_SOUTH are the lengths north and south 
*     of the source position to be used. If there is more than one detector
*     the user is then also prompted for the detectors to use, which can
*     be nearest, choose or all. If the user selects choose the detectors and
*     their cross-scan distances will be presented and the user can accept or 
*     reject each. If the user enters null '!' to the source position the
*     application will assume the full scan length should be searched for
*     all detectors.
*
*     If the application is run in automatic mode, the application assumes
*     the reference position as the expected source position, the scan lengths
*     as 40 arcminutes each, and the nearest detector.
*
*     The source detection consists of the following steps
*     For each detector trace
*     i)   The overall noise is calculated for the scanlength specified
*          or the whole scan. This includes an iterative proceedure
*          which rejects samples more than THRESH_SD times the standard
*          deviation from the average.
*     ii)  A zero sum filter is applied sucessively around each sample
*          point. The filter adds a few samples centered at this point, where 
*          the strongest signal would be found if one was there, and subtracts
*          a compensating number of outlying samples, so that if there is
*          no signal the value would be approximately zero.
*     iii) The mean and standard deviation of the filter output is calculated,
*          again rejecting values more than THRESH_SD times the standard
*          deviation from average.
*     iv)  The filter values are examined and any which are THRESH_FILTER
*          times the standard deviation are selected as candidate point
*          source positions.
*     v)   For each candidate position the correlation coefficient between
*          the samples surrounding the position and the point source profile
*          for an average detector in that band is calculated. If the 
*          correlation coefficient is greater than THRESH_CORR, the candidate
*          is accepted.
*     vi)  The local noise around the candidate source is calculated. The 
*          range of samples used is two sections each of length NOISE_SMPL
*          on either side of a point source profile centered at the 
*          candidate position. Again the samples are iteratively rejected if 
*          they lie outside THRESH_SD standard deviations. If more than 25% 
*          of the sample are bad or rejected the range is extended to provide 
*          the neccessary 75%. 
*     vii) A final test of the candidate position is applied. The signal at
*          the position is compared with the local noise and it accepted as
*          a point source if it exceeds THRESH_S2N times the local noise.
*          If THRESH_S2N is given the null value '!' this test will not be
*          applied and all candidates will be reported.
*
*     The output results consist of three sections, the detection parameter
*     values used, details of the scan, and details of each source.
*     For each scan the details include the scan name, details of the expected
*     source position, scan length and detectors used, and the mean sample 
*     value and noise value calculated together with the number of samples
*     used in this calcuation. 
*     The information given for each point source includes its position, 
*     in-scan and cross-scan position relative to the expected point source 
*     position if given, average background, slope and local noise (including
*     the number of samples used in calculating the noise), and the 
*     signal to noise ratio and correlation coefficient. 
*
*  ADAM Parameters:
*     AUTOMATIC = LOGICAL (Read)
*        If AUTOMATIC is true the application assumes the expected source 
*        position is the reference position, the scan to be searched is 40 
*        arcmin on either side of this position, and the nearest detector 
*        is to be used.
*        if AUTO is false the user is prompted for these values.
*     COORDS = LITERAL (Read)
*        Sky coordinate system used to specify the expected point source 
*        position and to decribe the sky positions of the found point sources 
*        when reporting the detection results.
*        Valid values include ECLIPTIC, EQUATORIAL and GALACTIC.
*        See help on "Sky_coordinates" for more information on on available 
*        sky coordinate systems.
*        [current sky coordinate system]
*     DET_CHOICE = LITERAL (Read)
*        Allows the user to select how detectors are to be chosen, and 
*        can take the values all(A), nearest(N), or choose(C). If all is
*        entered all detectors are used, if nearest only the detector with 
*        the minimum crossscan distance from the expected source position
*        is used. If choice is entered then detectors are chosen as described 
*        under DET_REQ below. [nearest]
*     DET_REQ = LOGICAL (Read)
*        Allows the user to specify whether the detector whose details have
*        just been presented to him should be included for examination or not.
*        [TRUE]
*     IN = NDF (Read)
*        The name of the input CRDD or CRDD-like NDF file to be
*        searched for the point source. The specified file can hold any
*        of the supported forms of CRDD (eg. Survey, AO, etc).
*     LOGFILE = LITERAL (Read)
*        The name of the text file to which the detection result will
*        be written. No file will be created if a null value is supplied. 
*        [pointcrdd.log]
*     MSG_FILTER = LITERAL (Given)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").  [current message filter setting]
*     NOISE_SMPL = REAL (Read)
*        The number of samples required in calculating local noise. 
*        Two segments each of half this length on either side of a point 
*        source profile surrounding a candidate source position are used 
*        to calculate local noise.
*     PROFILES = NDF (Read)
*        An NDF holding in-scan point source profiles used as that
*        expected from an ideal point source when comparing the data of
*        the slected candidate sources with the ideal point source. 
*        The default value is the files "profiles.sdf" contained in
*        the main IRAS90 directory, which contains profiles taken from
*        the IRAS Catalogs and Atlases Explanatory Supplement, page
*        V-14. [ ]
*     SCAN_NORTH = REAL (Read)
*        The length of scan segment in arcmin north of the expected point 
*        source position to be searched for point sources and used in the 
*        whole scan noise estimate. If a null, '!' response is given,
*        the trace(s) will be searched from the beginning of that end.
*        [40.0] 
*     SCAN_SOUTH = REAL (Read)
*        The length of scan segment in arcmin south of the expected point 
*        source position to be searched for point sources and used in the 
*        whole scan noise estimate. If a null, '!' response is given,
*        the trace(s) will be searched from the beginning of that end.
*        [40.0] 
*     SOURCE_LAT = LITERAL (Read)
*        The sky latitude of the expected source position, in the coordinate 
*        system specified by the parameter COORDS (eg if COORDS is EQUATORIAL 
*        then SOURCE_LAT should be given the Declination of the position). 
*        See help on "Sky_coordinates" for the formats allowed for this value.
*        If a null, '!', response is given, the search for the point source
*        will be performed from the start to the end of all traces for all
*        detectors in the CRDD file.
*     SOURCE_LON = LITERAL (Read)
*        The sky longitude of the expected source position, in the coordinate 
*        system specified by the parameter COORDS (eg if COORDS is EQUATORIAL 
*        then SOURCE_LON should be given the Right Ascension of the position). 
*        See help on "Sky_coordinates" for the formats allowed for this value.
*        If a null, '!', response is given, the search for the point source
*        will be performed from the start to the end of all traces for all
*        detectors in the CRDD file.
*     THRESH_CORR = REAL (Read)
*        The threshold of the correlation coeficient of the detected candidate 
*        with the point source profile for this band. Above this threshold
*        a detected candidate would be accepted as a point source. See
*        IRAS Explanatory Supplement C. 4. for details about the selection 
*        of the correlation coeficient threshold. [0.87] 
*     THRESH_FILTER = REAL (Read)
*        The threshold of the signal to noise ratio above which a peak in the 
*        square-wave filter output will be taken as a candidate for a point 
*        source. See above for details of the square-wave filtering, and
*        thresholding. [2.0]
*     THRESH_SD = REAL (Read)
*        The threshold used in rejcting samples in the iterative calculation
*        of various noise values, samples which fall above this threshold times
*        the standard deviation will be rejected in the next iteration. See 
*        details of the processing above for its use. [2.5]
*     THRESH_S2N = REAL (Read)
*        The threshold of the signal to local noise ration at which a candidate
*        should be accepted as a point source. If this is entered as null this
*        test is not performed and all candidates are reported. [2.5]
*     UNITS = LITERAL (Read)
*        The units in which the data values are to be displayed. See
*        help on "Data_units" for a list of the available units.  [Jy]

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DCP: Diana Parsons (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     6-JAN-1993 (WG):
*        Original version.
*     29-SEP-1994 (DCP):
*        Rewrite of main routine and much of subsequent processing
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 package constants
      INCLUDE 'IRA_PAR'          ! IRA package constants
      INCLUDE 'MSG_PAR'          ! MSG package constants
      INCLUDE 'PRM_PAR'          ! Primitive constants
      INCLUDE 'PAR_ERR'          ! Parameter error constants
                                      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants
      INTEGER MAXCRD             ! Maximum number of files in group
				 ! ( 0 = No limit )
      PARAMETER ( MAXCRD = 0 )
      INTEGER MAXDET             ! Maximum number of detectors
      PARAMETER ( MAXDET = I90__MAXDT  )
      INTEGER MAXSRC             ! Max. number of point sources 
      PARAMETER ( MAXSRC = 200 )
      INTEGER MINSMP             ! Minimum number of of non bad, not over
				 ! threshold samples to be included in
				 ! noise calculation
      PARAMETER ( MINSMP = 15 )

*  Local Variables:
      INTEGER ACTSMP( 2 )        ! The first and last sample numbers of
				 ! the actual sample range used
      LOGICAL AUTO               ! TRUE when required to run in automatic mode.
      INTEGER BANDLU             ! The waveband last used 
      INTEGER BAND               ! Waveband of the current NDF
      INTEGER CANDN              ! Number of candidate point sources
      INTEGER CANDSM( MAXSRC )   ! Sample numbers of each point source candidate
      LOGICAL DETER2             ! TRUE if trace has too many bad val in poina6
      LOGICAL DETER3             ! TRUE if trace has too many bad val in poina8
      LOGICAL DETERR             ! TRUE if an error in getting detectors 
      LOGICAL DETEXS( MAXDET )   ! TRUE if the detector has an expected source
      INTEGER DETLBD             ! Lower limit of det index in current NDF
      INTEGER DETNUM( MAXDET )   ! Detector number for given detector index
      REAL DETNSM( MAXDET )      ! Nearest sample number to expected source
				 ! position for this detector
      LOGICAL DETREQ( MAXDET )   ! TRUE if this detector should be analysed
      REAL DETSCA( MAXDET )      ! Scale factor used in translating units for
				 ! this detector
      INTEGER DETSMP( MAXDET, 2 )  ! The lower and upper sample numbers that
				   ! should be considered in analysis.
      INTEGER DETUBD             ! Upper limit of det index in current NDF
      LOGICAL DETVAL( MAXDET )   ! TRUE if detector trace valid
      REAL DETXSC( MAXDET )      ! Cross_scan value - detector to exp. source 
      DOUBLE PRECISION EXPDEC    ! Dec of exp. source position in eq(b1950) 
      DOUBLE PRECISION EXPRA     ! RA of exp. source position in eq(b1950) 
      LOGICAL EXPSRC             ! TRUE if an exp. source is given or assumed
      REAL FMEAN                 ! Mean of filtered data in calc. noise
      INTEGER FNOBAD             ! No. bad samples in range of filt. noise calc.
      INTEGER FNOLRG             ! No. too large samples in range of Filt noise 
      INTEGER FNOSMP             ! No. of samples used in calculating Filt noise
      REAL FSTDEV                ! Filtered Noise value
      INTEGER GROUID             ! Input group ID for NDFs
      INTEGER ICRDD              ! Index of the current NDF within the group
      INTEGER IDC                ! IRC ID of the input CRDD file
      INTEGER IDET               ! Current detector index
      INTEGER INTLBD             ! The begining sample of the interpolated data 
      INTEGER INTUBD             ! The end sample of the interpolated data 
      INTEGER LOGFID             ! Logfile ID
      LOGICAL LOGREQ             ! TRUE when logfile is required.
      INTEGER NCRDD              ! Number of CRDD files in group
      INTEGER NDFID              ! NDF id of the input CRDD file
      INTEGER NOISMP             ! Number of samples to be included for each
				 ! side of expected source in calculation
				 ! of local noise.
      INTEGER OUTLEN             ! Length of temporary work arrays
      INTEGER OUTSMP( 2 )        ! The lower and upper sample numbers in range
				 ! analysed for current detector
      INTEGER PDATA              ! Pointer to the CRDD data array
      INTEGER PINTRD             ! Pointer to the interpolated data array
      INTEGER POUTDA             ! Pointer to th single det. CRDD data array
      INTEGER POUWAV             ! Pointer to filtered output data
      INTEGER PRFWID             ! Width of point source profile in samples
      INTEGER PPROF              ! Pointer to the point source profile
      DOUBLE PRECISION S1        ! Sum of constant 1. - used in profile
      LOGICAL S2NREQ             ! TRUE if signal to local noise test is req
      REAL SCLENN                ! Length of scan examined north of exp. source
      REAL SCLENS                ! Length of scan examined south of exp. source
      REAL SCALE                 ! Scale factor - convert data to desired units
      CHARACTER*( IRA__SZSCS )SCS  ! Users sky coordinate system 
      DOUBLE PRECISION SI        ! Sum of sample indices - in profile calc.
      DOUBLE PRECISION SIP       ! Sum of sample indices * profile samples
      DOUBLE PRECISION SISQ      ! Sum of squared sample indices - in profile 
      INTEGER SMPLBD             ! Lower limit of sample index in current NDF
      INTEGER SMPUBD             ! Upper limit of sample index in current NDF
      DOUBLE PRECISION SP        ! Sum of profile samples - in profile calc
      DOUBLE PRECISION SPSQ      ! Sum of squared profile samples - in profile
      INTEGER SRCN               ! Number of point sources in this det. scan
      REAL SRCAMP( MAXSRC )      ! The amplitude of the detected point sources.
      DOUBLE PRECISION SRCANG( MAXSRC ) ! The scan angle at each source position.
      REAL SRCBAS( MAXSRC )      ! Baseline height of detected point sources.
      REAL SRCCOR( MAXSRC )      ! Correlation coefficient for det point sources
      DOUBLE PRECISION SRCDEC( MAXSRC ) ! The DEC of the source position.
      REAL SRCINS( MAXSRC )        ! In-scan distance of source from exp. source
      INTEGER SRCNON( MAXSRC )   ! Number of samples used in calc. local noise
      REAL SRCNOS( MAXSRC )      ! Estimated local noise.
      DOUBLE PRECISION SRCRA( MAXSRC ) ! The RA of the source position.
      REAL SRCSLP( MAXSRC )      ! The slope of the data in the region
				 ! surrounding the source.
      REAL SRCSMP( MAXSRC )      ! Sample position of detected point sources.
      REAL SRCSPD( MAXSRC )        ! The scan speed at each source position.
      REAL THCORR                ! Threshold of the correlation coefficient
      REAL THFILT                ! Threshold of the square wave filter
				 ! signal to noise.
      REAL THSD                  ! Threshold for rejection of samples in
				 ! calculation of various noise values.
      REAL THS2N                 ! Threshold of signal to local noise at which
				 ! a candidate point source is accepted.
      CHARACTER*( 20 )  UNITS     ! Units in which data values are to be reported
      DOUBLE PRECISION V         ! Determinant of the symmetric matrix formed
				 ! by SPSQ, SIP, SP, SISQ, SI, S1.
      REAL WMEAN                 ! Whole trace mean value of the samples over
				 ! which the noise is calculated
      INTEGER WNOBAD             ! Number of bad samples in whole noise range 
      INTEGER WNOLRG             ! Number of samples with value over the
				 ! threshold in whole noise range 
      INTEGER WNOSMP             ! Number of samples used in calculating noise
      REAL WSTDEV                ! "Whole scan" noise
      LOGICAL WHLSCN             ! TRUE if whole scan is required.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      SCALE = 0.0
      INTUBD = 0
      INTLBD = 0

*  Initialisation
      CALL POINA0( 'AUTOMATIC', 'LOGFILE', 
     :             AUTO, BANDLU, LOGFID, LOGREQ, STATUS )

*  Obtain parameters such as coordinate system, thresholds, and local noise size
      CALL POINA1( 'NOISE_SMPL', 'COORDS', 'THRESH_CORR',
     :             'THRESH_FILTER', 'THRESH_SD', 'THRESH_S2N',
     :             LOGFID, LOGREQ, NOISMP, S2NREQ, SCS,
     :             THCORR, THFILT, THSD, THS2N, 
     :             STATUS )


*  Get a group of CRDD NDF files from the environment
      CALL IRM_RDNDF( 'IN', MAXCRD, 1, ' ', GROUID, NCRDD, STATUS )

*  Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  For each NDF
      DO ICRDD = 1, NCRDD

*  Get details of the input NDF, including the expected source position, 
*  length of scan to be searched, and map the NDF.
         CALL POINA2( 'SCAN_NORTH', 'SCAN_SOUTH', 'SOURCE_LAT', 
     :                'SOURCE_LON', AUTO, GROUID, ICRDD, LOGFID, 
     :                LOGREQ, SCS, BAND, DETLBD, DETUBD, EXPSRC, 
     :                EXPRA, EXPDEC, IDC, NDFID, PDATA, SCLENN, 
     :                SCLENS, SMPLBD, SMPUBD, WHLSCN, 
     :                STATUS )

* Get detectors required and calculate start and end sample to be included
* in noise calculations and point source search.
         CALL POINA3( 'DET_CHOICE', 'DET_REQ', 'UNITS', AUTO, BAND,
     :                DETLBD, DETUBD, EXPDEC, EXPRA, EXPSRC, IDC,
     :                LOGFID, LOGREQ, MAXDET, NDFID, SCLENN, SCLENS,
     :                SMPLBD, SMPUBD, WHLSCN, %VAL(PDATA),
     :                DETERR, DETEXS, DETNUM, 
     :                DETNSM, DETREQ, DETSCA, DETSMP, DETVAL, 
     :                DETXSC, UNITS, STATUS )

* Check whether the status is ok.
         IF ( STATUS .NE. SAI__OK ) GOTO 999
       
* Check whether all the detectors are erroneous and the scan should be skipped
         IF ( .NOT. DETERR ) THEN

* Check whether the waveband of the current NDF is the same as the band of 
* the last NDF and if not get the correct point source profile and correlation
* coefficients for the band.
            IF ( BAND .NE. BANDLU ) THEN

* If it is not the first time the profile has been obtained free the work 
* space for the point spread function allocated in POINC0 last time round
               IF ( BANDLU .NE. 0 ) CALL PSX_FREE( PPROF, STATUS )

* Get the point spread function for the new band including allocating an
* work space for it.
               CALL POINA4( 'PROFILES', BAND, PRFWID, PPROF, 
     :                       SPSQ, SIP, SP, SISQ, SI, S1, V,
     :                       STATUS )
               BANDLU = BAND
            END IF

* For each detector
            DO IDET = DETLBD, DETUBD
      
* Check whether the detector is required and valid
               IF ( DETREQ( IDET ) .AND. DETVAL( IDET ) ) THEN

* Copy the data for the detector to a single dimension array, starting at sample
* one, and determine the new bounds of the extent of scan to be searched in
* terms of these sample numbers.
* Allocate space for array
                  OUTLEN = SMPUBD - SMPLBD + 1
                  CALL PSX_CALLOC( OUTLEN, '_REAL', POUTDA, STATUS )

* Copy data
                  CALL POINA5( DETLBD, DETUBD, IDET, MAXDET,
     :                         SMPLBD, SMPUBD, %VAL(PDATA), DETSMP,
     :                         %VAL(POUTDA), OUTSMP, STATUS ) 

* Calculate the noise for the detector along either the whole scan or the
* length determined by SCAN_NORTH and SCAN_SOUTH, and removing outlying
* samples 
                  CALL POINA6( .FALSE., VAL__BADI, VAL__BADI, MINSMP,
     :                         OUTSMP, .TRUE., SMPLBD, SMPUBD, THSD,
     :                         %VAL(POUTDA), 
     :                         ACTSMP, DETER2, WMEAN, WNOBAD,
     :                         WNOLRG, WNOSMP, WSTDEV, STATUS )

* Check whether the current detector is ok or should be skipped
                  IF ( .NOT. DETER2 ) THEN

* Report whole scan noise
                     CALL POINA7( MAXDET, DETNUM, DETSCA, IDET,
     :                            LOGFID, LOGREQ,
     :                            UNITS, WNOSMP, WSTDEV, STATUS )

*  Create a temporary work space to contain the input data with bad samples
*  replaced by interpolation of adjoining values
                     CALL PSX_CALLOC( OUTLEN, '_REAL', PINTRD, STATUS )

*  Create a temporary work space to contain the square wave filter output
                     CALL PSX_CALLOC( OUTLEN, '_REAL', POUWAV, STATUS )

* Calculate an outwav array containing the square wave filter applied at
* each point in the data array. NB outwav(N) contains the result of applying
* the filter to samples detsmp( N ) to detsamp( N ) + 7 which is centered
* at detsmp( N ) + 4.5 
                     CALL POINA8( SMPLBD, SMPUBD, %VAL(POUTDA), OUTSMP,
     :                            DETER3, %VAL(PINTRD), %VAL(POUWAV),
     :                            STATUS )

* Check whether the current detector is ok or should be skipped
                     IF ( .NOT. DETER3 ) THEN

* Calculate the noise of the square wave filtered data ( NB the last
* 7 samples of the data do not have a valid filtered value and are counted
* in the bad valued samples)
                        CALL POINA6( .FALSE., VAL__BADI, VAL__BADI,
     :                               MINSMP, OUTSMP, .TRUE., SMPLBD,
     :                               SMPUBD, THSD, %VAL(POUWAV), 
     :                               ACTSMP, DETER2, FMEAN, FNOBAD,
     :                               FNOLRG, FNOSMP, FSTDEV, STATUS )

* Scan the square wave filtered data for candidate point sources
                        CALL POINA9( MAXSRC, FMEAN, FSTDEV, OUTSMP,
     :                               SMPLBD, SMPUBD, %VAL(POUWAV),
     :                               THFILT, CANDSM, CANDN, STATUS )

* Check whether there are any candidate sources
                        IF ( CANDN .GT. 0 ) THEN

* For each candidate source calculate the correlation coefficient between the
* samples around the position and the point source profile and test the value
* obtained against the correlation coefficient threshold. If the candidate
* point source passes this test, calculate the local noise from two regions
* either side of the pointsource profile around the source. If the signal to
* noise test required flag is set TRUE then the amplitude of the signal at the
* candidate is tested against the signal to noise threshold. If the candidate
* point source passes the required tests its data is stored for subsequent
* output.      
                           CALL POINB0( MAXSRC, BAND, CANDN, CANDSM,
     :                                  SMPLBD, SMPUBD, %VAL(PINTRD),
     :                                  NOISMP, PRFWID,%VAL( PPROF ),
     :                                  SMPLBD, SMPUBD, %VAL(POUTDA),
     :                                  SPSQ, SIP, SP, SISQ, SI, S1, V,
     :                                  S2NREQ, THCORR, THS2N, THSD,
     :                                  MINSMP, SRCAMP, SRCBAS, SRCCOR,
     :                                  SRCNON, SRCNOS, SRCSLP, SRCSMP,
     :                                  SRCN, STATUS )
      
* If there are any point sources
                           IF (SRCN .GT. 0 ) THEN

* Find the positional information associated with the point sources
                              CALL POINB1( IDC, IDET, MAXDET, MAXSRC,
     :                                     EXPSRC, SRCN, SRCSMP, DETNSM,
     :                                     SRCRA, SRCDEC, SRCINS, 
     :                                     SRCSPD, SRCANG, STATUS )

* Report the point sources to the user
                              CALL POINB2( MAXDET, MAXSRC, DETNUM,
     :                                     EXPSRC, IDET, LOGREQ, LOGFID,
     :                                     DETSCA( IDET ), SCS, SRCN,
     :                                     SRCAMP, SRCANG, SRCBAS, 
     :                                     SRCCOR, SRCDEC, SRCINS,
     :                                     SRCNON, SRCNOS, SRCRA,
     :                                     SRCSLP, UNITS, STATUS )

* If there are no valid point sources from the candidates
                           ELSE

* Report no point sources
                              CALL POINB3( MAXDET, DETNUM, IDET,
     :                                     LOGFID, LOGREQ, STATUS )

* End if for check on whether there are any valid point sources ( there were
* some candidates )
                           END IF

* If there are no candidate sources
                        ELSE

* Report no point sources
                           CALL POINB3( MAXDET, DETNUM, IDET,
     :                                  LOGFID, LOGREQ, STATUS )

* End if for check on whether current detector has any candidate sources
                        END IF

* Continuation point for if the detector is in error and all temporary working
* arrays have been allocated ( DETER3)
                     END IF

* Release the temporary working arrays.
                  CALL PSX_FREE( PINTRD, STATUS )
      	          CALL PSX_FREE( POUWAV, STATUS )                          

* Continuation point for if the detector is in error and only the data copy
* temporary working array has been allocated (DETER2 )
               END IF
      	       CALL PSX_FREE( POUTDA, STATUS )

* Write a message of the detector is in error
               IF ( DETER2 )
     :            CALL POINB4( MAXDET, DETNUM, IDET,
     :                         LOGREQ, LOGFID, STATUS )

* End if for is this detector required
               END IF
      
* End of for each detector loop
            END DO 
         
* End if for if whole NDF is in error ( DETERR is true)
         END IF

*  End of loop for each NDF
      END DO

* Free the work space for the point sread function allocated in POINC0
* fro the last time
      CALL PSX_FREE( PPROF, STATUS )

 999  CONTINUE


*  If an error occured then report context message
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POINTCRDD_ERR', 
     :   'POINTCRDD: Error in assessing point sources', STATUS )
      END IF

*  End the NDF context.
      CALL NDF_END( STATUS )

      END
