      SUBROUTINE COADDCRDD( STATUS )
*+
*  Name:
*     COADDCRDD

*  Purpose:
*     Coadd CRDD traces crossing specified positions.
 
*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL COADDCRDD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The application takes a group of CRDD NDF files of any form
*     (Survey, AO, etc) and coadds the data traces for any detector which 
*     crosses the specified expected point sources. Separate coadditions 
*     are carried out for each waveband for each source position.
*
*     Any input CRDD NDF files should have been destriped previously, in 
*     order to remove detector to detector differences in calibration.
*
*     The first stage of coadding is the selection of expected source 
*     positions. All the reference positions associated with any of the 
*     input CRDD NDFs will be taken as expected point sources by COADDCRDD. 
*     The user can also specify additional expected point source positions.
*
*     By default both the displayed reference positions, and any positions 
*     of subsequent sources the user may want to enter, are specified in 
*     Equatorial(B1950) coordinates. If the user wants a different coordinate
*     system  to be used he should enter the coordinate system he requires 
*     as the COORDS parameter.
*
*     To enter additional expected point sources positions, the user should
*     set the ADD_SOURCE parameter TRUE. He will then be prompted for the 
*     name, title, longitude and latitude of each additional expected source 
*     position in the given coordinate system. This facility enables the user
*     to examine several positions close together using only one set of 
*     extracted CRDD NDF files which cover all these positions. The user 
*     should be aware that apparent point sources in a coadded trace, but not
*     at the expected source position may be spurious. He should coadd
*     the traces again using the apparent position as an expected position,
*     to see whether this source is real.
*
*     The second stage is the selection and processing of the traces, this
*     is carried out for each source at each wavelength. The processing 
*     consists of selecting those detector traces to be used, determining
*     the weights to be applied to them, and aligning the trace correctly
*     so that the correct samples are aligned at the expected point source.
*     
*     By default, a detector scan is about 4.5 arcmin wide, see Explanatory
*     Supplement Table II.C.3 for exact width of each detector, and if a
*     source position falls within that field of view, the detector
*     scan is regarded as a crossing scan and will be coadded with other
*     crossing scans. The user can modify this criterion by selecting a
*     non zero detector width extension, see parameter WIDEXT. 
*
*     When coadding, the detector data can be weighted according to a
*     combination of one or two factors, dependant on the reliability of
*     the detectors and the distance of the crossing from the expected
*     source position. The reliability factor can follow one of the
*     following schemes, see parameter WEIGHT:
*
*     1. Variance weighting: If the input CRDD files contain variance
*        values associate with each CRDD sample, the samples can be
*        weighted according to their variance so that the greater weight
*        is given to the more accurate samples. Samples with zero
*        variance are treated like bad samples and hence excluded from
*        the coadding.
*
*     2. NEFD Weight: The data from each detector can also be weighted
*        according to the noise equivalent flux density (NEFD) of the
*        detector, obtained from Exp. Supp. Fig IV.A.1., so that the
*        most noisy detectors have the lowest weighting. 
*
*     3. Equal weighting: All data from the crossing detectors
*        contribute equally to the coadded result.
*
*     All above weighting schemes can be combined with a distance effect
*     if required. If the distance effect is selected the weight(s) of a 
*     data trace is taken to be the product of the weight(s) decided 
*     from one of above schemes and a weight which is a Gaussian function 
*     of the distance of the trace centre from the expected source position.
*     With this distance weight, the effect of the detector which crosses 
*     the expected source at its edge can be reduced. The Gaussian function 
*     can have different width by specifying its value at the edges of the
*     detectors, see parameter GVDTEG. If distance weighting is not required 
*     the parameter DISTANCE should be set false.
*
*     When the input CRDD files have variance components, the default
*     weighting will be 'distance-variance', otherwise the weighting 
*     will be 'distance-NEFD'.
*
*     For each crossing scan, a sequence of about 80.0 arcmin length is 
*     extracted centered on the expected source position, these are usually
*     sufficient for source signal and noise estimation. Should a user feel 
*     it is not adequate to his requirement, he can select another 
*     sequence length via parameter SCNLEN.
*
*     During coadding, the traces are aligned with the trace which is
*     closest to the expected source position. This consists of two phases.
*     The detector traces are reversed if they traversed the sky in the 
*     opposite direction to the direction of the closest scan. Then the
*     samples are aligned so that the samples crossing the source position 
*     coincide. The interpolation used when aligning the traces can be either 
*     'nearest' or 'linear'. In nearest interpolation the sample nearest to 
*     the source position is used as a central point. In linear interpolation
*     the central value is a linear combination of the two samples closest
*     to the source position and the values for each sample worked outwards
*     from this centre is also a linear combination of the corresponding samples
*     in the original trace. The user should note that although this then 
*     aligns samples by their in-scan distance from the source point, it does
*     not take account of the fact that scans may be crossing through the source
*     at slightly different angles. Therefore the coadding may add signals that
*     are in a slightly different position in the sky but at the same in-scan 
*     distance from the source. The effect can be neglected except if looking
*     for point sources not at the expected position, where they should be
*     recoadded (see above).
*
*     If more than half of the samples in a crossing trace are bad, the trace 
*     is discarded.
* 
*     The adjusted weighted samples are then coadded together. 
*
*     For each expected point source there will be one NDF file created 
*     for each waveband where there was at least one detector which crossed 
*     the source position. Each NDF will inherit the IRAS specific information 
*     of the detector trace whose centre is closest to the expected source 
*     position among all crossing traces coadded in that NDF. This means 
*     that the position associated with each sample is likely to be very 
*     slightly wrong. The data will in fact refer to a tracelike series of
*     positions passing through the source position and parallel to the 
*     nearest detector trace, while the associated positional data will give
*     in effect the position of the center of the nearest detector trace. The 
*     maximum error in position caused by this effect will be half the width 
*     of the detector (after allowance for the users width extension). 
*     Thus more detector traces coadded gives a better result not only in 
*     the data reliability, but also in the positional accuracy. And the 
*     positional inaccuracy is only commensurate with the data inaccuracies 
*     caused by sampling at the edge of a detector.
*
*     The reason for the use of this nearest detector inheritance is that 
*     it means that coadded NDF files can be processed by some other IRAS90 
*     applications, such as TRACECRDD in the same way as a normal CRDD NDF.
*     But the user should be aware that some IRAS specific information, e.g. 
*     SOP, OBS and detector number, and such figures as cross scan distance
*     reported by the applications may be irrelevant to data trace it contains. 
*     Thus TRACECRDD will report the correct reference position, a cross scan
*     value and positions in 'get data value' that may be out by the half 
*     detector width, an average point source profile for the waveband, and
*     a detector number which is not applicable.
*
*     Users should also be aware that they should not use coadded scans in
*     subsequent coadding or image preparation with eg MAPCRDD. This is because
*     of two things, firstly the positional inaccuracies noted above, and 
*     secondly that since the coadded scan already contains processed weighting
*     applying a second weighting is going to use the wrong weight values, and
*     assume only accuracy appropriate for a single scan.
*
*     Under normal usage a logfile will be produced containing details of the
*     crossing information which are not reported to the screen. 

*  ADAM Parameters:
*     ADD_SOURCE = LOGICAL (Read)
*        When this is true the user is prompted to supply details of 
*        additional source positions.
*     AUTOMATIC = LOGICAL (Read)
*        When it has the value true, the application will run in the
*        automatic mode, where output file names, titles and labels of
*        the output NDF files (one for each expected source) will be
*        generated by the application. Otherwise the user will be
*        prompted for these values. [TRUE]
*     OUT = NDF (Read)
*        The name of the output NDF file containing the coadded data
*        trace for each expected point source. It will only be used when
*        the application is running in non-automatic mode. If the 
*        application is running in automatic mode the default value will
*        be "coadded_"sourcename"_b"waveband number eg coadded_wola_b3 .
*     COORDS = LITERAL (Read)
*        Sky coordinate system used to specify the additional expected
*        point source positions. When reporting in the log file, this
*        sky coordinate system will also be used to describe the crossed
*        expected point source position. Valid values include ECLIPTIC, 
*        EQUATORIAL and GALACTIC. See help on "Sky_coordinates" for more 
*        information on available sky coordinate systems.
*        [current sky coordinate system]
*     IN = NDF (Read)
*        Specifies a group of input CRDD NDF files. This should be in the
*        form of a group expression (see help on "Group_expressions").
*        All files in the group should be from the same CRDD type, but can 
*        be from different waveband. If an expected source is crossed by 
*        the detector traces from the different wavebands, the coadding 
*        will be performed for each waveband.  
*     DISTANCE = _LOGICAL (Read)
*        If it is true, the distance of the centre of a trace from the
*        expected source position will be taken into account when
*        deciding the weight used when coadding. Otherwise, the distance
*        will not be taken into account. [TRUE]
*     GVDTEG = _REAL (Read)
*        The value of the Gaussian weighting function at the edge of the
*        detector used when the distance of trace centre to the
*        expected source position is taken into account. The default
*        value is such that the detector traces which see the
*        expected source at the edge of their field of view will have
*        half weighting compared with the traces pass the expected
*        source at their centre. [0.5]
*     INTER = LITERAL (Read)
*        Specifies the interpolation method to be used when aligning the
*        crossing traces. It can take following values:
*
*         LINEAR - The linear interpolation method is to be used.
*
*         NEAREST - The nearest neighborhood method is to be used.
*
*        The input can be abbreviated to an unambiguous length and is
*        case insensitive. [LINEAR] 
*     LOGFILE = LITERAL (Read)
*        The name of the logging file to contain the reporting of the
*        crossing and related information for all expected point source
*        position. [coaddcrdd.log]
*     MSG_FILTER = LITERAL (Given)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").  [current message filter setting]
*     OFILTXT = LITERAL (Read)
*        The name of the text file containning the names of the output
*        NDF this file can be used as the input file for the following
*        applications which accept group NDF specification.
*        [outndf.log]
*     SCNLEN = _REAL (Read)
*        The length of the scan in aremin extracted around the expected
*        source from a crossing trace to be coadded with the other
*        crossing traces. [80.0]
*     SOURCE_LAT = LITERAL (Read)
*        The sky latitude of the additional expected source position,        
*        in the coordinate system specified by the parameter COORDS 
*        (eg if COORDS is EQUATORIAL then SOURCE_LAT should be given 
*         the Declination of the image centre). See help on
*        "Sky_coordinates" for the formats allowed for this value.
*        The application will keep prompting for the next expected
*        source position until a null, '!', value is given to the 
*        parameter SOURCE_NAME. 
*     SOURCE_LON = LITERAL (Read)
*        The sky longitude of the additional expected source position, 
*        in the coordinate system specified by the parameter COORDS 
*        (eg if COORDS is EQUATORIAL then source_LON should be given the
*        Right Ascension of the image centre). See help on
*        "Sky_coordinates" for the formats allowed for this value.
*        The application will keep prompting for the next expected
*        source position until a null, '!', value is given to the 
*        parameter SOURCE_NAME. 
*     SOURCE_NAME = LITERAL (Read)
*        The name of the additional expected sources specified by the
*        user. The application will keep prompting for the name of the
*        next source until a null value,'!' is supplied. [!]
*     TITLE = LITERAL (Read)
*        The title of the output NDF file which contains the coadded
*        data trace. In the automation mode, the title of the NDF for a
*        expected source will be created by the application by
*        concatenating the string 'COADDCRDD:' and the source name. And
*        hance this parameter will not be used in this mode. 
*     WEIGHT = LITERAL
*        The weighting scheme used when coadding. It can take following
*        values:
*        
*          VARIANCE - Samples in the traces are weighted according to
*                     the variance associated with them.
*
*          NEFD - The traces are weighted according to the noise equivalent
*                 flux density (NEFD) of their detector.
*
*          EQUAL - The traces are uniformly weighted.
*
*        The input can be abbrievated to an unambiguous length and is
*        case insensitive. []
*     WIDEXT = _REAL (Read)
*        The detector width extenion. With this parameter being non-
*        zero, the detector width used by the application becomes
*        DETWID + WIDEXT * DETWID, where DETWID is the width of the 
*        detector (about 4.5 arcmin, see exp. suppl. Table II.C.3 for 
*        its exact value for each detector), and if an expected source 
*        position falls within the scan field of this width, the detecor 
*        trace is regarded as a crossing trace. It is recommended that if
*        this parameter is to be used, its value should be selected 
*        between 0.0 and -1.0 so that the detectors which see the soucre 
*        at the edge of their field of view will not be selected. [0.0]

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     10-NOV-1992 (WG):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT package constants
      INCLUDE 'I90_DAT'          ! IRAS90 package constants
      INCLUDE 'IRA_PAR'          ! IRA package constants
      INCLUDE 'IRC_PAR'          ! IRC package constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER MXCRDD             ! Max number of CRDD files
      PARAMETER ( MXCRDD = 50 )
      INTEGER MXSRCE             ! Max number of expected sources
      PARAMETER ( MXSRCE = 60 )
      INTEGER MXCROS             ! Max number of crossing for a source
      PARAMETER ( MXCROS = 3 * MXCRDD )
                                 ! For each scan there are at most 3
                                 ! detectors cross a specified source

*  Local Variables:
      LOGICAL AUTO               ! Operation mode of the application
      INTEGER BAND( MXCRDD )     ! Waveband of input CRDD files
      CHARACTER*( 80 ) CMNT      ! Comt. written to output name file
      REAL CRSDIS( MXCROS )      ! X-scan distance of each crossing
      INTEGER CRSDTX( MXCROS )   ! Detector index of each crossing
      INTEGER CRSFLG( MXCROS )   ! Crossing flag
      INTEGER CRSFLX( MXCROS )   ! File index in group of each crossing
      REAL CRSSMP( MXCROS )      ! Crossing sample of each crossing
      REAL DETWID( I90__DETS )   ! Modified detector width
      LOGICAL DISCD              ! Flag of crossing traces discarded
      LOGICAL DIST               ! Flag of X-scan dist. weighting
      INTEGER DTARY              ! ID of temp. crossing array
      INTEGER DTPLCE             ! Placehold for temp. crossing array
      INTEGER DPNT               ! Point to temp. crossing array
      INTEGER EL                 ! Number of elements in temp. array
      INTEGER FID                ! ID of the logging file
      REAL GVDTEG                ! Gussian value at the edge of det.
      INTEGER GID                ! Input group ID
      INTEGER I                  ! Do loop index
      INTEGER IBND               ! Loop index through all wavebands
      INTEGER IFIL               ! Loop index through all CRDD files
      CHARACTER*( 8 ) INTER      ! Specified interpolation method
      INTEGER IRCID( MXCRDD )    ! IRC ID of input CRDD
      INTEGER ISRC               ! Loop index through all sources
      INTEGER LBND( 2 ), UBND( 2 ) ! Lower & upper bounds of output NDF
      DOUBLE PRECISION LAT( MXSRCE ), LON( MXSRCE )
                                 ! Lat. & lon. of expected sources
      INTEGER LATLN, LONLN       ! Used length of LATST and LONST
      CHARACTER*( IRA__SZFSC ) LATST, LONST
                                 ! String form of a source position
      REAL LENGTH                ! Lenght of extracted scan in arcmin
      INTEGER MXSMRT             ! Max sample rate
      CHARACTER*( 25 ) NAME( MXSRCE )
                                 ! Name of the expected sources
      INTEGER NCRDD              ! Number of input CRDD file
      INTEGER NCROS              ! Number of crossing from each waveband
      INTEGER NCRSPR             ! Number of previous crossing
      INTEGER NDFID( MXCRDD )    ! ID of input NDF file
      CHARACTER*( DAT__SZLOC ) NDFLOC( MXCRDD )
                                 ! Locaters to the input NDF files
      INTEGER NDIM               ! Number of output NDF's dimension
      INTEGER NOUT               ! Number of output NDFs
      INTEGER NSRCE              ! Number of expected soureces
      INTEGER NTOTCR             ! Number of total crossing for a source
      INTEGER OBS( MXCRDD )      ! Observation number of CRDD scans
      INTEGER OGID               ! ID of the group containing ouput NDFs
      INTEGER OUTNDF             ! Output NDF id
      INTEGER OPNT( 1 )          ! Pointer to output NDF array
      DOUBLE PRECISION RA( MXSRCE ), DEC( MXSRCE )
                                 ! RA & DEC of expected source position
      LOGICAL SCNDIR( MXCRDD )   ! CRDD scan direction, true: N to S
      INTEGER SCNLEN             ! Length of sector extr. around source
      CHARACTER*( IRA__SZSCS ) SCS
                                 ! Name of used sky coordinate system
      INTEGER SOP( MXCRDD )      ! SOP number of input CRDD files
      CHARACTER*( 4 ) TYPE     ! Type returned from IRM_CRTYP 
      INTEGER UBDTMP( 2 )        ! Upper bound of temporary arrays
      CHARACTER*( 8 ) WEIGHT     ! Specified weighting method
      INTEGER WTARY              ! ID of temp. weight array
      INTEGER WTPLCE             ! Placehold for temp. weight array
      INTEGER WPNT               ! Point to temp. weight
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Establish the conditional message filter level using parameter
*  MSG_LEVEL.
      CALL MSG_IFGET( STATUS )

*  Get a value for parameter AUTOMATIC from the environment.
      CALL PAR_GET0L( 'AUTOMATIC', AUTO, STATUS )

*  Begin a new NDF context.
      CALL NDF_BEGIN

*  Get a group of CRDD NDF files from the environment.
      CALL IRM_RDNDF( 'IN', MXCRDD, 1, ' ', GID, NCRDD, STATUS )           

*  If error status returned, exit.
      IF ( STATUS .NE. SAI__OK ) GOTO 990

*  Initialise the IRC_ system.
      CALL IRC_INIT( STATUS )

*  Get the NDF IDs and IRC IDs for all input CRDD files. 
      DO I = 1, NCRDD
         CALL NDG_NDFAS( GID, I, 'READ', NDFID( I ), STATUS )
         CALL IRC_IMPRT( NDFID( I ), IRCID( I ), STATUS )
      END DO
      
*  If the input NDFs are all CRDD files, check that they are from the
*  same type.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL IRM_CRTYP( NCRDD, NDFID, TYPE, STATUS )

*  If they are from the different type, report and exit.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'COADDCRDD_ERR1', 'The input CRDD NDFs '/
     :                   /'come from different types', STATUS )
            GOTO 980
         END IF

*  In case that not all input NDFs are CRDD files, report and exit.
      ELSE
         CALL ERR_REP( 'COADDCRDD_ERR2', 'Not all input NDFs are '/
     :                /'CRDD NDF files', STATUS )    
         GOTO 980
      END IF
            
*  Get the sky coordinate system to be used.
      CALL IRA_GTSCS( 'COORDS', .FALSE., SCS, STATUS )

*  Extract and report the expected source positions associated with
*  the input CRDD NDF files, and other IRAS information about the input
*  CRDD files.
      CALL CDCRA0( NCRDD, NDFID, IRCID, NSRCE, NAME, RA, DEC, BAND, 
     :             SOP, OBS, SCNDIR, STATUS )

*  Get additional expected source positions.
      CALL CDCRA1( 'ADD_SOURCE', 'SOURCE_NAME', 'SOURCE_LON', 
     :             'SOURCE_LAT', SCS, MXSRCE, 
     :             NSRCE, NAME, RA, DEC, LON, LAT, STATUS )

*  Modify the detector width according the user specified width
*  extension factor.
      CALL CDCRA2( 'WIDEXT', DETWID, STATUS )

*  Get the scan length to be extracted.
      CALL PAR_GET0R( 'SCNLEN', LENGTH, STATUS )
            
*  If there is an error in parameter response
         IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Get a text file to log the crossings.
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 0, FID, STATUS )

*  If error happens, report and exit.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'COADDCRDD_ERR3', 'Unable to open a logging '/
     :                /'file.', STATUS )
         GOTO 980
      END IF
      
*  Write the head of logging file.
      CALL FIO_WRITE( FID, ' ', STATUS )
      CALL FIO_WRITE( FID, '    *** COADDCRDD LOG FILE ***', STATUS )
      CALL FIO_WRITE( FID, ' ', STATUS )

*  Create a new empty group to contain names of the output NDFs.
      CALL GRP_NEW( 'Coadded CRDD traces', OGID, STATUS )
            
*  Create temporary array to hold the crossing data and weight. From the
*  structure of the IRAS focal plane, there are at most 3 crossing
*  detector traces for each scan.
      MXSMRT = 0
      DO IBND = 1, I90__BANDS
         MXSMRT = MAX( MXSMRT, I90__SRATE( IBND ) )
      END DO
      UBDTMP( 1 ) = MXSMRT * INT( LENGTH / I90__SPEED ) + 1
      UBDTMP( 2 ) = 3 * NCRDD
      CALL ARY_TEMP( DTPLCE, STATUS )
      CALL ARY_NEWP( '_REAL', 2, UBDTMP, DTPLCE, DTARY, STATUS )
      CALL ARY_MAP( DTARY, '_REAL', 'WRITE', DPNT, EL, STATUS )
      CALL ARY_TEMP( WTPLCE, STATUS )
      CALL ARY_NEWP( '_REAL', 2, UBDTMP, WTPLCE, WTARY, STATUS )
      CALL ARY_MAP( WTARY, '_REAL', 'WRITE', WPNT, EL, STATUS )

*  If unable to get temporary space, report and exit.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'COADDCRDD_ERR4', 'Unable to create temporary '/
     :                /'work space. Possibly because of no enough '/
     :                /'scratch disk space', STATUS )
         GOTO 970
      END IF
      
*  Get the way to weight the crossing traces when coadding.
      CALL CDCRA3( 'WEIGHT', 'DISTANCE', 'GVDTEG', NCRDD, NDFID,
     :              WEIGHT, DIST, GVDTEG, STATUS )
      
*  Get the interpolation method used when alignning crossing traces.
      CALL PAR_CHOIC( 'INTER', 'LINEAR', 'LINEAR,NEAREST', .FALSE.,
     :                INTER, STATUS )

*  If anything goes wrong, exit.
      IF ( STATUS .NE. SAI__OK ) GOTO 970

*  Now exam the crossing of the expected sources one by one.
      DO ISRC = 1, NSRCE
         NTOTCR = 0
      
*  Write an entry for this source in the log file.
         CALL FIO_WRITE( FID, 'Source Name:     '//NAME( ISRC ),
     :                   STATUS )
         CALL IRA_DTOC( LON( ISRC ), LAT( ISRC ), SCS, 1, LONST, LATST,
     :                  STATUS )
         LONLN = CHR_LEN( LONST )
         LATLN = CHR_LEN( LATST )
         CALL FIO_WRITE( FID, 'Source position: '//LONST( : LONLN )/
     :                 /', '//LATST( : LATLN ), STATUS )
         CALL FIO_WRITE( FID, ' ', STATUS )
            
*  Check the crossing from each waveband.
         DO IBND = 1, I90__BANDS 
            NCROS = 0

*  Convert the scan length to be extracted to odd sample numbers.
            SCNLEN = I90__SRATE( IBND ) * INT( LENGTH / I90__SPEED )
            SCNLEN = 1 + 2 * ( SCNLEN / 2 )
      
*  Check the file one by one.
            DO IFIL = 1, NCRDD

*  Only do the check when the waveband of the file is the same as
*  waveband under checking now.
               IF ( BAND( IFIL ) .EQ. IBND ) THEN
      
*  Find all crossings for this expected source from this file.
                  NCRSPR = NCROS
                  CALL CDCRA4( NDFID( IFIL ), IRCID( IFIL ), RA( ISRC ),
     :                         DEC( ISRC ), DETWID, MXCROS, NCROS,
     :                         CRSDTX, CRSDIS, CRSSMP, STATUS )

*  Record the file index of the found crossings. 
                  DO I = NCRSPR + 1, NCROS
                     CRSFLX( I ) = IFIL
                  END DO
               END IF
            END DO

*  If there is any crossing from this waveband, sort the crossing so
*  that they are in the order of increasing cross-scan distance order.
            IF ( NCROS .GT. 0 ) THEN
               CALL CDCRA5( NCROS, CRSFLX, CRSDTX, CRSDIS, CRSSMP,
     :                      STATUS )
            
*  Put the crossing trace sections and their weights to the temporary
*  array, alignning the traces to the trace closest to the source.
               CALL CDCRA6( NCRDD, NDFID, IRCID, SCNDIR, INTER, WEIGHT,
     :                     SCNLEN, NCROS, CRSFLX, CRSDTX, CRSSMP,
     :                     CRSFLG, %VAL( DPNT ), %VAL( WPNT ), STATUS )

*  Take distance effects into account if requirement.
               IF ( DIST ) THEN
                  CALL CDCRA7( NCRDD, IRCID, GVDTEG, SCNLEN, NCROS,
     :                         CRSFLX, CRSDTX, CRSDIS, DETWID,
     :                         %VAL( WPNT ), STATUS )
               END IF

*  Write the information about the crossings to the log file.
               CALL CDCRA8( FID, GID, NCRDD, NDFID, IRCID, SCNDIR, 
     :                      SOP, OBS, IBND, NCROS, CRSFLX, CRSDTX, 
     :                      CRSDIS, CRSSMP, CRSFLG, STATUS )  

*  See if all crossing traces will be discarded.
               DISCD = .TRUE.
               DO I = 1, NCROS
                  IF ( CRSFLG( I ) .EQ. 2 ) DISCD = .FALSE.
               END DO
      
*  If not all crossing traces are discarded, create an NDF file to hold
*  the coadded trace.
               IF ( ( .NOT.DISCD ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN
                  CALL CDCRA9( 'OUT', 'TITLE', AUTO, NAME( ISRC ), 
     :                          RA( ISRC ), DEC( ISRC ),
     :                          NDFID( CRSFLX( 1 ) ), IBND, CRSDTX( 1 ),
     :                          FID, OGID, OUTNDF, STATUS )

*  Find the bounds of the output NDF.
                  CALL NDF_BOUND( OUTNDF, 2, LBND, UBND, NDIM, STATUS )
      
*  Map the data-array of the output NDF.
                  CALL NDF_MAP( OUTNDF, 'Data', '_REAL', 'WRITE', OPNT,
     :                          EL, STATUS )
      
*  Coadd the crossing and output the result to an NDF file.
                  CALL CDCRD0( SCNLEN, NCROS, %VAL( DPNT ),
     :                        %VAL( WPNT ), INT( CRSSMP( 1 ) ), CRSFLG,
     :                         LBND( 1 ), UBND( 1 ), LBND( 2 ),
     :                         UBND( 2 ), %VAL( OPNT( 1 ) ), STATUS )

*  Un-map the output NDF data-array, annul the output NDF id and close
*  the output SDF file.
                  CALL NDF_UNMAP( OUTNDF, 'Data', STATUS )
                  CALL NDF_ANNUL( OUTNDF, STATUS )

*  Check status and reset if necessary
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_FLUSH( STATUS )
                     CALL MSG_OUT('COADDCRDD_MSG',
     :               'Error in preparing current coadded scan '/
     :               /'- discarded',
     :               STATUS )      
                  END IF

*  If all crossing traces are discarded, reset the number of crossings.
               ELSE
                  NCROS = 0
               END IF
      
*  Record the total crossings.
               NTOTCR = NTOTCR + NCROS
            END IF 
         END DO
      
*  If there is no crossing for this source, report to the user and write
*  into log file.
         IF ( NTOTCR .EQ. 0 ) THEN
            CALL MSG_BLANK( STATUS )
            CALL MSG_SETC( 'NAME', NAME( ISRC ) )
            CALL MSG_OUT( 'COADDCRDD_MSG1', 'No detector trace crosses'/
     :                   /' the expected source ^NAME.', STATUS )
            CALL MSG_BLANK( STATUS )
            CALL FIO_WRITE( FID, '     No detector trace crosses this '/
     :                    /'expected source.', STATUS )
            CALL FIO_WRITE( FID, ' ', STATUS )
         END IF
      END DO

*  Write a message to the user about logging file.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( 'COADDCRDD_MSG3', 'Crossing information is stored '/
     :             /'in a logging file (COADDCRDD.LOG by default).',
     :               STATUS )
      CALL MSG_BLANK( STATUS )

*  If there are some output NDF files created, write their names to a
*  text file.
      CALL GRP_GRPSZ( OGID, NOUT, STATUS )
      IF ( NOUT .GT. 0 ) THEN
         CMNT = '   ********* Coadded CRDD NDF **********'
         CALL GRP_LIST( 'OFILTXT', 1, NOUT, CMNT, OGID, STATUS )

*  Write a messgae to the user about the name file.
         CALL MSG_OUT( 'COADDCRDD_MSG3', 'Names of the created NDFs '/
     :                /'are in a text file (OUTNDF.DAT by default)',
     :                  STATUS )
                         
      END IF

 970  CONTINUE

*  Delete created groups.
      CALL GRP_DELET( GID, STATUS )
      CALL GRP_DELET( OGID, STATUS )
      
*  Release the templay work space.
      CALL ARY_ANNUL( DTARY, STATUS )
      CALL ARY_ANNUL( WTARY, STATUS )

*  Close the log file, cancel parameter and de-active FIO.
      CALL FIO_CANCL( 'LOGFILE', STATUS )
      CALL FIO_DEACT( STATUS )

 980  CONTINUE

*  Close down the IRC system.
      CALL IRC_CLOSE( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )      

 990  CONTINUE
      
      END
