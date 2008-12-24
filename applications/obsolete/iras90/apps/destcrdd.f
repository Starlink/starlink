      SUBROUTINE DESTCRDD( STATUS )
*+
*  Name:
*     DESTCRDD

*  Purpose:
*     Removes detector-to-detector stripes from a group of CRDD files.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DESTCRDD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine performs detector-to-detector destriping on a group
*     of CRDD files. An output CRDD file is produced for each input
*     CRDD file, holding the destriped data.  Each CRDD file is
*     destriped by subtracting a constant offset from each detector
*     data stream, so that all detectors in the output CRDD file have
*     the same background surface brightness. The mean background
*     surface brightness in each CRDD file (in MJy/sr) is preserved; no
*     attempt is made to match backgrounds between different CRDD
*     files.
*
*     Only the DATA component of each NDF is modified by this routine;
*     all other components are copied to the output without change.

*  Usage:
*     DESTCRDD IN OUT

*  ADAM Parameters:
*     BACKMEAN = _REAL (Write)
*        An output parameter to which is written the mean background 
*        surface brightness after destriping. The mean is taken over 
*        all the CRDD files specified by parameter IN, and is in units 
*        of Mega-Janskys per steradian. This value is also written
*        to the screen once all the CRDD files have been processed.
*     BACKSIGMA = _REAL (Write)
*        An output parameter to which is written the standard deviation
*        of the background surface brightness after destriping. The 
*        standard deviation is taken over all CRDD files specified by 
*        parameter IN, and is in units of Mega-Janskys per steradian.
*        This value is also written to the screen once all the CRDD 
*        files have been processed. 
*     BOX = _INTEGER (Read)
*        The size of the smoothing box used during the cleaning
*        algorithm, given as a number of samples. This is roughly the
*        extent of the largest source to be rejected from each detector
*        data stream before going on to estimate the background surface
*        brightness. A value of zero causes no cleaning to be
*        performed.                                                 [40]
*     CLIP = _REAL (Read)
*        The number of standard deviations at which data will be
*        rejected by the cleaning algorithm.                       [1.0]
*     HISTORY = _LOGICAL (Read)
*        Determines if history information is to be stored within the
*        output CRDD files. See help on "History_in_IRAS90" for more
*        information on history. The history information will contain
*        the names of the input CRDD files, DESTCRDD parameter values,
*        and the offsets subtracted from each detector.
*                                              [current history setting]
*     IN = NDF (Read)
*        Specifies a group of input CRDD files. This should be in the
*        form of a group expression (see help on "Group_expressions").
*        The CRDD files must contain data from the same IRAS waveband.
*        There is no limit on the number of CRDD files which can be
*        specified.
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").
*                                       [current message filter setting]
*     NITER = _INTEGER (Read)
*        The number of cleaning iterations to perform. A value of zero
*        causes no cleaning to be performed.                         [4]
*     OUT = NDF (Write)
*        A group of output CRDD files corresponding one-for-one with
*        the list of input CRDD files given for parameter IN.  This
*        should be in the form of a group expression (see help on
*        "Group_expressons"). Expressions such as "*_DS" are expanded
*        by replacing the "*" character with each input CRDD file in
*        turn.
*     QEXP = LITERAL (Read)
*        A quality expression giving the quality of samples which are
*        to be included in the estimation of the offset to be
*        subtracted from each detector. A value of "ANY" causes all
*        samples to be used, without regard to quality.            [ANY]
*     QNAME = LITERAL (Read)
*        The name of a quality to be assigned to all samples which do
*        not contribute to the estimation of a detector offset value.
*        Samples are omitted from the detector offset calculations if
*        they are bad in the input NDF, do not satisfy the quality
*        expression given by parameter QEXP, or are rejected as source
*        samples by the cleaning algorithm. A blank value for QNAME
*        causes no quality values to be assigned.                     []
*     XNAME = LITERAL (Read)
*        If any the input NDF do not already contain any quality name
*        definitions then new quality names are put in the extension
*        specified by XNAME. This extension is created if it does not
*        already exist.                                  [QUALITY_NAMES]
*     XTYPE = LITERAL (Read)
*        If any new NDF extensions are created to hold quality names
*        (see parameter XNAME), then parameter XTYPE is used to obtain
*        the HDS data type for the created extensions. The run time
*        default is to give the extension a type identical to its name.
*                                                                     []

*  Examples:
*     DESTCRDD ZCMA_B1S1 ZCMA_B1S1_DS
*        This command destripes CRDD file ZCMA_B1S1 and puts the
*        results in ZCMA_B1S1_DS.
*     DESTCRDD *_RAW *|_RAW|_DS| QEXP=.NOT.(SOURCE_A.OR.SOURCE_B)
*        This command attempts to destripe all NDFs in the current
*        directory which have names ending with "_RAW", placing the 
*        results in corresponding output NDFs in which "_RAW" is 
*        replaced by "_DS". Any samples which have either of the two 
*        qualities SOURCE_A and/or SOURCE_B are excluded from the 
*        estimation of the detector offsets.

*  Estimation of Detector Offsets:
*     The user may choose to exclude certain samples from the estimation
*     of the detector offsets, by giving a "quality expression" for
*     parameter QEXP. Only those samples which have qualities satisfying
*     the given quality expression are included in the estimation of the
*     detector offsets. Qualities can be assigned to selected samples
*     using routine SETQUAL.
*     
*     After removing samples which do not satisfy the quality
*     expression, each detector data stream is "cleaned" by removing
*     any remaining bright sources which may adversly influence the
*     estimation of the background surface brightness. To do this the
*     detector data stream is smoothed with a box filter, the size of
*     which is specified by parameter BOX. The RMS residual between the
*     smoothed data and the original data is then found. All samples
*     for which the residuals exceeds a given multiple of the RMS
*     residual (specified by parameter CLIP) are rejected as belonging
*     to a bright source. The process is then repeated, excluding the
*     samples just rejected, a given number of times (specified by
*     parameter NITER). This cleaning can be switched off by specifying
*     a value of zero for either NITER or BOX.
*
*     Next, the median value of the remaining data is found, and is
*     converted into a surface brightness value in Mega-Janskys per
*     steradian (see help on "Detector_solid_angles" and
*     "Detector_bandwidths" for lists of detector effective solid
*     angles and band widths). The mean of the median surface
*     brightnesses of all detectors in the CRDD file is found. The
*     detector offsets are then calculated by subtracting this mean
*     surface brightness from each individual detector's median surface
*     brightness. The offsets are finally converted back from
*     Mega-Janskys per steradian to the units of the input CRDD file.
*
*     The offsets are then subtracted from all input samples (including
*     those previously rejected by the cleaning algorithm or the
*     quality expression), and these are then stored in the output CRDD
*     file.

*  Background Levels in the Output CRDD Files:
*     Output CRDD files retain the mean background surface brightness
*     of the input data. This may result in scan-to-scan stripes being
*     visible in images formed from the output CRDD files. These
*     stripes can be removed by processing the CRDD files produced by
*     DESTCRDD with routine BACKCRDD before combining them into an
*     image.
*
*     After all CRDD files have been destriped, the mean background 
*     surface brightness (taken over all the output CRDD files) is 
*     displayed in Mega-Janskys per steradian. The spread in background
*     values in the output CRDD files is also displayed. These values
*     are written to the output parameters BACKMEAN and BACKSIGMA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-OCT-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'MSG_PAR'          ! MSG constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'I90_DAT'          ! IRAS90 data
      INCLUDE 'IRC_PAR'          ! IRC constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns used length of a string.

*  Local Variables:
      CHARACTER COMC*1           ! Comment character for output group.
      CHARACTER LOCS(5)*(DAT__SZLOC)! Locators for output quality names
      CHARACTER NAME*(GRP__SZNAM)! Name of current output CRDD file.
      CHARACTER QEXP*(IRQ__SZQEX)! The quality expression.
      CHARACTER QNAME*(IRQ__SZQNM)! Quality to assign to un-used samples
      CHARACTER TEXT*(GRP__SZNAM)! Text to replace a bad output name.
      CHARACTER UNITS*(IRC__SZUNI)! Units from input CRDD file.
      CHARACTER XNAME*(DAT__SZNAM)! NDF extension for new quality names.
      CHARACTER XTYPE*(DAT__SZTYP)! HDS type for quality names extension

      DOUBLE PRECISION REFDEC    ! DEC of reference point.
      DOUBLE PRECISION REFRA     ! RA of reference point.


      INTEGER BAND               ! Waveband no. of current CRDD file.
      INTEGER BAND0              ! Waveband no. of first CRDD file.
      INTEGER BOX                ! No. of samples in cleaning box.
      INTEGER DETNO( I90__MAXDT )! Detector numbers.
      INTEGER HBOX               ! Half the box size.
      INTEGER IDC                ! IRC identifier for input CRDD file.
      INTEGER IGRP1              ! Input group identifier.
      INTEGER IGRP2              ! Output group identifier.
      INTEGER INDEX              ! Group index to current input NDF.
      INTEGER INDF1              ! Input NDF identifier.
      INTEGER INDF2              ! Output NDF identifier.
      INTEGER IPIN               ! Pointer to mapped input DATA array.
      INTEGER IPOUT              ! Pointer to mapped output DATA array.
      INTEGER IPT                ! Pointer to temporary copy of input 
                                 ! DATA array.
      INTEGER IPW1               ! Pointer to first work array.
      INTEGER IPW2               ! Pointer to second work array.
      INTEGER IPW3               ! Pointer to third work array.
      INTEGER LBND( 2 )          ! Lower bounds of input CRDD file.
      INTEGER NCRDDF             ! No. of input CRDD files.
      INTEGER NCOUT              ! No. of output CRDD files.
      INTEGER NDIM               ! No. of dimensions in input CRDD file.
      INTEGER NEL                ! No. of mapped elements in an array.
      INTEGER NITER              ! No. of cleaning iterations.
      INTEGER NOUT               ! No. of good outputs.
      INTEGER NS                 ! No. of values contributing to BSB.
      INTEGER NSUM               ! No. of values summed in SBSUM and
                                 ! SB2SUM.
      INTEGER OBS                ! OBS number.
      INTEGER SOP                ! SOP number.
      INTEGER TNDF               ! A copy of the input NDF with samples
                                 ! set bad which do not satisfy the
                                 ! quality expression.
      INTEGER UBND( 2 )          ! Upper bounds of input CRDD file.
      INTEGER WSHIGH             ! Current upper bound of the work
                                 ! arrays.
      INTEGER WSIZE              ! Current size of the work arrays.
      INTEGER WSLOW              ! Current lower bound of the work
                                 ! arrays.


      LOGICAL BAD                ! True if the output contains any bad
                                 ! values.
      LOGICAL FIRST              ! True if this is the first CRDD file.
      LOGICAL OK                 ! True if locators in LOCS are valid.


      REAL BSB                   ! Background surface brightness in the
                                 ! current CRDD file after destriping.
      REAL CLIP                  ! Clipping threshold for cleaning.
      REAL DETOFF( I90__MAXDT )  ! Destriping offset for each detector.
      REAL NOMSPD                ! Nominal scan speed.
      REAL SB                    ! The mean of the BSB values.
      REAL SB2SUM                ! Sum of the squared BSB values.
      REAL SBSIG                 ! The standard deviation of BSB values.
      REAL SBSUM                 ! Sum of the BSB values.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the filter level for conditional message output.
      CALL MSG_IFGET( 'MSG_FILTER', STATUS )

*  Get a quality expression giving the quality of input CRDD samples
*  which are to be included in the calculation of the destripe
*  parameters. Note, the same quality expression is used for all the
*  CRDD files in the input group.
      CALL IRM_GETQX( 'QEXP', QEXP, STATUS )

*  Get the maximum size of source to be rejected, in samples, and store
*  the half box size. The actual used box size is 2*HBOX+1.
      CALL PAR_GDR0I( 'BOX', 10, 0, VAL__MAXI, .TRUE., BOX, STATUS )
      HBOX = BOX/2

*  Get the no. of standard deviations at which to reject data during
*  cleaning.
      CALL PAR_GDR0R( 'CLIP', 3.0, 0.0, VAL__MAXR, .TRUE., CLIP, 
     :                 STATUS )

*  Get the number of cleaning iterations to perform.
      CALL PAR_GDR0I( 'NITER', 2, 0, 10, .TRUE., NITER, STATUS )

*  See if a quality is to be assigned to the samples which do not
*  contribute to the estimation of detector offsets.
      CALL PAR_GET0C( 'QNAME', QNAME, STATUS )

*  Get the name of an extension in which to put new qulaity name
*  definitions (in case any of the supplied NDFs do not already have
*  such information).
      CALL PAR_GET0C( 'XNAME', XNAME, STATUS )

*  Get the HDS type of the extension in which new qulaity name
*  definitions are put.
      CALL PAR_DEF0C( 'XTYPE', XNAME, STATUS )
      CALL PAR_GET0C( 'XTYPE', XTYPE, STATUS )

*  Get a GRP identifier for a group of NDFs containing the input CRDD.
      CALL IRM_RDNDF( 'IN', 0, 1, 'Give more CRDD file names',
     :                IGRP1, NCRDDF, STATUS )

*  Get a GRP identifier for a group of NDFs to contain the output CRDD
*  and store the comment character for the output group.
      CALL IRM_WRNDF( 'OUT', IGRP1, NCRDDF, NCRDDF,
     :                'Give more output CRDD file names', IGRP2,
     :                NCOUT, STATUS )
      CALL GRP_GETCC( IGRP2, 'COMMENT', COMC, STATUS )

*  Initialise the IRC package.
      CALL IRC_INIT( STATUS )      

*  Allocate some workspace for use by the cleaning algorithm. It is
*  expanded as necssary.
      WSIZE = 100
      CALL PSX_MALLOC( VAL__NBR*WSIZE, IPW1, STATUS )
      CALL PSX_MALLOC( VAL__NBR*WSIZE, IPW2, STATUS )
      CALL PSX_MALLOC( VAL__NBI*WSIZE, IPW3, STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise the sum of the surface brightnesses and squared surface
*  brightnesses, and the sum of the number of values included in the
*  sums.
      SBSUM = 0.0
      SB2SUM = 0.0
      NSUM = 0

*  Indicate that the first CRDD file is being processed.
      FIRST = .TRUE.

*  Loop round each pair of input and output CRDD files...
      NOUT = 0
      DO INDEX = 1, NCRDDF 
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP1, INDEX, 'READ', INDF1, STATUS )

*  Tell the user which NDF is currently being procesed.
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL MSG_OUTIF( MSG__NORM, 'DESTCRDD_MSG1',
     :                   '  Processing ^NDF...', STATUS )

*  Import the input CRDD file into the IRC system.
         CALL IRC_IMPRT( INDF1, IDC, STATUS )

*  Get the waveband number.
         CALL IRC_INFO( IDC, BAND, REFRA, REFDEC, NOMSPD, SOP, OBS,
     :                  STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 998

*  Check it is the same as the first one. If not, report an error.
         IF( .NOT. FIRST ) THEN

            IF( BAND .NE. BAND0 ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'W', I90__WAVEL( BAND ) )
               CALL ERR_REP( 'DESTCRDD_ERR1',
     :'DESTCRDD: CRDD file contains data from a different band (^W um)',
     :                        STATUS )
            END IF

         ELSE 
            FIRST = .FALSE.
            BAND0 = BAND

         END IF

*  Get an NDF identifier for the output NDF. The output NDF is
*  initialised to be a copy of the input NDF (excluding the DATA
*  component).
         CALL NDG_NDFPR( INDF1, 'UNITS,VARIANCE,QUALITY,AXIS',
     :                   IGRP2, INDEX, INDF2, STATUS )

*  Produce a temporary copy of the input NDF in which any samples
*  not satisfing the given quality expression are set bad. A clone of
*  the input NDF is returned if no such samples can be found.
         CALL IRM_QNDF( INDF1, QEXP, .TRUE., .FALSE., TNDF, STATUS )

*  Find the bounds of the current input NDF.
         CALL NDF_BOUND( TNDF, 2, LBND, UBND, NDIM, STATUS )

*  Find the units of the current input NDF.
         CALL NDF_CGET( TNDF, 'UNITS', UNITS, STATUS )

*  Map the DATA component of the temporary copy of the input CRDD
*  file for READ access.
         CALL NDF_MAP( TNDF, 'DATA', '_REAL', 'READ', IPT, NEL,
     :                 STATUS )

*  Map the DATA component of the input CRDD file for READ access.
         CALL NDF_MAP( INDF1, 'DATA', '_REAL', 'READ', IPIN, NEL,
     :                 STATUS )

*  Map the DATA component of the output CRDD file.
         CALL NDF_MAP( INDF2, 'DATA', '_REAL', 'WRITE', IPOUT, NEL,
     :                 STATUS )

*  Ensure that the work space size is not updated if an error has
*  occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 998

*  Set up the bounds of the work arrays used by the cleaning algorithm.
         WSLOW = LBND( 1 ) - HBOX - 1
         WSHIGH = UBND( 1 ) + HBOX

*  If necessary, increase the size of the workspace for use by the
*  cleaning algorithm.
         IF( WSHIGH - WSLOW + 1 .GT. WSIZE ) THEN
            WSIZE = WSHIGH - WSLOW + 1 
            CALL PSX_REALLOC( VAL__NBR*WSIZE, IPW1, STATUS )
            CALL PSX_REALLOC( VAL__NBR*WSIZE, IPW2, STATUS )
            CALL PSX_REALLOC( VAL__NBI*WSIZE, IPW3, STATUS )
         END IF

*  Obtain locators to the structures holding quality name definitions
*  in the output NDF.
         CALL DESTA3( QNAME, INDF2, XNAME, XTYPE, LOCS, OK, STATUS )

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 998

*  Destripe the current CRDD file. 
         CALL DESTA0( IDC, UBND( 2 ), LBND( 2 ), UBND( 1 ), LBND( 1 ),
     :                %VAL( IPIN ), %VAL( IPT ), HBOX, NITER, CLIP, 
     :                UNITS, WSLOW, WSHIGH, OK, QNAME, LOCS,
     :                %VAL( IPOUT ), DETNO, DETOFF, BSB, NS, BAD,
     :                %VAL( IPW1 ), %VAL( IPW2 ), %VAL( IPW3 ), STATUS )

*  Set the output bad pixel flag.
         CALL NDF_SBAD( BAD, INDF2, 'DATA', STATUS )

*  Ensure that the surface brightness statistics are not incremented if
*  an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 998
      
*  Increment the sum of the mean surface brightnesses, the squared mean
*  surface brightnesses and the number of points summed.
         SBSUM = SBSUM + NS*BSB
         SB2SUM = SB2SUM + NS*(BSB**2)
         NSUM = NSUM + NS

*  Update the HISTORY structure in the output NDF.
         CALL DESTA1( INDF1, INDF2, UBND( 2 ), LBND( 2 ), DETNO, DETOFF,
     :                UNITS, STATUS )

*  If required, release the locators which relate to quality information
*  in the output NDF.
 998     CONTINUE
         IF( OK ) CALL IRQ_RLSE( LOCS, STATUS )

*  Annul the IRC identifier for the current input CRDD file.
         CALL IRC_ANNUL( IDC, STATUS )

*  Annul the identifiers for the input NDF and the temporary copy of
*  the input NDF.
         CALL NDF_ANNUL( TNDF, STATUS )
         CALL NDF_ANNUL( INDF1, STATUS )

*  If an error has occurred, delete the output NDF, otherwise just 
*  annul its identifier.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL NDF_DELET( INDF2, STATUS )
         ELSE
            CALL NDF_ANNUL( INDF2, STATUS )
         END IF

*  If any error has occured, then
         IF ( STATUS .NE. SAI__OK ) THEN

*  Flush the error message, and reset STATUS to SAI__OK.
            CALL ERR_FLUSH( STATUS )

*  Warn the user that no output CRDD file will be created for the
*  current input CRDD file.
            CALL GRP_GET( IGRP2, INDEX, 1, NAME, STATUS )
            CALL MSG_SETC( 'NDF', NAME )
            CALL MSG_OUTIF( MSG__QUIET, 'DESTCRDD_MSG2',
     :                      'WARNING: ^NDF cannot be produced',
     :                      STATUS )

*  Overwrite the current output NDF name with a string which will be
*  interpreted as a comment by later applications.
            TEXT = COMC//'          '//NAME( : CHR_LEN( NAME ) )//
     :             ' could not be produced'
            CALL GRP_PUT( IGRP2, 1, TEXT, INDEX, STATUS )

*  If a good output was produced, increment the count of good outputs.
         ELSE
            NOUT = NOUT + 1
         ENDIF

*  Do the next pair of input and output CRDD files.
      END DO

*  Produce a text file holding the names of all the output CRDD files.
*  The user can specify this text file within a group expression for a
*  subsequent stage of the IRAS90 data processing. Note, the names of
*  output CRDD files which were not succesfully created are replaced by
*  strings starting with a comment character and identifying the output
*  CRDD file which could not be produced.
      IF( NOUT .GT. 0 ) CALL IRM_LISTN( 'NDFLIST', IGRP2, 'DESTCRDD', 
     :                                   STATUS )      

*  Write out the mean and standard deviation of the background surface 
*  brightnesses to the parameters BACKMEAN and BACKSIGMA, and to the 
*  screen.
      IF( NSUM .GT. 0.0 ) THEN
         SB = SBSUM/REAL( NSUM )
         SBSIG = SQRT( MAX( 0.0, ( SB2SUM/REAL( NSUM ) ) - SB**2 ) )

         CALL PAR_PUT0R( 'BACKMEAN', SB, STATUS )
         CALL PAR_PUT0R( 'BACKSIGMA', SBSIG, STATUS )

         CALL MSG_BLANKIF( MSG__NORM, STATUS )

         IF( NOUT .GT. 1 ) THEN

            CALL MSG_OUTIF( MSG__NORM, 'DESTCRDD_MSG3',
     :     '  The background surface brightnesses in the output CRDD '//
     :     'files', STATUS )

            CALL MSG_SETR( 'BSB', SB )
            CALL MSG_SETC( 'U', IRC__MJPS )
            CALL MSG_OUTIF( MSG__NORM, 'DESTCRDD_MSG4',
     :       '  are distributed with a mean of ^BSB ^U and a standard ',
     :                      STATUS )

            CALL MSG_SETR( 'SIG', SBSIG )
            CALL MSG_SETC( 'U', IRC__MJPS )
            CALL MSG_OUTIF( MSG__NORM, 'DESTCRDD_MSG5',
     :                      '  deviation of ^SIG ^U', STATUS )

         ELSE

            CALL MSG_OUTIF( MSG__NORM, 'DESTCRDD_MSG6',
     :       '  The background surface brightness in the output CRDD '//
     :       'file', STATUS )

            CALL MSG_SETR( 'BSB', SB )
            CALL MSG_SETC( 'U', IRC__MJPS )
            CALL MSG_OUTIF( MSG__NORM, 'DESTCRDD_MSG7',
     :       '  is ^BSB ^U', STATUS )

         END IF

         CALL MSG_BLANKIF( MSG__NORM, STATUS )

      ELSE
         CALL PAR_PUT0R( 'BACKMEAN', 0.0, STATUS )
         CALL PAR_PUT0R( 'BACKSIGMA', 0.0, STATUS )
         CALL MSG_BLANKIF( MSG__NORM, STATUS )
         CALL MSG_OUTIF( MSG__NORM, 'DESTCRDD_MSG8',
     :   '  No mean background surface brightness could be calculated.',
     :                   STATUS )
      END IF

*  Close down IRC.
 999  CONTINUE
      CALL IRC_CLOSE( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Free the workspace.
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPW2, STATUS )
      CALL PSX_FREE( IPW3, STATUS )
      
*  Delete the input and output groups.
      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )

*  If a parameter null or abort error exists, annul it.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'DESTCRDD_ERR2',
     :   'DESTCRDD: Error destriping a group of CRDD files.',
     :   STATUS )
      END IF

      END
