      SUBROUTINE BACKCRDD( STATUS )
*+
*  Name:
*     BACKCRDD

*  Purpose:
*     Estimate and remove backgrounds from a group of CRDD files.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL BACKCRDD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine estimates and removes a variety of different
*     backgrounds from a group of CRDD files. An output CRDD file is
*     produced for each input CRDD file. Depending on the value of
*     parameter OUTTYPE, this output CRDD file will hold either the
*     input data with the estimated background subtracted from it, or
*     the estimated background data itself. The type of background used
*     is specified by parameter TYPE. The mean background surface
*     brightness in the data after background subtraction can be
*     controlled using parameter OUTBACK. This may for instance be
*     given the value of the mean background surface brightness
*     calculated by routine DESTCRDD, in which case the mean background
*     in the data would be preserved.
*
*     Only the DATA component of each NDF is modified by this routine;
*     all other components are copied to the output without change. The
*     exception to this is that any output VARIANCE component is deleted
*     if parameter OUTTYPE had the value BACKGROUND.

*  Usage:
*     BACKCRDD IN OUT

*  ADAM Parameters:
*     CLIP = _REAL (Read)
*        The number of standard deviations at which data will be
*        rejected.                                                 [3.0]
*     HISTORY = _LOGICAL (Read)
*        Determines if history information is to be stored within the
*        output CRDD files. See help on "History_in_IRAS90" for more
*        information on history.  The history information will contain
*        the names of the input CRDD files and BACKCRDD parameter
*        values.                               [current history setting]
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
*     OUT = NDF (Write)
*        A group of output CRDD files corresponding one-for-one with
*        the list of input CRDD files given for parameter IN.  This
*        should be in the form of a group expression (see help on
*        "Group_expressions"). Expressions such as "*_BAK" are expanded
*        by replacing the "*" character with each input CRDD file in
*        turn.
*     OUTBACK = _REAL (Read)
*        The mean background surface brightness required in the data
*        after background subtraction. The same value is used for each
*        CRDD file. The value should be given in Mega-Janskys per
*        steradian.                                                [0.0]
*     OUTTYPE = LITERAL (Read)
*        This can take the values DATA and BACKGROUND. DATA causes the
*        output CRDD files to hold the background subtracted input data;
*        BACKGROUND causes the output CRDD files to hold the estimated
*        background data itself.                                  [DATA]
*     QEXP = LITERAL (Read)
*        A quality expression giving the quality of samples which are
*        to be included in the estimation of the backgrounds to be
*        subtracted from each CRDD file. A value of "ANY" causes all
*        samples to be used, without regard to quality.            [ANY]
*     QNAME = LITERAL (Read)
*        The name of a quality to be assigned to all samples which do
*        not contribute to the estimation of the scan background.
*        Samples are omitted from the scan background calculations if
*        they are bad in the input NDF, do not satisfy the quality
*        expression given by parameter QEXP, or are rejected as source
*        samples by the cleaning algorithm. A blank value for QNAME
*        causes no quality values to be assigned.                     []
*     TYPE = LITERAL (Read)
*        The type of background to be subtracted from the input CRDD
*        files. Currently supported values are: UNIFORM, LINEAR.
*                                                              [UNIFORM]
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

*  Examples:
*     BACKCRDD ZCMA_B1S1_DS ZCMA_B1S1_BAK TYPE=UNIFORM
*        This command causes a constant background value to be
*        estimated for CRDD file ZCMA_B1S1_DS and the background
*        subtracted data to be stored in ZCMA_B1S1_BAK.
*     BACKCRDD *_DS *|_DS|_FIT| QEXP=.NOT.(SOURCE_A.OR.SOURCE_B) OUTTYPE=B
*        This command attempts to estimate linear backgrounds for all
*        NDFs in the current directory which have names ending with
*        "_DS". The estimated backgrounds are stored in corresponding
*        output NDFs in which "_DS" is replaced by "_FIT". Any samples
*        which have either of the two qualities SOURCE_A and/or
*        SOURCE_B are excluded from the estimation of the background.

*  Excluding Samples from the Background Estimate:
*     The user may choose to exclude certain samples from the
*     estimation of the background by giving a "quality expression" for
*     parameter QEXP. Only those samples which have qualities
*     satisfying the given quality expression are included in the
*     estimation of the background. Qualities can be assigned to
*     selected samples using routine SETQUAL.

*  Estimation of Uniform Backgrounds:
*     After removing samples which do not satisfy the quality
*     expression, the median surface brightness value in each CRDD file
*     is found (see help on "Detector_solid_angles" and
*     "Detector_bandwidths" for lists of the detector effective solid
*     angles and band widths used to convert between flux, flux density
*     and surface brightness values). An attempt is then made to
*     improve the fit to the background by iteratively removing any
*     outlying data values from the median estimation. This is done by
*     removing samples which lie further than a specified multiple of
*     the noise from the current median value (see parameter CLIP). The
*     noise is taken to be the RMS residual between the input data and
*     the current median value.  After rejecting these outlying
*     samples, a new median value is found. This rejection process is
*     repeated until the RMS residual changes by less than 0.1% between
*     iterations. The required output background surface brightness
*     (specified by parameter OUTBACK) is subtracted from the final
*     median value. This value is converted back to the same units as
*     the input CRDD to form the background estimate.

*  Estimation of Linear Backgrounds:
*     After removing samples which do not satisfy the quality
*     expression, the data is compressed in the cross-scan direction by
*     taking the median of the cross-scan surface brightness values at
*     a range of in-scan positions along the scan. This results in a
*     single dimensional array holding an estimate of the background
*     surface brightness at each in-scan position. A least squares
*     linear fit is then made to this data, and the RMS residual of the
*     data from the fit is found.  An attempt is then made to improve
*     the fit to the background by iteratively removing any outlying
*     data values from the estimation of the linear fit.  This is done
*     by removing samples which lie far from the current linear fit.
*     The threshold for rejection is the value of parameter CLIP times
*     the RMS residual from the current fit.  After rejecting these
*     outlying samples, a new linear fit is found. This rejection 
*     process is repeated until the RMS residual changes by less than 
*     0.1% between iterations. The required output background surface 
*     brightness (specified by parameter OUTBACK) is subtracted from 
*     the final fit. This fit is converted back to the same units as 
*     the input CRDD to form the background estimate.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1992 (DSB):
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
      LOGICAL BAD                ! True if the output contains any bad
                                 ! values.
      INTEGER BAND               ! Waveband no. of current CRDD file.
      INTEGER BAND0              ! Waveband no. of first CRDD file.
      REAL    CLIP               ! Clipping threshold for cleaning.
      CHARACTER COMC*1           ! Comment character for output group.
      INTEGER DNLOW              ! Detector number corresponding to DLOW
      LOGICAL FIRST              ! True if this is the first CRDD file.
      REAL    GRAD               ! Gradient of linear background.
      INTEGER IDC                ! IRC identifier for input CRDD file.
      INTEGER IGRP1              ! Input group identifier.
      INTEGER IGRP2              ! Output group identifier.
      INTEGER INDEX              ! Group index to current input NDF.
      INTEGER INDF1              ! Input NDF identifier.
      INTEGER INDF2              ! Output NDF identifier.
      INTEGER IPIN               ! Pointer to mapped input DATA array.
      INTEGER IPT                ! Pointer to copy of input DATA array.
      INTEGER IPOUT              ! Pointer to mapped output DATA array.
      INTEGER IPW1               ! Pointer to a work array.
      INTEGER IPW2               ! Pointer to a work array.
      INTEGER IPW3               ! Pointer to a work array.
      INTEGER IPW4               ! Pointer to a work array.
      INTEGER IPW5               ! Pointer to a work array.
      INTEGER LBND( 2 )          ! Lower bounds of input CRDD file.
      CHARACTER LOCS(5)*(DAT__SZLOC)! Locators for output quality names
      CHARACTER NAME*(GRP__SZNAM)! Name of current output CRDD file.
      INTEGER NCOUT              ! No. of output CRDD files.
      INTEGER NCRDDF             ! No. of input CRDD files.
      INTEGER NDET               ! No. of detectors in current CRDD file.
      INTEGER NDIM               ! No. of dimensions in input CRDD file.
      INTEGER NEL                ! No. of mapped elements in an array.
      REAL    NOMSPD             ! Nominal scan speed.
      INTEGER NOUT               ! No. of good output NDFs.
      INTEGER NSAMP              ! No. of samples per detector in the
                                 ! current CRDD file.
      INTEGER OBS                ! OBS number.
      REAL    OFFSET             ! Offset of linear background.
      LOGICAL OK                 ! True if locators in LOCS are valid.
      REAL    OUTBAC             ! Required output background surface
                                 ! brightness.
      CHARACTER OUTTYP*10        ! Required output background surface
      CHARACTER QEXP*(IRQ__SZQEX)! The quality expression.
      CHARACTER QNAME*(IRQ__SZQNM)! Quality to assign to un-used samples
      DOUBLE PRECISION REFDEC    ! DEC of reference point.
      DOUBLE PRECISION REFRA     ! RA of reference point.
      REAL    REMOVE             ! The uniform background value in
                                 ! MJy/sr.
      INTEGER SOP                ! SOP number.
      CHARACTER TEXT*(GRP__SZNAM)! Text to replace a bad output name.
      CHARACTER TYPE*7           ! Text to replace a bad output name.
      INTEGER TNDF               ! A copy of the input NDF with samples
                                 ! set bad which do not satisfy the
                                 ! quality expression.
      INTEGER UBND( 2 )          ! Upper bounds of input CRDD file.
      CHARACTER UNITS*(IRC__SZUNI)! Units from input CRDD file.
      INTEGER WSIZE1             ! Current size of the first set of
                                 ! work arrays.
      INTEGER WSIZE2             ! Current size of the second set of
                                 ! work arrays.
      CHARACTER XNAME*(DAT__SZNAM)! NDF extension for new quality names.
      CHARACTER XTYPE*(DAT__SZTYP)! HDS type for quality names extension


*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      BAND0 = 0

*  Set the filter level for conditional message output.
      CALL MSG_IFGET( STATUS )

*  Get a quality expression giving the quality of input CRDD samples
*  which are to be included in the calculation of the background model.
*  Note, the same quality expression is used for all the CRDD files in
*  the input group.
      CALL IRM_GETQX( 'QEXP', QEXP, STATUS )

*  Get the no. of standard deviations at which to reject data.
      CALL PAR_GDR0R( 'CLIP', 3.0, 0.0, VAL__MAXR, .TRUE., CLIP, 
     :                 STATUS )

*  Get the required background surface brightness after background
*  subtraction (in Maga-Janskys per steradian).
      CALL PAR_GET0R( 'OUTBACK', OUTBAC, STATUS )

*  Get the type of output CRDD files to create.
      CALL PAR_CHOIC( 'OUTTYPE', 'DATA', 'DATA,BACKGROUND', .TRUE.,
     :                OUTTYP, STATUS )

*  Get the type of background to be used.
      CALL PAR_CHOIC( 'TYPE', 'LINEAR', 'UNIFORM,LINEAR', .TRUE.,
     :                TYPE, STATUS )

*  Get a GRP identifier for a group of NDFs containing the input CRDD.
      CALL IRM_RDNDF( 'IN', 0, 1, 'Give more CRDD file names',
     :                IGRP1, NCRDDF, STATUS )

*  See if a quality is to be assigned to the samples which do not
*  contribute to the estimation of scan background.
      CALL PAR_GET0C( 'QNAME', QNAME, STATUS )

*  Get the name of an extension in which to put new qulaity name
*  definitions (in case any of the supplied NDFs do not already have
*  such information).
      CALL PAR_GET0C( 'XNAME', XNAME, STATUS )

*  Get the HDS type of the extension in which new qulaity name
*  definitions are put.
      CALL PAR_DEF0C( 'XTYPE', XNAME, STATUS )
      CALL PAR_GET0C( 'XTYPE', XTYPE, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get a GRP identifier for a group of NDFs to contain the output CRDD
*  and store the comment character for the output group.
      CALL IRM_WRNDF( 'OUT', IGRP1, NCRDDF, NCRDDF,
     :                'Give more output CRDD file names', IGRP2,
     :                NCOUT, STATUS )
      CALL GRP_GETCC( IGRP2, 'COMMENT', COMC, STATUS )

*  Allocate some workspace for use by the cleaning algorithm. It is
*  expanded as necssary.
      WSIZE1 = 16
      WSIZE2 = 16
      CALL PSX_MALLOC( VAL__NBR*WSIZE1, IPW1, STATUS )
      CALL PSX_MALLOC( VAL__NBR*WSIZE1, IPW2, STATUS )
      CALL PSX_MALLOC( VAL__NBR*WSIZE1, IPW3, STATUS )
      CALL PSX_MALLOC( VAL__NBR*WSIZE1, IPW4, STATUS )
      CALL PSX_MALLOC( VAL__NBR*WSIZE1, IPW5, STATUS )

*  Initialise the IRC package.
      CALL IRC_INIT( STATUS )      

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Begin an NDF context.
      CALL NDF_BEGIN

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
         CALL MSG_OUTIF( MSG__NORM, 'BACKCRDD_MSG1',
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
               CALL ERR_REP( 'BACKCRDD_ERR1',
     :'BACKCRDD: CRDD file contains data from a different band (^W um)',
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

*  Find the bounds of the current input NDF, and store the size of each
*  dimension.
         CALL NDF_BOUND( TNDF, 2, LBND, UBND, NDIM, STATUS )
         NDET = UBND( 2 ) - LBND( 2 ) + 1
         NSAMP = UBND( 1 ) - LBND( 1 ) + 1

*  Find the units of the current input NDF.
         CALL NDF_CGET( TNDF, 'UNITS', UNITS, STATUS )

*  Map the DATA component of the input CRDD file for READ access.
         CALL NDF_MAP( INDF1, 'DATA', '_REAL', 'READ', IPIN, NEL,
     :                 STATUS )

*  Map the DATA component of the temporary copy of the input CRDD
*  file for READ access.
         CALL NDF_MAP( TNDF, 'DATA', '_REAL', 'READ', IPT, NEL,
     :                 STATUS )

*  Map the DATA component of the output CRDD file.
         CALL NDF_MAP( INDF2, 'DATA', '_REAL', 'WRITE', IPOUT, NEL,
     :                 STATUS )

*  Obtain locators to the structures holding quality name definitions
*  in the output NDF.
         CALL BACKA3( QNAME, INDF2, XNAME, XTYPE, LOCS, OK, STATUS )

*  Ensure that the sizes of the work arrays are not updated if an error
*  has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 998

*  Find and optionally remove the background from the current CRDD
*  file. First handle uniform backgrounds.
         IF( TYPE .EQ. 'UNIFORM' ) THEN

*  If necessary, increase the size of the work arrays.
            IF( NEL .GT. WSIZE1 ) THEN
               WSIZE1 = NEL
               CALL PSX_REALLOC( VAL__NBR*WSIZE1, IPW1, STATUS )
            END IF
   
            IF( NDET .GT. WSIZE2 ) THEN
               WSIZE2 = NDET
               CALL PSX_REALLOC( VAL__NBR*WSIZE2, IPW2, STATUS )
            END IF

*  Abort if an error has occurred.
            IF ( STATUS .NE. SAI__OK ) GO TO 998

*  Do the work.
            CALL BACKA0( IDC, UBND( 2 ), LBND( 2 ), UBND( 1 ),
     :                   LBND( 1 ), %VAL( IPIN ), %VAL( IPT ), CLIP,
     :                   UNITS, OUTBAC, OUTTYP, OK, QNAME, LOCS,
     :                   %VAL( IPOUT ), BAD, REMOVE, %VAL( IPW1),
     :                   %VAL( IPW2 ), STATUS )


*  Now handle linear backgrounds.
         ELSE IF( TYPE .EQ. 'LINEAR' ) THEN

*  If necessary, increase the size of the work arrays.
            IF( NSAMP .GT. WSIZE1 ) THEN
               WSIZE1 = NSAMP
               CALL PSX_REALLOC( VAL__NBR*WSIZE1, IPW1, STATUS )
               CALL PSX_REALLOC( VAL__NBR*WSIZE1, IPW4, STATUS )
               CALL PSX_REALLOC( VAL__NBR*WSIZE1, IPW5, STATUS )
            END IF
   
            IF( NDET .GT. WSIZE2 ) THEN
               WSIZE2 = NDET
               CALL PSX_REALLOC( VAL__NBR*WSIZE2, IPW2, STATUS )
               CALL PSX_REALLOC( VAL__NBR*WSIZE2, IPW3, STATUS )
            END IF

*  Abort if an error has occurred.
            IF ( STATUS .NE. SAI__OK ) GO TO 998

*  Do the work.
            CALL BACKA1( IDC, UBND( 2 ), LBND( 2 ), UBND( 1 ),
     :                   LBND( 1 ), %VAL( IPIN ), %VAL( IPT ), CLIP,
     :                   UNITS, OUTBAC, OUTTYP, OK, QNAME, LOCS,
     :                   %VAL( IPOUT ), BAD, GRAD, OFFSET, DNLOW,
     :                   %VAL( IPW1), %VAL( IPW2 ), %VAL( IPW3 ),
     :                   %VAL( IPW4 ), %VAL( IPW5 ), STATUS )

*  If an unknown background type was specified, abort.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'T', TYPE )
            CALL ERR_REP( 'BACKCRDD_ERR2',
     :                    'BACKGROUND: Unknown background type - ^T',
     :                    STATUS )
            GO TO 999

         END IF

*  Set the output bad pixel flag.
         CALL NDF_SBAD( BAD, INDF2, 'DATA', STATUS )

*  Update the HISTORY structure in the output NDF.
         CALL BACKA2( INDF1, INDF2, OUTTYP, TYPE, REMOVE, GRAD, OFFSET,
     :                LBND( 1 ), DNLOW, STATUS )

*  If required, release the locators which relate to quality information
*  in the output NDF.
 998     CONTINUE
         IF( OK ) CALL IRQ_RLSE( LOCS, STATUS )

*  Annul the IRC identifier for the current input CRDD file.
         CALL IRC_ANNUL( IDC, STATUS )

*  If the output holds the background estimate, reset the variance
*  component in the output.
         IF( OUTTYP .EQ. 'BACKGROUND' ) CALL NDF_RESET( INDF2, 'VAR',
     :                                                  STATUS )

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
            CALL MSG_OUTIF( MSG__QUIET, 'BACKCRDD_MSG2',
     :                      'WARNING: ^NDF cannot be produced',
     :                      STATUS )

*  Overwrite the current output NDF name with a string which will be
*  interpreted as a comment by later applications.
            TEXT = COMC//'          '//NAME( : CHR_LEN( NAME ) )//
     :             ' could not be produced'
            CALL GRP_PUT( IGRP2, 1, TEXT, INDEX, STATUS )

*  If no error has occurred, increment the no. of good output NDFs 
*  produced.
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
      IF( NOUT .GT. 0 ) CALL IRM_LISTN( 'NDFLIST', IGRP2, 'BACKCRDD', 
     :                                   STATUS )      

*  Close down IRC.
 999  CONTINUE
      CALL IRC_CLOSE( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Free the workspace.
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPW2, STATUS )
      CALL PSX_FREE( IPW3, STATUS )
      CALL PSX_FREE( IPW4, STATUS )
      CALL PSX_FREE( IPW5, STATUS )
      
*  Delete the input and output groups.
      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )

*  If a parameter null or abort error exists, annul it.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'BACKCRDD_ERR3',
     :   'BACKCRDD: Error estimating or removing backgrounds from a'//
     :   ' group of CRDD files.', STATUS )
      END IF

      END
