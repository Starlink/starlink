      SUBROUTINE APERADD( STATUS )
*+
*  Name:
*     APERADD

*  Purpose:
*     Integrates pixel values within an aperture of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL APERADD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine displays statistics for pixels that lie within a
*     specified aperture of an NDF. The aperture can either be circular
*     (specified by parameters CENTRE and DIAM), or arbitrary (specified
*     by parameter ARDFILE). If the aperture is specified using parameters
*     CENTRE and DIAM, then it must be either one- or two-dimensional.
*
*     The following statistics are displayed:
*
*     - The total number of pixels within the aperture
*     - The number of good pixels within the aperture
*     - The total data sum within the aperture
*     - The standard deviation on the total data sum (that is, the
*       square root of the sum of the individual pixel variances)
*     - The mean pixel value within the aperture
*     - The standard deviation on the mean pixel value (that is, the
*       standard deviation on the total data sum divided by the number of
*       values)
*     - The standard deviation of the pixel values within the aperture
*
*     If individual pixel variances are not available within the input NDF
*     (i.e. if it has no VARIANCE component), then each pixel is assumed to
*     have a constant variance equal to the variance of the pixel values
*     within the aperture. There is an option to weight pixels so that
*     pixels with larger variances are given less weight (see parameter
*     WEIGHT). The statistics are displayed on the screen and written to
*     output parameters. They may also be written to a log file.
*
*     A pixel is included if its centre is within the aperture, and is not
*     included otherwise. This simple approach may not be suitable for
*     accurate aperture photometry, especially where the aperture diameter
*     is less than about ten times the pixel size. A specialist photometry
*     package should be used if accuracy, rather than speed, is paramount.

*  Usage:
*     aperadd ndf centre diam

*  ADAM Parameters:
*     ARDFILE = FILENAME (Read)
*        The name of an ARD file containing a description of the aperture.
*        This allows apertures of almost any shape to be used. If a null
*        (!) value is supplied then the aperture is assumed to be circular
*        with centre and diameter given by parameters CENTRE and DIAM. ARD
*        files can be created either "by hand" using an editor, or using a
*        specialist application should as KAPPA:ARDGEN.
*
*        The co-ordinate system in which positions within the ARD file are
*        given should be indicated by including suitable COFRAME or WCS
*        statements within the file (see SUN/183), but will default to
*        pixel co-ordinates in the absence of any such statements. For
*        instance, starting the file with a line containing the text
*        "COFRAME(SKY,System=FK5)" would indicate that positions are
*        specified in RA/DEC (FK5,J2000). The statement "COFRAME(PIXEL)"
*        indicates explicitly that positions are specified in pixel
*        co-ordinates.  [!]
*     CENTRE = LITERAL (Read)
*        The co-ordinates of the centre of the circular aperture. Only
*        used if parameter ARDFILE is set to null. The position must be
*        given in the current co-ordinate Frame of the NDF (supplying
*        a colon ":" will display details of the current co-ordinate
*        Frame). The position should be supplied as a list of formatted
*        axis values separated by spaces or commas. See also parameter
*        USEAXIS. The current co-ordinate Frame can be changed using
*        KAPPA:WCSFRAME.
*     DIAM = LITERAL (Read)
*        The diameter of the circular aperture. Only used if parameter
*        ARDFILE is set to null. If the current co-ordinate Frame of the
*        NDF is a SKY Frame (e.g. RA and DEC), then the value should be
*        supplied as an increment of celestial latitude (e.g. DEC). Thus,
*        "10.2" means 10.2 degrees, "0:30" would mean 30 arcminutes,
*        and "0:0:1" would mean 1 arcsecond. If the current co-ordinate
*        Frame is not a SKY Frame, then the diameter should be specified
*        as an increment along axis 1 of the current co-ordinate Frame.
*        Thus, if the current Frame is PIXEL, the value should be given
*        simply as a number of pixels.
*     LOGFILE  =  FILENAME (Read)
*        Name of the text file to log the results.  If null, there
*        will be no logging.  Note this is intended for the human reader
*        and is not intended for passing to other applications.  [!]
*     MASK = NDF (Write)
*        An output NDF containing the pixel mask used to evaluate the
*        reported statistics. The NDF will contain the value +1 for
*        pixels that are included in the statistics, and bad values
*        for all other pixels. The pixel bounds of the NDF will be the
*        smallest needed to encompass all used pixels. [!]
*     MEAN = _DOUBLE (Write)
*        The mean of the pixel values within the aperture.
*     NDF = NDF (Read)
*        The input NDF.
*     NGOOD = _INTEGER (Write)
*        The number of good pixels within the aperture.
*     NUMPIX = _INTEGER (Write)
*        The total number of pixels within the aperture.
*     SIGMA = _DOUBLE (Write)
*        The standard deviation of the pixel values within the
*        aperture.
*     SIGMEAN = _DOUBLE (Write)
*        The standard deviation on the mean pixel value. If variances
*        are available this is the RMS value of the standard deviations
*        associated with each included pixel value. If variances are not
*        available, it is the standard deviation of the pixel values
*        divided by the square root of the number of good pixels in
*        the aperture.
*     SIGTOTAL = _DOUBLE (Write)
*        The standard deviation on the total data sum.  Only created if
*        variances are available this is the RMS value of the standard
*        deviations associated with each included pixel value.  If
*        variances are not available, it is the standard deviation of
*        the pixel values divided by the square root of the number of
*        good pixels in the aperture.
*     TOTAL = _DOUBLE (Write)
*        The total of the pixel values within the aperture.
*     USEAXIS = GROUP (Read)
*        USEAXIS is only accessed if the current co-ordinate Frame of
*        the NDF has too many axes. A group of strings should be
*        supplied specifying the axes which are to be used when
*        specifying the aperture using parameters ARDFILE, CENTRE and
*        DIAM.  Each axis can be specified using one of the following
*        options.
*
*        - Its integer index within the current Frame of the input
*        NDF (in the range 1 to the number of axes in the current
*        Frame).
*        - Its symbol string such as "RA" or "VRAD".
*        - A generic option where "SPEC" requests the spectral axis,
*        "TIME" selects the time axis, "SKYLON" and "SKYLAT" picks the
*        sky longitude and latitude axes respectively.  Only those axis
*        domains present are available as options.
*
*        A list of acceptable values is displayed if an illegal value is
*        supplied.  If a null (!) value is supplied, the axes with the
*        same indices as the two used pixel axes within the NDF are
*        used.  [!]
*     WEIGHT = _LOGICAL (Read)
*        If a TRUE value is supplied, and the input NDF has a VARIANCE
*        component, then pixels with larger variances will be given
*        smaller weight in the statistics. The weight associated with
*        each pixel is proportional to the reciprocal of its variance.
*        The constant of proportionality is chosen so that the mean weight
*        is unity. The pixel value and pixel variance are multiplied by
*        the pixels weight before being used to calculate the statistics.
*        The calculation of the statistics remains unchanged in all other
*        respects.  [FALSE]

*  Examples:
*     aperadd neb1 "13.5,201.3" 20
*        This calculates the statistics of the pixels within a circular
*        aperture of NDF neb1. Assuming the current co-ordinate Frame of
*        neb1 is PIXEL, the aperture is centred at pixel co-ordinates
*        (13.5, 201.3) and has a diameter of 20 pixels.
*     aperadd neb1 "15:23:43.2 -22:23:34.2" "10:0"
*        This also calculates the statistics of the pixels within a circular
*        aperture of NDF neb1. Assuming the current co-ordinate Frame of
*        neb1 is a SKY Frame describing RA and DEC, the aperture is centred
*        at RA 15:23:43.2 and DEC -22:23:34.2, and has a diameter of 10
*        arcminutes.
*     aperadd ndf=neb1 ardfile=outline.dat logfile=obj1
*        This calculates the statistics of the pixels within an aperture
*        of NDF neb1 described within the file "outline.dat". The file
*        contains an ARD description of the required aperture. The results
*        are written to the log file "obj1".

*  Notes:
*     -  The statistics are not displayed on the screen when the
*     message filter environment variable MSG_FILTER is set to QUIET.
*     The creation of output parameters and the log file is unaffected
*     by MSG_FILTER.

*  ASCII-region-definition Descriptors:
*     The ARD file may be created by ARDGEN or written manually.  In the
*     latter case consult SUN/183 for full details of the ARD
*     descriptors and syntax; however, much may be learnt from looking
*     at the ARD files created by ARDGEN and the ARDGEN documentation.
*     There is also a summary with examples in the main body of SUN/95.

*  Related Applications:
*     KAPPA: STATS, MSTATS, ARDGEN, ARDMASK, ARDPLOT, WCSFRAME.

*  Implementation Status:
*     -  This routine correctly processes the WCS, AXIS, DATA, and VARIANCE
*     components of an NDF data structure.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  Bad pixels and automatic quality masking are supported.
*     -  All non-complex numeric data types can be handled.

*  Copyright:
*     Copyright (C) 2001, 2003-2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2009, 2012 Science and Technology Facilities
*     Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-2001 (DSB):
*        Original NDF version.
*     24-NOV-2003 (DSB):
*        Use 0.5*DIAM to create the ARD circle (previously used DIAM).
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2009 July 24 (MJC):
*        Remove QUIET parameter and use the current reporting level
*        instead (set by the global MSG_FILTER environment variable).
*     2-JUL-2010 (DSB):
*        - Report an error if the aperture does not overlap the NDF.
*        - Correct docs for DIAM parameter.
*     2012 May 8 (MJC):
*        Add _INT64 support.
*     29-JAN-2016 (DSB):
*        Add parameter MASK.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PRM_PAR'          ! VAL constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'MSG_PAR'          ! Message-system constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER APTXT*80         ! Description of aperture
      CHARACTER DOM*30           ! Current Frame Domain
      CHARACTER TEXT*(GRP__SZNAM)! General text bufferName of ARD file
      CHARACTER TYPE*(NDF__SZTYP)! Numeric type for processing
      DOUBLE PRECISION BC( 2 )   ! CENTRE base Frame axis value
      DOUBLE PRECISION CC( 2 )   ! CENTRE current Frame axis value
      DOUBLE PRECISION DIAM      ! The diameter of circular aperture
      DOUBLE PRECISION MEAN      ! Mean pixel value
      DOUBLE PRECISION SGMEAN    ! Std devn on mean pixel value
      DOUBLE PRECISION SGTOT     ! Std devn on total pixel value
      DOUBLE PRECISION SIGMA     ! Std devn of pixel values
      DOUBLE PRECISION TOT       ! Total pixel value
      INTEGER CURFRM             ! AST pointer to current WCS Frame
      INTEGER DAX                ! Index of axis for measuring diameter
      INTEGER DIM( NDF__MXDIM )  ! NDF dimensions
      INTEGER EL                 ! Total number of pixels in the image
      INTEGER FDL                ! File descriptor
      INTEGER IAT                ! Used length of a string
      INTEGER IGRP               ! Group identifier
      INTEGER INDF1              ! Identifier for the input NDF
      INTEGER INDF2              ! Identifier for NDF section containing aperture
      INTEGER INDF3              ! Identifier for output NDF containing aperture
      INTEGER IPDAT              ! Pointer to the data component of input NDF
      INTEGER IPIX               ! Index of PIXEL Frame within IWCS
      INTEGER IPMASK             ! Pointer to the full sized ARD logical mask
      INTEGER IPMSK2             ! Pointer to the minimal ARD logical mask
      INTEGER IPVAR              ! Pointer to the variance component of input NDF
      INTEGER IWCS               ! NDF WCS FrameSet
      INTEGER LBNDE( NDF__MXDIM )! Lower bounds of a box encompassing all external array elements
      INTEGER LBNDI( NDF__MXDIM )! Lower bounds of a box encompassing all internal array elements
      INTEGER NDIM               ! Number of dimensions in the image
      INTEGER NGOOD              ! Number of good pixels
      INTEGER NUMPIX             ! Total number of pixels
      INTEGER NVAL               ! Number of supplied values
      INTEGER NREP               ! Number of background pixels
      INTEGER REGVAL             ! Value assignied to the first ARD region
      INTEGER SDIM( NDF__MXDIM ) ! Indices of significant axes
      INTEGER SLBND( NDF__MXDIM )! Lower limit for input NDF
      INTEGER SUBND( NDF__MXDIM )! Upper limit for input NDF
      INTEGER UBNDE( NDF__MXDIM )! Upper bounds of a box encompassing all external array elements
      INTEGER UBNDI( NDF__MXDIM )! Upper bounds of a box encompassing all internal array elements
      LOGICAL ARD                ! Aperture specified by an ARD file?
      LOGICAL CONT               ! ARD description to continue?
      LOGICAL LOG                ! Log results?
      LOGICAL QUIET              ! Supress screen output?
      LOGICAL VAR                ! Are variances availab le?
      LOGICAL WEIGHT             ! Weight pixels?
      REAL TRCOEF( ( NDF__MXDIM + 1 ) * NDF__MXDIM ) ! Data to world co-ordinate conversions

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the NDF structure to be examined.
      CALL LPG_ASSOC( 'NDF', 'READ', INDF1, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  See if a circular aperture is to be used (in which case the NDF must
*  be 2-d). Do this by seeing if the user has supplied an ARD file (in
*  which case store a suitable ARD expression in TEXT which will cause
*  the file to be read). If not, anull the error.
      CALL FIO_ASSOC( 'ARDFILE', 'READ', 'LIST', 0, FDL, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL NDF_DIM( INDF1, NDF__MXDIM, DIM, NDIM, STATUS )
         ARD = .TRUE.
         TEXT( 1:1 ) = '^'
         CALL FIO_FNAME( FDL, TEXT( 2: ), STATUS )
         CALL FIO_ANNUL( FDL, STATUS )
         APTXT = TEXT( 2: )
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         NDIM = 2
         ARD = .FALSE.
      END IF

*  See if pixels are to be weighted.
      CALL PAR_GET0L( 'WEIGHT', WEIGHT, STATUS )

*  Get the WCS FrameSet and the bounds of the significant axes.
      CALL KPG1_ASGET( INDF1, NDIM, .FALSE., .TRUE., .TRUE., SDIM,
     :                 SLBND, SUBND, IWCS, STATUS )

*  Get a pointer to the current Frame.
      CURFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  If an ARD file was supplied...
      IF( ARD ) THEN

*  Create a GRP group holding the ARD description.
         IGRP = GRP__NOID
         CALL ARD_GRPEX( TEXT, GRP__NOID, IGRP, CONT, STATUS )

*  Indicate that the ARD file can contain coords in any of the Frames
*  contained in the NDFs WCS FrameSet. First set the current Frame of
*  the FrameSet to PIXEL to indicate that PIXEL is the default coord
*  system for positions in the ARD file.
         CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )
         CALL AST_SETI( IWCS, 'CURRENT', IPIX, STATUS )
         CALL ARD_WCS( IWCS, 'PIXEL', STATUS )

*  If no ARD file was supplied, get the centre and diameter of the circular
*  aperture, and construct a group holding an equivalent ARD description.
      ELSE

*  Get the CENTRE parameter value.
         CC( 1 ) = AST__BAD
         CALL KPG1_GTPOS( 'CENTRE', IWCS, .FALSE., CC, BC, STATUS )

*  Choose the axis to use, and then get the DIAM parameter value.
         IF( AST_ISASKYFRAME( CURFRM, STATUS ) ) THEN
            DAX = AST_GETI( CURFRM, 'LATAXIS', STATUS )
         ELSE
            DAX = 1
         END IF
         DIAM= AST__BAD
         CALL KPG1_GTAXV( 'DIAM', 1, .TRUE., CURFRM, DAX, DIAM, NVAL,
     :                    STATUS )

*  Create the ARD description.
         TEXT = 'CIRCLE('
         IAT = 8
         CALL CHR_APPND( AST_FORMAT( CURFRM, 1, CC( 1 ), STATUS ), TEXT,
     :                   IAT )
         CALL CHR_APPND( ',', TEXT, IAT )
         IAT = IAT + 1
         CALL CHR_APPND( AST_FORMAT( CURFRM, 2, CC( 2 ), STATUS ), TEXT,
     :                   IAT )
         CALL CHR_APPND( ',', TEXT, IAT )
         IAT = IAT + 1
         CALL CHR_APPND( AST_FORMAT( CURFRM, DAX, 0.5*DIAM, STATUS ),
     :                   TEXT, IAT )
         CALL CHR_APPND( ' )', TEXT, IAT )

*  Put this text into a GRP group.
         CALL GRP_NEW( ' ', IGRP, STATUS )
         CALL GRP_PUT( IGRP, 1, TEXT( : IAT ), 0, STATUS )
         APTXT = TEXT( : IAT )

*  Store the WCS FrameSet. Since no COFRAME or WCS statements were included
*  in the ARD description above, positions will be interpreted as being in
*  the current Frame of the WCS FrameSet.
         CALL ARD_WCS( IWCS, 'PIXEL', STATUS )

      END IF

*  Allocate the memory needed for the logical mask array.
      CALL NDF_SIZE( INDF1, EL, STATUS )
      CALL PSX_CALLOC( EL, '_INTEGER', IPMASK, STATUS )

*  Create the mask.  Value 2 should be used to represent pixels
*  specified by the first keyword in the ARD description. TRCOEF is
*  ignored because we have previously called ARD_WCS.
      REGVAL = 2
      CALL ARD_WORK( IGRP, NDIM, SLBND, SUBND, TRCOEF, .FALSE., REGVAL,
     :               %VAL( CNF_PVAL( IPMASK ) ),
     :               LBNDI, UBNDI, LBNDE, UBNDE,
     :               STATUS )

*  Check the region overlaps the NDF.
      IF( LBNDI( 1 ) .GT. UBNDI( 1 ) .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'APERADD_ERR1', 'The supplied aperture does '//
     :                 'not overlap the supplied NDF.', STATUS )
         GO TO 999
      END IF

*  Create a section from the input NDF which just encloses the aperture.
      CALL NDF_SECT( INDF1, NDIM, LBNDI, UBNDI, INDF2, STATUS )

*  Obtain the numeric type of the array, and map it.
      CALL NDF_TYPE( INDF2, 'DATA', TYPE, STATUS )
      CALL NDF_MAP( INDF2, 'DATA', TYPE, 'READ', IPDAT, EL, STATUS )

*  If variances are available, map them.
      CALL NDF_STATE( INDF2, 'VARIANCE', VAR, STATUS )
      IF( VAR ) CALL NDF_MAP( INDF2, 'VARIANCE', TYPE, 'READ', IPVAR,
     :                        EL, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) go to 999

*  If the mask is to be retained in an output NDF, create the output NDF,
*  set its data type to _INTEGER and map it.
      CALL LPG_PROP( INDF2, 'WCS,Axis', 'MASK', INDF3, STATUS );
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL NDF_STYPE( '_INTEGER', INDF3, 'DATA', STATUS )
         CALL NDF_MAP( INDF3, 'DATA', '_INTEGER', 'WRITE', IPMSK2, EL,
     :                 STATUS )

*  Otherwise, annul the error and allocate work array for the mask.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL PSX_CALLOC( EL, '_INTEGER', IPMSK2, STATUS )
      END IF

*  Copy the section of the mask array which just encloses the aperture into
*  a new mask array.
      CALL KPG1_CPNDI( NDIM, SLBND, SUBND,
     :                 %VAL( CNF_PVAL( IPMASK ) ), LBNDI, UBNDI,
     :                 %VAL( CNF_PVAL( IPMSK2 ) ), EL, STATUS )

*  Calculate the required statistics. Call the appropriate routine for the
*  data type.
      IF ( TYPE .EQ. '_REAL' ) THEN
         CALL KPS1_APADR( WEIGHT, EL, %VAL( CNF_PVAL( IPMSK2 ) ),
     :                    %VAL( CNF_PVAL( IPDAT ) ),
     :                    VAR, %VAL( CNF_PVAL( IPVAR ) ),
     :                    NGOOD, MEAN, SGMEAN, TOT,
     :                    SGTOT, NUMPIX, SIGMA, STATUS )

      ELSE IF( TYPE .EQ. '_BYTE' ) THEN
         CALL KPS1_APADB( WEIGHT, EL, %VAL( CNF_PVAL( IPMSK2 ) ),
     :                    %VAL( CNF_PVAL( IPDAT ) ),
     :                    VAR, %VAL( CNF_PVAL( IPVAR ) ),
     :                    NGOOD, MEAN, SGMEAN, TOT,
     :                    SGTOT, NUMPIX, SIGMA, STATUS )

      ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPS1_APADD( WEIGHT, EL, %VAL( CNF_PVAL( IPMSK2 ) ),
     :                    %VAL( CNF_PVAL( IPDAT ) ),
     :                    VAR, %VAL( CNF_PVAL( IPVAR ) ),
     :                    NGOOD, MEAN, SGMEAN, TOT,
     :                    SGTOT, NUMPIX, SIGMA, STATUS )

      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
         CALL KPS1_APADI( WEIGHT, EL, %VAL( CNF_PVAL( IPMSK2 ) ),
     :                    %VAL( CNF_PVAL( IPDAT ) ),
     :                    VAR, %VAL( CNF_PVAL( IPVAR ) ),
     :                    NGOOD, MEAN, SGMEAN, TOT,
     :                    SGTOT, NUMPIX, SIGMA, STATUS )

      ELSE IF( TYPE .EQ. '_INT64' ) THEN
         CALL KPS1_APADK( WEIGHT, EL, %VAL( CNF_PVAL( IPMSK2 ) ),
     :                    %VAL( CNF_PVAL( IPDAT ) ),
     :                    VAR, %VAL( CNF_PVAL( IPVAR ) ),
     :                    NGOOD, MEAN, SGMEAN, TOT,
     :                    SGTOT, NUMPIX, SIGMA, STATUS )

      ELSE IF( TYPE .EQ. '_UBYTE' ) THEN
         CALL KPS1_APADUB( WEIGHT, EL, %VAL( CNF_PVAL( IPMSK2 ) ),
     :                     %VAL( CNF_PVAL( IPDAT ) ),
     :                    VAR, %VAL( CNF_PVAL( IPVAR ) ),
     :                    NGOOD, MEAN, SGMEAN, TOT,
     :                    SGTOT, NUMPIX, SIGMA, STATUS )

      ELSE IF( TYPE .EQ. '_UWORD' ) THEN
         CALL KPS1_APADUW( WEIGHT, EL, %VAL( CNF_PVAL( IPMSK2 ) ),
     :                     %VAL( CNF_PVAL( IPDAT ) ),
     :                    VAR, %VAL( CNF_PVAL( IPVAR ) ),
     :                    NGOOD, MEAN, SGMEAN, TOT,
     :                    SGTOT, NUMPIX, SIGMA, STATUS )

      ELSE IF( TYPE .EQ. '_WORD' ) THEN
         CALL KPS1_APADW( WEIGHT, EL, %VAL( CNF_PVAL( IPMSK2 ) ),
     :                    %VAL( CNF_PVAL( IPDAT ) ),
     :                    VAR, %VAL( CNF_PVAL( IPVAR ) ),
     :                    NGOOD, MEAN, SGMEAN, TOT,
     :                    SGTOT, NUMPIX, SIGMA, STATUS )

      END IF


*  If the mask is being writeen to an output NDF< convert the background
*  values from zoer to bad.
      IF( INDF3 .NE. NDF__NOID ) THEN
         CALL KPG1_CHVAI( EL, %VAL( CNF_PVAL( IPMSK2 ) ), 0, VAL__BADI,
     :                    %VAL( CNF_PVAL( IPMSK2 ) ), NREP, STATUS )
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to open a log file to store the results.
      LOG = .FALSE.
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 80, FDL, STATUS )

*  Annul the error if a null value was given, and indicate that a log
*  file is not to be created.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         LOG = .TRUE.
      END IF

*  See if we are to run quietly, i.e not at NORMAL or lower priority.
      QUIET = .NOT. MSG_FLEVOK( MSG__NORM, STATUS )

*  Display a blank line.
      CALL KPG1_REPRT( ' ', QUIET, LOG, FDL, STATUS )

*  Describe the aperture.
      IF( ARD ) THEN
         TEXT = '  ARD file: '
         IAT = 12
         CALL CHR_APPND( APTXT, TEXT, IAT )
         CALL KPG1_REPRT( TEXT( : IAT ), QUIET, LOG, FDL, STATUS )

      ELSE
         TEXT = '  Aperture: '
         IAT = 12
         CALL CHR_APPND( APTXT, TEXT, IAT )
         CALL KPG1_REPRT( TEXT( : IAT ), QUIET, LOG, FDL, STATUS )

*  If a non-ARD aperture was given, describe the coordinate system.
         TEXT = '  Co-ords : '
         IAT = 12
         DOM = AST_GETC( CURFRM, 'DOMAIN', STATUS )
         CALL CHR_APPND( DOM, TEXT, IAT )
         IF( DOM .EQ. 'SKY' ) THEN
            CALL CHR_APPND( ' (', TEXT, IAT )
            CALL CHR_APPND( AST_GETC( CURFRM, 'SYSTEM', STATUS ),
     :                     TEXT, IAT )

            IF( AST_GETD( CURFRM, 'EQUINOX', STATUS ) .LT. 1984.0 ) THEN
               CALL CHR_APPND( ' B', TEXT, IAT )
            ELSE
               CALL CHR_APPND( ' J', TEXT, IAT )
            END IF

            CALL CHR_APPND( AST_GETC( CURFRM, 'EQUINOX', STATUS ),
     :                      TEXT, IAT )
            CALL CHR_APPND( ')', TEXT, IAT )
         END IF
         CALL KPG1_REPRT( TEXT( : IAT ), QUIET, LOG, FDL, STATUS )
      END IF

*  Display the NDF name.
      CALL NDF_MSG( 'NDF', INDF1 )
      CALL MSG_LOAD( ' ', '  NDF     : ^NDF', TEXT, IAT, STATUS )
      CALL KPG1_REPRT( TEXT( : IAT ), QUIET, LOG, FDL, STATUS )

*  Display the NDF's title.
      CALL NDF_CMSG( 'TITLE', INDF1, 'Title', STATUS )
      CALL MSG_LOAD( ' ', '  Title   : ^TITLE', TEXT, IAT, STATUS )
      CALL KPG1_REPRT( TEXT( : IAT ), QUIET, LOG, FDL, STATUS )

*  Display a blank line.
      CALL KPG1_REPRT( ' ', QUIET, LOG, FDL, STATUS )

*  Display the total number of pixels within the aperture.
      TEXT = '  No. of pixels in aperture:'
      IAT = 29
      CALL CHR_PUTI( NUMPIX, TEXT, IAT )
      CALL KPG1_REPRT( TEXT( : IAT ), QUIET, LOG, FDL, STATUS )

*  Display the number of good pixels included in the statistics.
      TEXT = '  No. of good pixels       :'
      IAT = 29
      CALL CHR_PUTI( NGOOD, TEXT, IAT )
      CALL KPG1_REPRT( TEXT( : IAT ), QUIET, LOG, FDL, STATUS )

*  Display a blank line.
      CALL KPG1_REPRT( ' ', QUIET, LOG, FDL, STATUS )

*  Display the total pixel sum.
      TEXT = '  Total data sum                      :'
      IAT = 41
      IF( TOT .NE. VAL__BADD ) THEN
         CALL CHR_PUTD( TOT, TEXT, IAT )
      ELSE
         CALL CHR_APPND( '(undefined)', TEXT, IAT )
      END IF
      CALL KPG1_REPRT( TEXT( : IAT ), QUIET, LOG, FDL, STATUS )

*  Display the standard deviation on the total data sum.
      TEXT = '  Standard deviation on total data sum:'
      IAT = 41
      IF( SGTOT .NE. VAL__BADD ) THEN
         CALL CHR_PUTD( SGTOT, TEXT, IAT )
      ELSE
         CALL CHR_APPND( '(undefined)', TEXT, IAT )
      END IF
      CALL KPG1_REPRT( TEXT( : IAT ), QUIET, LOG, FDL, STATUS )

*  Display the mean pixel value.
      TEXT = '  Mean pixel value                    :'
      IAT = 41
      IF( MEAN .NE. VAL__BADD ) THEN
         CALL CHR_PUTD( MEAN, TEXT, IAT )
      ELSE
         CALL CHR_APPND( '(undefined)', TEXT, IAT )
      END IF
      CALL KPG1_REPRT( TEXT( : IAT ), QUIET, LOG, FDL, STATUS )

*  Display the standard deviation on the mean pixel value.
      TEXT = '  Standard deviation on mean value    :'
      IAT = 41
      IF( SGMEAN .NE. VAL__BADD ) THEN
         CALL CHR_PUTD( SGMEAN, TEXT, IAT )
      ELSE
         CALL CHR_APPND( '(undefined)', TEXT, IAT )
      END IF
      CALL KPG1_REPRT( TEXT( : IAT ), QUIET, LOG, FDL, STATUS )

*  Display the standard deviation of the pixel values.
      TEXT = '  Standard deviation of pixel values  :'
      IAT = 41
      IF( SIGMA .NE. VAL__BADD ) THEN
         CALL CHR_PUTD( SIGMA, TEXT, IAT )
      ELSE
         CALL CHR_APPND( '(undefined)', TEXT, IAT )
      END IF
      CALL KPG1_REPRT( TEXT( : IAT ), QUIET, LOG, FDL, STATUS )

*  Display a blank line.
      CALL KPG1_REPRT( ' ', QUIET, LOG, FDL, STATUS )

*  Write the final results to the output parameters.
      CALL PAR_PUT0D( 'MEAN', MEAN, STATUS )
      CALL PAR_PUT0I( 'NGOOD', NGOOD, STATUS )
      CALL PAR_PUT0I( 'NUMPIX', NUMPIX, STATUS )
      CALL PAR_PUT0D( 'SIGMA', SIGMA, STATUS )
      CALL PAR_PUT0D( 'SIGMEAN', SGMEAN, STATUS )
      CALL PAR_PUT0D( 'SIGTOTAL', SGTOT, STATUS )
      CALL PAR_PUT0D( 'TOTAL', TOT, STATUS )

*  Shutdown procedure.
*  ===================
 999  CONTINUE

*  Close any log file.
      IF( LOG ) CALL FIO_ANNUL( FDL, STATUS )

*  Free the dynamic array space of the logical mask.
      CALL PSX_FREE( IPMASK, STATUS )
      IF( INDF3 .EQ. NDF__NOID ) CALL PSX_FREE( IPMSK2, STATUS )

*  Delete the group used to hold the ARD description.
      CALL GRP_DELET( IGRP, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Add a context report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'APERADD_ERR', 'APERADD: Failed to sum NDF '//
     :                 'pixels within an aperture.', STATUS )
      END IF

      END
