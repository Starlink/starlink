      SUBROUTINE COLCOMP( STATUS )
*+
*  Name:
*     COLCOMP

*  Purpose:
*     Produces a colour composite of up to three 2-dimensional NDFs.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL COLCOMP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application combines up to three 2-dimensional NDFs, using a
*     different primary colour (red, green or blue) to represent each
*     NDF. The resulting colour composite image is available in two
*     forms; as an NDF with an associated colour table (see parameter
*     OUT and LUT), and as an ASCII PPM image file (see parameter PPM).
*     The full pixel resolution of the input NDFs is retained. Note,
*     this application does not actually display the image, it just
*     creates various output files which must be displayed using other
*     tools (see below).
*
*     The data values in each of the input NDFs which are to be mapped
*     on to zero intensity and full intensity can be given manually
*     using parameters RLOW, RHIGH, GLOW, GHIGH, BLOW and BHIGH, but by
*     default they are evaluated automatically. This is done by finding
*     specified percentile points within the data histograms of each of
*     the input images (see parameter PERCENTILES).
*
*     The NDF outputs are intended to be displayed with KAPPA
*     application DISPLAY, using the command:
*
*        display <out> scale=no lut=<lut>
*
*     where "<out>" and "<lut>" are the names of the NDF image and
*     colour table created by this application using parameters OUT and
*     LUT. The main advantage of this NDF form of output over the PPM
*     form is that any WCS or AXIS information in the input NDFs is
*     still available, and can be used to create axis annotations by the
*     DISPLAY command. The graphics device which will be used to display
*     the image must be specified when running this application (see
*     parameter DEVICE).
*
*     The PPM form of output can be displayed using tools such as "xv",
*     or converted into other forms (GIF or JPEG, for instance) using
*     tools such as "ppmtogif" and "cjpeg" from the NetPbm or PbmPlus
*     packages. These tools provide more sophisticated colour
*     quantisation methods than are used by this application when
*     creating the NDF outputs, and so may give better visual results.

*  Usage:
*     colcomp inr ing inb out lut device

*  ADAM Parameters:
*     BADCOL = LITERAL (Read)
*        The colour with which to mark any bad (i.e. missing) pixels in
*        the  display.  There are a number of options described below:
*
*        - "MAX" -- The maximum colour index used for the display of
*        the image.
*
*        - "MIN" -- The minimum colour index used for the display of
*        the image.
*
*        - An integer -- The actual colour index.  It is constrained
*        between 0 and the maximum colour index available on the device.
*
*        - A named colour -- Uses the named colour from the palette, and
*        if it is not present, the nearest colour from the palette is
*        selected.
*
*        -  An HTML colour code such as \#ff002d.
*
*        If the colour is to remain unaltered as the lookup table is
*        manipulated choose an integer between 0 and 15, or a named
*        colour.  Note, if only the PPM output is to be created (see
*        parameter PPM), then a named colour must be given for BADCOL.
*        [current value]
*     BHIGH = _DOUBLE (Read)
*        The data value corresponding to full blue intensity. If a null
*        (!) value is supplied, the value actually used will be
*        determined by forming a histogram of the data values in the
*        NDF specified by parameter INB, and finding the data value at
*        the second histogram percentile specified by parameter
*        PERCENTILES. [!]
*     BLOW = _DOUBLE (Read)
*        The data value corresponding to zero blue intensity. If a null
*        (!) value is supplied, the value actually used will be
*        determined by forming a histogram of the data values in the
*        NDF specified by parameter INB, and finding the data value at
*        the first histogram percentile specified by parameter
*        PERCENTILES. [!]
*     DEVICE = DEVICE (Read)
*        The name of the graphics device which will be used to display
*        the NDF output (see parameter OUT). This is needed only to
*        determine the number of available colours. No graphics output
*        is created by this application. This parameter is not accessed
*        if a null (!) value is supplied for parameter OUT. The device
*        must have at least 24 colour indices or greyscale intensities.
*        [current image-display device]
*     GHIGH = _DOUBLE (Read)
*        The data value corresponding to full green intensity. If a null
*        (!) value is supplied, the value actually used will be
*        determined by forming a histogram of the data values in the
*        NDF specified by parameter ING, and finding the data value at
*        the second histogram percentile specified by parameter
*        PERCENTILES. [!]
*     GLOW = _DOUBLE (Read)
*        The data value corresponding to zero green intensity. If a null
*        (!) value is supplied, the value actually used will be
*        determined by forming a histogram of the data values in the
*        NDF specified by parameter ING, and finding the data value at
*        the first histogram percentile specified by parameter
*        PERCENTILES. [!]
*     INB = NDF (Read)
*        The input NDF containing the data to be displayed in blue. A
*        null (!) value may be supplied in which case the blue intensity
*        in the output will be zero at every pixel.
*     ING = NDF (Read)
*        The input NDF containing the data to be displayed in green. A
*        null (!) value may be supplied in which case the green
*        intensity in the output will be zero at every pixel.
*     INR = NDF (Read)
*        The input NDF containing the data to be displayed in red. A
*        null (!) value may be supplied in which case the red intensity
*        in the output will be zero at every pixel.
*     LUT = NDF (Write)
*        Name of the output NDF to contain the colour lookup table which
*        should be used when displaying the NDF created using parameter
*        OUT. This colour table can be loaded using LUTREAD, or
*        specified when the image is displayed. This parameter is not
*        accessed if a null (!) value is given for parameter OUT.
*     RHIGH = _DOUBLE (Read)
*        The data value corresponding to full red intensity. If a null
*        (!) value is supplied, the value actually used will be
*        determined by forming a histogram of the data values in the
*        NDF specified by parameter INR, and finding the data value at
*        the second histogram percentile specified by parameter
*        PERCENTILES. [!]
*     RLOW = _DOUBLE (Read)
*        The data value corresponding to zero red intensity. If a null
*        (!) value is supplied, the value actually used will be
*        determined by forming a histogram of the data values in the
*        NDF specified by parameter INR, and finding the data value at
*        the first histogram
*        percentile specified by parameter PERCENTILES. [!]
*     OUT = NDF (Write)
*        The output colour composite image in NDF format. Values in
*        this output image are integer colour indices into the colour
*        table created using parameter LUT. The values are shifted to
*        account for the indices reserved for the palette (i.e. the
*        first entry in the colour table is addressed as entry 16, not
*        entry 1). The NDF is intended to be used as the input data in
*        conjunction with DISPLAY SCALE=FALSE. If a null value (!) is
*        supplied, no output NDF will be created.
*     PERCENTILES( 2 ) = _REAL (Read)
*        The percentiles that define the default scaling limits. For
*        example, [25,75] would scale between the quartile values.
*        [5,95]
*     PPM = FILE (Write)
*        The name of the output text file to contain the PPM form of the
*        colour composite image. The colours specified in this file
*        represent the input data values directly. They are not
*        quantised or dithered in any way. Also note that because this
*        is a text file, containing formatted data values, it is
*        portable, but can be very large, and slow to read and write. If
*        a null (!) value is supplied, no PPM output is created. [!]

*  Examples:
*     colcomp m31_r m31_g m31_b m31_col m31_lut
*        Combines the 3 NDFs m31_r, m31_g, and m31_b to create a colour
*        composite image stored in NDF m31_col. A colour look-up table
*        is also created and stored in NDF m31_lut. It is assumed that
*        the output image will be displayed on the current graphics
*        device. The created colour composite image should be displayed
*        using the command:
*
*           display m31_col scale=no lut=m31_lut
*
*     colcomp m31_r m31_g m31_b out=! ppm=m31.ppm
*        As above, but no NDF outputs are created. Instead, a file
*        called m31.ppm is created which (for instance) can be displayed
*        using the command:
*
*           xv m31.ppm
*
*        It can be converted to a GIF (for instance, for inclusion in
*        WWW pages) using the command:
*
*           ppmquant 256 m31.ppm | ppmtogif > m31.gif
*
*        These commands assume you have "xv", "ppmquant" and "ppmtogif"
*        installed at your site. None of them command are part of KAPPA.

*  Notes:
*     -  The output image (PPM or NDF) covers the area of overlap
*     between the input NDFs at full resolution.
*     -  The output image is based on the values in the DATA components
*     of the input NDFs. Any VARIANCE and QUALITY arrays in the input
*     NDFs are ignored.

*  Related Applications:
*     KAPPA: DISPLAY, LUTREAD; XV; PBMPLUS; NETPBM.

*  Implementation Status:
*     -  The HISTORY, WCS and AXIS components, together with any
*     extensions are propagated to the output NDF, from the first
*     supplied input NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  Only data of type _REAL can be processed directly. Data of
*     other types will be converted to _REAL before being processed.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     Copyright (C) 1999, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-NOV-1999 (DSB):
*        Original version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 February 24 (MJC):
*        Added new CUMUL argument set to .FALSE. to KPG1_GHSTx call.
*     2006 April 12 (MJC):
*        Remove unused variable and wrapped long lines.
*     2011-08-22 (TIMJ):
*        Add new WGTS and WEIGHT arguments to KPG1_GHSTx calls.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'PRM_PAR'        ! VAL_ constants
      INCLUDE 'NDF_PAR'        ! NDF constants
      INCLUDE 'PAR_ERR'        ! PAR error constants
      INCLUDE 'SUBPAR_PAR'     ! SUBPAR constants
      INCLUDE 'CTM_PAR'        ! Colour-table management constants
      INCLUDE 'CNF_PAR'        ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER R, G, B          ! Indices for red, green and blue array
      PARAMETER ( R = 1, G = 2, B = 3 ) ! elements

      INTEGER MINCOL           ! Minimum number of colour indices on
                               ! device to be classed as an image
                               ! display
      PARAMETER ( MINCOL = 8 + CTM__RSVPN )

      INTEGER NHIST            ! No. of bins in 3D histogram.
      PARAMETER ( NHIST = 128*128*128 )

      INTEGER NUMBIN           ! Number of bins in histogram
      PARAMETER ( NUMBIN = 2048 )

*  Local Variables:
      CHARACTER BADCOL*30      ! Name of bad colour
      CHARACTER COL( 3 )*5     ! Primary colour names
      CHARACTER NAME*255       ! An input NDF name
      CHARACTER PARHI( 3 )*5   ! Parameter names for upper scaling limit
      CHARACTER PARLO( 3 )*4   ! Parameter names for lower scaling limit
      INTEGER ACTHI            ! The HIGH parameter state
      INTEGER ACTLO            ! The LOW parameter state
      INTEGER BPCI             ! Bad pixel colour index
      INTEGER EL               ! No. of mapped elements
      INTEGER FD               ! File descriptor for output PPM image
      INTEGER FIRST            ! Colour of first supplied input image
      INTEGER HIST( NUMBIN )   ! Histogram for finding scaling limits
      INTEGER I                ! Loop count
      INTEGER IAX              ! Axis index
      INTEGER INDF( 3 )        ! Input NDF identifiers
      INTEGER INDFL            ! Identifier output NDF containing LUT
      INTEGER INDFO            ! Identifier output NDF containing image
      INTEGER INDFS            ! Identifier for input NDF section
      INTEGER IPBHST           ! Pointer to work space
      INTEGER IPGHST           ! Pointer to work space
      INTEGER IPIC             ! AGI id. for current picture
      INTEGER IPIN( 3 )        ! Pointers to mapped input data arrays
      INTEGER IPLUT            ! Pointer to mapped output LUT
      INTEGER IPOUT            ! Pointer to mapped output data array
      INTEGER IPPHST           ! Pointer to work space
      INTEGER IPRHST           ! Pointer to work space
      INTEGER J                ! Loop count
      INTEGER LBND( NDF__MXDIM )! Lower bounds of input NDF
      INTEGER LBNDO( 2 )       ! Lower bounds of output NDF
      INTEGER LUTBND( 2 )      ! Upper bounds of output LUT
      INTEGER MAXPOS           ! Position of the maximum (not used)
      INTEGER MINPOS           ! Position of the minimum (not used)
      INTEGER NDATA            ! No. of data elements in output image
      INTEGER NDIM             ! No. of axes in input NDF
      INTEGER NIM              ! No. of supplied input NDFs
      INTEGER NINVAL           ! Number of bad values in the input array
      INTEGER NLUT             ! No. of entries in output LUT
      INTEGER NMLEN            ! Used length of NAME
      INTEGER SDIM( 2, 3 )     ! Indices fo significan axes
      INTEGER SLBND( 2, 3 )    ! Lower bounds of significant axes
      INTEGER SUBND( 2, 3 )    ! Upper bounds of significant axes
      INTEGER UBND( NDF__MXDIM )! Upper bounds of input NDF
      INTEGER UBNDO( 2 )       ! Upper bounds of output image
      LOGICAL BAD              ! The array may contain bad pixels?
      LOGICAL GOT( 3 )         ! Which input NDFs were supplied?
      LOGICAL POSTIV           ! Scaling of the array is to be positive?
      REAL DUMMY               ! Used to swap percentiles
      REAL HI( 3 )             ! Upper scaling limits
      REAL LO( 3 )             ! Lower scaling limits
      REAL PERCNT( 2 )         ! Percentiles
      REAL PERVAL( 2 )         ! Values at the percentiles
      REAL RGBBAD( 3 )         ! RGB values for bad colour
      REAL RMAXV               ! Minimum value in the array
      REAL RMINV               ! Maximum value in the array

*  Local Data:
      DATA PARLO/ 'RLOW', 'GLOW', 'BLOW' /,
     :     PARHI/ 'RHIGH', 'GHIGH', 'BHIGH' /,
     :     COL/ 'red', 'green', 'blue' /

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise the number of input NDFs supplied.
      NIM = 1
      FIRST = 0

*  Initialise the area common to all input images.
      LBNDO( 1 ) = VAL__MINI
      UBNDO( 1 ) = VAL__MAXI
      LBNDO( 2 ) = VAL__MINI
      UBNDO( 2 ) = VAL__MAXI

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the red input NDF, ensuring that it does not have more than 2
*  significant dimensions. The indices of the two axes to use, together
*  with their pixel index bounds are returned.
      CALL KPG1_GTNDF( 'INR', 2, .FALSE., 'READ', INDF( R ),
     :                 SDIM( 1, R ), SLBND( 1, R ), SUBND( 1, R ),
     :                 STATUS )

*  If a null value for supplied for the parameter, annul the error, and
*  set a flag to indicate that we do not have the Red image.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOT( R ) = .FALSE.

*  Otherwise, set a flag to indicate that we do have the Red image,
*  update the overlap area common to all the input images obtained so
*  far, store the colour of this input image and increment the number
*  of input images obtained so far.
      ELSE
         GOT( R ) = .TRUE.

         LBNDO( 1 ) = MAX( LBNDO( 1 ), SLBND( 1, R ) )
         UBNDO( 1 ) = MIN( UBNDO( 1 ), SUBND( 1, R ) )
         LBNDO( 2 ) = MAX( LBNDO( 2 ), SLBND( 2, R ) )
         UBNDO( 2 ) = MIN( UBNDO( 2 ), SUBND( 2, R ) )

         IF( FIRST .EQ. 0 ) FIRST = R
         NIM = NIM + 1
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Do the same for the green input image.
      CALL KPG1_GTNDF( 'ING', 2, .FALSE., 'READ', INDF( G ),
     :                 SDIM( 1, G ), SLBND( 1, G ), SUBND( 1, G ),
     :                 STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOT( G ) = .FALSE.

      ELSE
         GOT( G ) = .TRUE.

         LBNDO( 1 ) = MAX( LBNDO( 1 ), SLBND( 1, G ) )
         UBNDO( 1 ) = MIN( UBNDO( 1 ), SUBND( 1, G ) )
         LBNDO( 2 ) = MAX( LBNDO( 2 ), SLBND( 2, G ) )
         UBNDO( 2 ) = MIN( UBNDO( 2 ), SUBND( 2, G ) )

         IF( FIRST .EQ. 0 ) FIRST = G
         NIM = NIM + 1
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Do the same for the blue input image.
      CALL KPG1_GTNDF( 'INB', 2, .FALSE., 'READ', INDF( B ),
     :                 SDIM( 1, B ), SLBND( 1, B ), SUBND( 1, B ),
     :                 STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOT( B ) = .FALSE.

      ELSE
         GOT( B ) = .TRUE.

         LBNDO( 1 ) = MAX( LBNDO( 1 ), SLBND( 1, B ) )
         UBNDO( 1 ) = MIN( UBNDO( 1 ), SUBND( 1, B ) )
         LBNDO( 2 ) = MAX( LBNDO( 2 ), SLBND( 2, B ) )
         UBNDO( 2 ) = MIN( UBNDO( 2 ), SUBND( 2, B ) )

         IF( FIRST .EQ. 0 ) FIRST = B
         NIM = NIM + 1
      END IF

*  Correct the number of input images supplied.
      NIM = NIM - 1

*  Report an error if no images were supplied.
      IF( NIM .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COLCOMP_ERR1', 'No input NDFs supplied.',
     :                 STATUS )
         GO TO 999
      END IF

*  Report an error if there is no common overlap area.
      IF( ( LBNDO( 1 ) .GT. UBNDO( 1 ) .OR.
     :      LBNDO( 2 ) .GT. UBNDO( 2 ) ) .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'N', NIM )
         CALL ERR_REP( 'COLCOMP_ERR2', 'There is no overlap between '//
     :                 'the ^N supplied input images.', STATUS )
         GO TO 999
      END IF

*  Find the percentiles required.
      PERCNT( 1 ) = 5.0
      PERCNT( 2 ) = 95.0
      CALL PAR_GDR1R( 'PERCENTILES', 2, PERCNT, 0.0, 100.0, .TRUE.,
     :                PERCNT, STATUS )

*  Convert percentiles to fractions.
      PERCNT( 1 ) = PERCNT( 1 ) * 0.01
      PERCNT( 2 ) = PERCNT( 2 ) * 0.01

*  Record the polarity.
      POSTIV = PERCNT( 2 ) .GT. PERCNT( 1 )

*  Prepare each supplied input image.
      DO I = 1, 3
         IF( GOT( I ) ) THEN

*  Abort if an error has occurred.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Save the name of the original supplied NDF before a section is taken
*  from it.
            CALL KPG1_NDFNM( INDF( I ), NAME, NMLEN, STATUS )

*  Get the bounds of the NDF, including any insignificant axes.
            CALL NDF_BOUND( INDF( I ), NDF__MXDIM, LBND, UBND, NDIM,
     :                      STATUS )

*  Replace the bounds for the axes being used within this input NDF
*  with the bounds of the overlap region.
            DO J = 1, 2
               IAX = SDIM( J, I )
               LBND( IAX ) = LBNDO( J )
               UBND( IAX ) = UBNDO( J )
            END DO

*  Get an NDF identifier for a section matching the overlap area.
            CALL NDF_SECT( INDF( I ), NDIM, LBND, UBND, INDFS, STATUS )

*  Use the section identifier from now on.
            CALL NDF_ANNUL( INDF( I ), STATUS )
            INDF( I ) = INDFS

*  Map the DATA array of the section.
            CALL NDF_MAP( INDF( I ), 'DATA', '_REAL', 'READ', IPIN( I ),
     :                    EL, STATUS )

*  If the upper and lower scaling limits were supplied on the command
*  line, just use them.
            CALL LPG_STATE( PARLO( I ), ACTLO, STATUS )
            CALL LPG_STATE( PARHI( I ), ACTHI, STATUS )

            IF( ACTLO .EQ. SUBPAR__ACTIVE .AND.
     :          ACTHI .EQ. SUBPAR__ACTIVE ) THEN

               CALL PAR_GET0R( PARLO( I ), LO( I ), STATUS )
               CALL PAR_GET0R( PARHI( I ), HI( I ), STATUS )

*  If either of the limits was not supplied, we need to find default
*  values to suggest for the limits.
            ELSE

*  Obtain the maximum and minimum values to define the bounds
*  of the histogram used to find the percentiles.
               CALL KPG1_MXMNR( .TRUE., EL,
     :                          %VAL( CNF_PVAL( IPIN( I ) ) ), NINVAL,
     :                          RMAXV, RMINV, MAXPOS, MINPOS, STATUS )

*  The number of bad pixels has been counted so it might be
*  possible to save future processing.
               BAD = NINVAL .EQ. 0

*  Generate the histogram between those bounds.
               CALL KPG1_GHSTR( BAD, EL, %VAL( CNF_PVAL( IPIN( I ) ) ),
     :                          %VAL( CNF_PVAL( IPIN( I ) ) ), 0.0D0,
     :                          NUMBIN, .FALSE., RMAXV, RMINV, HIST,
     :                          STATUS )

*  Estimate the values at the percentiles.
               CALL KPG1_HSTFR( NUMBIN, HIST, RMAXV, RMINV, 2,
     :                          PERCNT, PERVAL, STATUS )

*  Swap the percentiles back if they were flipped.
               IF ( .NOT. POSTIV ) THEN
                  DUMMY = PERVAL( 1 )
                  PERVAL( 1 ) = PERVAL( 2 )
                  PERVAL( 2 ) = DUMMY
               END IF

*  Set the dynamic defaults for the scaling limits.  We do this so that
*  the default values will appear in any parameter prompts which are
*  issued (assuming PPATH=DYNAMIC in the interface file).
               CALL PAR_DEF0R( PARLO( I ), PERVAL( 1 ), STATUS )
               CALL PAR_DEF0R( PARHI( I ), PERVAL( 2 ), STATUS )

*  Check that no error has occurred.
               IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the lower limit.
               CALL PAR_GET0R( PARLO( I ), LO( I ), STATUS )

*  If a null value was supplied, annul the error and use the dynamic
*  default.  We do it like this, rather than just setting VPATH=DYNAMIC
*  in the interface file to facilitate automatic re-invocation of the
*  application for process groups of NDFs (see lpg_again.f).
               IF( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  LO( I ) = PERVAL( 1 )
               END IF

*  Now get the upper limit in the same way.
               CALL PAR_GET0R( PARHI( I ), HI( I ), STATUS )
               IF( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  HI( I ) = PERVAL( 2 )
               END IF

            END IF

*  Report the scaling limits for future use.
            CALL MSG_SETC( 'N', NAME( : NMLEN ) )
            CALL MSG_SETR( 'MX', HI( I ) )
            CALL MSG_SETR( 'MN', LO( I ) )
            CALL MSG_SETC( 'C', COL( I ) )
            CALL MSG_OUT( 'COLCOMP_MSG1', 'The ^C image (^N) will '//
     :                    'be scaled from ^MN to ^MX', STATUS )

*  If no image is available for this colour, store bad limits.
         ELSE
            HI( I ) = VAL__BADR
            LO( I ) = VAL__BADR
         END IF

      END DO

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Create the output NDF to contain the integer colour indices.
*  Propagate the section from the first supplied input NDF.
      CALL LPG_PROP( INDF( FIRST ), 'AXIS,WCS,NOLABEL,NOTITLE', 'OUT',
     :               INDFO, STATUS )

*  Annul the error if a null value was supplied.  We can still create
*  the PPM version of the image.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Otherwise, continue to create the output NDFs.
      ELSE

*  Set the output data type to _INTEGER.
         CALL NDF_STYPE( '_INTEGER', INDFO, 'DATA', STATUS )

*  Map the output data array.
         CALL NDF_MAP( INDFO, 'DATA', '_INTEGER', 'WRITE', IPOUT,
     :                 NDATA, STATUS )

*  Open the graphics device on which the image is to be displayed.
        CALL KPG1_PGOPN( 'DEVICE', 'READ', IPIC, STATUS )

*  Check whether chosen device is an 'image display' with a suitable
*  minimum number of colour indices, and obtain the number of colour
*  indices.
         CALL KPG1_PQVID( 'DEVICE', 'IMAGE_DISPLAY,IMAGE_OVERLAY,'//
     :                    'WINDOW,MATRIX_PRINTER', ' ', MINCOL, NLUT,
     :                    STATUS )

*  Obtain the colour for bad pixels as a colour index into the palette.
         CALL KPG1_PACOL( 'BADCOL', 0, CTM__RSVPN - 1, BPCI, STATUS )

*  Get the RGB values for the BAD colour.
         CALL PGQCR( BPCI, RGBBAD( R ), RGBBAD( G ), RGBBAD( B ) )

*  Shutdown the graphics device.
         CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  Correct the number of entries in the colour table to remove the
*  reserved pens.
         NLUT = NLUT + 1 - CTM__RSVPN

*  Create the output colour table.
         LUTBND( 1 ) = 3
         LUTBND( 2 ) = NLUT
         CALL LPG_CREP( 'LUT', '_REAL', 2, LUTBND, INDFL, STATUS )

*  Map the output colour table.
         CALL NDF_MAP( INDFL, 'DATA', '_REAL', 'WRITE', IPLUT, EL,
     :                 STATUS )

*  Get work space.
         CALL PSX_CALLOC( NHIST, '_INTEGER', IPPHST, STATUS )
         CALL PSX_CALLOC( NHIST, '_REAL', IPRHST, STATUS )
         CALL PSX_CALLOC( NHIST, '_REAL', IPGHST, STATUS )
         CALL PSX_CALLOC( NHIST, '_REAL', IPBHST, STATUS )

*  Create the output colour index array and colour table.
         CALL KPS1_CCMQN( NLUT, NDATA, %VAL( CNF_PVAL( IPIN( R ) ) ),
     :                    HI( R ),
     :                 LO( R ), %VAL( CNF_PVAL( IPIN( G ) ) ),
     :                 HI( G ), LO( G ),
     :                 %VAL( CNF_PVAL( IPIN( B ) ) ),
     :                 HI( B ), LO( B ), BPCI,
     :                 CTM__RSVPN, NHIST, %VAL( CNF_PVAL( IPRHST ) ),
     :                 %VAL( CNF_PVAL( IPGHST ) ),
     :                 %VAL( CNF_PVAL( IPBHST ) ),
     :                 %VAL( CNF_PVAL( IPPHST ) ),
     :                 %VAL( CNF_PVAL( IPLUT ) ),
     :                 %VAL( CNF_PVAL( IPOUT ) ), STATUS )

*  Free the work space.
         CALL PSX_FREE( IPPHST, STATUS )
         CALL PSX_FREE( IPRHST, STATUS )
         CALL PSX_FREE( IPGHST, STATUS )
         CALL PSX_FREE( IPBHST, STATUS )

      END IF

*  Check no error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to open a file to contain the PPM version of the composite
*  image.
      CALL FIO_ASSOC( 'PPM', 'WRITE', 'LIST', 80, FD, STATUS )

*  Annul the error if a null value was given.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Otherwise, create the PPM output.
      ELSE

*  If an output NDF was not created, get the BAD colour as a named
*  colour, and find the corresponding RGB values.
         IF( INDFO .EQ. NDF__NOID ) THEN
            CALL PAR_GET0C( 'BADCOL', BADCOL, STATUS )
            CALL KPG1_NMCOL( BADCOL, RGBBAD( R ), RGBBAD( G ),
     :                       RGBBAD( B ), STATUS )
         END IF

*  Store the data in the output file.
         CALL KPS1_CCMPP( FD, UBNDO( 1 ) - LBNDO( 1 ) + 1,
     :                    UBNDO( 2 ) - LBNDO( 2 ) + 1,
     :                    %VAL( CNF_PVAL( IPIN( R ) ) ),
     :                    HI( R ), LO( R ),
     :                    %VAL( CNF_PVAL( IPIN( G ) ) ), HI( G ),
     :                    LO( G ), %VAL( CNF_PVAL( IPIN( B ) ) ),
     :                    HI( B ), LO( B ),
     :                    RGBBAD, STATUS )

*  Close the output file.
         CALL FIO_CLOSE( FD, STATUS )

      END IF

*  Tidy up.
 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Add a context report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'COLCOMP_ERR', 'COLCOMP: Failed to create a '//
     :                 'colour composite NDF from separate RGB NDFs.',
     :                 STATUS )
      END IF

      END
