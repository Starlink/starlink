      SUBROUTINE LOOK( STATUS )
*+
*  Name:
*     LOOK

*  Purpose:
*     List pixel values in an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation
*     CALL LOOK( STATUS )

*  Description:
*     This application lists pixel values within a rectangular region of
*     a 2D NDF. The listing may be displayed on the screen and logged in
*     a text file (see parameters QUIET and LOGFILE). The rectangular region 
*     to be listed can be specified either by giving its centre and size 
*     (see parameters CENTRE and SIZE), or its corners (see parameters LBOUND 
*     and UBOUND). The top-right pixel value is also written to an output 
*     parameter (VALUE). The listing may be produced in several different
*     formats (see parameter FORMAT), and the format of each individual
*     displayed data value can be controlled using parameter STYLE.

*  Usage:
*     look ndf centre [size] [lbound] [ubound] [logfile] [format] [comp] 

*  ADAM Parameters:
*     CENTRE = LITERAL (Read)
*        The co-ordinates of the data pixel at the centre of the area to
*        be displayed, in the current co-ordinate Frame of the NDF (supplying 
*        a colon ":" will display details of the current co-ordinate Frame). 
*        The position should be supplied as a list of formatted axis values 
*        separated by spaces or commas. See also parameter USEAXIS.  A
*        null (!) value causes the displayed area to be determined using
*        parameters LBOUND and UBOUND.
*     COMP = LITERAL (Read)
*        The NDF array component to be displayed.  It may be "Data",
*        "Quality", "Variance", or "Error" (where "Error" is an
*        alternative to "Variance" and causes the square root of the
*        variance values to be displayed).  If "Quality" is specified,
*        then the quality values are treated as numerical values (in
*        the range 0 to 255). ["Data"]
*     FORMAT = LITERAL (Read)
*        Specifies the format for the listing:
*
*        - "strips" -- The area being displayed is divided up into
*        vertical strips of limited width. Each strip is displayed in
*        turn, with Y pixel index at the left of each row, and
*        X pixel index at the top of each column. The highest row is
*        listed first in each strip. This format is intended for human
*        readers - the others are primarily intended for being read by
*        other software.
*
*        - "clist" -- Each row of textual output consists of an X pixel 
*        index, followed by a Y pixel index, followed by the pixel data
*        value. No headers or blank lines are included. The pixels are
*        listed in "fortran order" - the lower left pixel first, and the
*        upper right pixel last. 
*
*        - "vlist" -- Each row of textual output consists of just the
*        pixel data value. No headers or blank lines are included. The 
*        pixels are listed in "fortran order" - the lower left pixel first, 
*        and the upper right pixel last.
*
*        - "region" -- The pixel data values are listed as a 2 dimensional
*        region. Each row of textual output contains a whole row of data 
*        values. The textual output may be truncated if it is too wide. The
*        lowest row is listed first.
*
*        In all cases, adjacent values are sepaerated by spaces, and bad
*        pixel values are represented by the string "BAD". ["strips"]
*
*     LBOUND = LITERAL (Read)
*        The co-ordinates of the data pixel at the bottom-left of the 
*        area to be displayed, in the current co-ordinate Frame of the NDF 
*        (supplying a colon ":" will display details of the current 
*        co-ordinate Frame). The position should be supplied as a list of 
*        formatted axis values separated by spaces or commas. See also 
*        parameter USEAXIS.  A null (!) value causes the bottom-left corner
*        of the supplied NDF to be used. The value is ignored unless a null 
*        value is supplied for parameter CENTRE or SIZE.
*     LOGFILE = FILENAME (Write)
*        The name of the text file in which the textual output may be stored. 
*        See MAXLEN. A null string (!) means that no file is created.  [!]
*     MAXLEN = _INTEGER (Read)
*        The maximum number of characters in a line of textual output. The 
*        line is truncated after the last complete value if it would extend
*        beyond this value. [80]
*     NDF = NDF (Read)
*        The input NDF structure containing the data to be displayed.
*     QUIET = LOGICAL (Read)
*        If TRUE then output is not displayed on the screen. The log file
*        and output parameter values are still created. [FALSE]
*     SIZE( 2 ) = _INTEGER (Read)
*        The dimensions of the rectangular area to be displayed, in pixels.
*        If a single value is given, it is used for both axes. The area
*        is centred on the position specified by parameter CENTRE. A null 
*        (!) value for SIZE causes the displayed area to be determined using 
*        parameters LBOUND and UBOUND. [7]
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the format to use 
*        for individual data values.
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text file
*        preceded by an up-arrow character "^". Such text files should
*        contain further comma-separated lists which will be read and 
*        interpreted in the same manner. Attribute settings are applied in 
*        the order in which they occur within the list, with later settings
*        over-riding any earlier settings given for the same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*        
*        where <name> is the name of a Frame attribute, and <value> is
*        the value to assign to the attribute. Default values will be
*        used for any unspecified attributes. All attributes will be
*        defaulted if a null value (!) is supplied. See section "Plotting
*        Attributes" in SUN/95 for a description of the available
*        attributes. Any unrecognised attributes are ignored (no error is
*        reported). 
*
*        Data values are formatted using attributes Format(1) and Digits(1). 
*        [current value] 
*     UBOUND = LITERAL (Read)
*        The co-ordinates of the data pixel at the top-right corner of the 
*        area to be displayed, in the current co-ordinate Frame of the NDF 
*        (supplying a colon ":" will display details of the current 
*        co-ordinate Frame). The position should be supplied as a list of 
*        formatted axis values separated by spaces or commas. See also 
*        parameter USEAXIS.  A null (!) value causes the top right corner
*        of the supplied NDF to be used. The value is ignored unless a null 
*        value is supplied for parameter CENTRE or SIZE.
*     USEAXIS = GROUP (Read)
*        USEAXIS is only accessed if the current co-ordinate Frame of the 
*        NDF has more than 2 axes. A group of two strings should be
*        supplied specifying the 2 axes which are to be used when supplying 
*        positions for parameters CENTRE, LBOUND and UBOUND. Each axis can 
*        be specified either by its integer index within the current Frame 
*        (in the range 1 to the number of axes in the current Frame), or by
*        its symbol string. A list of acceptable values is displayed if an 
*        illegal value is supplied. If a null (!) value is supplied, the axes 
*        with the same indices as the 2 used pixel axes within the NDF
*        are used. [!]
*     VALUE = _DOUBLE (Write)
*        An output parameter to wghich is written the data value at the 
*        top-right pixel in the displayed region.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     look ngc6872 "1:27:23 -22:41:12" quiet logfile=log
*        Lists a 7x7 block of pixel values centred on RA/DEC 1:27:23,
*        -22:41:12 (this assumes that the current co-ordinate Frame in
*        the NDF is an RA/DEC Frame). The listing is written to the text 
*        file "log" but is not displayed on the screen.
*     look ngc6872 centre=\! lbound="189 207" ubound="203 241" values.dat 
*        Lists the pixel values in an NDF called ngc6872, within a
*        rectangular region from pixel (189,207) to (203,241) (this
*        assumes that the current co-ordinate Frame in the NDF is pixel
*        co-ordinates). The listing is displayed on the screen and written 
*        to the text file "values.dat".
*     look ngc6872 centre="10 11" size=1 quiet 
*        Stores the value of pixel (10,11) in output parameter VALUE, but
*        does not display it on the screen or store it in a log file. This
*        assumes that the current co-ordinate Frame in the NDF is pixel
*        co-ordinates. 

*  Related Applications:
*     KAPPA: TRANDAT.

*  Implementation Status:
*     -  This routine correctly processes the DATA, QUALITY and 
*     VARIANCE components of the input NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-OCT-2001 (DSB):
*        Original NDF/AST version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'PAR_ERR'        ! PAR error constants
      INCLUDE 'AST_PAR'        ! AST constants and functions

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER COMP*8         ! Component to be displayed
      CHARACTER FORMAT*6       ! Output format
      CHARACTER MCOMP*8        ! Component to be mapped
      CHARACTER TEXT*80        ! Text buffer
      DOUBLE PRECISION CC( 2 ) ! Current Frame co-ords 
      DOUBLE PRECISION GC( 2 ) ! GRID co-ords 
      DOUBLE PRECISION VALUE   ! Final data value
      INTEGER EL               ! No. of mapped values
      INTEGER FDL              ! Log file descriptor
      INTEGER FRM              ! Frame used to format data values
      INTEGER I                ! Loop counter
      INTEGER INDF1            ! Input NDF
      INTEGER INDF2            ! Section of input NDF to be listed
      INTEGER IPDAT            ! Pointer to mapped pixel values
      INTEGER IPLINE           ! Pointer to line buffer
      INTEGER IWCS             ! Pointer to the WCS FrameSet from the NDF
      INTEGER MAXLEN           ! Maximum line length
      INTEGER NC               ! No. of characters used
      INTEGER NVAL             ! No. of values obtained from parameter
      INTEGER RLBND( 2 )       ! The lower bounds of region to be listed
      INTEGER RUBND( 2 )       ! The upper bounds of region to be listed
      INTEGER SDIM( 2 )        ! The significant NDF axes
      INTEGER SIZE( 2 )        ! Region dimensions
      INTEGER SLBND( 2 )       ! Significant lower bounds of the image
      INTEGER SUBND( 2 )       ! Significant upper bounds of the image
      LOGICAL LOG              ! Write to log file?
      LOGICAL QUIET            ! Suppress screen output?
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the input NDF identifier.
      CALL LPG_ASSOC( 'NDF', 'READ', INDF1, STATUS )

*  Find which component to display. MCOMP is for use with NDF_MAP and may
*  be set to 'Error'. COMP is for use with all other NDF routines (which
*  do not accept 'Error' as an NDF component name), and has 'Variance' in
*  place of 'Error'.
      CALL KPG1_ARCOG( 'COMP', INDF1, MCOMP, COMP, STATUS )

*  Now get the WCS FrameSet from the NDF.
      CALL KPG1_ASGET( INDF1, 2, .FALSE., .TRUE., .TRUE., SDIM, 
     :                 SLBND, SUBND, IWCS, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the centre position (used as a default for CENTRE).
      GC( 1 ) = 0.5*( SUBND( SDIM( 1 ) ) - SLBND( SDIM( 1 ) ) )  + 1.0
      GC( 2 ) = 0.5*( SUBND( SDIM( 2 ) ) - SLBND( SDIM( 2 ) ) )  + 1.0
      CALL AST_TRAN2( IWCS, 1, GC( 1 ), GC( 2 ), .TRUE., CC( 1 ), 
     :                CC( 2 ), STATUS ) 

*  KPG1_GTPOS displays the default with large accuracy. This is probably
*  more than we need here, so reduce the accuracy of the defaults by
*  formatting them and unformatting them.
      DO I = 1, 2
         TEXT = AST_FORMAT( IWCS, I, CC( I ), STATUS )
         NC = AST_UNFORMAT( IWCS, I, TEXT, CC( I ), STATUS ) 
      END DO

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the Current Frame co-ordinates (returned in CC) to put at the 
*  centre of the listing, using parameter CENTRE. 
      CALL KPG1_GTPOS( 'CENTRE', IWCS, .FALSE., CC, GC, STATUS )

*  Get the box size in pixel, and store the pixel index bounds of the 
*  region to be listed.
      CALL PAR_GDRVI( 'SIZE', 2, 1, 200, SIZE, NVAL, STATUS )
      IF ( NVAL .LT. 2 ) SIZE( 2 ) = SIZE( 1 )

*  If no error occurred, store the pixel index bounds of the 
*  region to be listed.
      IF( STATUS .EQ. SAI__OK ) THEN
         RLBND( 1 ) = NINT( GC( 1 ) ) - 1 + SLBND( SDIM( 1 ) ) - 
     :                SIZE( 1 )/2
         RUBND( 1 ) = RLBND( 1 ) + SIZE( 1 ) - 1
         RLBND( 2 ) = NINT( GC( 2 ) ) - 1 + SLBND( SDIM( 2 ) ) - 
     :                SIZE( 2 )/2
         RUBND( 2 ) = RLBND( 2 ) + SIZE( 2 ) - 1
 
*  If no centre or size value was obtained, annul the error and get the pixel
*  index bounds of the region to be listed using parameters LBOUND and
*  UBOUND.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )   

         CC( 1 ) = AST__BAD
         CALL KPG1_GTPOS( 'LBOUND', IWCS, .FALSE., CC, GC, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )   
            RLBND( 1 ) = SLBND( SDIM( 1 ) ) 
            RLBND( 2 ) = SLBND( SDIM( 2 ) ) 
         ELSE
            RLBND( 1 ) = NINT( GC( 1 ) ) - 1 + SLBND( SDIM( 1 ) )
            RLBND( 2 ) = NINT( GC( 2 ) ) - 1 + SLBND( SDIM( 2 ) ) 
         END IF

         CC( 1 ) = AST__BAD
         CALL KPG1_GTPOS( 'UBOUND', IWCS, .FALSE., CC, GC, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )   
            RUBND( 1 ) = SUBND( SDIM( 1 ) ) 
            RUBND( 2 ) = SUBND( SDIM( 2 ) ) 
         ELSE
            RUBND( 1 ) = NINT( GC( 1 ) ) - 1 + SLBND( SDIM( 1 ) )
            RUBND( 2 ) = NINT( GC( 2 ) ) - 1 + SLBND( SDIM( 2 ) ) 
         END IF

      END IF

*  Get an NDF identifier for the relevant section.
      CALL NDF_SECT( INDF1, 2, RLBND, RUBND, INDF2, STATUS ) 

*  Map the required NDF component.
      CALL NDF_MAP( INDF2, MCOMP, '_DOUBLE', 'READ', IPDAT, EL,
     :              STATUS )

*  Get the maximum line length, and allocate a buffer string of this size.
      CALL PAR_GDR0I( 'MAXLEN', 80, 1, 10000, .TRUE., MAXLEN, STATUS )
      CALL PSX_CALLOC( MAXLEN, '_CHAR', IPLINE, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to open a log file to store the results.
      LOG = .FALSE.
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', MAXLEN, FDL, STATUS )

*  Annul the error if a null value was given, and indicate that a log
*  file is not to be created.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         LOG = .TRUE.

      END IF

*  See if we are to run quietly.
      CALL PAR_GET0L( 'QUIET', QUIET, STATUS )

*  Obtain the formatting method to use.
      CALL PAR_CHOIC( 'FORMAT', 'strips', 'Strips,Clist,Vlist,Region', 
     :                .FALSE., FORMAT, STATUS )

*  Create a 1D Frame and set its style. The AST_FORMAT method for axis 1 of
*  this Frame is used to format the data values.
      FRM = AST_FRAME( 1, ' ', STATUS )
      CALL KPG1_ASSET( 'KAPPA_LOOK', 'STYLE', FRM, STATUS )

*  Do the work.
      CALL KPS1_LOOK( FRM, RLBND( 1 ), RUBND( 1 ), RLBND( 2 ), 
     :                RUBND( 2 ), %VAL( IPDAT ), QUIET, LOG, FDL, 
     :                %VAL( IPLINE ), FORMAT, MAXLEN, VALUE, STATUS, 
     :                %VAL( MAXLEN ) )

*  Write out the last data value to the output parameter.
      CALL PAR_PUT0D( 'VALUE', VALUE, STATUS )

*  Shutdown procedure.
*  ===================
 999  CONTINUE

*  Close any log file.
      IF( LOG ) CALL FIO_ANNUL( FDL, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Add a context report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'LOOK_ERR', 'LOOK: Failed to list pixel values'//
     :                 ' in a 1 or 2-dimensional data set.', STATUS )
      END IF

      END
