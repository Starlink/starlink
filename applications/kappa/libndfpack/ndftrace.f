      SUBROUTINE NDFTRACE( STATUS )
*+
*  Name:
*     NDFTRACE

*  Purpose:
*     Displays the attributes of an NDF data structure.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDFTRACE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine displays the attributes of an NDF data structure
*     including:
*     -  its name;
*     -  the values of its character components (title, label and
*     units);
*     -  its shape (pixel bounds, dimension sizes, number of dimensions
*     and total number of pixels);
*     -  axis co-ordinate information (axis labels, units and extents);
*     -  optionally, axis array attributes (type and storage form) and
*     the values of the axis normalisation flags;
*     -  attributes of the main data array and any other array
*     components present (including the type and storage form and an
*     indication of whether `bad' pixels may be present);
*     -  world coordinate system information (the title, domain, axis
*     labels and axis units for each coordinate system, plus the system
*     epoch and projection for sky coordinate systems).
*     -  a list of any NDF extensions present, together with their data
*     types; and
*     -  history information (creation and last-updated dates, the
*     update mode and the number of history records).
*
*     Most of this information is output to parameters.

*  Usage:
*     ndftrace ndf

*  ADAM Parameters:
*     AEND( ) = _DOUBLE (Write)
*        The axis upper extents of the NDF.  For non-monotonic axes,
*        zero is used.  See parameter AMONO.  This is not assigned if
*        AXIS is FALSE.
*     AFORM( ) = LITERAL (Write)
*        The storage forms of the axis centres of the NDF.  This is
*        only written when FULLAXIS is TRUE and AXIS is TRUE.
*     ALABEL( ) = LITERAL (Write)
*        The axis labels of the NDF.  This is not assigned if AXIS is
*        FALSE.
*     AMONO( ) = _LOGICAL (Write)
*        These are TRUE when the axis centres are monotonic, and FALSE
*        otherwise.  This is not assigned if AXIS is FALSE.
*     ANORM( ) = _LOGICAL (Write)
*        The axis normalisation flags of the NDF.  This is only written
*        when FULLAXIS is TRUE and AXIS is TRUE.
*     ASTART( ) = _DOUBLE (Write)
*        The axis lower extents of the NDF.  For non-monotonic axes,
*        zero is used.  See parameter AMONO.  This is not assigned if
*        AXIS is FALSE.
*     ATYPE( ) = LITERAL (Write)
*        The data types of the axis centres of the NDF.  This is only
*        written when FULLAXIS is TRUE and AXIS is TRUE.
*     AUNITS( ) = LITERAL (Write)
*        The axis units of the NDF.  This is not assigned if AXIS is
*        FALSE.
*     AVARIANCE( ) = _LOGICAL (Write)
*        Whether or not there are axis variance arrays present in the
*        NDF.  This is only written when FULLAXIS is TRUE and AXIS is
*        TRUE.
*     AXIS = _LOGICAL (Write)
*        Whether or not the NDF has an axis system.
*     BAD = _LOGICAL (Write)
*        If TRUE, the NDF's data array may contain bad values.
*     BADBITS = LITERAL (Write)
*        The BADBITS mask.  This is only valid when QUALITY is TRUE.
*     DIMS( ) = _INTEGER (Write)
*        The dimensions of the NDF.
*     EXTNAME( ) = LITERAL (Write)
*        The names of the extensions in the NDF.  It is only written
*        when NEXTN is positive.
*     EXTTYPE( ) = LITERAL (Write)
*        The types of the extensions in the NDF.  Their order
*        corresponds to the names in EXTNAME.  It is only written when
*        NEXTN is positive.
*     FDIM( ) = _INTEGER (Write)
*        The numbers of axes in each world coordinate system stored
*        in the NDF. The elements in this parameter correspond to those
*        in the FDOMAIN and FTITLE parameters. The number of elements in 
*        each of these parameters is given by NFRAME.
*     FDOMAIN( ) = LITERAL (Write)
*        The domain in which each world coordinate system stored 
*        in the NDF is defined. The list excludes GRID, PIXEL, and
*        AXIS, which are always available in any NDF. The elements in 
*        this parameter correspond to those in the FDIM and FTITLE 
*        parameters. The number of elements in each of these parameters 
*        is given by NFRAME.
*     FORM = LITERAL (Write)
*        The storage form of the NDF's data array.
*     FTITLE( ) = LITERAL (Write)
*        The title for each world coordinate system stored in the NDF.
*        The elements in this parameter correspond to those in the 
*        FDOMAIN and FDIM parameters. The number of elements in each 
*        of these parameters is given by NFRAME.
*     FULLAXIS = _LOGICAL (Read)
*        If the NDF being examined has an axis co-ordinate system
*        defined, then by default only the label, units and extent of
*        each axis will be displayed.  However, if a TRUE value is given
*        for this parameter, full details of the attributes of all the
*        axis arrays will also be given. [FALSE]
*     HISTORY = _LOGICAL (Write)
*        Whether or not the NDF contains HISTORY records.
*     LABEL = LITERAL (Write)
*        The label of the NDF.
*     LBOUND( ) = _INTEGER (Write)
*        The lower bounds of the NDF.
*     NDF = NDF (Read)
*        The NDF data structure whose attributes are to be displayed.
*     NDIM = _INTEGER (Write)
*        The number of dimensions of the NDF.
*     NEXTN = _INTEGER (Write)
*        The number of extensions in the NDF.
*     NFRAME = _INTEGER (Write)
*        The number of world coordinate systems described by parameters
*        FDIM, FDOMAIN and FTITLE. Set to zero if WCS is FALSE.
*     QUALITY = _LOGICAL (Write)
*        Whether or not the NDF contains a QUALITY array.
*     QUIET = _LOGICAL (Read)
*        A TRUE value suppresses the reporting of the NDF's attributes.
*        It is intended for procedures and scripts where only the
*        output parameters are needed. [FALSE]
*     TITLE = LITERAL (Write)
*        The title of the NDF.
*     TYPE = LITERAL (Write)
*        The data type of the NDF's data array.
*     UBOUND( ) = _INTEGER (Write)
*        The upper bounds of the NDF.
*     UNITS = LITERAL (Write)
*        The units of the NDF.
*     VARIANCE = _LOGICAL (Write)
*        Whether or not the NDF contains a VARIANCE array.
*     WCS = _LOGICAL (Write)
*        Whether or not the NDF has any world coordinate systems, over
*        and above the usual GRID, PIXEL and AXIS systems.
*     WIDTH( ) = _LOGICAL (Write)
*        Whether or not there are axis width arrays present in the NDF.
*        This is only written when FULLAXIS is TRUE and AXIS is TRUE.

*  Examples:
*     ndftrace mydata
*        Displays information about the attributes of the NDF structure
*        called mydata.
*     ndftrace ndf=r106 fullaxis
*        Displays information about the NDF structure r106, including
*        full details of any axis arrays present.
*     ndftrace mydata quiet ndim=(mdim)
*        Passes the number of dimensions of the NDF called mydata
*        into the ICL variable mdim.  No information is displayed.

*  Related Applications:
*     HDSTRACE.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 April 6 (RFWS):
*        Original version.
*     1991 April 29 (RFWS):
*        Added support for axis components and added the usage and
*        examples sections to the prologue.
*     1992 November 30 (MJC):
*        Reports non-monotonic axis centres.
*     1993 February 4 (MJC):
*        Fixed bug that caused incorrect axis extents to be reported
*        when there were no axis-width arrays and the axis centres did
*        not increment by one.
*     1994 July 29 (RFWS):
*        Adapted to handle history information.
*     1995 April 24 (MJC):
*        Made usage and examples lowercase.  Added Related Applications.
*        Added History to the Description.  Sorted the variable
*        declarations and other tidying.
*     1995 June 18 (MJC):
*        Added QUIET option and the output parameters.
*     1997 November 27 (DSB):
*        Added support for WCS component.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'AST_PAR'          ! AST_ public constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER MXEXTN             ! Maximum number of extensions
      PARAMETER ( MXEXTN = 32 )

      INTEGER MXFRM              ! Maximum number of WCS Frames
      PARAMETER ( MXFRM = 32 )

*  Local Variables:
      DOUBLE PRECISION AEND( NDF__MXDIM ) ! End of NDF extent along an axis
      CHARACTER * ( 80 ) ALABEL( NDF__MXDIM ) ! Axis label
      CHARACTER * ( 35 ) APPN    ! Last recorded application name
      DOUBLE PRECISION ASTART( NDF__MXDIM ) ! Start of NDF extent along an axis
      CHARACTER * ( 20 ) ATTRIB  ! AST Frame attribute name
      CHARACTER * ( NDF__SZTYP ) ATYPE ! Type for axis extent value
      CHARACTER * ( 80 ) AUNITS( NDF__MXDIM ) ! Axis units
      LOGICAL AVAR( NDF__MXDIM ) ! Whether NDF axis-variance components are defined
      INTEGER AXPNTR( 1 )        ! Pointer to axis centres
      LOGICAL BAD                ! Bad pixel flag
      BYTE BADBIT                ! Bad-bits mask
      INTEGER BBI                ! Bad-bits value as an integer
      CHARACTER * ( 8 ) BINSTR   ! Binary bad-bits mask string
      CHARACTER * ( NDF__MXDIM * ( 2 * VAL__SZI + 3 ) - 2 ) BUF ! Text buffer for shape information
      CHARACTER * ( 80 ) CCOMP   ! Character component
      DOUBLE PRECISION CFIRST( 1, NDF__MXDIM ) ! Frame coords of first pixel
      CHARACTER * ( NDF__SZFRM ) CFORM( NDF__MXDIM ) ! Type for axis centres 
      CHARACTER * ( NDF__SZHDT ) CREAT ! History component creation date
      CHARACTER * ( NDF__SZTYP ) CTYPE( NDF__MXDIM ) ! Type for axis centres 
      CHARACTER * ( NDF__SZHDT ) DATE ! Date of last history update
      INTEGER DIGVAL             ! Binary digit value
      INTEGER DIM( NDF__MXDIM )  ! Dimension sizes
      INTEGER EL                 ! Number of array elements mapped
      DOUBLE PRECISION EP        ! Epoch of observattion
      DOUBLE PRECISION EQ        ! Epoch of reference equinox
      CHARACTER * ( NDF__SZFRM ) FORM ! Storage form
      INTEGER FRM                ! AST pointer to Frame
      CHARACTER * ( 80 ) FRMDMN  ! Frame domain
      INTEGER FRMNAX             ! Frame dimensionality
      CHARACTER * ( 80 ) FRMTTL  ! Frame title
      CHARACTER * ( NDF__SZFTP ) FTYPE ! Full data type
      LOGICAL FULLAX             ! Display full axis information?
      DOUBLE PRECISION GFIRST( 1, NDF__MXDIM ) ! GRID coords of first pixel
      CHARACTER * ( NDF__SZHUM ) HMODE ! History update mode
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IAT                ! Current length of a string
      INTEGER IAXIS              ! Loop counter for axes
      INTEGER IBASE              ! Index of Base Frame in WCS FrameSet
      INTEGER ICURR              ! Index of Current Frame in WCS FrameSet
      INTEGER IDIG               ! Loop counter for binary digits
      INTEGER IEXTN              ! Extension index
      INTEGER IFRAME             ! Frame index
      INTEGER INDF               ! NDF identifier
      INTEGER IWCS               ! AST identifier for NDF's WCS FrameSet
      INTEGER LBND( NDF__MXDIM ) ! Lower pixel-index bounds
      LOGICAL MONOTO( NDF__MXDIM ) ! Axis monotonic flags
      INTEGER N                  ! Loop counter for extensions
      INTEGER NC                 ! Character count
      INTEGER NDIM               ! Number of dimensions
      INTEGER NEXTN              ! Number of extensions
      INTEGER NFRAME             ! Total number of WCS Frames
      INTEGER NFRM               ! Number of interesting WCS Frames
      LOGICAL NORM( NDF__MXDIM ) ! Axis normalisation flags
      INTEGER NREC               ! Number of history records
      INTEGER PNTR( 2 )          ! Pointers to axis elements
      CHARACTER * ( 50 ) PRJ     ! Sky projection
      LOGICAL QUIET              ! Do not report the trace?
      LOGICAL REPORT             ! Report the trace?
      INTEGER SIZE               ! Total number of pixels
      LOGICAL SHOWCS             ! Display an AST dump of the WCS component?
      CHARACTER * ( 30 ) SYS     ! Sky coordinate system
      LOGICAL THERE              ! Whether NDF component is defined
      CHARACTER * ( DAT__SZTYP ) TYPE ! Data type
      INTEGER UBND( NDF__MXDIM ) ! Upper pixel-index bounds
      CHARACTER * ( 80 ) WCSDMN( MXFRM )  ! Frame domains
      INTEGER WCSNAX( MXFRM )    ! Frame dimensionalities
      CHARACTER * ( 80 ) WCSTTL( MXFRM )  ! Frame titles
      LOGICAL WIDTH( NDF__MXDIM ) ! Whether NDF axis-width components are defined
      CHARACTER * ( DAT__SZLOC ) XLOC ! Extension locator
      CHARACTER * ( NDF__SZXNM ) XNAME( MXEXTN ) ! Extension name
      CHARACTER * ( NDF__SZTYP ) XTYPE( MXEXTN ) ! Extension name

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an identifier for the NDF structure to be examined.
      CALL NDF_ASSOC( 'NDF', 'READ', INDF, STATUS )

*  See if full axis information is to be obtained.
      CALL PAR_GET0L( 'FULLAXIS', FULLAX, STATUS )

*  See if any information is to be displayed.
      CALL PAR_GET0L( 'QUIET', QUIET, STATUS )
      REPORT = .NOT. QUIET

*  Display the NDF's name.
      IF ( REPORT ) THEN
         CALL MSG_BLANK( STATUS )
         CALL NDF_MSG( 'NDF', INDF )
         CALL MSG_OUT( 'HEADER', '   NDF structure ^NDF:', STATUS )
      END IF


*  Character components:
*  =====================
*  See if the title component is defined.  If so, then display its
*  value.
      CALL NDF_STATE( INDF, 'Title', THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_CGET( INDF, 'Title', CCOMP, STATUS )
         IF ( REPORT ) THEN
            CALL MSG_SETC( 'TITLE', CCOMP )
            CALL MSG_OUT( 'M_TITLE', '      Title:  ^TITLE', STATUS )
         END IF
      ELSE
         CCOMP = ' '
      END IF

*  Write the title to the output parameter.
      CALL PAR_PUT0C( 'TITLE', CCOMP, STATUS )

*  See if the label component is defined.  If so, then display its
*  value.
      CALL NDF_STATE( INDF, 'Label', THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_CGET( INDF, 'Label', CCOMP, STATUS )
         IF ( REPORT ) THEN
            CALL MSG_SETC( 'LABEL', CCOMP )
            CALL MSG_OUT( 'M_LABEL', '      Label:  ^LABEL', STATUS )
         END IF
      ELSE
         CCOMP = ' '
      END IF

*  Write the label to the output parameter.
      CALL PAR_PUT0C( 'LABEL', CCOMP, STATUS )

*  See if the units component is defined.  If so, then display its
*  value.
      CALL NDF_STATE( INDF, 'Units', THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_CGET( INDF, 'Units', CCOMP, STATUS )
         IF ( REPORT ) THEN
            CALL MSG_SETC( 'UNITS', CCOMP )
            CALL MSG_OUT( 'M_UNITS', '      Units:  ^UNITS', STATUS )
         END IF
      ELSE
         CCOMP = ' '
      END IF

*  Write the units to the output parameter.
      CALL PAR_PUT0C( 'UNITS', CCOMP, STATUS )


*  NDF shape:
*  ==========
*  Obtain the dimension sizes.
      CALL NDF_DIM( INDF, NDF__MXDIM, DIM, NDIM, STATUS )

*  Display a header for this information.
      IF ( REPORT ) THEN
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( 'SHAPE_HEADER', '   Shape:', STATUS )

*  Display the number of dimensions.
         CALL MSG_SETI( 'NDIM', NDIM )
         CALL MSG_OUT( 'DIMENSIONALITY',
     :     '      No. of dimensions:  ^NDIM', STATUS )

*  Construct a string showing the dimension sizes.
         IF ( STATUS .EQ. SAI__OK ) THEN
            NC = 0
            DO 1 I = 1, NDIM
               IF ( I .GT. 1 ) CALL CHR_PUTC( ' x ', BUF, NC )
               CALL CHR_PUTI( DIM( I ), BUF, NC )
    1       CONTINUE
            CALL MSG_SETC( 'DIMS', BUF( : NC ) )
         END IF

*  Display the dimension size information.
         CALL MSG_OUT( 'DIMENSIONS',
     :     '      Dimension size(s):  ^DIMS', STATUS )
      END IF

*  Output the dimensionality.
      CALL PAR_PUT0I( 'NDIM', NDIM, STATUS )

*  Obtain the pixel-index bounds.
      CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Construct a string showing the pixel-index bounds.
      IF ( REPORT ) THEN
         IF ( STATUS .EQ. SAI__OK ) THEN
            NC = 0
            DO 2 I = 1, NDIM
               IF ( I .GT. 1 ) CALL CHR_PUTC( ', ', BUF, NC )
               CALL CHR_PUTI( LBND( I ), BUF, NC )
               CALL CHR_PUTC( ':', BUF, NC )
               CALL CHR_PUTI( UBND( I ), BUF, NC )
    2       CONTINUE
            CALL MSG_SETC( 'BNDS', BUF( : NC ) )
         END IF

*  Display the pixel-index bounds information.
         CALL MSG_OUT( 'BOUNDS',
     :     '      Pixel bounds     :  ^BNDS', STATUS )
      END IF

*  Output the dimensions and bounds.
      CALL PAR_PUT1I( 'DIMS', NDIM, DIM, STATUS )
      CALL PAR_PUT1I( 'LBOUND', NDIM, LBND, STATUS )
      CALL PAR_PUT1I( 'UBOUND', NDIM, UBND, STATUS )

*  Obtain the NDF size and display this information.
      IF ( REPORT ) THEN
         CALL NDF_SIZE( INDF, SIZE, STATUS )
         CALL MSG_SETI( 'SIZE', SIZE )
         CALL MSG_OUT( 'SIZE',
     :     '      Total pixels     :  ^SIZE ', STATUS )
      END IF

*  Axis component:
*  ===============
*  See if the axis co-ordinate system is defined. If so, then display a
*  header for it.  Set the AXIS output parameter.
      CALL NDF_STATE( INDF, 'Axis', THERE, STATUS )
      CALL PAR_PUT0L( 'AXIS', THERE, STATUS )

      IF ( THERE ) THEN
         IF ( REPORT ) THEN
            CALL MSG_BLANK( STATUS )
            CALL MSG_OUT( 'AXIS_HEADER', '   Axes:', STATUS )
         END IF

*  Loop to display information for each NDF axis, starting with a
*  heading showing the axis number.
         DO 3 IAXIS = 1, NDIM
            IF ( REPORT ) THEN
               CALL MSG_SETI( 'IAXIS', IAXIS )
               CALL MSG_OUT( 'AXIS_NUMBER',
     :            '      Axis ^IAXIS:', STATUS )
            END IF

*  Obtain the label for the axis and display it.  Note that since the
*  values must be written to output parameters, a call to NDF_ACMSG is
*  inadequate.
            CALL NDF_ASTAT( INDF, 'Label', IAXIS, THERE, STATUS )
            IF ( THERE ) THEN
               CALL NDF_ACGET( INDF, 'Label', IAXIS, ALABEL( IAXIS ),
     :                         STATUS )

               IF ( REPORT ) THEN
                  CALL MSG_SETC( 'LABEL', ALABEL( IAXIS ) )
                  CALL MSG_OUT( 'AXIS_LABEL',
     :              '         Label : ^LABEL', STATUS )
               END IF
            ELSE
               ALABEL( IAXIS ) = ' '
            END IF

*  Obtain the units for the axis and display it.  Note that since the
*  values must be written to output parameters, a call to NDF_ACMSG is
*  inadequate.
            CALL NDF_ASTAT( INDF, 'Units', IAXIS, THERE, STATUS )
            IF ( THERE ) THEN
               CALL NDF_ACGET( INDF, 'Units', IAXIS, AUNITS( IAXIS ),
     :                         STATUS )

               IF ( REPORT ) THEN
                  CALL MSG_SETC( 'UNITS', AUNITS( IAXIS ) )
                  CALL MSG_OUT( 'AXIS_UNITS',
     :              '         Units : ^UNITS', STATUS )
               END IF
            ELSE
               AUNITS( IAXIS ) = ' '
            END IF


*  Axis Extent:
*  ============

*  First check for monotonic axis centre values.  Map the axis centre
*  array, using double precision to prevent loss of precision.
            CALL NDF_AMAP( INDF, 'Centre', IAXIS, '_DOUBLE',
     :                     'READ', AXPNTR, EL, STATUS )

*  Are all the axes monotonic?  Start a new error context so that the
*  error reports concerning a non-monotonic axis may be annulled.
*  Instead we issue a warning message so that the application can
*  continue by using world co-ordinates.
            CALL ERR_MARK
            CALL KPG1_MONOD( .TRUE., EL, %VAL( AXPNTR( 1 ) ),
     :                       MONOTO( IAXIS ), STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
               MONOTO( IAXIS ) = .FALSE.
            END IF
            CALL ERR_RLSE

*  Unmap the axis.
            CALL NDF_AUNMP( INDF, 'Centre', IAXIS, STATUS )

*  Report a non-monotonic axis.
            IF ( .NOT. MONOTO( IAXIS ) ) THEN
               IF ( REPORT ) CALL MSG_OUT( 'AXIS_EXTENT',
     :           '         Extent: Non-monotonic', STATUS )
            ELSE

*  Map the axis centre and width arrays and use them to determine the
*  overall extent of the NDF along the current axis.  Unmap the arrays
*  afterwards.
               CALL NDF_AMAP( INDF, 'Centre,Width', IAXIS, '_DOUBLE',
     :                        'READ', PNTR, EL, STATUS )
               CALL KPG1_AXRNG( EL, %VAL( PNTR( 1 ) ),
     :                          %VAL( PNTR( 2 ) ), ASTART( IAXIS ),
     :                          AEND( IAXIS ), STATUS )
               CALL NDF_AUNMP( INDF, 'Centre,Width', IAXIS, STATUS )

*  Determine the numeric type which should be used to display the NDF's
*  extent and define message tokens appropriately.
               CALL NDF_ATYPE( INDF, 'Centre,Width', IAXIS, ATYPE,
     :                         STATUS )
               IF ( REPORT ) THEN
                  IF ( ATYPE .EQ. '_DOUBLE' ) THEN
                     CALL MSG_SETD( 'ASTART', ASTART( IAXIS ) )
                     CALL MSG_SETD( 'AEND', AEND( IAXIS ) )
                  ELSE
                     CALL MSG_SETR( 'ASTART', SNGL( ASTART( IAXIS ) ) )
                     CALL MSG_SETR( 'AEND', SNGL( AEND( IAXIS ) ) )
                  END IF

*  Display the NDF's extent.
                  CALL MSG_OUT( 'AXIS_EXTENT',
     :              '         Extent: ^ASTART to ^AEND', STATUS )
               END IF
            END IF


*  Axis Centre Array:
*  ==================
*  If full axis information is to be displayed, then obtain the axis
*  centre array attributes.
            IF ( FULLAX ) THEN
               CALL NDF_ATYPE( INDF, 'Centre', IAXIS, CTYPE( IAXIS ),
     :                         STATUS )
               CALL NDF_AFORM( INDF, 'Centre', IAXIS, CFORM( IAXIS ),
     :                         STATUS )

*  Display the axis centre array attributes.
               IF ( REPORT ) THEN
                  CALL MSG_BLANK( STATUS )
                  CALL MSG_OUT( 'AXISC_HEADER',
     :              '            Centre Array:', STATUS )
                  CALL MSG_SETC( 'TYPE', CTYPE( IAXIS ) )
                  CALL MSG_OUT( 'AXISC_TYPE',
     :              '               Type        :  ^TYPE', STATUS )
                  CALL MSG_SETC( 'FORM', CFORM( IAXIS ) )
                  CALL MSG_OUT( 'AXISC_FORM',
     :              '               Storage form:  ^FORM', STATUS )
               END IF


*  Axis Normalisation Flag:
*  ========================
*  Obtain and display the axis normalisation flag.
               CALL NDF_ANORM( INDF, IAXIS, NORM( IAXIS ), STATUS )
               IF ( REPORT ) THEN
                  CALL MSG_SETL( 'NORM', NORM( IAXIS ) )
                  CALL MSG_OUT( 'AXIS_NORM',
     :              '            Normalisation Flag: ^NORM', STATUS )
               END IF


*  Axis Width Array:
*  =================
*  See whether the axis width array is defined. If so, then obtain its
*  attributes.
               CALL NDF_ASTAT( INDF, 'Width', IAXIS, WIDTH( IAXIS ),
     :                         STATUS )
               IF ( THERE ) THEN
                  CALL NDF_ATYPE( INDF, 'Width', IAXIS, TYPE, STATUS )
                  CALL NDF_AFORM( INDF, 'Width', IAXIS, FORM, STATUS )

*  Display the axis width attributes.
                  IF ( REPORT ) THEN
                     CALL MSG_BLANK( STATUS )
                     CALL MSG_OUT( 'AXISW_HEADER',
     :                 '            Width Array:', STATUS )
                     CALL MSG_SETC( 'TYPE', TYPE )
                     CALL MSG_OUT( 'AXISW_TYPE',
     :                 '               Type        :  ^TYPE', STATUS )
                     CALL MSG_SETC( 'FORM', FORM )
                     CALL MSG_OUT( 'AXISW_FORM',
     :                 '               Storage form:  ^FORM', STATUS )
                  END IF
               END IF


*  Axis Variance Array:
*  ====================
*  See whether the axis variance array is defined. If so, then obtain
*  its attributes.
               CALL NDF_ASTAT( INDF, 'Variance', IAXIS, AVAR( IAXIS ),
     :                         STATUS )
               IF ( THERE ) THEN
                  CALL NDF_ATYPE( INDF, 'Variance', IAXIS, TYPE,
     :                            STATUS )
                  CALL NDF_AFORM( INDF, 'Variance', IAXIS, FORM,
     :                            STATUS )

*  Display the axis variance attributes.
                  IF ( REPORT ) THEN
                     CALL MSG_BLANK( STATUS )
                     CALL MSG_OUT( 'AXISV_HEADER',
     :                 '            Variance Array:', STATUS )
                     CALL MSG_SETC( 'TYPE', TYPE )
                     CALL MSG_OUT( 'AXISV_TYPE',
     :                 '               Type        :  ^TYPE', STATUS )
                     CALL MSG_SETC( 'FORM', FORM )
                     CALL MSG_OUT( 'AXISV_FORM',
     :                 '               Storage form:  ^FORM', STATUS )
                  END IF
               END IF
            END IF

*  Add a spacing line after the information for each axis.
            IF ( IAXIS .NE. NDIM .AND. REPORT ) CALL MSG_BLANK( STATUS )
    3    CONTINUE

      END IF

*  Write the output axis parameters.
      CALL PAR_PUT1D( 'AEND', NDIM, AEND, STATUS )
      CALL PAR_PUT1C( 'ALABEL', NDIM, ALABEL, STATUS )
      CALL PAR_PUT1D( 'ASTART', NDIM, ASTART, STATUS )
      CALL PAR_PUT1C( 'AUNITS', NDIM, AUNITS, STATUS )
      IF ( FULLAX ) THEN
         CALL PAR_PUT1C( 'AFORM', NDIM, CFORM, STATUS )
         CALL PAR_PUT1L( 'AMONO', NDIM, MONOTO, STATUS )
         CALL PAR_PUT1L( 'ANORM', NDIM, NORM, STATUS )
         CALL PAR_PUT1C( 'ATYPE', NDIM, CTYPE, STATUS )
         CALL PAR_PUT1L( 'AVARIANCE', NDIM, AVAR, STATUS )
         CALL PAR_PUT1L( 'WIDTH', NDIM, WIDTH, STATUS )
      END IF


*  Data component:
*  ===============
*  Obtain the data component attributes.
      CALL NDF_FTYPE( INDF, 'Data', FTYPE, STATUS )
      CALL NDF_FORM( INDF, 'Data', FORM, STATUS )

*  Output the values to parameters.
      CALL PAR_PUT0C( 'TYPE', FTYPE, STATUS )
      CALL PAR_PUT0C( 'FORM', FORM, STATUS )

*  Display the data component attributes.
      IF ( REPORT ) THEN
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( 'DATA_HEADER', '   Data Component:', STATUS )
         CALL MSG_SETC( 'FTYPE', FTYPE )
         CALL MSG_OUT( 'DATA_TYPE', '      Type        :  ^FTYPE',
     :                 STATUS )
         CALL MSG_SETC( 'FORM', FORM )
         CALL MSG_OUT( 'DATA_FORM', '      Storage form:  ^FORM',
     :                 STATUS )
      END IF

*  Determine if the data values are defined. Issue a warning message if
*  they are not.
      CALL NDF_STATE( INDF, 'Data', THERE, STATUS )
      IF ( .NOT. THERE ) THEN
         IF ( REPORT ) THEN
            CALL MSG_OUT( 'DATA_UNDEF',
     :        '      WARNING: the Data component values are not '/
     :        /'defined', STATUS )
         END IF

*  Disable automatic quality masking and see if the data component may
*  contain bad pixels.  If so, then display an appropriate message.
      ELSE
         CALL NDF_SQMF( .FALSE., INDF, STATUS )
         CALL NDF_BAD( INDF, 'Data', .FALSE., BAD, STATUS )
         IF ( BAD ) THEN
            IF ( REPORT ) THEN
               CALL MSG_OUT( 'DATA_ISBAD',
     :           '      Bad pixels may be present', STATUS )
            END IF

*  If there were no bad pixels present, then re-enable quality masking
*  and test again.  Issue an appropriate message.
         ELSE
            CALL NDF_SQMF( .TRUE., INDF, STATUS )
            CALL NDF_BAD( INDF, 'Data', .FALSE., BAD, STATUS )
            IF ( REPORT ) THEN
               IF ( .NOT. BAD ) THEN
                  CALL MSG_OUT( 'DATA_NOBAD',
     :              '      There are no bad pixels present', STATUS )
               ELSE
                  CALL MSG_OUT( 'DATA_QBAD',
     :            '      Bad pixels may be introduced via the Quality '/
     :            /'component', STATUS )
               END IF
            END IF

         END IF

*  Set the output parameter.
         CALL PAR_PUT0L( 'BAD', BAD, STATUS )
      END IF


*  Variance component:
*  ===================
*  See if the variance component is defined.  If so, then obtain its
*  attributes.  Indicate its presence or otherwise through the output
*  parameter.
      CALL NDF_STATE( INDF, 'Variance', THERE, STATUS )
      CALL PAR_PUT0L( 'VARIANCE', THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_FTYPE( INDF, 'Variance', FTYPE, STATUS )
         CALL NDF_FORM( INDF, 'Variance', FORM, STATUS )

*  Display the variance component attributes.
         IF ( REPORT ) THEN
            CALL MSG_BLANK( STATUS )
            CALL MSG_OUT( 'VAR_HEADER', '   Variance Component:',
     :                    STATUS )
            CALL MSG_SETC( 'FTYPE', FTYPE )
            CALL MSG_OUT( 'VAR_TYPE', '      Type        :  ^FTYPE',
     :                    STATUS )
            CALL MSG_SETC( 'FORM', FORM )
            CALL MSG_OUT( 'VAR_FORM', '      Storage form:  ^FORM',
     :                    STATUS )
         END IF

*  Disable automatic quality masking and see if the variance component
*  may contain bad pixels.  If so, then display an appropriate message.
         CALL NDF_SQMF( .FALSE., INDF, STATUS )
         CALL NDF_BAD( INDF, 'Variance', .FALSE., BAD, STATUS )
         IF ( REPORT ) THEN
            IF ( BAD ) THEN
               CALL MSG_OUT( 'VAR_ISBAD',
     :           '      Bad pixels may be present', STATUS )

*  If there were no bad pixels present, then re-enable quality masking
*  and test again. Issue an appropriate message.
            ELSE
               CALL NDF_SQMF( .TRUE., INDF, STATUS )
               CALL NDF_BAD( INDF, 'Variance', .FALSE., BAD, STATUS )
               IF ( .NOT. BAD ) THEN
                  CALL MSG_OUT( 'VAR_NOBAD',
     :              '      There are no bad pixels present', STATUS )
               ELSE
                  CALL MSG_OUT( 'VAR_QBAD',
     :              '      Bad pixels may be introduced via the '/
     :              /'Quality component', STATUS )
               END IF
            END IF
         END IF
      END IF


*  Quality component:
*  ==================
*  See if the quality component is defined.  If so, then obtain its
*  attributes.  Indicate its presence or otherwise through the output
*  parameter.
      CALL NDF_STATE( INDF, 'Quality', THERE, STATUS )
      CALL PAR_PUT0L( 'QUALITY', THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_FORM( INDF, 'Quality', FORM, STATUS )

*  Display the quality component attributes.
         IF ( REPORT ) THEN
            CALL MSG_BLANK( STATUS )
            CALL MSG_OUT( 'QUALITY_HEADER', '   Quality Component:',
     :                    STATUS )
            CALL MSG_SETC( 'FORM', FORM )
            CALL MSG_OUT( 'QUALITY_FORM', '      Storage form :  ^FORM',
     :                    STATUS )
         END IF

*  Obtain the bad-bits mask value.
         CALL NDF_BB( INDF, BADBIT, STATUS )

*  Generate a binary representation in a character string.
         IF ( STATUS .EQ. SAI__OK ) THEN
            BBI = NUM_UBTOI( BADBIT )
            DIGVAL = 2 ** 7
            DO 4 IDIG = 1, 8
               IF ( BBI .GE. DIGVAL ) THEN
                  BINSTR( IDIG : IDIG ) = '1'
                  BBI = BBI - DIGVAL
               ELSE
                  BINSTR( IDIG : IDIG ) = '0'
               END IF
               DIGVAL = DIGVAL / 2
    4       CONTINUE
         END IF

*  Display the bad-bits mask information.
         IF ( REPORT ) THEN
            CALL MSG_SETI( 'BADBIT', NUM_UBTOI( BADBIT ) )
            CALL MSG_SETC( 'BINARY', BINSTR )
            CALL MSG_OUT( 'QUALITY_BADBIT',
     :        '      Bad-bits mask:  ^BADBIT (binary ^BINARY)', STATUS )
         END IF

*  Output the BADBITS mask to a parameter.
         CALL PAR_PUT0C( 'BADBITS', BINSTR, STATUS )
      END IF

*  WCS component:
*  ==============
*  Initialise the number of interesting Frames found, and see if the WCS 
*  component is defined. Only proceed if it is.
      NFRM = 0
      CALL NDF_STATE( INDF, 'WCS', THERE, STATUS )
      IF ( THERE ) THEN

*  Start an AST context.
         CALL AST_BEGIN( STATUS )

*  Get an AST pointer for the FrameSet defining the NDF's World Coordinate
*  Systems. Store the number of coordinate systems ("Frames") described
*  by the FrameSet. 
         CALL NDF_GTWCS( INDF, IWCS, STATUS )
         NFRAME = AST_GETI( IWCS, 'NFRAME', STATUS )

*  We do not consider the default Frames automatically generated by the
*  NDF library (i.e. the GRID, PIXEL and AXIS Frames). So if there are
*  fewer than 4 Frames, pretend there is no WCS component.
         IF( NFRAME .LT. 4 ) THEN
            THERE = .FALSE.

*  If there are any extra Frames, over and above those created
*  automatically by the NDF library, display a header for their descriptions.
         ELSE
            IF ( REPORT ) THEN
               CALL MSG_BLANK( STATUS )
               CALL MSG_OUT( 'WCS_HEADER', '   World Coordinate '//
     :                       'Systems:', STATUS )
            END IF

*  Save the index of the original current Frame, so that it can be
*  re-instated later.
            ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Store the GRID coordinates of the centre of the first pixel. This is
*  defined to be (1.0,1.0,...). This poisition will be mapped into each of the
*  other Frame, to find the coordinates of the first pixel.
            DO 301 IAXIS = 1, NDIM
               GFIRST( 1, IAXIS ) = 1.0
 301        CONTINUE

*  Loop to display information for each coordinate system (excluding the
*  automatically generated ones).
            DO 304 IFRAME = 1, NFRAME

*  Get an AST pointer to the Frame with index IFRAME. 
               FRM = AST_GETFRAME( IWCS, IFRAME, STATUS )            

*  Get the Frame title, domain and dimensionality.
               FRMTTL = AST_GETC( FRM, 'TITLE', STATUS )
               FRMDMN = AST_GETC( FRM, 'DOMAIN', STATUS )
               FRMNAX = AST_GETI( FRM, 'NAXES', STATUS )

*  Ignore the Frames generated automatically by the NDF library (i.e. the
*  ones with Domain GRID, AXIS and PIXEL).
               IF( FRMDMN .NE. 'GRID' .AND. FRMDMN .NE. 'AXIS' 
     :             .AND. FRMDMN .NE. 'PIXEL' ) THEN

*  Put the title, domain and dimensionality in the arrays to be stored in 
*  the output parameters if there is room. 
                  IF( NFRM .LT. MXFRM ) THEN               
                     NFRM = NFRM + 1
                     WCSTTL( NFRM ) = FRMTTL
                     WCSDMN( NFRM ) = FRMDMN
                     WCSNAX( NFRM ) = FRMNAX
                  END IF

*  The rest we only do if we are reporting information on the screen.
                  IF ( REPORT ) THEN

*  Display the title (upto 50 characters), and domain if necessary. The number 
*  of dimensions is implied by the list of axes displayed later.
                     CALL MSG_SETC( 'TTL', FRMTTL( : 50 ) )
                     IF( CHR_LEN( FRMTTL ) .GT. 50 ) THEN
                        CALL MSG_SETC( 'TTL', '...' )
                     END IF

                     CALL MSG_OUT( 'WCS_TITLE',
     :                   '      Frame title: "^TTL"', STATUS )

                     CALL MSG_SETC( 'DOMAIN', FRMDMN )
                     CALL MSG_OUT( 'WCS_DOMAIN',
     :              '         Domain              : ^DOMAIN', STATUS )

*  If the Frame is a SkyFrame, display the epoch, equinox, system and
*  projection.
                     IF( AST_ISASKYFRAME( FRM, STATUS ) ) THEN
                        EP = AST_GETD( FRM, 'EPOCH', STATUS )
                        EQ = AST_GETD( FRM, 'EQUINOX', STATUS )
                        SYS = AST_GETC( FRM, 'SYSTEM', STATUS )
                        PRJ = AST_GETC( FRM, 'PROJECTION', STATUS )

                        IF( SYS .EQ. 'FK4' .OR. SYS .EQ. 'FK5' ) THEN
                           CALL MSG_SETC( 'SYS', 'Equatorial (' )
                           CALL MSG_SETC( 'SYS', SYS )
                           CALL MSG_SETC( 'SYS', ' -' )
                           IF( EQ .LT. 1984.0 ) THEN
                              CALL MSG_SETC( 'SYS', ' B' )
                           ELSE 
                              CALL MSG_SETC( 'SYS', ' J' )
                           END IF
                           CALL MSG_SETD( 'SYS', EQ )
                           CALL MSG_SETC( 'SYS', ')' )
                 
                        ELSE IF( SYS .EQ. 'FK4-NO-E' ) THEN
                           CALL MSG_SETC( 'SYS', 'Equatorial without '//
     :                                    'E-terms (FK4 -' )
                           IF( EQ .LT. 1984.0 ) THEN
                              CALL MSG_SETC( 'SYS', ' B' )
                           ELSE 
                              CALL MSG_SETC( 'SYS', ' J' )
                           END IF
                           CALL MSG_SETD( 'SYS', EQ )
                           CALL MSG_SETC( 'SYS', ')' )

                        ELSE IF( SYS .EQ. 'GAPPT' ) THEN
                           CALL MSG_SETC( 'SYS', 'Equatorial '//
     :                                    '(geocentric apparent)' )

                        ELSE IF( SYS .EQ. 'ECLIPTIC' ) THEN
                           CALL MSG_SETC( 'SYS', 'Ecliptic (' )
                           IF( EQ .LT. 1984.0 ) THEN
                              CALL MSG_SETC( 'SYS', ' B' )
                           ELSE 
                              CALL MSG_SETC( 'SYS', ' J' )
                           END IF
                           CALL MSG_SETD( 'SYS', EQ )
                           CALL MSG_SETC( 'SYS', ')' )

                        ELSE IF( SYS .EQ. 'GALACTIC' ) THEN
                           CALL MSG_SETC( 'SYS', 'Galactic' )

                        ELSE IF( SYS .EQ. 'SUPERGALACTIC' ) THEN
                           CALL MSG_SETC( 'SYS', 'Supergalactic' )
                        ELSE
                           CALL MSG_SETC( 'SYS', SYS )
                        END IF                        

                        CALL MSG_OUT( 'WCS_SYS',
     :               '         System              : ^SYS', STATUS )

                        IF( EP .LT. 1984.0 ) THEN
                           CALL MSG_SETC( 'EPOCH', 'B' )
                        ELSE 
                           CALL MSG_SETC( 'EPOCH', 'J' )
                        END IF
                        CALL MSG_SETD( 'EPOCH', EP )
                        CALL MSG_OUT( 'WCS_EPOCH',
     :              '         Epoch of observation: ^EPOCH', STATUS )

                        IF( PRJ .NE. ' ' ) THEN
                           CALL MSG_SETC( 'PROJ', PRJ )
                           CALL MSG_OUT( 'WCS_PROJ',
     :              '         Projection          : ^PROJ', STATUS )
                        END IF

                     END IF

*  Map the GRID coordinates at the centre of the first pixel to obtain the 
*  corresponding coordinates in the Frame with index IFRAME. First make this 
*  Frame the FrameSet's Current Frame. This enables us to the use the FrameSet 
*  itself as the mapping (since the NDF library ensures that the base Frame is 
*  the GRID Frame).
                     CALL AST_SETI( IWCS, 'CURRENT', IFRAME, STATUS )
                     CALL AST_TRANN( IWCS, 1, NDIM, 1, GFIRST,
     :                               .TRUE., FRMNAX, 1, CFIRST,
     :                               STATUS )

*  Display the resulting coordinates.
                     CALL MSG_SETC( 'FIRST', AST_FORMAT( FRM, 1, 
     :                              CFIRST( 1, 1 ), STATUS ) )

                     DO 302 IAXIS = 2, NDIM
                        CALL MSG_SETC( 'FIRST', ',' )
                        CALL MSG_SETC( 'FIRST', ' ' )
                        CALL MSG_SETC( 'FIRST', AST_FORMAT( FRM, 
     :                             IAXIS, CFIRST( 1, IAXIS ), STATUS ) )
 302                 CONTINUE

                     CALL MSG_OUT( 'WCS_FIRSTP',
     :              '         First pixel centre  : ^FIRST', STATUS )

*  Now display the axis number, label and units for each axis of the Frame.
*  Note, these are not written to output parameters.
                     CALL MSG_BLANK( STATUS )
                     DO 303 IAXIS = 1, FRMNAX

*  Display the axis number.
                        CALL MSG_SETI( 'IAXIS', IAXIS )
                        CALL MSG_OUT( 'AXIS_NUMBER',
     :                    '            Axis ^IAXIS:', STATUS )

*  Construct the name of the attribute holding the label for this axis.
                        ATTRIB = 'LABEL('
                        IAT = 6
                        CALL CHR_PUTI( IAXIS, ATTRIB, IAT )
                        CALL CHR_APPND( ')', ATTRIB, IAT )

*  Get the label and display it.
                        CALL MSG_SETC( 'LABEL', 
     :                    AST_GETC( FRM, ATTRIB( : IAT ), STATUS ) )
                        CALL MSG_OUT( 'AXIS_LABEL',
     :                    '               Label: ^LABEL', STATUS )

*  Construct the name of the attribute holding the units for this axis.
                        ATTRIB = 'UNIT('
                        IAT = 5
                        CALL CHR_PUTI( IAXIS, ATTRIB, IAT )
                        CALL CHR_APPND( ')', ATTRIB, IAT )

*  Get the units string and display it.
                        CALL MSG_SETC( 'UNIT', 
     :                    AST_GETC( FRM, ATTRIB( : IAT ), STATUS ) )
                        CALL MSG_OUT( 'AXIS_UNITS',
     :                    '               Units: ^UNIT', STATUS )

*  Add a spacing line after the information for each axis.
                        IF ( IAXIS .NE. FRMNAX ) THEN
                           CALL MSG_BLANK( STATUS )
                        END IF

 303                 CONTINUE

*  Add a spacing line after the information for each Frame.
                     IF ( IFRAME .NE. NFRAME ) CALL MSG_BLANK( STATUS )

                  END IF

               END IF

*  Annul the pointer to the Frame.
               CALL AST_ANNUL( FRM, STATUS )

 304        CONTINUE 

*  Re-instate the original current Frame.
            CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )

*  Write the output Frame parameters.
            CALL PAR_PUT1C( 'FTITLE', NFRM, WCSTTL, STATUS )
            CALL PAR_PUT1C( 'FDOMAIN', NFRM, WCSDMN, STATUS )
            CALL PAR_PUT1I( 'FDIM', NFRM, WCSNAX, STATUS )

         END IF

*  End the AST context.
         CALL AST_END( STATUS )

      END IF

*  Write out the WCS and NFRAME parameter values.
      CALL PAR_PUT0L( 'WCS', THERE, STATUS )
      CALL PAR_PUT0I( 'NFRAME', NFRM, STATUS )

*  Extensions:
*  ===========
*  Determine how many extensions are present.
      CALL NDF_XNUMB( INDF, NEXTN, STATUS )

*  Output the number to a parameter.
      CALL PAR_PUT0I( 'NEXTN', NEXTN, STATUS )

*  Display a heading for the extensions.
      IF ( NEXTN .GT. 0 ) THEN
         IF ( REPORT ) THEN
            CALL MSG_BLANK( STATUS )
            CALL MSG_OUT( 'EXTN_HEADER', '   Extensions:', STATUS )
         END IF

*  Loop to obtain the name and data type of each extension.
*  Protect against array-bounds errors.  It is unlikely that there will
*  ever be more the MXEXTN extensions.
         DO 5 N = 1, NEXTN
            IEXTN = MIN( N, MXEXTN )
            CALL NDF_XNAME( INDF, N, XNAME( IEXTN ), STATUS )
            CALL NDF_XLOC( INDF, XNAME( IEXTN ), 'READ', XLOC, STATUS )
            CALL DAT_TYPE( XLOC, XTYPE( IEXTN ), STATUS )
            CALL DAT_ANNUL( XLOC, STATUS )
            XLOC = ' '

*  Display the information for each extension.
            IF ( REPORT ) THEN
               CALL MSG_SETC( 'TYPE', XTYPE( IEXTN ) )
               CALL MSG_OUT( 'EXTN',
     :           '      ' // XNAME( IEXTN ) // '  <^TYPE>', STATUS )
            END IF
    5    CONTINUE

*   Output the names and types of the extensions.
         CALL PAR_PUT1C( 'EXTNAME', NEXTN, XNAME, STATUS )
         CALL PAR_PUT1C( 'EXTTYPE', NEXTN, XTYPE, STATUS )

      END IF
      IF ( REPORT ) CALL MSG_BLANK( STATUS )

*  History:
*  ========
*  See if a history component is present, and send the result to an
*  output parameter.
      CALL NDF_STATE( INDF, 'History', THERE, STATUS )
      CALL PAR_PUT0L( 'HISTORY', THERE, STATUS )

*  If so, then obtain its attributes.
      IF ( THERE .AND. REPORT ) THEN
         CALL NDF_HINFO( INDF, 'CREATED', 0, CREAT, STATUS )
         CALL NDF_HNREC( INDF, NREC, STATUS )
         CALL NDF_HINFO( INDF, 'MODE', 0, HMODE, STATUS )
         CALL NDF_HINFO( INDF, 'DATE', NREC, DATE, STATUS )
         CALL NDF_HINFO( INDF, 'APPLICATION', NREC, APPN, STATUS )

*  Convert the date format to KAPPA style.
         CALL KPG1_FHDAT( CREAT, STATUS )
         CALL KPG1_FHDAT( DATE, STATUS )

*  Display the history component attributes.
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( 'HISTORY_HEADER', '   History Component:',
     :                 STATUS )
         CALL MSG_SETC( 'CREAT', CREAT( : 20 ) )
         CALL MSG_OUT( 'HISTORY_CREAT',
     :                 '      Created    :  ^CREAT', STATUS )
         CALL MSG_SETI( 'NREC', NREC )
         CALL MSG_OUT( 'HISTORY_NREC',
     :                 '      No. records:  ^NREC', STATUS )
         CALL MSG_SETC( 'DATE', DATE( : 20 ) )
         CALL MSG_SETC( 'APPN', APPN )
         CALL MSG_OUT( 'HISTORY_DATE',
     :                 '      Last update:  ^DATE (^APPN)', STATUS )
         CALL MSG_SETC( 'HMODE', HMODE )
         CALL MSG_OUT( 'HISTORY_HMODE',
     :                 '      Update mode:  ^HMODE', STATUS )
      END IF
      IF ( REPORT ) CALL MSG_BLANK( STATUS )

*  Clean up:
*  ========
*  Annul the NDF identifier.
      CALL NDF_ANNUL( INDF, STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDFTRACE_ERR',
     :     'NDFTRACE: Error displaying the attributes of an NDF ' //
     :     'data structure.', STATUS )
      END IF

      END
