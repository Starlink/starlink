      SUBROUTINE ARDMASK( STATUS )
*+
*  Name:
*     ARDMASK

*  Purpose:
*     Uses an ARD file to set some pixels of an NDF to be bad.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARDMASK( STATUS )

*  Arguments:   
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This task allows regions of a NDF's data array to be masked, so
*     that they can be excluded from subsequent data processing.  ARD
*     (ASCII Region Definition) descriptions stored in a text
*     file define which pixels of the data array are masked.  An
*     output NDF is created which is the same as the input file except
*     that all pixels specified by the ARD file have been assigned the
*     bad value.

*  Usage:
*     ardmask in ardfile out

*  ADAM Parameters:
*     ARDFILE = FILENAME (Read)
*        The name of the ARD file containing a description of the parts
*        of the image to be masked out, i.e. set to bad.  The suggested
*        default is the current value or ardfile.dat if there is no
*        current value.
*     COSYS = LITERAL (Read)
*        The co-ordinate system to be used.  This can be either "World"
*        or "Data".  If COSYS = "World" the co-ordinates used in the
*        ARD file are pixel co-ordinates or indices.  If COSYS = "Data"
*        the co-ordinates used in the ARD file are interpreted as data
*        co-ordinates, provided the NDF contains axes that map onto
*        pixel co-ordinates using linear transformations.  If there are
*        no axes, pixel co-ordinates are assumed; if axes are present
*        but non-linear, the task fails.  COSYS="World" is recommended.
*        [Current co-ordinate system]
*     IN = NDF (Read)
*        The name of the source NDF.
*     OUT = NDF (Write)
*        The name of the masked NDF.
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF. [!]

*  Examples:
*     ardmask a1060 galaxies.ard a1060_sky title="A1060 galaxies masked"
*        This flags pixels defined by the ARD file galaxies.ard within
*        the NDF called a1060 to create a new NDF called a1060_sky.
*        a1060_sky has a title="A1060 galaxies masked".  This might be
*        to flag the pixels where bright galaxies are located to
*        exclude them from sky-background fitting.
*     ardmask in=ic3374 ardfil=ardfile.txt out=ic3374a
*        This example uses as the source image the NDF called ic3374
*        and sets the pixels specified by the ARD description contained
*        in ardfile.txt to the bad value.  The resultant image is
*        output to the NDF called ic3374a.  The title is unchanged.
*     ardmask in=ic3374 ardfil=ardfile.txt out=ic3374a cosys=data
*        As the previous example except that the ARD file is
*        written using data co-ordinates.

*  ASCII-region-definition Descriptors:
*     The ARD file may be created by ARDGEN or written manually.  In the
*     latter case consult SUN/183 for full details of the ARD
*     descriptors and syntax; however, much may be learnt from looking
*     at the ARD files created by ARDGEN and the ARDGEN documentation.
*     There is also a summary with examples in the main body of SUN/95
*     and the online help.

*  Implementation Status:
*     -  This routine correctly processes the WCS, AXIS, DATA, QUALITY,
*     LABEL, TITLE, UNITS, HISTORY, and VARIANCE components of an NDF
*     data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  Bad pixels and automatic quality masking are supported.
*     -  All non-complex numeric data types can be handled.

*  Related Applications:
*     KAPPA: ARDGEN.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     08-Jun-1994 (GJP):
*        Original version.
*     1995 June 29 (MJC):
*        Converted for KAPPA use: completed and re-ordered the
*        prologue, lowercase usage and examples, various stylistic
*        changes, standard comment indentation, removed unnecessary
*        status checks and code, allowed all numeric data types, added
*        handling of data co-ordinates and COSYS parameter, propagates
*        other components (axes, units, variance and quality), made to
*        work on n-dimensional NDFs, validated the input text file and
*        removed the need for the leading caret, and made the output
*        TITLE a parameter.
*     1996 July 31 (MJC):
*        Made ARDFILE have type FILENAME for IRAF usage.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constant
      INCLUDE 'GRP_PAR'          ! GRP_ data constants
      INCLUDE 'PRM_PAR'          ! VAL_ data constants
               
*  Status:     
      INTEGER STATUS             ! Global status

*  Local Variables:      
      INTEGER AXES( NDF__MXDIM ) ! Indices of axes
      CHARACTER * ( NDF__SZTYP ) ATYPE ! Numeric type for mapping AXIS
                                 ! centres
      LOGICAL CONT               ! ARD description to continue?
      CHARACTER * ( 5 ) COSYS    ! Co-ordinate system
      LOGICAL DATAVL             ! Are data co-ordinates available?
      DOUBLE PRECISION DOFSET( NDF__MXDIM ) ! Axis co-ords at pixel
                                 ! co-ords (0,0)
      DOUBLE PRECISION DSCALE( NDF__MXDIM ) ! Dimensions of a pixel in
                                 ! axis units
      INTEGER EL                 ! Total number of pixels in the image
      INTEGER FD                 ! File descriptor
      CHARACTER * ( 132 ) FILNAM ! Name of ARD file
      INTEGER I                  ! Loop counter
      INTEGER IGRP               ! Group identifier
      INTEGER J                  ! Loop counter
      INTEGER LBND( NDF__MXDIM ) ! Lower limit for image index  
      INTEGER LBNDE( NDF__MXDIM ) ! Lower bounds of a box encompassing
                                 ! all external array elements
      INTEGER LBNDI( NDF__MXDIM ) ! Lower bounds of a box encompassing
                                 ! all internal array elements
      INTEGER NDFI               ! Identifier for the source NDF  
      INTEGER NDFO               ! Identifier for the output NDF
      INTEGER NDIM               ! Number of dimensions in the image
      INTEGER NDP1               ! Numberof dimensions plus one.
      REAL OFFSET( NDF__MXDIM )  ! Axis co-ords at pixel co-ords (0,0)
      INTEGER POINT1( 1 )        ! Pointer to the data component of 
                                 ! for the output NDF
      INTEGER POINT2( 1 )        ! Pointer to the ARD logical mask
      INTEGER REGVAL             ! Value assignied to the first ARD
                                 ! region
      REAL SCALE( NDF__MXDIM )   ! Dimensions of a pixel in axis units
      REAL TRCOEF( ( NDF__MXDIM + 1 ) * NDF__MXDIM ) ! Data to world
                                 ! co-ordinate conversions
      CHARACTER * ( NDF__SZTYP ) TYPE ! Numeric type for processing
      INTEGER UBND( NDF__MXDIM ) ! Upper limit for image index
      INTEGER UBNDE( NDF__MXDIM ) ! Upper bounds of a box encompassing
                                 ! all external array elements
      INTEGER UBNDI( NDF__MXDIM ) ! Upper bounds of a box encompassing
                                 ! all internal array elements
                                                          
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the NDF structure to be examined.       
      CALL NDF_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Obtain the numeric type of the NDF array component to be analysed.
      CALL NDF_TYPE( NDFI, 'Data', TYPE, STATUS )

*  Get the image bounds and also the size of the axes in pixels.
      CALL NDF_BOUND( NDFI, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Use a literal parameter to obtain the value to avoid having to give
*  the indirection and continuation.  Call FIO to open the file to
*  ensure that the obtained file exists.  Get the name and add the
*  indirection symbol so that ARD does not treat the filename as a
*  literal ARD description.
      CALL FIO_ASSOC( 'ARDFILE', 'READ', 'LIST', 0, FD, STATUS )
      CALL AIF_FLNAM( 'ARDFILE', FILNAM( 2: ), STATUS )
      FILNAM( 1:1 ) = '^'
      CALL FIO_ANNUL( FD, STATUS )

*  Identify the ARD file name.  Use a literal parameter to obtain the
      IGRP = GRP__NOID
      CALL ARD_GRPEX( FILNAM, GRP__NOID, IGRP, CONT, STATUS )

*  Obtain the desired co-ordinate system.
      CALL PAR_CHOIC( 'COSYS', 'World', 'Data,World', .FALSE., COSYS,
     :                STATUS )

*  If data co-ordinates are to be supplied, we need to find out how to
*  transform data co-ordinates into world co-ordinates within the output
*  NDF.
      IF ( COSYS .EQ. 'DATA' ) THEN

*  Find the precision needed for the system of axis arrays.
         CALL NDF_ATYPE( NDFI, 'Centre', 0, ATYPE, STATUS )

*  See if there is an axis co-ordinate system defined within the NDF.
         CALL NDF_STATE( NDFI, 'Axis', DATAVL, STATUS )
         IF ( DATAVL ) THEN

*  Specify the axes to be used.
            DO I = 1, NDIM
               AXES( I ) = I
            END DO

*  Obtain the scales and offsets of the linear transformation from
*  pixel co-ordinates to data co-ordinates.  Axis co-ordinates must be
*  monotonic to be usable.  If the axes are non-linear a warning
*  message is issued and a linear approximation to the axis
*  co-ordinates is returned.  Note that if we were to map a
*  single-precision axis array as double precision, the linearity might
*  be violated merely because of the increased sensitivity of the test.
*  Thus the testing is done with the appropriate type.  Hereafter though
*  the single-precision transformation is adequate for the job.
            IF ( ATYPE .EQ. '_DOUBLE' ) THEN
               CALL KPG1_CHAXD( NDFI, NDIM, AXES, DATAVL, DSCALE,
     :                          DOFSET, STATUS )
               DO I = 1, NDIM
                  SCALE( I ) = SNGL( DSCALE( I ) )
                  OFFSET( I ) = SNGL( DOFSET( I ) )
               END DO
            ELSE
               CALL KPG1_CHAXR( NDFI, NDIM, AXES, DATAVL, SCALE, OFFSET,
     :                          STATUS )
            END IF

*  If no usable AXIS structures have been found, report an error and
*  abort.
            IF ( ( .NOT. DATAVL ) .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'ARDMASK_WARN1', 'No usable data '/
     :           /'co-ordinates can be found in the supplied NDF.  '/
     :           /'Assuming that the ARD file is specified using '/
     :           /'pixel co-ordinates.', STATUS )

            ELSE

*  Generate the transformation array.
*
*  First initialise the array.
               NDP1 = NDIM + 1
               DO J = 1, NDIM
                  DO I = 1, NDP1
                     TRCOEF( I + ( J - 1 ) * NDP1 ) = 0.0
                  END DO
               END DO

*  Assign the offsets and scale factors.  Note that the scale and
*  offsets returned above are for converting pixel co-ordinates to
*  data, and ARD needs the inverse mapping.  A zero scale is not
*  possible but double check for safety.
               DO J = 1, NDIM
                  IF ( ABS( SCALE( J ) ) .LT. VAL__EPSR ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP( 'ARDMASK_SCALE',
     :                 'The scale factor for converting world to data '/
     :                 /'co-ordinates along axis ^I is zero.', STATUS )
                  ELSE
                     TRCOEF( 1 + ( J - 1 ) * NDP1 ) = -OFFSET( J ) /
     :                                                SCALE( J )
                     TRCOEF( 1 + J + ( J - 1 ) * NDP1 ) = 1.0 /
     :                                                    SCALE( J )
                  END IF
               END DO
            END IF

*  Use the identity matrix when there are no axes or the co-ordinate
*  system is world.
         ELSE
            TRCOEF( 1 ) = VAL__BADR
         END IF

      ELSE
         TRCOEF( 1 ) = VAL__BADR
      END IF

*  Propagate the bits of the source NDF required.
      CALL NDF_PROP( NDFI, 'Data,Variance,Quality,Axis,Units,WCS', 
     :               'OUT', NDFO, STATUS )

*  Get the title for the output NDF.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )

*  Map the output NDF data array as _REAL values for updating.
      CALL NDF_MAP( NDFO, 'Data', TYPE, 'UPDATE', POINT1( 1 ), EL,
     :              STATUS )

*  Allocate the memory needed for the logical mask array.
      CALL PSX_CALLOC( EL, '_INTEGER', POINT2( 1 ), STATUS )
      
*  Create the mask.  Value 2 should be used to represent pixels
*  specified by the first keyword in the ARD description.
      REGVAL = 2
      CALL ARD_WORK( IGRP, NDIM, LBND, UBND, TRCOEF, .FALSE., REGVAL,
     :               %VAL( POINT2( 1 ) ), LBNDI, UBNDI, LBNDE, UBNDE,
     :               STATUS )
       
*  Correct the output image to have bad pixels where indicated on the
*  mask.  Call the appropriate routine for the data type.
      IF ( TYPE .EQ. '_REAL' ) THEN
         CALL KPS1_ARDMR( EL, %VAL( POINT2( 1 ) ),
     :                    %VAL( POINT1( 1 ) ), STATUS )

      ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL KPS1_ARDMB( EL, %VAL( POINT2( 1 ) ),
     :                    %VAL( POINT1( 1 ) ), STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPS1_ARDMD( EL, %VAL( POINT2( 1 ) ),
     :                    %VAL( POINT1( 1 ) ), STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL KPS1_ARDMI( EL, %VAL( POINT2( 1 ) ),
     :                    %VAL( POINT1( 1 ) ), STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL KPS1_ARDMUB( EL, %VAL( POINT2( 1 ) ),
     :                     %VAL( POINT1( 1 ) ), STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL KPS1_ARDMUW( EL, %VAL( POINT2( 1 ) ),
     :                     %VAL( POINT1( 1 ) ), STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL KPS1_ARDMW( EL, %VAL( POINT2( 1 ) ),
     :                    %VAL( POINT1( 1 ) ), STATUS )

      END IF

*   Free the dynamic array space of the logical mask.
      CALL PSX_FREE( POINT2( 1 ), STATUS )

*  Close down the group used to hold the pixel mask.
      CALL GRP_DELET( IGRP, STATUS )

*   End the NDF context.
      CALL NDF_END( STATUS )                              

      END
