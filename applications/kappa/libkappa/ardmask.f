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
*        of the image to be masked out, i.e. set to bad. The suggested 
*        default is the current value or ardfile.dat if there is no current 
*        value. The co-ordinate system in which positions within this file 
*        are given should be indicated by including suitable COFRAME or WCS 
*        statements within the file (see SUN/183). For instance, starting the 
*        file with a line containing the text "COFRAME(PIXEL)" will indicate 
*        that positions are specified in pixel coordinates. The statement 
*        "COFRAME(SKY,System=FK5)" would indicate that positions are
*        specified in RA/DEC (FK5,J2000). If no such statements are included, 
*        then it is assumed that positions are given within the current 
*        co-ordinate system of the input NDF. 
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
*     1997 December 12 (MJC):
*        Sets the bad-pixel flag to indicate the possible presence of
*        bad pixels in the output dataset.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     20-AUG-2001 (DSB):
*        Converted to ARD V2 by removing COSYS parameter.
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
      CHARACTER ATYPE*( NDF__SZTYP ) ! Numeric type for mapping AXIS centres
      CHARACTER FILNAM*132 ! Name of ARD file
      CHARACTER TYPE*( NDF__SZTYP )  ! Numeric type for processing
      INTEGER AXES( NDF__MXDIM ) ! Indices of axes
      INTEGER EL                 ! Total number of pixels in the image
      INTEGER FD                 ! File descriptor
      INTEGER I                  ! Loop counter
      INTEGER IGRP               ! Group identifier
      INTEGER J                  ! Loop counter
      INTEGER LBND( NDF__MXDIM ) ! Lower limit for image index  
      INTEGER LBNDE( NDF__MXDIM )! Lower bounds of a box encompassing all external array elements
      INTEGER LBNDI( NDF__MXDIM )! Lower bounds of a box encompassing all internal array elements
      INTEGER INDF1              ! Identifier for the source NDF  
      INTEGER INDF2              ! Identifier for the output NDF
      INTEGER IWCS               ! NDF WCS FrameSet
      INTEGER NDIM               ! Number of dimensions in the image
      INTEGER NDP1               ! Numberof dimensions plus one.
      INTEGER IPIN               ! Pointer to the data component of for the output NDF
      INTEGER IPMASK             ! Pointer to the ARD logical mask
      INTEGER REGVAL             ! Value assignied to the first ARD region
      INTEGER UBND( NDF__MXDIM ) ! Upper limit for image index
      INTEGER UBNDE( NDF__MXDIM )! Upper bounds of a box encompassing all external array elements
      INTEGER UBNDI( NDF__MXDIM )! Upper bounds of a box encompassing all internal array elements
      LOGICAL CONT               ! ARD description to continue?
      LOGICAL DATAVL             ! Are data co-ordinates available?
      REAL OFFSET( NDF__MXDIM )  ! Axis co-ords at pixel co-ords (0,0)
      REAL SCALE( NDF__MXDIM )   ! Dimensions of a pixel in axis units
      REAL TRCOEF( ( NDF__MXDIM + 1 ) * NDF__MXDIM ) ! Data to world co-ordinate conversions
                                                          
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the NDF structure to be examined.       
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Obtain the numeric type of the NDF array component to be analysed.
      CALL NDF_TYPE( INDF1, 'Data', TYPE, STATUS )

*  Get the image bounds and also the size of the axes in pixels.
      CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

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

*  Propagate the bits of the source NDF required.
      CALL LPG_PROP( INDF1, 'Data,Variance,Quality,Axis,Units,WCS', 
     :               'OUT', INDF2, STATUS )

*  Get the title for the output NDF.
      CALL NDF_CINP( 'TITLE', INDF2, 'Title', STATUS )

*  Map the output NDF data array for updating.
      CALL NDF_MAP( INDF2, 'Data', TYPE, 'UPDATE', IPIN, EL,
     :              STATUS )

*  Allocate the memory needed for the logical mask array.
      CALL PSX_CALLOC( EL, '_INTEGER', IPMASK, STATUS )
      
*  Get the WCS FrameSet from the NDF and use it to establish the WCS
*  information used by the following cal to ARD_WORK.
      CALL KPG1_GTWCS( INDF2, IWCS, STATUS )
      CALL ARD_WCS( IWCS, STATUS )

*  Create the mask.  Value 2 should be used to represent pixels
*  specified by the first keyword in the ARD description. TRCOEF is
*  ignored because we have previously called ARD_WCS.
      REGVAL = 2
      CALL ARD_WORK( IGRP, NDIM, LBND, UBND, TRCOEF, .FALSE., REGVAL,
     :               %VAL( IPMASK ), LBNDI, UBNDI, LBNDE, UBNDE,
     :               STATUS )
       
*  Correct the output image to have bad pixels where indicated on the
*  mask.  Call the appropriate routine for the data type.
      IF( TYPE .EQ. '_REAL' ) THEN
         CALL KPS1_ARDMR( EL, %VAL( IPMASK ), %VAL( IPIN ), STATUS )

      ELSE IF( TYPE .EQ. '_BYTE' ) THEN
         CALL KPS1_ARDMB( EL, %VAL( IPMASK ), %VAL( IPIN ), STATUS )

      ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPS1_ARDMD( EL, %VAL( IPMASK ), %VAL( IPIN ), STATUS )

      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
         CALL KPS1_ARDMI( EL, %VAL( IPMASK ), %VAL( IPIN ), STATUS )

      ELSE IF( TYPE .EQ. '_UBYTE' ) THEN
         CALL KPS1_ARDMUB( EL, %VAL( IPMASK ), %VAL( IPIN ), STATUS )

      ELSE IF( TYPE .EQ. '_UWORD' ) THEN
         CALL KPS1_ARDMUW( EL, %VAL( IPMASK ), %VAL( IPIN ), STATUS )

      ELSE IF( TYPE .EQ. '_WORD' ) THEN
         CALL KPS1_ARDMW( EL, %VAL( IPMASK ), %VAL( IPIN ), STATUS )

      END IF

*  Set the bad-pixel flag.
      CALL NDF_SBAD( .TRUE., INDF2, 'Data', STATUS )

*  Free the dynamic array space of the logical mask.
      CALL PSX_FREE( IPMASK, STATUS )

*  Close down the group used to hold the pixel mask.
      CALL GRP_DELET( IGRP, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )                              

*  End the AST context.
      CALL AST_END( STATUS )

*  Add a context report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARDMASK_ERR', 'ARDMASK: Failed to mask an NDF'//
     :                 ' using an ARD file.', STATUS )
      END IF

      END
