      SUBROUTINE NDF2TIFF( STATUS )
*+
*  Name:
*     NDF2TIFF

*  Purpose:
*     Converts an NDF into an 8-bit TIFF-6.0-format file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDF2TIFF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description: 
*     This application converts an NDF to a Image File Format (TIFF).
*     One- or two-dimensional arrays can be handled.

*     The routine first finds the brightest and darkest pixel values in
*     the image.  It then uses these to determine suitable scaling
*     factors to convert the image into an 8-bit representation.  These
*     are then output to a simple greyscale TIFF-6.0 file.

*  Usage:
*     ndf2tiff in out

*  ADAM Parameters:
*     IN = NDF (Read)
*        The name of the input NDF data structure (without the .sdf
*        extension).  The suggested default is the current NDF if one
*        exists, otherwise it is the current value.
*     OUT = _CHAR (Read)
*        The name of the TIFF file to be generated.  The .tif name
*        extension is added to any output filename that does not
*        contain it.

*  Examples:
*     ndf2tiff old new
*         This converts the NDF called old (in file old.sdf) to the
*         TIFF file called new.tif.
*     ndf2dst in=spectre out=spectre.tif
*         This converts the NDF called spectre (in file spectre.sdf) 
*         to the TIFF file called spectre.tif.

*  Notes:
*     This application generates only 256 grey levels and does not use
*     any image colour lookup table so absolute data values may be lost.
*
*     No compression is applied.

*  Related Applications:
*     CONVERT: TIFF2NDF.

*  Implementation Status:
*     Bad values in the data array are replaced with zero in the output
*     TIFF file.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-Nov-1995 (GJP):
*        Original version.
*     1996 February 19 (MJC):
*        Tidied to standard style.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constant
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
               
*  Status:     
      INTEGER STATUS             ! Global status
 
*  External references:
      INTEGER CHR_LEN            ! Length of string sans trailing blanks

*  Local Constants:
      INTEGER MDIM               ! Maximum array dimensionality
      PARAMETER ( MDIM = 2 )

*  Local Variables:  
      INTEGER DIMS( MDIM )       ! Length of the x and y axes
      INTEGER ELEMS              ! Total number of pixels in the image
      INTEGER FLEN               ! The length of the output file
      INTEGER FIOD               ! FIO identifier
      INTEGER LBND( NDF__MXDIM ) ! Lower limit for image index  
      INTEGER NC                 ! String length
      INTEGER NDF                ! Identifier for the source NDF  
      INTEGER NDIM               ! Number of dimensions in the 
                                 ! image
      CHARACTER * ( 255 ) OUT    ! Name of the output file     
      INTEGER PD( 1 )            ! Pointer to the data component of 
                                 ! for the input NDF
      INTEGER PDO( 1 )           ! Pointer to the output image
      CHARACTER * ( 255 ) TEMP   ! Temporary file name
      INTEGER UBND( NDF__MXDIM ) ! Upper limit for image index  
                                                          
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN   

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the NDF structure to be examined.
      CALL NDF_ASSOC( 'IN', 'READ', NDF, STATUS )

*  Map the input NDF data array as double precision values for updating.
      CALL NDF_MAP( NDF, 'DATA', '_DOUBLE', 'READ', PD( 1 ), ELEMS,
     :              STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Get the image bounds and also the size of the axes in pixels.
      CALL NDF_BOUND( NDF, MDIM, LBND, UBND, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP( 'NDF2TIFF_DIMS', 
     :     'NDF ^NDF has more than the maximum number (2) of '/
     :     /'dimensions.', STATUS )
         GOTO 999
      END IF
     
*  Determine size of axes.      
      DIMS( 1 ) = UBND( 1 ) - LBND( 1 ) + 1
      DIMS( 2 ) = UBND( 2 ) - LBND( 2 ) + 1

*  Set the output file length.
      FLEN = ELEMS + 122

*  Make a 1-dimensional image a special case of 2-dimensional.
      IF ( NDIM .EQ. 1 ) THEN
         DIMS( 2 ) = 1
      END IF
      
*  Get the image name.
      CALL PAR_GET0C( 'OUT', OUT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Append the .tif name if not there.
      TEMP = OUT
      CALL CHR_UCASE( TEMP )
      NC = CHR_LEN( TEMP )
      IF ( TEMP( NC - 3:NC ) .NE. '.TIF' ) OUT = OUT( 1:NC )//'.tif'

*  Create space for the output image.
      CALL PSX_CALLOC( FLEN, '_CHAR', PDO( 1 ), STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Open the file. 
      CALL RIO_OPEN( OUT, 'WRITE', 'UNFORMATTED', FLEN, FIOD, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*   Write the image.
      CALL CON_WRTIF( ELEMS, %VAL( PD( 1 ) ), DIMS, FLEN, FIOD,
     :                %VAL( PDO( 1 ) ), STATUS )
 
*  Close the output file.
      CALL RIO_CLOSE( FIOD, STATUS )

*   Free the output image space.
      CALL PSX_FREE( PDO( 1 ), STATUS )

  999 CONTINUE

*   End the NDF context.
      CALL NDF_END( STATUS )                              

      END
