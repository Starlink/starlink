      SUBROUTINE NDF2IRAF( STATUS )
*+
*  Name:
*     NDF2IRAF

*  Purpose:
*     Converts an NDF to an IRAF (OIF) image.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDF2IRAF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts an NDF to an IRAF image in the old
*     format (OIF).  See the Notes for details of the conversion.
*  
*  Usage:
*     NDF2IRAF IN OUT

*  ADAM Parameters:
*     IN = NDF (Read)
*        Input NDF data structure. The suggested default is the current
*        NDF if one exists, otherwise it is the current value.
*     OUT = _CHAR (Write)
*        The name of the output IRAF image.  Two files are produced
*        with the same name but different extensions. The ".PIX" file
*        contains the data array, and ".IMH" is the associated header
*        file that may contain a copy of the NDF's FITS extension.
*        The suggested default is the current value.

*  Examples:
*     NDF2IRAF ABELL119 A119
*        Converts an NDF called ABELL119.SDF into the IRAF image
*        comprising the pixel file A119.PIX and the header file
*        A119.IMH.

*  Notes:
*     The rules for the conversion are as follows:
*     -  The NDF data array is copied to the ".PIX" file.
*     -  The NDF title is written to the header object i_title in
*     the ".IMH" header file. There is a limit of twenty characters.
*     -  If there is a FITS extension in the NDF, then this is added to
*     the `user area' of the IRAF header file.
*     -  A HISTORY record is added to the IRAF header file indicating
*     that it originated in the named NDF and was converted by
*     NDF2IRAF.
*     -  All other NDF components are ignored.
*  
*  Pitfalls:
*     The IMFORT routines refuse to overwrite an IRAF image if an image
*     with the same name exists.  The application then aborts.

*  Prior Requirements:
*     This routine can only be linked on a machine which has the IRAF
*     system installed, or access to the object libraries required.

*  Implementation Status:
*     -  Only handles two-dimensional NDFs, though a one-dimensional NDF
*     is treated as an image with a second dimension equal to one.
*     -  Of the NDF's array components only the data array may be
*     copied.
*     -  The IRAF image produced has type REAL independent of the
*     type of the NDF's data array.  The pixel type of the image can be
*     changed from within IRAF using the 'chpixtype' task in the
*     'images' package.

*  External Routines Used:
*     IRAF IMFORT subroutine library.
*
*  VAX-specific features used:
*     Link command for this routine is
*     
*     $ ALINK NDF2IRAF,LINE_COPY,FITSPARSE,-
*     IRAFLIB:LIBIMFORT/LIB,-
*     IRAFLIB:LIBSYS/LIB,-
*     IRAFLIB:LIBVOPS/LIB,-
*     IRAFHLIB:LIBOS/LIB

*  References:
*     IRAF IMFORT subroutine library manual.

*  Keywords:
*     CONVERT, IRAF

*  Authors:
*     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-AUG-1992 (RAHM):
*        Original version.
*     1992 September 29 (MJC):
*        Standardised the prologue.  Corrected the error reporting and
*        closedown.  Made the data copying more efficient.  Corrected
*        some minor bugs.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'NDF_ERR'          ! NDF error constants
      INCLUDE 'DAT_PAR'          ! Standard HDS constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_EQUAL          ! CHR routine to test string equality

*  Local Constants:
      INTEGER NDIM               ! Number of dimensions the program can
      PARAMETER ( NDIM = 2 )     ! handle
        
      INTEGER STRLEN             ! Length of output title string.
      PARAMETER ( STRLEN = 80 )
      
*  Local Variables:
      INTEGER ACCESS             ! Access mode for opening IRAF image
                                 ! using imopen()
      INTEGER AXLEN( 7 )         ! IMFORT requires this array to store
                                 ! up to 7 axis lengths, although it
                                 ! can only cope with 3.
      INTEGER DIMS( NDF__MXDIM ) ! Image dimensions
      INTEGER EL                 ! Number of pixels in image
      INTEGER ERR                ! IMFORT error code
      INTEGER FIPNTR( 1 )        ! Pointer to mapped FITS extension.
      CHARACTER * ( DAT__SZLOC ) FITLOC ! Locator to the FITS extension
      INTEGER IMDESC             ! The image descriptor returned by
                                 ! IMOPEN()
      CHARACTER * ( STRLEN ) IMERRM ! IMFORT error message text
      INTEGER IMTYPE             ! Integer describing the IRAF image
                                 ! data type (only REAL and short
                                 ! INTEGER types are supported)
      CHARACTER * ( STRLEN ) IRAFIL ! Output IRAF image name
      INTEGER J                  ! Loop counter over the number of
                                 ! lines in an image.
      INTEGER JUNKIT             ! Integer to absorb unwanted output
                                 ! form PSX_LOCALTIME()
      INTEGER LBND( 2 )          ! Lower bounds array
      CHARACTER * ( 80 ) LINE    ! FITS format header line
      INTEGER LIPNTR( 1 )        ! Pointer returned by PSX_CALLOC to
                                 ! contain each line of pixels
      INTEGER MDIM               ! Number of dimensions of the NDF
      INTEGER NAMELN             ! Variable to absorb an unwanted
                                 ! integer returned by MSG_LOAD.
      INTEGER NCARDS             ! The number of header lines in the
                                 ! FITS extension.
      INTEGER NDF                ! Identifier for input NDF
      INTEGER NTICKS             ! Integer returned by PSX_TIME()
      INTEGER NXTENS             ! The number of extensions in the NDF
      INTEGER PNTR( 1 )          ! Pointer for the data array
      LOGICAL THERE              ! True if there is a FITS extension
      CHARACTER * ( 132 ) TITLE  ! Title of the NDF 
      CHARACTER * ( 25 ) TSTRING ! The current time and data as
                                 ! returned by PSX_ASCTIME()
      INTEGER TSTRUCT            ! The time structure from
                                 ! PSX_LOCALTIME()
      INTEGER UBND( 2 )          ! Upper bounds array

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the IRAF status.
      ERR = 0

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Associate NDF identifier with IN, via the parameter system.      
      CALL NDF_ASSOC( 'IN', 'READ', NDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the bounds of the NDF.
      CALL NDF_BOUND( NDF, NDIM, LBND, UBND, MDIM, STATUS )

*  Check dimensions of the input image.  Strictly we should look for
*  two significant dimensions.  Just check for too many dimensions.
*  Have to determine how many dimensions there are, since NDF_BOUND
*  has a bug and will not return the actual number of dimensions, but 1
*  instead.  To make this work the status must be temporarily set to
*  good, then reinstated.

      IF ( STATUS .EQ. NDF__XSDIM ) THEN
         CALL NDF_MSG( 'NDF', NDF )
         STATUS = SAI__OK
         CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, MDIM, STATUS )
         CALL MSG_SETI( 'MDIM', MDIM )
         STATUS = NDF__XSDIM
         CALL ERR_REP( 'NDF2IRAF',
     :     'NDF2IRAF: Can only deal with two-dimensional images. '/
     :     /'^NDF has ^MDIM dimensions.', STATUS )
         GOTO 999
      END IF

*  Force the NDF to be two-dimensional even if it only actually has
*  one dimension, by using a dummy dimension.
      MDIM = NDIM
      
*  Derive the number of columns in the input NDF.
      DIMS( 1 ) = UBND( 1 ) - LBND( 1 ) + 1
      
*  Derive the number of lines in the input NDF.
      DIMS( 2 ) = UBND( 2 ) - LBND( 2 ) + 1
      
*  Map the data array
      CALL NDF_MAP( NDF, 'Data', '_REAL', 'READ', PNTR, EL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Get a character string, via the parameter system, which will be the
*  name of the IRAF image.
      CALL PAR_GET0C( 'OUT', IRAFIL, STATUS )

*  Covert the file name to lower case.
      CALL CHR_LCASE( IRAFIL )
      
*  The output image will have REAL pixels. If users want to change the
*  pixel type, there are tasks within IRAF to do that.
      IMTYPE = 6
      
*  Create the IRAF image.
      CALL IMCREA( IRAFIL, DIMS, MDIM, IMTYPE, ERR )
      IF ( ERR .NE. 0 ) GOTO 999

*  Set up some constants required by the IMFORT imopen() routine.

*  Access mode is 1 for read only and 3 for read and write access.
      ACCESS = 3

*  Open the IRAF image.
      CALL IMOPEN( IRAFIL, ACCESS, IMDESC, ERR )
      IF ( ERR .NE. 0 ) GOTO 999

*  Call PSX_CALLOC() for a one dimensional array of REALS to hold each
*  line of the image.
      CALL PSX_CALLOC( DIMS( 1 ), '_REAL', LIPNTR, STATUS )

*  Copy the data array to the IRAF image, using the line buffer to
*  reduce the required virtual memory.
      CALL CON_CD2IR( DIMS( 1 ), DIMS( 2 ), %VAL( PNTR( 1 ) ),
     :                %VAL( LIPNTR( 1 ) ), IMDESC, ERR, STATUS )

*  Free the CALLOCked array.
      CALL PSX_FREE( LIPNTR, STATUS )

*  Unmap the NDF.
      CALL NDF_UNMAP( NDF, 'Data', STATUS )

*  Now that the tidying has been done, exit if there was an error
*  copying the array tothe IRAF file.
      IF ( ERR .NE. 0 ) GOTO 999

*  Set the title of the IRAF image to the same as that of the NDF.
*  Use a longer title to prevent an error as there is an NDF_ bug.
*  Longer text should be truncated with an ellpsis inserted, but
*  currently that does not happen.
      CALL NDF_CGET( NDF, 'Title', TITLE, STATUS )

*  Need to use the IMPKWC() routine to set already existing keyword and
*  standard header keywords.
      CALL IMPKWC( IMDESC, 'i_title', TITLE( :20 ), ERR )
      IF ( ERR .NE. 0 .OR. STATUS .NE. SAI__OK ) GOTO 999
      
*=====================================================================
*              Dealing with FITS extension if it exists.
*=====================================================================

*  See whether or not there is a FITS extension.
      CALL NDF_XSTAT( NDF, 'FITS', THERE, STATUS )
      IF ( THERE ) THEN

*  Get a locator for the FITS extension. 
         CALL NDF_XLOC( NDF, 'FITS', 'READ', FITLOC, STATUS )

*  Map the FITS extension
         CALL DAT_MAPV( FITLOC, '_CHAR*80', 'READ', FIPNTR, NCARDS,
     :                  STATUS )

*  Pass to the routine ADDFITS() which will add the lines to the IRAF
*  image.  Add length for UNIX.
         CALL CON_AFHIR( NCARDS, %VAL( FIPNTR( 1 ) ), IMDESC, STATUS,
     :                   %VAL(80) )

*  Unmap the FITS extension and tidy the locator.
         CALL DAT_ANNUL( FITLOC, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 999
      END IF

*  Update the history information of the IRAF image by adding a
*  FITS-format HISTORY line to say where the image came from.

*  Initialise the line.
      LINE = ' '

*  Copy information to a string.
      CALL CHR_MOVE( 'HISTORY Image converted using STARLINK'/
     :               /' utility NDF2IRAF from the NDF:', LINE ) 

*  Add the line to the image.
      CALL ADLINE( IMDESC, LINE )
      
*  Initialise the line.
      LINE = ' '

*  Find the name of the NDF and the current time and date.
      CALL NDF_MSG( 'NDFNAME', NDF )
      CALL PSX_TIME( NTICKS, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL PSX_LOCALTIME( NTICKS, JUNKIT, JUNKIT, JUNKIT, JUNKIT,
     :                       JUNKIT, JUNKIT, JUNKIT, JUNKIT, JUNKIT,
     :                       TSTRUCT, STATUS )
         CALL PSX_ASCTIME( TSTRUCT, TSTRING, STATUS )      
         CALL MSG_SETC( 'TIME', TSTRING )
         CALL MSG_LOAD( ' ', 'HISTORY ^NDFNAME on ^TIME', LINE,
     :                  NAMELN, STATUS )
      ELSE
         CALL MSG_LOAD( ' ', 'HISTORY ^NDFNAME.', LINE, NAMELN, STATUS )
      END IF

*  Add the card image.
      CALL ADLINE( IMDESC, LINE )
      
*  Copy the 'END' keyword to the line and add it.
      LINE = ' '
      CALL CHR_MOVE( 'END', LINE )
      CALL ADLINE( IMDESC, LINE )

*  Close the IRAF image.
      CALL IMCLOS( IMDESC, ERR )
      
  999 CONTINUE

*  Check for an error from IMFORT.
      IF ( ERR .NE. 0 ) THEN

*  Convert the IMFORT error status into the appropriate error text.
         CALL IMEMSG( ERR, IMERRM )

*  Make the error report.
         CALL MSG_SETC( 'IRAFER', IMERRM )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'NDF2IRAF_IRAFERR',
     :     'NDF2IRAF: There has been an error creating the IRAF file. '/
     :     /'The error text is: ^IRAFER', STATUS )
      END IF

*  End the NDF context.
      CALL NDF_END( STATUS )

      END
