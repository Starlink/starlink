#if HAVE_CONFIG_H
# include "config.h"
#endif
      SUBROUTINE IRAF2NDF( STATUS )
*+
*  Name:
*     IRAF2NDF

*  Purpose:
*     Converts an IRAF image to an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IRAF2NDF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts an IRAF image to an NDF.  See the Notes
*     for details of the conversion.

*  Usage:
*     iraf2ndf in out

*  ADAM Parameters:
*     IN = LITERAL (Read)
*        The name of the IRAF image.  Note that this excludes the
*        ".imh" file extension.
*     OUT = NDF (Write)
*        The name of the NDF to be produced.
*     PROFITS = _LOGICAL (Read)
*        If TRUE, the user headers of the IRAF file are written
*        verbatim to the NDF's FITS extension.  Any IRAF history
*        records are also appended to the FITS extension.  The FITS
*        extension is not created if there are no user headers
*        present in the IRAF file. [TRUE]
*     PROHIS = _LOGICAL (Read)
*        This parameter decides whether or not to create NDF HISTORY
*        records.  Only the IRAF headers with keyword HISTORY, and
*        which originated from NDF HISTORY records are used.  If
*        PROHIS=TRUE, NDF HISTORY records are created.  [TRUE]

*  Examples:
*     iraf2ndf ell_galaxy new_galaxy
*        Converts the IRAF image ell_galaxy (comprising files
*        ell_galaxy.imh and ell_galaxy.pix) to an NDF called new_galaxy.
*     iraf2ndf ell_galaxy new_galaxy noprofits noprohis
*        As above, except no FITS extension is created, and NDF-style
*        HISTORY lines in ell_galaxy.imh are not transferred to HISTORY
*        records in NDF new_galaxy.

*  Notes:
*     The rules for the conversion are as follows:
*     -  The NDF is created with bounds determined by any LBOUNDn 
*        keywords in the IRAF image header.
*     -  The NDF data array is copied from the ".pix" file.
*     -  The title of the IRAF image (object i_title in the ".imh"
*     header file) becomes the NDF title.  Likewise headers OBJECT and
*     BUNIT become the NDF label and units respectively.
*     -  The pixel origin is set if any LBOUNDn headers are present.
*     -  Lines from the IRAF image header file may be transferred to
*     the FITS extension of the NDF, when PROFITS=TRUE.  Any 
*     compulsory FITS keywords that are missing are added.  Certain
*     other keywords are not propagated.  These are the IRAF "Mini
*     World Coordinate System" (MWCS) keywords WCSDIM, DC_FLAG,
*     WATd_nnn (d is dimension, nnn is the line number).  Certain
*     NDF-style HISTORY lines in the header are also be ignored when
*     PROHIS=TRUE (see two notes below).
*     -  When PROFITS=TRUE, lines from the HISTORY section of the IRAF
*     image are also extracted and added to the NDF's FITS extension as
*     FITS HISTORY lines.  Two extra HISTORY lines are added to record
*     the original name of the image and the date of the format
*     conversion.
*     -  When PROHIS=TRUE, any HISTORY lines in the IRAF headers, which
*     originated from an NDF2IRAF conversion of NDF HISTORY records.
*     Such headers are not transferred to the FITS airlock, when
*     PROFITS=TRUE.
*     -  Most axis information can be propagated either from standard
*     FITS-like keywords, or certain MCWS headers.  Supported systems
*     and formats are listed below.
*        o  FITS
*           - linear
*           - log-linear
*        o  Equispec
*           - linear
*           - log-linear
*        o  Multispec
*           - linear
*           - log-linear
*           - Chebyshev and Legendre polynomials
*           - Linear and cubic Spline
*           - Explicit list of co-ordinates
*
*     However, for Multispec axes, only the first (spec1) axis
*     co-ordinates are transferred to the NDF AXIS centres.  Any
*     spec2...specn co-ordinates, present when the data array is not
*     one-dimensional or multiple fits have been stored, are ignored.
*     The weights for multiple fits are thus also ignored.  The data
*     type of the axis centres is _REAL or _DOUBLE depending on the
*     number of significant digits in the co-ordinates or coefficients.
*
*     The axis labels and units are also propagated, where present, to
*     the NDF AXIS structure.  In the FITS system, these are derived
*     from the CTYPEn and CUNITn keywords.  In the MWCS, these
*     components originate in the label and units parameters.

*     The redshift correction, when present, is applied to the MCWS
*     axis co-ordinates.

*  Related Applications:
*     CONVERT: NDF2IRAF.

*  Pitfalls:
*     -  Bad pixels in the IRAF image are not replaced.
*     -  Some of the routines required for accessing the IRAF header
*     file are written in SPP.  Macros are used to find the start of the
*     header line section, this constitutes an `Interface violation' as
*     these macros are not part of the IMFORT interface specification.
*     It is possible that these may be changed in the future, so
*     beware.

*  Implementation Status:
*     -  Only handles one-, two-, and three-dimensional IRAF files.
*     -  The NDF produced has type _WORD or _REAL corresponding to the
*     type of the IRAF image.  (The IRAF imfort FORTRAN subroutine
*     library only supports these data types: signed words and real.)
*     The pixel type of the image can be changed from within IRAF using
*     the 'chpixtype' task in the 'images' package.
*     -  See "Release Notes" for IRAF version compatibility.
*
*  Implementation Deficiencies:
*     -  Does not support wildcards.
*     -  There is no facility for taking an IRAF bad-pixel-mask file,
*     to set bad pixels in the NDF.

*  References:
*     IRAF User Handbook Volume 1A: "A User's Guide to FORTRAN
*     Programming in IRAF, the IMFORT Interface", by Doug Tody.

*  Keywords:
*     CONVERT, IRAF

*  Authors:
*     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
*     GJP: Grant Privett (STARLINK, University of Wales, Cardiff)
*     MJC: Malcolm J. Currie (STARLINK)
*     AJC: Alan Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1992 (RAHM):
*        Original version.
*     23-NOV-1992 (RAHM):
*        Produces NDFs of type UWORD from IRAF short integer
*        type images.
*     9-JUL-1993 (RAHM):
*        Added VERBOSE parameter and tidied up.
*     1993 July 23 (MJC):
*        Reworked the prologue to a standard arrangement.  Tidied, made
*        to conform to SGP/16.  Made to work for cubes.  Fixed some
*        bugs.
*     1993 July 28 (MJC):
*        Removed the VERBOSE parameter.  This functionality should be
*        provided by a global parameter.
*     1993 September 30 (MJC):
*        Do not copy standard FITS headers already added to the FITS
*        extension.
*     08-AUG-1995 (GJP):
*        Modified the method used to obtain the date/time string to
*        avoid the use of pointer.  Gave up PSX_ASCTIME and used
*        PSX_CTIME instead.
*     09-AUG 1995 (GJP):
*        Removed erroneous mention of LIN2MAP, PREFITS and PUTLIN.
*     09-AUG-1995 (GJP):
*        Added the ability to generate a sensible error message when
*        IMOPEN fails - as would happen if the file requested did 
*        not exist.
*     11-AUG-1995 (GJP):
*        Added a check to see if any header lines were found before 
*        calling GETLIN to obtain them.
*     1997 April and July (MJC):
*        Added PROFITS and PROHIS facilities.  Now propagates axis
*        information, and the NDF label and units.  Expanded and
*        corrected the documentation, including a second example.
*        Improved the code structure.  Removed the conversion of the
*        IRAF filename to lowercase.
*     1997 September 25 (MJC):
*        Protect against case where PROFITS=TRUE but there are no
*        headers.
*     13-NOV-1997 (AJC):
*        Set bounds of NDF according to LBOUNDn keywords if any.
*        Correct number of arguments to CON_CI2DW/CI2DR.
*     1997 November 17 (MJC):
*        No longer attempts to close the IRAF file if the open-file
*        subroutine fails.  IMCLOS crashes otherwise.  Propagates all
*        IRAF history records (including blanks) to the FITS airlock
*        when PROFITS=TRUE.  IRAF HISTORY lines which are too long for
*        a FITS header are truncated with an ellipsis.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'MSG_PAR'          ! MSG constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_CVAL function
      
*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Finds the length of a string less
                                 ! trailing blanks

*  Local Constants:
      INTEGER NDIM               ! Maximum number of dimensions the 
      PARAMETER ( NDIM = 3 )     ! application can handle

      INTEGER STRLEN             ! Length of output title string
      PARAMETER ( STRLEN = 80 )
        
*  Local Variables:
      INTEGER ACCESS             ! IRAF access mode
      INTEGER BUFLEN             ! Text-buffer length
      INTEGER BUPNTR             ! Pointer to buffer workspace
      INTEGER DIMS( NDF__MXDIM ) ! Length of axes
      INTEGER DTYPE              ! IRAF data-type code
      INTEGER EL                 ! Number of pixels in image
      INTEGER ERR                ! IRAF error indicator
      INTEGER FIPNTR( 1 )        ! Pointer to FITS extension
      INTEGER IMDESC             ! Image descriptor returned by
                                 ! IMOPEN() for IMFORT routines
      CHARACTER * ( STRLEN ) IMERRM ! IMFORT error message text
      CHARACTER * ( STRLEN ) IRAFIL ! Input IRAF image name
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Type of the NDF
      INTEGER K                  ! Loop counter
      CHARACTER * ( 70 ) LABEL   ! Label of the NDF 
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF axes
      INTEGER LIPNTR             ! Pointer to a line of pixels
      INTEGER MDIM               ! Number of axes
      INTEGER NBANDS             ! Number of bands
      INTEGER NCHARS             ! Number of characters in the character
                                 ! component
      INTEGER NCOLS              ! Number of columns
      INTEGER NDF                ! NDF identifier of output NDF
      INTEGER NHDRLI             ! Number of IRAF header lines
      INTEGER NROWS              ! Number of rows
      INTEGER NTICKS             ! Returned by PSX_TIME
      LOGICAL OPEN               ! IRAF file is open
      INTEGER PNTR( 1 )          ! Pointer to NDF data array
      LOGICAL PROFIT             ! Propagate headers to FITS
      LOGICAL PROHIS             ! Propagate NDF-style headers to
                                 ! HISTORY structure in the NDF
      INTEGER REPNTR             ! Pointer to header-propagation flags
      INTEGER SPNTR              ! Pointer to spec1 buffer workspace
      CHARACTER * ( 70 ) TITLE   ! Title of the NDF 
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF axes
      CHARACTER * ( 70 ) UNITS   ! Units of the NDF 

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

#if !HAVE_IRAF_LIBIMFORT_A

*  We do not have IRAF libraries so give an error message
	STATUS = SAI__ERROR
	CALL ERR_REP('IRAF2NDF', 'IRAF2NDF: Convert not built with '//
     :		'IRAF libraries. You will need to rebuild CONVERT',
     :		STATUS)

#else

*  Initialise the IRAF status.
      ERR = 0
      OPEN = .FALSE.

*  Access the input IRAF image.
*  ============================

*  Get the name of the IRAF image.
      CALL PAR_GET0C( 'IN', IRAFIL, STATUS )
            
*  Access mode is 1 for read only and 3 for read and write access.
      ACCESS = 1

*  Open the IRAF image.
      CALL IMOPEN( IRAFIL, ACCESS, IMDESC, ERR )
      IF ( ERR .NE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRAF2NDF_IRAFERR',
     :     'IRAF2NDF: Error found while loading the IRAF file. ',
     :     STATUS )
         GOTO 999
      END IF

*  Record that the IRAF file was opened successfully.
      OPEN = .TRUE.

*  Obtain the shape of the IRAF image.
*  ===================================

*  Obtain the dimensions and pixeltype of the IRAF image.
      CALL IMGSIZ( IMDESC, DIMS, MDIM, DTYPE, ERR )
      IF ( ERR .NE. 0 ) GOTO 999
      
*  Check the data type of the input image.  It must be real (6) or
*  signed word (3).
      IF ( DTYPE .NE. 6 .AND. DTYPE .NE. 3 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IF', IRAFIL )
         CALL ERR_REP( 'IRAF2NDF_DATATYPE',
     :     'IRAF2NDF: The data type of image ^IF.imh is not supported '/
     :     /'by the IRAF IMFORT subroutine library.  Use the IRAF '/
     :     /'task images.chpixtype to change the pixel type.', STATUS )
         GOTO 999
      END IF

*  Validate the number of dimensions.
      IF ( MDIM .GT. NDIM ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IF', IRAFIL )
         CALL MSG_SETI( 'MDIM', MDIM )
         CALL MSG_SETI( 'NDIM', NDIM )
         CALL ERR_REP( 'IRAF2NDF_DIMS',
     :     'IRAF2NDF: Cannot process image ^IF since it has ^MDIM '/
     :     /'dimensions.  The maximum permitted dimension is ^NDIM.',
     :     STATUS )
         GOTO 999
      END IF

*  Define the bounds according to any LBOUNDn keywords in the header.
*  ==================================================================
*
*  Use the IRAF convention to define the array's shape.
      NCOLS = DIMS( 1 )
      NROWS = DIMS( 2 )
      NBANDS = DIMS( 3 )

*  Look for any LBOUNDn keywords.  Just use the default origin when
*  the keyword is absent, as indicated by a non-zero IRAF status.
      CALL IMGKWI( IMDESC, 'LBOUND1', LBND( 1 ), ERR ) 
      IF ( ERR .NE. 0 ) LBND( 1 ) = 1

      CALL IMGKWI( IMDESC, 'LBOUND2', LBND( 2 ), ERR ) 
      IF ( ERR .NE. 0 ) LBND( 2 ) = 1

      CALL IMGKWI( IMDESC, 'LBOUND3', LBND( 3 ), ERR ) 
      IF ( ERR .NE. 0 ) LBND( 3 ) = 1

*  Set the upper bounds.
      UBND( 1 ) = LBND( 1 ) + NCOLS - 1
      UBND( 2 ) = LBND( 2 ) + NROWS - 1
      UBND( 3 ) = LBND( 3 ) + NBANDS - 1

*  Set the type of the NDF.
*  ========================

*  Only signed words or real is supported by IMFORT.  Match the NDF type
*  to the IRAF type.
      IF ( DTYPE .EQ. 3 ) THEN
         ITYPE = '_WORD'
      ELSE
         ITYPE = '_REAL'
      END IF

*  Create the output NDF.
*  ======================
      
*  Start an NDF context.
      CALL NDF_BEGIN
      
*  Create an NDF with the required dimensions and type.
      CALL NDF_CREAT( 'OUT', ITYPE, MDIM, LBND, UBND, NDF, STATUS )

*  Map the data component to a memory array
      CALL NDF_MAP( NDF, 'Data', ITYPE, 'WRITE', PNTR, EL, STATUS )

*  Use PSX_CALLOC() to obtain dynamic storage for each line.
*  As a temporary measure until PSX_CALLOC supports _WORD type we have
*  to call PSX_MALLOC instead.
      IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL PSX_MALLOC( NCOLS * VAL__NBW, LIPNTR, STATUS )
      ELSE
         CALL PSX_CALLOC( NCOLS, ITYPE, LIPNTR, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 980
      
*  Pass the Image descriptor, image dimensions, line buffer,
*  dimensionality and the mapped array to a subroutine. Each line of
*  the IRAF image will be extracted and propagated to the NDF array.
      IF ( DTYPE .EQ. 3 ) THEN
         CALL CON_CI2DW( MDIM, NCOLS, NROWS, NBANDS, IMDESC, ERR,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), 
     :                   %VAL( CNF_PVAL( LIPNTR ) ), STATUS )
      ELSE
         CALL CON_CI2DR( MDIM, NCOLS, NROWS, NBANDS, IMDESC, ERR,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), 
     :                   %VAL( CNF_PVAL( LIPNTR ) ), STATUS )
      END IF

*  Obtain other parameters.
*  ========================

*  Determine whether or not the FITS extension is to be created.
      CALL PAR_GET0L( 'PROFITS', PROFIT, STATUS )

*  Determine whether or not other HISTORY records are to be regenerated.
      CALL PAR_GET0L( 'PROHIS', PROHIS, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 980
            
*  Get and validate header and history records.
*  ============================================

*  Call RAHM's SPP routine nlines.x (translated later to nlines.f or
*  nlines.for by the IRAF SPP compiler xc).  This routine tries to
*  discover the number of header lines in the image.  These can be
*  transferred directly to the FITS extension of the NDF.
      CALL NLINES( IMDESC, NHDRLI, ERR )
      CALL MSG_SETI( 'NH', NHDRLI )
      CALL MSG_OUTIF( MSG__VERB, ' ', 'There are ^NH header lines '/
     :  /'to propagate.', STATUS )

*  Instruct that the FITS airlock is to use all the headers regardless
*  whether or not they are NDF-style HISTORY records.  This uses a
*  logical work array to flag whether or not to propagate the header to
*  the airlock.
      IF ( ( PROFIT .OR. PROHIS ) .AND. NHDRLI .GT. 0 ) THEN
         CALL PSX_CALLOC( NHDRLI, '_LOGICAL', REPNTR, STATUS )
         CALL CON_CONSL( .TRUE., NHDRLI, %VAL( CNF_PVAL( REPNTR ) ), 
     :                   STATUS )
      END IF

*  Deal with the NDF-style history records.  Search for such records,
*  transfer their information back into NDF HISTORY records, and flag
*  that these headers should not to be propagated to the FITS airlock.
*  This is to avoid growing duplication of potentially bulky text
*  especially if a user is mixing Starlink and IRAF tasks.
      IF ( PROHIS .AND. NHDRLI .GT. 0 )
     :   CALL COI_CHISR( IMDESC, NDF, NHDRLI, 
     :                   %VAL( CNF_PVAL( REPNTR ) ), STATUS )

*  Propagate the FITS headers, and include mandatory headers too.
      IF ( PROFIT .AND. NHDRLI .GT. 0 ) CALL COI_HEADS( IMDESC, IRAFIL,
     :  NDF, NHDRLI, %VAL( CNF_PVAL( REPNTR ) ), PROHIS, STATUS )

*  Free the work space.
      CALL PSX_FREE( REPNTR, STATUS )

*  Write the IRAF image's title to the NDF title.
*  ==============================================      

*  Get the title of the IRAF image using imfort routine imgkwc.
      CALL IMGKWC( IMDESC, 'i_title', TITLE, ERR )

*  Ignore a bad status.  Just do not write a title.
      IF ( ERR .NE. 0 ) THEN
         ERR = 0

*  Write the title to the NDF.
      ELSE
         IF ( TITLE .NE. ' ' ) THEN
            NCHARS = CHR_LEN( TITLE )
            CALL NDF_CPUT( TITLE( :NCHARS ), NDF, 'TITLE', STATUS )
         END IF
      END IF

*  Write the NDF UNITS component.
*  ==============================

*  Get the title of the IRAF image using imfort routine imgkwc.
      CALL IMGKWC( IMDESC, 'BUNIT', UNITS, ERR )

*  Ignore a bad status.  Just do not write a title.
      IF ( ERR .NE. 0 ) THEN
         ERR = 0

*  Write the title to the NDF.
      ELSE
         IF ( UNITS .NE. ' ' ) THEN
            NCHARS = CHR_LEN( UNITS )
            CALL NDF_CPUT( UNITS( :NCHARS ), NDF, 'UNITS', STATUS )
         END IF
      END IF

*  Write the NDF LABEL component.
*  ==============================

*  Get the title of the IRAF image using imfort routine imgkwc.
      CALL IMGKWC( IMDESC, 'OBJECT', LABEL, ERR )

*  Ignore a bad status.  Just do not write a title.
      IF ( ERR .NE. 0 ) THEN
         ERR = 0

*  Write the title to the NDF.
      ELSE
         IF ( LABEL .NE. ' ' ) THEN
            NCHARS = CHR_LEN( LABEL )
            CALL NDF_CPUT( LABEL( :NCHARS ), NDF, 'LABEL', STATUS )
         END IF
      END IF

*  Transfer axis information.
*  ==========================
*
*  Obtain workspace for multi-line MWCS header values, and spec1
*  parameter.
      BUFLEN = 68 * MAX( NHDRLI, 1 )
      CALL PSX_MALLOC( BUFLEN, BUPNTR, STATUS )
      CALL PSX_MALLOC( BUFLEN, SPNTR, STATUS )

*  Create the axis structure.
      CALL COI_AXIMP( IMDESC, NDF, %VAL( CNF_PVAL( BUPNTR ) ), 
     :                %VAL( CNF_PVAL( SPNTR ) ),
     :                STATUS, %VAL( CNF_CVAL( BUFLEN ) ), 
     :                %VAL( CNF_CVAL( BUFLEN ) ) )

*  Free the buffers.
      CALL PSX_FREE( BUPNTR, STATUS )
      CALL PSX_FREE( SPNTR, STATUS )

*  Closedown sequence.
*  ===================

*  Unmap the NDF.
      CALL NDF_UNMAP( NDF, 'Data', STATUS )

 980  CONTINUE

*  Tidy the NDF context.
      CALL NDF_END( STATUS )

 999  CONTINUE

*  Close the IRAF image
      IF ( OPEN ) CALL IMCLOS( IMDESC, ERR )

*  Check for error from IMFORT, ie err is not equal to 0.
      IF ( ERR .NE. 0 ) THEN

*  Convert the IMFORT error status into the appropriate error text.
         CALL IMEMSG( ERR, IMERRM )
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IERR', IMERRM )
         CALL ERR_REP( 'IRAF2NDF_IRAFERR',
     :      'IRAF2NDF: There has been an error reading the IRAF file. '/
     :      /'The error text is: "^IERR".  Check the pixel type as '/
     :      /'IRAF IMFORT subroutines can only deal with REAL or '/
     :      /'SHORT images', STATUS )
      END IF

*   Issue the standard error message if something has failed.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRAF2NDF_ERR',
     :     'IRAF2NDF: Unable to convert the IRAF image into an NDF.',
     :     STATUS )
      END IF

#endif

      END
