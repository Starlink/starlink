      SUBROUTINE COF_EXDIM( FUNIT, OBJECT, MAXDIM, NAME, NDIM, DIMS,
     :                      INDICE, STATUS )
*+
*  Name:
*     COF_EXDIM

*  Purpose:
*     Determines the name, element indices, and shape of an extension
*     structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_EXDIM( FUNIT, OBJECT, NAME, NDIM, DIMS, INDICE, STATUS )

*  Description:
*     The routine takes a string in the format produced by HDS_TRACE,
*     and extracts the name and indices of a element.  It also accesses
*     the FITS header EXTSHAPE to determine the shape of the structure.
*     It is intended for processing EXTNAME strings created by
*     NDF2FITS.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     OBJECT = CHARACTER * ( * ) (Given)
*        The component specification to be broken into its constituents.
*        It comprises a name with optional comma-separated indices of
*        an element given between parentheses.  Index ranges, i.e.
*        containing ':' are not permitted.
*     NAME = CHARACTER * ( * ) (Given)
*        The component name extracted from the OBJECT, i.e. all the
*        characters before any leading left parenthesis.
*     MAXDIM = INTEGER (Given)
*        The maximum number of dimensions of the structure, and declared
*        size of DIMS and INDICE arrays in the calling routine.  It is
*        recommended to be set to DAT__MXDIM.
*     NDIM = INTEGER (Given)
*        The number of dimensions of the structure.
*     DIMS( MAXDIM ) = INTEGER (Given)
*        The dimensions of the structure.  For dimensionalities greater
*        than NDIM, the dimensions are set to 1.
*     INDICE( MAXDIM ) = INTEGER (Given)
*        The indices of the element of the structure.  For
*        dimensionalities greater than NDIM, the indices are set to 1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The FITS file must be open and in the appropriate extension.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 March 21 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants

*  Arguments Given:
      INTEGER FUNIT
      CHARACTER * ( * ) OBJECT
      INTEGER MAXDIM

*  Arguments Returned:
      CHARACTER * ( * ) NAME
      INTEGER NDIM
      INTEGER DIMS( MAXDIM )
      INTEGER INDICE( MAXDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER FITSOK             ! Good status for FITSIO library
      PARAMETER( FITSOK = 0 )

      INTEGER MAXWRD             ! Maximum number of words in object
      PARAMETER( MAXWRD = DAT__MXDIM + 1 ) ! specification.

*  Local Variables:
      CHARACTER * ( 48 ) COMENT  ! FITS header comment
      INTEGER END( MAXWRD )      ! End columns of words (not used)
      CHARACTER * ( 30 ) EXSHAP  ! Extension shape
      INTEGER I                  ! Loop counter
      INTEGER NWORD              ! Number of words in HISTORY card
      INTEGER START( MAXWRD )    ! Start columns of words (not used)
      LOGICAL THERE              ! Keyword is present?
      CHARACTER * ( DAT__SZNAM ) WORDS( MAXWRD ) ! Words in the
                                 ! component specfication

*.

*  Initialize returned variables.
      NDIM = 0
      NAME = ' '
      DO I = 1, MAXDIM
         INDICE( I ) = 1
         DIMS( I ) = 1
      END DO

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Replace the parentheses and commas by spaces in the structure
*  definition.
      CALL CHR_TRCHR( ',()', '   ', OBJECT, STATUS )

*  Break the structure definition into words.
      CALL CHR_DCWRD( OBJECT, MAXWRD, NWORD, START, END, WORDS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  One word means just the name is present.
      NAME = WORDS( 1 )
      IF ( NWORD .GT. 1 ) THEN
         NDIM = NWORD - 1

*  Convert the string to integer.  Thus assumes no range definition,
*  which is reasonable because the FITS writing code only creates a
*  single cell per FITS binary table.
         DO I = 2, NWORD
            CALL CHR_CTOI( WORDS( I ), INDICE( I - 1 ), STATUS )
         END DO
      END IF

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the EXTSHAPE keyword.
      CALL COF_GKEYC( FUNIT, 'EXTSHAPE', THERE, EXSHAP, COMENT, STATUS )

      IF ( .NOT. THERE ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COF_EXDIM_EXTSHAPE',
     :     'EXTSHAPE keyword is missing.  Unable to recreate the '/
     :     /'NDF extension.', STATUS )
         GOTO 999
      END IF

*  Replace the commas by spaces in the shape.
      CALL CHR_TRCHR( ',', '  ', EXSHAP, STATUS )

*  Break the shape into words.
      CALL CHR_DCWRD( EXSHAP, MAXWRD, NWORD, START, END, WORDS, STATUS )

*  Assign the indices.  Convert the string to integer.  Thus assumes no
*  range definition, which is reasonable because the FITS writing code
*  only creates a single cell per FITS binary table.
      DO I = 1, NWORD
         CALL CHR_CTOI( WORDS( I ), DIMS( I ), STATUS )
      END DO

  999 CONTINUE

      END
