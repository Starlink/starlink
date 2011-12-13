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
*     CALL COF_EXDIM( FUNIT, OBJECT, MAXDIM, NAME, NDIM, DIMS, INDICE,
*                     STATUS )

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
*     MAXDIM = INTEGER (Given)
*        The maximum number of dimensions of the structure, and declared
*        size of DIMS and INDICE arrays in the calling routine.  It is
*        recommended to be set to DAT__MXDIM.
*     NAME = CHARACTER * ( * ) (Returned)
*        The component name extracted from the OBJECT, i.e. all the
*        characters before any leading left parenthesis.
*     NDIM = INTEGER (Returned)
*        The number of dimensions of the structure.
*     DIMS( MAXDIM ) = INTEGER (Returned)
*        The dimensions of the structure.  For dimensionalities greater
*        than NDIM, the dimensions are set to 1.
*     INDICE( MAXDIM ) = INTEGER (Returned)
*        The indices of the element of the structure.  For
*        dimensionalities greater than NDIM, the indices are set to 1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The FITS file must be open and in the appropriate extension.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     Copyright (C) 2008 Science & Technology Facilities Council. All
*     Rights Reserved.

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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 March 21 (MJC):
*        Original version.
*     1997 November 15 (MJC):
*        Corrected some errors in the prologue, notably the accesses
*        for the returned arguments.
*     2008 February 12 (MJC):
*        Default to scalar if EXTSHAPE is missing rather than report
*        an error and exit.
*     {enter_further_changes_here}

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
                                 ! component specification

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

*  Default to a scalar.
      IF ( .NOT. THERE ) THEN
         DIMS( 1 ) = 0

      ELSE

*  Replace the commas by spaces in the shape.
         CALL CHR_TRCHR( ',', ' ', EXSHAP, STATUS )

*  Break the shape into words.
         CALL CHR_DCWRD( EXSHAP, MAXWRD, NWORD, START, END, WORDS,
     :                   STATUS )

*  Assign the indices.  Convert the string to integer.  Thus assumes no
*  range definition, which is reasonable because the FITS writing code
*  only creates a single cell per FITS binary table.
         DO I = 1, NWORD
            CALL CHR_CTOI( WORDS( I ), DIMS( I ), STATUS )
         END DO
      END IF

  999 CONTINUE

      END
