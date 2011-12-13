      SUBROUTINE CON_EKEY<T>( NDF, NAME, NOCCUR, THERE, VALUE, COMENT,
     :                        STATUS )
*+
*  Name:
*     CON_EKEYx

*  Purpose:
*     Gets the value and comment of a named header of type <COMM> from
*     an NDF's FITS extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_EKEYx( NDF, NAME, NOCCUR, THERE, VALUE, COMENT, STATUS )

*  Description:
*     This routine searches the FITS NDF extension containing the
*     header card images from a FITS file for a keyword NAME; and
*     returns its value and comment, and the number of the card image
*     within the buffer array that contains the named keyword.  The
*     search ends when the next end of a header block, marked by the
*     END keyword, is encountered or the buffer is exhausted.  If the
*     keyword is present, THERE is .TRUE., otherwise it is .FALSE.
*     Since all cards images are in character format, type conversion
*     is performed.  An error status will be returned if the conversion
*     has failed.  If the keyword expected to be present more than
*     once, then the argument NOCCUR controls which occurrence will be
*     retrieved.  If a keyword is not found, then no error results and
*     the argument VALUE remains unmodified.
*
*     The name may be compound to permit reading of hierarchical
*     keywords.  This routine will probably only work for HISTORY,
*     COMMENT and ' ' (blank) if there is just one value given on the
*     line, i.e. only one "keyword = value" before any comment
*     marker.  An error will result otherwise.

*  Arguments:
*     NDF = INTEGER (Given)
*        The NDF identifier.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the keyword whose value is required.  This may be
*        a compound name to handle hierarchical keywords, and it has
*        the form keyword1.keyword2.keyword3 etc.  The maximum number of
*        keywords per FITS card is 20.  Comparisons are performed in
*        uppercase and blanks are removed.  Each keyword must be no
*        longer than 8 characters.
*     NOCCUR = INTEGER (Given)
*        The value of this argument specifies which occurrence of a
*        keyword should be used, if multiple ones are expected.  Any
*        value less than or equal to 1 indicates the first occurrence.
*     THERE = LOGICAL (Returned)
*        If .TRUE., the keyword given by argument NAME is present,
*        regardless of the exit status.  Note THERE=.FALSE. whenever
*        there is no FITS extension.
*     VALUE = ? (Returned)
*        The value of the keyword.
*     COMENT = CHARACTER * ( * ) (Returned)
*        The comment associated with the keyword.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Notes:
*     - There is a routine for each of the data types character,
*     logical, integer, real and double precision: replace "x" in the
*     routine name by C, L, I, R or D as appropriate.

*  Copyright:
*     Copyright (C) 1997, 2004 Central Laboratory of the Research
*     Councils.  2009 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1997 March 16 (MJC):
*        Original version.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2009 June 29 (MJC):
*        Replace cloned CON_GKEYx with KAPLIBS FTS1_GKEYx.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'DAT_PAR'          ! DAT__ global definitions
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) NAME
      INTEGER NOCCUR

*  Arguments Returned:
      LOGICAL THERE
      <TYPE> VALUE
      CHARACTER * ( * ) COMENT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CARD               ! Number of the card image containing
                                 ! keyword NAME
      INTEGER EL                 ! Number of FITS header records mapped
      LOGICAL FITSPR             ! FITS extension is present in NDF?
      INTEGER LENGTH             ! Length of a character being mapped
      CHARACTER * ( DAT__SZLOC ) LOC ! FITS extension locator
      INTEGER PNTR( 1 )          ! Pointer to mapped FITS headers
      INTEGER SCARD              ! Search-start card number

*.

*  Initialise the important presence flag even before global status is
*  checked.
      THERE = .FALSE.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check whether or not a FITS extension is present.
      CALL NDF_XSTAT( NDF, 'FITS', FITSPR, STATUS )

*  Find the FITS extension and map it.
      IF ( FITSPR ) THEN

         CALL NDF_XLOC( NDF, 'FITS', 'READ', LOC, STATUS )
         CALL DAT_MAPV( LOC, '_CHAR*80', 'READ', PNTR( 1 ), EL, STATUS )
         LENGTH = 80

*  Obtain the value and comment.
         SCARD = 1
         CALL FTS1_GKEY<T>( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                      SCARD, NAME, NOCCUR,
     :                      THERE, VALUE, COMENT, CARD, STATUS,
     :                      %VAL( CNF_CVAL( LENGTH ) ) )

*  Unmap the array of FITS header cards and release the locator.
         CALL DAT_UNMAP( LOC, STATUS )
         CALL DAT_ANNUL( LOC, STATUS )
      END IF

      END
