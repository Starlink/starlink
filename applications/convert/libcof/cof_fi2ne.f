      SUBROUTINE COF_FI2NE( FUNIT, NDF, NDFEXT, STATUS )
*+
*  Name:
*     COF_FI2NE

*  Purpose:
*     Converts a FITS IMAGE extension into a skeletal NDF extension
*     structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_FI2NE( FUNIT, NDF, NDFEXT, STATUS )

*  Description:
*     The routine recreates an NDF extension from a FITS IMAGE written
*     by NDF2FITS.  The extension can be a structure, sub-structure,
*     or structure-array element component.  It uses the EXTNAME and
*     EXTTYPE keywords to determine the extension's path and data type.
*     EXTNAME also has the element indices if the extension or
*     sub-structure is an array; the dimensions of such a structure or
*     a primitive array are taken from the EXTSHAPE keyword.  The
*     routine creates the extension or structure only if it does not
*     exist.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     NDF = INTEGER (Given)
*        The identifier for the NDF to contain the extension in the
*        current HDU.
*     NDFEXT = INTEGER (Returned)
*        The identifier for the skeletal extension NDF to contain the
*        FITS IMAGE extension in the current HDU.  Skeletal means it
*        has a dummy data type and dimensions, and has to be filled
*        components other than the data array.  This extension NDF
*        is located at its original path within the main NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The NDF and FITS files must be open.

*  Copyright:
*     Copyright (C) 2008, 2009 Science & Technology Facilities Council.
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
*     {enter_new_authors_here}

*  History:
*     2008 February 7 (MJC):
*        Original version based upon COF_FT2NE.
*     2009 November 30 (MJC):
*        Allow for long strings to be stored in EXTNAME keyword
*        via indirection.  Set the type for a MORE structure in an NDF
*        extension to be EXT instead of the generic STRUCT.
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
      INTEGER NDF

*  Arguments Returned:
      INTEGER NDFEXT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Compare character strings

*  Local Constants:
      INTEGER FITSOK             ! Good status for FITSIO library
      PARAMETER( FITSOK = 0 )

      INTEGER MAXWRD             ! Maximum number of words in line
      PARAMETER( MAXWRD = 10 )

*  Local Variables:
      CHARACTER * ( 48 ) COMENT  ! FITS header comment
      INTEGER DIMS( DAT__MXDIM ) ! Dimensions of the structure
      INTEGER ELEVEL             ! Top level of extensions
      INTEGER END( MAXWRD )      ! End columns of words (not used)
      CHARACTER * ( ( MAXWRD+1 ) * DAT__SZNAM ) EXPATH ! Extension path
      CHARACTER * ( DAT__SZTYP ) EXTYPE ! Extension data type
      CHARACTER * ( DAT__SZTYP ) EXTYPEM ! Extension data type, modified
      LOGICAL GOMORE             ! Look for the top MORE component?
      INTEGER INDICE( MAXWRD - 2 ) ! Indices of the structure's cell
      INTEGER LEVEL              ! Extension level
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to full structure
      CHARACTER * ( DAT__SZNAM ) NAME ! Extension or structure name
      LOGICAL NDFEX              ! Extension is an NDF?
      INTEGER NDIM               ! Number of dimensions of the structure
      CHARACTER * ( DAT__SZLOC ) NLOC ! Locator to NDF
      INTEGER NWORD              ! Number of words in HISTORY card
      INTEGER PLACE              ! NDF placeholder for NDF extension
      CHARACTER * ( DAT__SZLOC ) SALOC ! Locator to structure array
      INTEGER START( MAXWRD )    ! Start columns of words (not used)
      CHARACTER * ( DAT__SZLOC ) SXLOC( MAXWRD - 2 ) ! Locators to
                                 ! nested structures or cells thereof
      LOGICAL THERE              ! Keyword is present?
      CHARACTER * ( 30 ) WORDS( MAXWRD ) ! Words in extension's path

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the EXTNAME keyword, allowing for long strings.
      CALL COF_GENAM( FUNIT, EXPATH, COMENT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Validate the extension name.
      CALL CHR_UCASE( EXPATH )
      IF ( INDEX( EXPATH, 'MORE' ) .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'EN', EXPATH )
         CALL ERR_REP( 'COF_FI2NE_EXTMORE',
     :     'EXTNAME keyword value (^EN) does not contain the NDF '/
     :     /'extension component.  Unable to recreate the NDF '/
     :     /'extension.', STATUS )
         GOTO 999
      END IF

*  Obtain the EXTTYPE keyword.
      CALL COF_GKEYC( FUNIT, 'EXTTYPE', THERE, EXTYPE, COMENT, STATUS )

      IF ( .NOT. THERE ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COF_FI2NE_EXTYPE',
     :     'EXTTYPE keyword is missing.  Unable to recreate the '/
     :     /'NDF extension.', STATUS )
         GOTO 999
      END IF

*  If some obscure reason, there is no type specified, say because there
*  wasn't any defined in the original NDF extension, assign a default
*  data type.
      IF ( EXTYPE .EQ. ' ' ) EXTYPE = 'STRUCT'

*  See if the data type is an NDF.
      NDFEX = CHR_SIMLR( EXTYPE, 'NDF' )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Replace the dots in the path by spaces.
      CALL CHR_TRCHR( '.', ' ', EXPATH, STATUS )

*  Break the path into words.  Extension must be at least the second
*  item because the first is the top-level NDF name.  The extensions
*  normally begin at level 3 in an NDF, but allow for on-the-fly
*  conversion where the path is longer.  Find only the first MORE
*  component in case there extensions within the extensions.  Since
*  the number of levels isn't huge go for the regular DO loop rather
*  than a DO WHILE once the MORE is found.
      CALL CHR_DCWRD( EXPATH, MAXWRD, NWORD, START, END, WORDS, STATUS )
      GOMORE = .TRUE.
      DO LEVEL = 2, NWORD
         IF ( WORDS( LEVEL ) .EQ. 'MORE' .AND. GOMORE ) THEN
            ELEVEL = LEVEL + 1
            GOMORE =.FALSE.
         END IF
      END DO

*  Extract the extension's name, number of dimensions and their values,
*  dimensions, and indices to a structure element.
      CALL COF_EXDIM( FUNIT, WORDS( ELEVEL ), DAT__MXDIM, NAME, NDIM,
     :                DIMS, INDICE, STATUS )

      NAME = WORDS( ELEVEL )
      CALL NDF_XSTAT( NDF, NAME, THERE, STATUS )

*  If the structure exists, obtain an HDS locator to the structure.
*  This can be done directly if the extension is not an array, but for
*  an array of extensions we have to obtain a locator to the current
*  element of the extension.
      IF ( THERE ) THEN
         IF ( NDIM .EQ. 0 ) THEN
            CALL NDF_XLOC( NDF, NAME, 'UPDATE', SXLOC( 1 ), STATUS )

         ELSE
            CALL NDF_XLOC( NDF, NAME, 'UPDATE', LOC, STATUS )
            CALL DAT_CELL( LOC, NDIM, INDICE, SXLOC( 1 ), STATUS )
            CALL DAT_ANNUL( LOC, STATUS )
         END IF

*  Otherwise create a new extension of the appropriate shape.  Obtain a
*  locator to the current array element for an array of extensions.
      ELSE
         IF ( NAME .EQ. 'SMURF' ) THEN
           EXTYPEM = 'SMURF_EXT'
         ELSE
           EXTYPEM = EXTYPE
         END IF

         IF ( NDIM .EQ. 0 ) THEN
            CALL NDF_XNEW( NDF, NAME, EXTYPEM, 0, 0,
     :                     SXLOC( ELEVEL - 2 ), STATUS )
         ELSE
            CALL NDF_XNEW( NDF, NAME, EXTYPEM, NDIM, DIMS, LOC, STATUS )
            CALL DAT_CELL( LOC, NDIM, INDICE,
     :                     SXLOC( ELEVEL - 2 ), STATUS )
            CALL DAT_ANNUL( LOC, STATUS )
         END IF
      END IF

*  Create sub-structures of the extension.
      IF ( NWORD .GT. ELEVEL ) THEN
         DO LEVEL = ELEVEL - 1, NWORD - 2

*  Extract the structure's name, number of dimensions and their values,
*  dimensions, and indices to a structure element.
            CALL COF_EXDIM( FUNIT, WORDS( LEVEL + 2 ), DAT__MXDIM,
     :                      NAME, NDIM, DIMS, INDICE, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               print *, WORDS( LEVEL + 2 )
            END IF

*  Determine whether or not the structure exists.
            CALL DAT_THERE( SXLOC( LEVEL - 1 ), NAME, THERE, STATUS )

            IF ( .NOT. THERE ) THEN

               IF ( LEVEL .EQ. NWORD - 2 ) THEN

*  Make the structure using the data type.  When it is the last
*  structure in the path, it is the component to which EXTYPE refers.
                  IF ( NDIM .EQ. 0 ) THEN
                     CALL DAT_NEW( SXLOC( LEVEL - 1 ), NAME, EXTYPE,
     :                             0, 0, STATUS )
                  ELSE
                     CALL DAT_NEW( SXLOC( LEVEL - 1 ), NAME, EXTYPE,
     :                             NDIM, DIMS, STATUS )
                  END IF

*  NDF2FITS via COF_H2BIN only creates binary tables when the structure
*  contains primitive objects.  If the structure merely contains other
*  structures then at present there is no way of determining the data
*  type.  So use something generic, except for the special case of an
*  NDF within an extension having its own extension.
               ELSE
                  IF ( NDIM .EQ. 0 ) THEN
                     IF ( NAME .EQ. 'MORE' ) THEN
                        CALL DAT_NEW( SXLOC( LEVEL - 1 ), NAME, 'EXT',
     :                             0, 0, STATUS )
                     ELSE
                        CALL DAT_NEW( SXLOC( LEVEL - 1 ), NAME,
     :                                'STRUCT', 0, 0, STATUS )
                     END IF

                  ELSE
                     CALL DAT_NEW( SXLOC( LEVEL - 1 ), NAME, 'STRUCT',
     :                             NDIM, DIMS, STATUS )
                  END IF
               END IF

            END IF

*  Obtain the locator to the structure.
            IF ( NDIM .EQ. 0 ) THEN
               CALL DAT_FIND( SXLOC( LEVEL - 1 ), NAME,
     :                        SXLOC( LEVEL ), STATUS )

            ELSE

*  Obtain the locator to the cell of the structure array via the
*  structure locator.
               CALL DAT_FIND( SXLOC( LEVEL - 1 ), NAME, SALOC, STATUS )
               CALL DAT_CELL( SALOC, NDIM, INDICE, SXLOC( LEVEL ),
     :                        STATUS )
               CALL DAT_ANNUL( SALOC, STATUS )
            END IF

         END DO
      END IF

*  At this point the structure is accessed via locator SXLOC( NWORD-2 ),
*  but we need the parent of that for NDF_NEW to create the NDF.

*  Create a new NDF in the extension via an NDF placeholder.  The data
*  type and bounds will be changed below once they are known.
      CALL NDF_PLACE( SXLOC( NWORD - 3 ), NAME, PLACE, STATUS )
      CALL NDF_NEW( '_UBYTE', 1, 1, 1, PLACE, NDFEXT, STATUS )

*  Annul all the locators.
      DO LEVEL = ELEVEL - 2, NWORD - 2
         CALL DAT_ANNUL( SXLOC( LEVEL ), STATUS )
      END DO

  999 CONTINUE

      END
