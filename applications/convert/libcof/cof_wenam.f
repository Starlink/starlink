      SUBROUTINE COF_WENAM( FUNIT, EXTNAM, COMENT, STATUS )
*+
*  Name:
*     COF_WENAM

*  Purpose:
*     Writes extension-name metadata.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_WENAM( FUNIT, EXTNAM, COMENT, STATUS )

*  Description:
*     The routine writes the extension-name metadata for long strings.
*     In most cases this the regular EXTNAME keyword as defined in
*     "Generalized extensions and blocking factors for FITS" by
*     Grosbol et al. 1988, A&A Supp. Ser. 73, 359 will suffice.
*     However, since the full component name can be longer than the 68
*     characters that can be accommodated in the EXTNAME keyword, and
*     it is not good practice to use the long-string CONTINUE convention
*     for mandatory (or reserved) keywords, an alternative approach is
*     needed and is encapsulated in this routine.
*
*     The approach taken for long component names is to create an
*     EXTNAMEF header that uses the Long-string CONTINUE convention
*     (http://fits.gsfc.nasa.gov/registry/continue_keyword.html) to
*     record the full path.  EXTNAME is reset to '@EXTNAMEF' and
*     keyword EXTVER set to the index of the current HDU.  The latter
*     is required to avoid ambiguity should a user specify an extension
*     by its EXTNAME name.  It also prevents fitsverify from issuing a
*     warning.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The logical unit number of the output FITS file.
*     EXTNAM = CHARACTER * ( * ) (Given)
*        The name of the structure.
*     COMENT = CHARACTER * ( * ) (Given)
*        The comment string for the EXTNAM.  If set to a blank string
*        the following default comments will be used depending on the
*        type of extension.
*           Image         "Component"
*           Binary table  "name of this binary-table extension"
*           ASCII table   "name of this ASCII-table extension"
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This will return an error status if the current HDU is not an
*     extension.

*  Prior Requirements:
*     -  A primary HDU unit exists in the FITS file, and the FITS file
*     is open.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     2009 November 28 (MJC):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FUNIT
      CHARACTER * ( * ) EXTNAM
      CHARACTER * ( * ) COMENT

*  Arguments Returned:
      LOGICAL WRITTN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Effective string length

*  Local Constants:
      INTEGER   FITSCH           ! Maximum length of a FITS character
      PARAMETER( FITSCH = 68 )   ! value

      INTEGER FITSOK             ! Good status for FITSIO library
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      CHARACTER*48 COM           ! Comment
      INTEGER FSTAT              ! FITSIO status
      INTEGER HDUTYP             ! HDU type 0=Image, 1=ASCII, 2=binary
      INTEGER NC                 ! Number of characters in keyword
      INTEGER NHDU               ! Index of the current HDU

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      FSTAT = FITSOK

*  Determine whether the current HDU is an extension.
      CALL FTGHDN( FUNIT, NHDU )
      IF ( NHDU .EQ. 1 ) THEN
         STATUS = SAI__ERROR
	 CALL ERR_REP( 'COF_WENAM_ERR1', 'Attempting to write an '/
     :                 /'EXTNAME keyword in the primary HDU (probable '/
     :                 /'programming error).', STATUS )
         GOTO 999
      END IF

*  Check that the EXTNAM header is not too long for a single header.
      NC = CHR_LEN( EXTNAM )
      IF ( NC .GT. FITSCH ) THEN

*  Write a dummy value for EXTNAME.
         CALL FTUKYS( FUNIT, 'EXTNAME', '@EXTNAMEF', ' ', FSTAT )

*  Write the EXTVER header.  By using the HDU index we are guaranteed of
*  a unique number.
         CALL FTPKYJ( FUNIT, 'EXTVER', NHDU, 'Unique extension version',
     :                FSTAT )

*  Set the comment string.
         IF ( COMENT .EQ. ' ' ) THEN
	    CALL FTGHDT( FUNIT, HDUTYP, FSTAT )
	    IF ( HDUTYP .EQ. 0 ) THEN
               COM = 'Component'
	    ELSE IF ( HDUTYP .EQ. 1 ) THEN
               COM = 'name of this ASCII-table extension'
	    ELSE IF ( HDUTYP .EQ. 2 ) THEN
               COM = 'name of this binary-table extension'
	    ELSE
	       COM = ' '
	    END IF
	 ELSE
	    COM = COMENT
	 END IF

*  Some structures can generate long names, for which the CONTINUE
*  Long-string convention is needed.  First indicate that the
*  convention is in use by writing the LONGSTRN keyword containing the
*  version number of the convention.  Then write the EXTNAMEF long
*  string with the chosen comment.
         CALL FTPLSW( FUNIT, FSTAT )
         CALL FTPKLS( FUNIT, 'EXTNAMEF', EXTNAM, COM, FSTAT )
      END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_WENAM_ERR2', '',
     :     'Error setting EXTNAMEF keyword.', STATUS )
      END IF

  999 CONTINUE

      END
