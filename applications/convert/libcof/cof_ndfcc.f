      SUBROUTINE COF_NDFCC( FUNIT, NDF, STATUS )
*+
*  Name:
*     COF_NDFCC

*  Purpose:
*     Sets the character components of an NDF using a FITS header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_NDFCC( FUNIT, NDF, STATUS )

*  Description:
*     This routine sets the title, and units character components
*     of an NDF, by using the information stored in a FITS header.
*     Specifically, the OBJECT keyword's value, if present, becomes the
*     NDF title.  Similarly the value of the BUNIT keyword is mapped to
*     the NDF units.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     NDF = INTEGER (Given)
*        The identifier of the NDF which is to have character components
*        assigned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The current header and data unit must either be primary or an
*     IMAGE extension.  The routine aborts with an error status if this
*     requirement is not satisfied.
*
*     [optional_subroutine_items]...

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     Copyright (C) 2007 Science & Technology Facilities Council. All
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
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     1996 January 25 (MJC):
*        Original version.
*     2007 July 11 (PWD):
*        When extension has a symbolic type of IMAGE use that
*        to override the XTENSION value (for compressed images).
*     2009 November 30 (MJC):
*        Add NDF label.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants

*  Arguments Given:
      INTEGER FUNIT
      INTEGER NDF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks

*  Local Variables:
      CHARACTER*48 COMENT        ! Keyword comment
      INTEGER HDUTYP             ! Current HDU type
      CHARACTER*70 LABEL         ! NDF label
      INTEGER NC                 ! Number of characters in a component
      INTEGER NHDU               ! Number of the current HDU
      LOGICAL THERE              ! Keyword is present?
      CHARACTER*70 TITLE         ! NDF title
      CHARACTER*70 UNITS         ! NDF units
      CHARACTER*8 XTENS          ! Extension name

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the current HDU is the primary or IMAGE extension.
*  At present there is no inquiry routine, so inquire the number of
*  the HDU.  1 is the primary HDU.
      CALL FTGHDN( FUNIT, NHDU )
      IF ( NHDU .GT. 1 ) THEN

*  Obtain the value of the XTENSION keyword.
         CALL COF_GKEYC( FUNIT, 'XTENSION', THERE, XTENS, COMENT,
     :                   STATUS )

         CALL FTGHDT( FUNIT, HDUTYP, STATUS )
         IF ( XTENS .EQ. 'BINTABLE' .AND. HDUTYP .EQ. 0 ) THEN
            XTENS = 'IMAGE'
         END IF


         IF ( .NOT. ( THERE .AND. STATUS .EQ. SAI__OK
     :        .AND. XTENS .EQ. 'IMAGE' ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COF_NDFCC',
     :        'Current header and data unit is not primary or IMAGE '/
     :        /' extension, therefore cannot define NDF character '/
     :        /'components.', STATUS )
            GOTO 999
         END IF
      END IF

*  Obtain the title from the OBJECT keyword in the header.
      CALL COF_GKEYC( FUNIT, 'OBJECT', THERE, TITLE, COMENT, STATUS )

*  If it is present, set the NDF title, truncating unnecessary blanks.
      IF ( THERE .AND. TITLE .NE. ' ' ) THEN
         NC = CHR_LEN( TITLE )
         CALL NDF_CPUT( TITLE( :NC ), NDF, 'Title', STATUS )
      END IF

*  Obtain the label from the LABEL keyword in the header.
      CALL COF_GKEYC( FUNIT, 'LABEL', THERE, LABEL, COMENT, STATUS )

*  If it is present, set the NDF label, truncating unnecessary blanks.
      IF ( THERE .AND. LABEL .NE. ' ' ) THEN
         NC = CHR_LEN( LABEL )
         CALL NDF_CPUT( LABEL( :NC ), NDF, 'Label', STATUS )
      END IF

*  Obtain the units from the BUNIT keyword in the header.
      CALL COF_GKEYC( FUNIT, 'BUNIT', THERE, UNITS, COMENT, STATUS )

*  If it is present, set the NDF units, truncating unnecessary blanks.
      IF ( THERE .AND. UNITS .NE. ' ' ) THEN
         NC = CHR_LEN( UNITS )
         CALL NDF_CPUT( UNITS( :NC ), NDF, 'Units', STATUS )
      END IF

  999 CONTINUE

      END
