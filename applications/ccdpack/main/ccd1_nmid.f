      SUBROUTINE CCD1_NMID( ID, JNDF, NCARD, IPFITS, SINDEX, MATCH,
     :                      STATUS )
*+
*  Name:
*     CCD1_NMID

*  Purpose:
*     Check if stored frameset matches NDF.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_NMID( ID, JNDF, NCARD, IPFITS, SINDEX, MATCH, STATUS )

*  Description:
*     This routine checks the characteristics of a given NDF to see
*     whether it matches the ID string.  The ID string is
*     (of the same type as) that written by
*     the ASTEXP task to the AST file identifying framesets, so that
*     matching is in the sense defined by that task.  The ID string
*     consists of a keyword indicating the kind of test, followed by
*     some text in a format specific to that keyword.  Currently
*     implemented keywords are:
*
*        FITSID <fitskey> <value>
*           An NDF matches this ID if the first FITS header card with
*           the keyword <fitskey> has the value <value>.  If the value
*           is of type CHARACTER it must be in single quotes.  <fitskey>
*           may be compound to permit reading of hierarchical keywords.
*        INDEX <number>
*           An NDF matches this ID if the JNDF argument is equal to
*           <number>.
*        SET <number>
*           An NDF matches this ID if its Set Index attribute is equal
*           to <number>

*  Arguments:
*     ID = CHARACTER * ( * ) (Given)
*        String identifying the NDF.
*     JNDF = INTEGER (Given)
*        Index of NDF in set being considered.  This is used if the ID
*        is of type INDEX.
*     NCARD = INTEGER (Given)
*        The number of FITS header cards pointed to by IPFITS.  This is
*        used if the ID is of type FITS.
*     IPFITS = INTEGER (Given)
*        A pointer to an array of mapped FITS header cards.  This is
*        used if the ID is of type FITS.
*     SINDEX = INTEGER (Given)
*        The Set Index attribute of the NDF.  This is used if the ID is
*        of type SET.
*     MATCH = LOGICAL (Returned)
*        Whether the NDF matches the given ID string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     08-MAR-1999 (MBT):
*        Original version.
*     27-FEB-2001 (MBT):
*        Upgraded for use with Sets.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER JNDF
      INTEGER NCARD
      INTEGER IPFITS
      INTEGER SINDEX
      CHARACTER * ( * ) ID

*  Arguments Returned:
      LOGICAL MATCH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) FTSVAL  ! Character value of FITS card
      INTEGER IAT                ! Position in string
      INTEGER ICARD              ! Index of matched FITS card
      INTEGER IS                 ! Chosen Set Index value
      INTEGER IWE                ! Position of word end
      INTEGER IWS                ! Position of word start
      INTEGER JFSET              ! Index of frameset
      INTEGER JSET               ! Index of CCD_SET frameset

*.

*  Set default return value.
      MATCH = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Identify the type of ID string we have and switch on the result.
      IAT = 1

*  FITS ID type.
      IF ( ID( IAT:IAT + 6 ) .EQ. 'FITSID ' ) THEN
         IAT = IAT + 7
         CALL CHR_FIWS( ID, IAT, STATUS )
         IWS = IAT
         CALL CHR_FIWE( ID, IAT, STATUS )
         IWE = IAT
         CALL CCD1_FTGET( NCARD, IPFITS, 1, ID( IWS:IWE ), FTSVAL,
     :                    ICARD, STATUS )
         IAT = IWE + 1
         CALL CHR_FIWS( ID, IAT, STATUS )
         MATCH = ID( IAT: ) .EQ. FTSVAL

*  INDEX type.
      ELSE IF ( ID( IAT:IAT + 5 ) .EQ. 'INDEX ' ) THEN
         IAT = IAT + 6
         CALL CHR_FIWS( ID, IAT, STATUS )
         IWS = IAT
         CALL CHR_FIWE( ID, IAT, STATUS )
         IWE = IAT
         CALL CHR_CTOI( ID( IWS:IWE ), JFSET, STATUS )
         MATCH = JFSET .EQ. JNDF

*  SET type.
      ELSE IF ( ID( IAT:IAT + 3 ) .EQ. 'SET ' ) THEN
         IAT = IAT + 4
         CALL CHR_FIWS( ID, IAT, STATUS )
         IWS = IAT
         CALL CHR_FIWE( ID, IAT, STATUS )
         IWE = IAT
         CALL CHR_CTOI( ID( IWS:IWE ), IS, STATUS )
         MATCH = SINDEX .EQ. IS

*  Unidintified ID string type
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ID', ID )
         CALL ERR_REP( 'CCD1_NMID_BADID',
     :                    '  ID string "^ID" unrecognised', STATUS )

      END IF

      END
* $Id$
