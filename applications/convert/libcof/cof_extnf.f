      SUBROUTINE COF_EXTNF( FILNAM, STATUS )
*+
*  Name:
*     COF_EXTNF

*  Purpose:
*     Check for unmatched extension specifiers remaining in the EXTABLE

*  Language:
*     Fortran 77

*  Invocation:
*     CALL COF_CHKXT( FILNAM, STATUS )

*  Description:
*     Check the extable for extensions which have not been matched and
*     reports them.

*  Arguments:
*     FILNAM = CHARACTER*(*) (Given)
*        The name of the FITS file
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}
*     [routine_example]...

*  Notes:
*     -  {noted_item}
*     [routine_notes]...

*  Implementation Deficiencies:
*     -  {deficiency}
*     [routine_deficiencies]...

*  External Routines Used:
*     MERS
*        MSG_SETC
*        MSG_OUTIF

*  Pitfalls:
*     -  {pitfall}
*     [pitfall_description]...

*  Prior Requirements:
*     -  All insignificant spaces should have been removed from the extension
*        specifiers
*     [routine_prior_requirements]...

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

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
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     13-JUN-2000 (AJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'         ! HDS constants
      INCLUDE 'MSG_PAR'         ! MSG constants

*  Global Variables:
      INCLUDE 'F2NDF1_CMN'      ! The EXTABLE
*     Symbolic constants defined
*        MAXCMP = INTEGER
*           The maximum number of components allowed
*        MAXEXT = INTEGER
*           The maxtimum number of extension sets allowed
*     Global variables declared
*        NCMP = INTEGER
*           Number of component lines in EXTABLE
*        NEXTS = INTEGER
*           Number of extension sets in EXTABLE
*        EXTNS(MAXCMP,MAXEXT) = CHARACTER*32
*           Extension table from EXTABLE
*        NDFNMS(MAXEXT) = CHARACTER*(DAT__SZNAM)
*           NDF names from EXTABLE
*        COMPS(MAXCMP) = CHARACTER*(DAT__SZNAM*2)
*           Component names from EXTABLE
*        CODES(MAXCMP) = CHARACTER*12
*           Transformation codes from EXTABLE

*  Arguments Given:
      CHARACTER*(*) FILNAM

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      LOGICAL FIRST             ! If first not found
      INTEGER I, J              ! Loop counters

      IF ( STATUS .NE. SAI__OK ) RETURN

      FIRST = .TRUE.

      DO J = 1, NEXTS
         DO I = 1, NCMP
            IF( .NOT. DONE(I,J) ) THEN
               IF ( FIRST ) THEN
                  FIRST = .FALSE.
                  CALL MSG_SETC( 'FILE', FILNAM )
                  CALL MSG_OUTIF( MSG__NORM, 'COF_EXTNF_1',
     :             'The file $EXTABLE, specified by the EXTABLE '//
     :             'parameter, contained the following extension '//
     :             'specifiers which were not matched by extensions '//
     :             'in file ^FILE',
     :             STATUS )
               END IF
               CALL MSG_SETC( 'NDF', NDFNMS(I) )
               CALL MSG_SETC( 'COMP', COMPS(J) )
               CALL MSG_SETC( 'EXT', EXTNS(I,J) )
               CALL MSG_OUTIF( MSG__NORM, 'COF_F2NDF_NOTFND',
     :          'NDF ^NDF, COMPONENT ^COMP, SPECIFIER ^EXT',
     :          STATUS )
            END IF ! Not done
         END DO ! for each component
      END DO ! for each extension set

      END
