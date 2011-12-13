      SUBROUTINE CON_RCCC( INLOC, CUBID, STATUS )
*+
*  Name:
*     CON_RCCC

*  Purpose:
*     Copies the character components for an Asterix cube.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL CON_RSCC( INLOC, CUBID; STATUS )

*  Description:
*     Copy the character components from an Asterix data cube to
*     an output NDF.

*  Arguments:
*     INLOC  =  CHARACTER*(*) (Given)
*        Locator to the input cube.
*     CUBID  =  INTEGER (Given)
*        Identifier for the data cube.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     For each character component:-
*       Attempt to get a locator
*       If ok then
*         Attempt to get the value of the component.
*       else
*         Set the value to 'Unknown'
*         If the failure was due to not being able to find the component
*           Reset the status.
*         end if
*       end if
*     end for
*     Set the character components for the output NDF.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
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
*     ACD: A C Davenhall (Edinburgh)
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     3/9/97. (ACD):
*        Original version.
*     2009 June 29 (MJC):
*        Used modern coding style.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard Starlink constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'DAT_ERR'          ! HDS error codes

*  Arguments Given:
      CHARACTER*(*) INLOC
      INTEGER CUBID

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of string less trailing blanks

*  Local Variables:
      INTEGER NC                 ! Used length of string
      CHARACTER*80 LABEL         ! Value of the LABEL component
      CHARACTER*(DAT__SZLOC) LABLOC ! Locator to the input LABEL component
      CHARACTER*80 TITLE         ! Value of the TITLE component
      CHARACTER*(DAT__SZLOC) TTLLOC ! Locator to the input TITLE component
      CHARACTER*80 UNITS         ! Value of the UNITS component
      CHARACTER*(DAT__SZLOC) UNTLOC ! Locator to the input UNITS component

*.

*  Check the global inherited status.
      IF (STATUS .EQ. SAI__OK) THEN

*  For each component, attempt to get a locator in the input Asterix
*  structure.  If the attempt succeeds then attempt to get the value.
*  However, if a locator is not obtained then set the component value to
*  'Unknown' and reset the status if (and only if) the failure was
*  because the component was not found.
*
*  ... the title component.
         CALL DAT_FIND( INLOC, 'TITLE', TTLLOC, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL DAT_GET0C( TTLLOC, TITLE, STATUS )

         ELSE
            TITLE = 'Unknown'

            IF ( STATUS .EQ. DAT__OBJNF ) THEN
               CALL ERR_ANNUL( STATUS )
            END IF

         END IF

*  ... the label component.
         CALL DAT_FIND( INLOC, 'LABEL', LABLOC, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL DAT_GET0C( LABLOC, LABEL, STATUS )

         ELSE
            LABEL = 'Unknown'

            IF ( STATUS .EQ. DAT__OBJNF ) THEN
               CALL ERR_ANNUL( STATUS )
            END IF

         END IF

*  ... the units component.
         CALL DAT_FIND( INLOC, 'UNITS', UNTLOC, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL DAT_GET0C( UNTLOC, UNITS, STATUS )

         ELSE
            UNITS = 'Unknown'

            IF ( STATUS .EQ. DAT__OBJNF ) THEN
               CALL ERR_ANNUL( STATUS )
            END IF

         END IF

*  Set the character components in the output NDF.
         NC = CHR_LEN( TITLE )
         CALL NDF_CPUT( TITLE, CUBID, 'TITLE', STATUS )

         NC = CHR_LEN( LABEL )
         CALL NDF_CPUT( LABEL, CUBID, 'LABEL', STATUS )

         NC = CHR_LEN( UNITS )
         CALL NDF_CPUT( UNITS, CUBID, 'UNITS', STATUS )

      END IF

      END
