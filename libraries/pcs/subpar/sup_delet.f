      SUBROUTINE SUBPAR_DELET ( NAMECODE, STATUS )
*+
*  Name:
*     SUBPAR_DELET

*  Purpose:
*     delete an object associated with a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_DELET ( NAMECODE, STATUS )

*  Description:
*     Get an object name and delete a data_system object.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        Pointer to the Parameter whose associated object is
*        to be deleted.
*     STATUS=INTEGER

*  Algorithm:
*     The character string associated with the given parameter is
*     obtained, and interpreted as a VMS filename (an HDS container
*     file), followed by the full name of the structure component
*     required. The component is deleted if possible. The data structure
*     down to the level immediately above the required new component
*     must exist already.
*     Cancel the parameter.

*  Copyright:
*     Copyright (C) 1985, 1987, 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-MAR-1985 (BDK):
*        Original
*     05-MAY-1987 (BDK):
*        moved from DATPAR library as HDS recursive erase now
*        available
*     16-NOV-1990 (AJC):
*        use SUBPAR_HDSOPEN to cope with slice or cell at
*        top level
*        close file correctly after error
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*      9-AUG-1993 (AJC):
*        Remove INCLUDE PAR_ERR
*     19-MAR-2018 (DSB):
*        Cater for long file names (<=1000).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Arguments Given:
      INTEGER NAMECODE                     ! pointer to parameter


*  Status:
      INTEGER STATUS

*    Global variable :
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      CHARACTER*1000 FILENAME              ! name of VMS container file

      CHARACTER*1000 STRUCTNAME            ! character string associated
                                           ! with named parameter

      CHARACTER*15 COMPONENT(30)           ! names of the levels in the
                                           ! data structure

      INTEGER NUMLEVS                      ! number of levels in the
                                           ! named structure

      CHARACTER*(DAT__SZLOC) FILOC         ! HDS locator of container
                                           ! file

      CHARACTER*(DAT__SZLOC) TOPLOC        ! HDS locator (temporary)

      CHARACTER*(DAT__SZLOC) BOTLOC        ! HDS locator (temporary)

      INTEGER LEVEL                        ! structure- level counter

      INTEGER ISTAT                        ! local status

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get the file/structure name associated with the named
*   parameter.
*   NOTE - if there isn't a current name, GETNAME will go looking for
*   one following VPATH.
      CALL SUBPAR_GETNAME ( NAMECODE, STRUCTNAME, STATUS )

*   Split the name up into a VMS filename, followed by a set of
*   component names leading down the hierarchy to the part of the
*   structure required.
      CALL SUBPAR_SPLIT ( STRUCTNAME, 30, NUMLEVS, COMPONENT,
     :  FILENAME, STATUS )

*   If the only the top-level of the file has been specified, then delete
*   the container file.
      IF ( NUMLEVS .EQ. 1 ) THEN

*     Only one level specified
         CALL HDS_OPEN ( FILENAME, 'WRITE', FILOC, STATUS )
         CALL HDS_ERASE ( FILOC, STATUS )

      ELSE

*     A lower-level object is specified
*     Open the container file.
*     TOPLOC will correctly locate slice or cell.
*     HDSOPEN will nullify locators if error
         CALL SUBPAR_HDSOPEN( FILENAME, COMPONENT(1), 'UPDATE',
     :                        FILOC, TOPLOC, STATUS )

*      Move down to the lower levels of the structure
         BOTLOC = TOPLOC

         DO LEVEL = 2, NUMLEVS - 1

            CALL SUBPAR_DATFIND ( TOPLOC, COMPONENT(LEVEL), BOTLOC,
     :        STATUS )
            CALL DAT_ANNUL ( TOPLOC, STATUS )
            TOPLOC = BOTLOC

         ENDDO

         IF ( STATUS .EQ. SAI__OK ) THEN

*        Delete the bottom-level object and any lower components
            CALL DAT_ERASE ( BOTLOC, COMPONENT(NUMLEVS), STATUS )

*        Clean up
*        Annul the locator, and close the file
            CALL DAT_ANNUL ( BOTLOC, STATUS )

*        Close the file
*        HDS_CLOSE requires OK status
            ISTAT = SAI__OK
            CALL HDS_CLOSE ( FILOC, ISTAT )

         ENDIF

      ENDIF

*   Cancel the parameter
      CALL SUBPAR_CANCL ( NAMECODE, STATUS )

      END
