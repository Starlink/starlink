      SUBROUTINE SUBPAR_CURSAV ( NAMECODE, STRUCTNAME, STATUS )
*+
*  Name:
*     SUBPAR_CURSAV

*  Purpose:
*     Save the current name value in the parameter store.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_CURSAV ( NAMECODE, STRUCTNAME, STATUS )

*  Description:
*     Save the name in storage for current value unless parameter is
*     'internal'.
*     The storage in the tasks parameter file for a name associated with
*     a parameter should look like:
*         Type            Name
*      ADAM_PARNAME   parameter_name
*        _CHAR*132      NAMEPTR

*  Arguments:
*     NAMECODE=INTEGER (given)
*     STRUCTNAME=CHARACTER*(*) (given)
*     STATUS=INTEGER

*  Algorithm:
*     NAMECODE, which is the address associated with the parameter is
*     used to access external parameter storage. If the required object
*     does not exist, it is created (any existing object of the wrong
*     type being erased first) and the structure name is copied to the
*     external store.

*  Copyright:
*     Copyright (C) 1988, 1990, 1993 Science & Engineering Research Council.
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
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-DEC-1988 (AJC):
*        Original version code removed from SUBPAR_DEACT
*     05-FEB-1990 (AJC):
*        Guard against hanging locators
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'


*  Arguments Given:
      INTEGER NAMECODE                    ! index to the program
                                          ! parameter

      CHARACTER*(*) STRUCTNAME            ! the name of the data structure
                                          ! to be associated with the
                                          ! parameter.


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      CHARACTER*15 HDSTYPE               ! Type of located structure
      CHARACTER*(DAT__SZLOC) BOTLOC      ! Locator to ADAM_PARNAME structure
      CHARACTER*(DAT__SZLOC) LOC         ! Locator to this parameter
      LOGICAL THERE                      ! structure exist flag

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      IF (PARVPATH(1,NAMECODE) .NE. SUBPAR__INTERNAL ) THEN

*      Check that a component exists for this parameter
         CALL DAT_THERE ( EXTLOC, PARNAMES(NAMECODE), THERE, STATUS )

         IF ( THERE ) THEN
*         There is a component for this parameter
*         Check its type
            BOTLOC = ' '
            CALL DAT_FIND ( EXTLOC, PARNAMES(NAMECODE), BOTLOC, STATUS )
            CALL DAT_TYPE ( BOTLOC, HDSTYPE, STATUS )
            IF ( HDSTYPE .NE. 'ADAM_PARNAME' ) THEN
*            If it is not a structure for name storage
*            Erase it.
               CALL DAT_ANNUL ( BOTLOC, STATUS )
               CALL DAT_ERASE ( EXTLOC, PARNAMES(NAMECODE), STATUS )
               THERE = .FALSE.
            ELSE
*            If it is already a structure for name storage.
*            check for the 'NAMEPTR' component within it.
               CALL DAT_THERE ( BOTLOC, 'NAMEPTR', THERE, STATUS )
               IF ( THERE ) THEN
*               If the 'NAMEPTR' component exists, get a locator to it
                  LOC = ' '
                  CALL DAT_FIND ( BOTLOC, 'NAMEPTR', LOC, STATUS )
               ELSE
*               If the 'NAMEPTR' component does not exist, erase the storage
*               for this parameter.
                  CALL DAT_ANNUL ( BOTLOC, STATUS )
                  CALL DAT_ERASE ( EXTLOC, PARNAMES(NAMECODE), STATUS )
                  THERE = .FALSE.
               ENDIF
            ENDIF
         ENDIF

*      Now if name storage does not exist for this parameter, create it
         IF ( .NOT. THERE ) THEN
            BOTLOC = ' '
            LOC = ' '
            CALL DAT_NEW ( EXTLOC, PARNAMES(NAMECODE), 'ADAM_PARNAME',
     :              0, 0, STATUS )
            CALL DAT_FIND ( EXTLOC, PARNAMES(NAMECODE), BOTLOC, STATUS )
            CALL DAT_NEW ( BOTLOC, 'NAMEPTR', '_CHAR*132', 0, 0,
     :              STATUS )
            CALL DAT_FIND ( BOTLOC, 'NAMEPTR', LOC, STATUS )
         ENDIF
*
*         The structure should now exist. Copy the name into it.
*
         CALL DAT_PUTC ( LOC, 0, 0, STRUCTNAME, STATUS )
         CALL DAT_ANNUL ( LOC, STATUS )
         CALL DAT_ANNUL ( BOTLOC, STATUS )

      ENDIF

      END
