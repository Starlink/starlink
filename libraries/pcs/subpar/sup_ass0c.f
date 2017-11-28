      SUBROUTINE SUBPAR_ASS0C ( NAMECODE, ACCESS, CLEN, LOC, STATUS )
*+
*  Name:
*     SUBPAR_ASS0C

*  Purpose:
*     return a locator associated with a scalara character parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_ASS0C ( NAMECODE, ACCESS, CLEN, LOC, STATUS )

*  Description:
*     Given the index of a scalar character program parameter, an HDS
*     locator is returned corresponding to the associated data structure.
*     The length of the structure is set to the supplied length.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        Internal number identifying program parameter
*     ACCESS=CHARACTER*(*) (given)
*        Access mode, 'READ', 'WRITE' or 'UPDATE'
*     CLEN=INTEGER (given)
*        Length of character string to be stored, excluding trailing
*        blanks.
*     LOC=CHARACTER*DAT__SZLOC (returned)
*        Locator to data structure
*     STATUS=INTEGER
*        Status return

*  Algorithm:
*     This is a replacement for SUBPAR_ASSOC that is intended to be used
*     when storing scalar character strings in output parameters (i.e. it
*     is called from subpar_put0c). It ensures that the component of the
*     HDS parameter file in which the string will be stored is sufficiently
*     long to hold the whole string (excluding trailing spaces) without
*     truncation. The required string length is supplied as an argument.
*
*     This is a bit of a kludge. The whole of SUBPAR seems to be littered
*     with places where string lengths are hard-wired at 132. It would be
*     better to parameterise that value, and increase it to some more
*     realistic value (there already exists the SUBPAR_STRLEN constant,
*     but it seems hardly ever to be used). I tried doing this but there
*     was clearly subtle issues going on which I could not spot, since I
*     kept getting lots of stack smashing and random behaviour. The
*     change implemented in this file seems a more confined/encapsulated
*     fix that is less likely to have unforeseen consequences.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory
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
*     DSB: Davd S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     28-NOV-2017 (DSB):
*        Original
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
      INCLUDE 'SUBPAR_ERR'
      INCLUDE 'SUBPAR_PARERR'

*  Arguments Given:
      INTEGER NAMECODE             ! Number of program parameter
      CHARACTER*(*) ACCESS         ! Access mode, 'READ', 'WRITE'
                                   ! or 'UPDATE'
      INTEGER CLEN                 ! Length of string to be stored

*  Arguments Returned:
      CHARACTER*(DAT__SZLOC) LOC   ! Locator to data structure

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  External Functions:
      CHARACTER*(DAT__SZGRP) SUBPAR_PARGP           ! HDS group name
      EXTERNAL SUBPAR_PARGP

*  Local Variables:
      CHARACTER BOTLOC*(DAT__SZLOC)
      CHARACTER NAME*(DAT__SZNAM)
      CHARACTER PLOC*(DAT__SZLOC)
      INTEGER ALEN
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call the usual subpar_assoc routine to get the locator for the HDS
*  component in which the string will be stored.
      CALL SUBPAR_ASSOC( NAMECODE, ACCESS, LOC, STATUS )

*  Get its length. If it is longer than the requested length, we are ok and
*  can return without further action.
      CALL DAT_LEN( LOC, ALEN, STATUS )
      IF( ALEN .LT.  CLEN ) THEN

*  Erase the existing component within its parent and replace it with a longer
*  one.
         CALL DAT_PAREN( LOC, PLOC, STATUS )
         CALL DAT_NAME( LOC, NAME, STATUS )
         CALL DAT_ANNUL( LOC, STATUS )
         CALL DAT_ERASE( PLOC, NAME, STATUS )
         CALL DAT_NEW0C( PLOC, NAME, CLEN, STATUS )

*  Get a locator for the new component, then annul the parent locator.
         CALL DAT_FIND( PLOC, NAME, BOTLOC, STATUS )
         CALL DAT_ANNUL( PLOC, STATUS )

*  Associate the new HDS object with the parameter.
         CALL SUBPAR_PUTLOC( NAMECODE, BOTLOC, STATUS )

*  Return a clone of the locator.
         CALL DAT_CLONE( BOTLOC, LOC, STATUS )

*  Put the locators into a group so that they can eb annulled when the
*  parameter is cancelled.
         CALL HDS_LINK ( BOTLOC, SUBPAR_PARGP(NAMECODE), STATUS )
         CALL HDS_LINK ( LOC, SUBPAR_PARGP(NAMECODE), STATUS )

      END IF

      END
