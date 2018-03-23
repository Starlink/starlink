      SUBROUTINE SUBPAR_HDSASS ( NAMECODE, ACCESS, LOC, STATUS )
*+
*  Name:
*     SUBPAR_HDSASS

*  Purpose:
*     Gets locator for associated parameter value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_HDSASS ( NAMECODE, ACCESS, LOC, STATUS )

*  Description:
*     Gets an HDS locator to the global association for a parameter.
*     Nullify (annul and set to blank) the locator in the event of an
*     error.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        number of parameter
*     ACCESS=CHARACTER*(*) (given)
*        required access mode
*     LOC=CHARACTER*(DAT__SZLOC) (returned)
*        HDS locator to the value
*     STATUS=INTEGER

*  Algorithm:
*     If the GLOBAL component is primitive, copy it to the task's
*     parameter file. IN THIS CASE THE TYPE OF THE PARAMETER FILE COMPONENT
*     IS NOT NECESSARILY THE DECLARED TYPE FOR THE PARAMETER. Any existing
*     parameter file component with the same name will be deleted.
*     If the data isn't actually in the global asociation, but rather the
*     name of an HDS structure, then call SUBPAR_GETHDS
*     to update the 'current' value and return a locator to the structure.

*  Copyright:
*     Copyright (C) 1984, 1985, 1988, 1989, 1990, 1991, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     B.D.Kelly (REVAD::BDK
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-OCT-1984 (BDK):
*        Original
*     15-MAY-1985 (BDK):
*        handle scalars
*     03-JUN-1985 (BDK):
*        declare strings as _CHAR*132
*     16-AUG-1988 (AJC):
*        Include SUBPAR_PAR - How did it work before
*     09-AUG-1989 (AJC):
*        use HDS data conversion not SUBPAR_INCOPYx
*     16-JAN-1990 (AJC):
*        use GETHDS to ensure 'current' is updated
*     02-FEB-1990 (AJC):
*        guard against hanging locator
*     19-JUL-1991 (AJC):
*        remove unused declarations
*     02-OCT-1991 (AJC):
*        unmap and annul locator to global
*     06-APR-1992 (AJC):
*        translate ADAM_USER in GLOBAL name
*     07-APR-1992 (AJC):
*        copy GLOBAL component to parameter file
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*      3-FEB-2000 (AJC):
*        Use SUBPAR_PARGP to get an HDS group name for the parameter
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
      INTEGER NAMECODE                ! parameter number

      CHARACTER*(*) ACCESS            ! read, write or update


*  Arguments Returned:
      CHARACTER*(DAT__SZLOC) LOC      ! HDS locator to value


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  External Functions:
      CHARACTER*(DAT__SZGRP) SUBPAR_PARGP           ! HDS group name
      EXTERNAL SUBPAR_PARGP


*  Local Variables:
      CHARACTER*1000 GLONAM          ! expanded GLOBAL association name

      CHARACTER*(DAT__SZLOC) ASSLOC  ! top-level locator to global
                                     ! association file

      CHARACTER*(DAT__SZLOC) BOTLOC  ! locator to element in global
                                     ! association file

      CHARACTER*15 HDSTYPE

      INTEGER AULEN                  ! length of ADAM_USER translation

      LOGICAL PRIM

      LOGICAL THERE

      INTEGER ISTAT

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialise locator
      LOC = ' '
*
*   Open the HDS object specified in the association string.
*   This may contain the actual value(s) required, or it may contain the
*   name of the HDS structure containing the object
*   _HDSLOCS will nullify locators if it fails.
*
*   First expand ADAM_USER.
      IF ( CHARLIST(PARASSOC(1,NAMECODE))(1:10) .EQ. 'ADAM_USER:' )
     : THEN
         CALL SUBPAR_ADMUS( GLONAM, AULEN, STATUS )
         GLONAM(AULEN+1:) = CHARLIST(PARASSOC(1,NAMECODE))(11:)
      ELSE
         GLONAM = CHARLIST(PARASSOC(1,NAMECODE))
      ENDIF

      CALL SUBPAR_HDSLOCS ( GLONAM, ACCESS, ASSLOC, BOTLOC, STATUS )
*
*   Find what kind of an object has been located. If it is a primitive,
*   then it is the one required. If it is a structure, then it is either
*   the structure required or a pointer to another structure.
*
      CALL DAT_TYPE ( BOTLOC, HDSTYPE, STATUS )
      CALL DAT_PRIM ( BOTLOC, PRIM, STATUS )

      IF ( PRIM ) THEN
*
*      A primitive object. Create space for it in the program's private
*      HDS store and copy its value.
*
         CALL DAT_THERE ( EXTLOC, PARNAMES(NAMECODE), THERE, STATUS )
*
*      Space already exists for the parameter. Delete it, as it may not
*      have the correct shape/type
*
         IF ( THERE ) THEN
            CALL DAT_ERASE ( EXTLOC, PARNAMES(NAMECODE), STATUS )
         ENDIF

*
*       Copy the GLOBAL component to the parameter file
*
         CALL DAT_COPY ( BOTLOC, EXTLOC, PARNAMES(NAMECODE), STATUS )
*       and annul the locator
         CALL DAT_ANNUL ( BOTLOC, STATUS )
*
*       and get a locator to it
         CALL DAT_FIND ( EXTLOC, PARNAMES(NAMECODE), LOC, STATUS )
*
*      Set-up the parameter state and its locators
*
         CALL DAT_CLONE ( LOC, BOTLOC, STATUS )
         CALL SUBPAR_PUTFLOC ( NAMECODE, EXTLOC, STATUS )
         CALL SUBPAR_PUTLOC ( NAMECODE, BOTLOC, STATUS )
         CALL HDS_LINK ( LOC, SUBPAR_PARGP(NAMECODE), STATUS )
         CALL HDS_LINK ( BOTLOC, SUBPAR_PARGP(NAMECODE), STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN
            PARSTATE(NAMECODE) = SUBPAR__ACTIVE
            PARTYPE(NAMECODE) = 10 + MOD ( PARTYPE(NAMECODE), 10 )
         ENDIF

      ELSE
*
*      The global association is a structure. If it is a pointer to
*      another structure, find the real structure name required.
*
         IF ( HDSTYPE .EQ. 'ADAM_PARNAME' ) THEN

*         Get the name of the referenced pointer.
            CALL DAT_FIND ( BOTLOC, 'NAMEPTR', LOC, STATUS )
            CALL DAT_GETC ( LOC, 0, 0, PARVALS(NAMECODE), STATUS )
            CALL DAT_ANNUL ( LOC, STATUS )
            LOC = ' '
            CALL DAT_ANNUL ( BOTLOC, STATUS )

         ELSE

*         Use the GLOBAL component itself
            PARVALS(NAMECODE) = CHARLIST(PARASSOC(1,NAMECODE))

         ENDIF

*      Set the parameter info in common and obtain locator
         CALL SUBPAR_GETHDS( NAMECODE, PARVALS(NAMECODE), ACCESS, LOC,
     :      STATUS )

      ENDIF
*
*   Close the container file containing the associated value
*
      ISTAT = SAI__OK
      CALL HDS_CLOSE ( ASSLOC, ISTAT )

      END
