      SUBROUTINE SUBPAR_NAMEASS ( NAMECODE, STRUCTNAME, STATUS )
*+
*  Name:
*     SUBPAR_NAMEASS

*  Purpose:
*     Gets HDS name for parameter global association.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_NAMEASS ( NAMECODE, STRUCTNAME, STATUS )

*  Description:
*     Gets an HDS name from the global association for a parameter.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        number of parameter
*     ACCESS=CHARACTER*(*) (given)
*        required access mode
*     LOC=CHARACTER*(DAT__SZLOC) (returned)
*        HDS locator to the value
*     STATUS=INTEGER

*  Algorithm:
*     Get the name actually stored in the parameter association store.
*     Access the HDS structure it names, and check its type. If it is a
*     pointer to another structure, get the structure name from it.
*     Otherwise return the original name.

*  Copyright:
*     Copyright (C) 1984, 1986, 1992, 1993 Science & Engineering Research Council.
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
*     04-JUL-1986 (BDK):
*        put answer in STRUCTNAME
*     01-FEB-1986 (AJC):
*        prevent hanging locator problem
*        also close down whatever happens
*     05-JUN-1992 (AJC):
*        translate ADAM_USER in GLOBAL name
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


*  Arguments Given:
      INTEGER NAMECODE                ! parameter number


*  Arguments Returned:
      CHARACTER*(*) STRUCTNAME        ! name of associated structure


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      INTEGER ISTAT                   ! internal status

      INTEGER AULEN                   ! length of ADAM_USER translation

      CHARACTER*(DAT__SZLOC) FILOC    ! HDS file locator

      CHARACTER*200 GLONAM            ! expanded GLOBAL association name

      CHARACTER*(DAT__SZLOC) BOTLOC   ! HDS locator

      CHARACTER*(DAT__SZLOC) LOC      ! HDS locator

      CHARACTER*15 HDSTYPE            ! type of named object

      LOGICAL PRIM                    ! .TRUE. => primitive object


*.


      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Open the HDS object specified in the association string.
*   This may be the actual value required, or it may contain the
*   name required.
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

      CALL SUBPAR_HDSLOCS ( GLONAM, 'READ', FILOC, BOTLOC, STATUS )
*
*   Find what kind of an object has been located. If it is a primitive,
*   then it is the one required. If it is a structure, then it is either
*   the structure required or a pointer to another structure.
*
      CALL DAT_TYPE ( BOTLOC, HDSTYPE, STATUS )
      CALL DAT_PRIM ( BOTLOC, PRIM, STATUS )

      IF ( .NOT. PRIM ) THEN
*
*      The global association is a structure. If it is a pointer to
*      another structure, find the real structure name required.
*
         IF ( HDSTYPE .EQ. 'ADAM_PARNAME' ) THEN

            LOC = ' '
            CALL DAT_FIND ( BOTLOC, 'NAMEPTR', LOC, STATUS )
            CALL DAT_GETC ( LOC, 0, 0, STRUCTNAME, STATUS )
            CALL DAT_ANNUL ( LOC, STATUS )

         ELSE

            STRUCTNAME = CHARLIST(PARASSOC(1,NAMECODE))

         ENDIF

      ELSE

         STRUCTNAME = CHARLIST(PARASSOC(1,NAMECODE))

      ENDIF


      CALL DAT_ANNUL ( BOTLOC, STATUS )
      ISTAT = SAI__OK
      CALL HDS_CLOSE ( FILOC, ISTAT )

      END
