      SUBROUTINE CAT1_TIDNT (CI, GNAME, GI, STATUS)
*+
*  Name:
*     CAT1_TIDNT
*  Purpose:
*     Get an identifier for a named pre-existing component.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_TIDNT (CI, GNAME; GI; STATUS)
*  Description:
*     Get an identifier for a named pre-existing component.  The
*     component may be of any type.  If the specified component could
*     not be found then GI is set to the null identifier.  If the
*     specified component could not be found an error status is NOT
*     raised.
*
*     The comparison of the given name with all the existing names is
*     case independent.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Identifier to the catalogue to which the component belongs.
*     GNAME  =  CHARACTER*(*) (Given)
*        Name of the component.  The component may be of any type.
*     GI  =  INTEGER (Returned)
*        Identifier to the component.  The null identifier is returned
*        if the specified component could not be found.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Remember: the list of attributes holds the attributes for all
*     the components; an attribute is identified by its name and the
*     component to which it belongs.  The list of identifiers holds
*     details for every identifier.  The details for each identfier
*     include its value, its parent and its type, but not its name.
*     The name of an identifier is held as the value of the
*     attribute 'NAME' of that identifier.  Finally, it is a rule,
*     that a 'NAME' attribute will be of data type character.
*
*     Hence:
*
*     If the identifier is valid then
*       For each element of the attributes array
*         If the attribute name is 'NAME' and
*            the attribute type is character and
*            the attribute value is the name given for the component then
*           For each element in the identifiers array
*             If the identifiers in the attributes and identifiers
*             arrays match and the parent matches the given catalogue
*             identifier then
*               The requiured identifier has been found; set the return
*               value.
*             end if
*           end for
*         end if
*       end for
*     end if
*     If the component was not found then
*       Set the return value to the null identifier.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93  (ACD): Prologue only.
*     4/7/93  (ACD): First implementation.
*     27/7/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     2/2/94  (ACD): CAT1_TIDNT created from CAT_TIDNT.
*     12/6/94 (ACD): Made comparison of the given name with existing
*        names case independent.
*     11/4/95 (ACD): Changed the name of the null identifier.
*     27/5/98 (ACD): Speeded up search for the attribute.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  GNAME*(*)
*  Arguments Returned:
      INTEGER
     :  GI
*  Status:
      INTEGER STATUS  ! Global status
*  External References:
      LOGICAL CHR_SIMLR
*  Local Variables:
      INTEGER
     :  ATLOOP,   ! Current attribute.
     :  CURID     ! Current identifier.
      LOGICAL
     :  FOUND,    ! Flag: has requested component been found?
     :  MORE      ! Flag; continue hunting for attribute?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*     Check that the identifier is valid.

         FOUND = .FALSE.

         IF (CI .GT. 0  .AND.  CI .LE. NIDS__CAT1) THEN

*
*          Search the arrays for the required identifier.

            MORE = .TRUE.
            ATLOOP = IDATT__CAT1(CI) - 1

            DO WHILE (MORE)
               ATLOOP = ATLOOP + 1

               IF (ATTNM__CAT1(ATLOOP) .EQ. 'NAME'  .AND.
     :             ATTYP__CAT1(ATLOOP) .EQ. CAT__TYPEC) THEN
                  IF (CHR_SIMLR(ATTVC__CAT1(ATLOOP), GNAME) ) THEN
                     CURID = ATTID__CAT1(ATLOOP)

                     IF (IDPRN__CAT1(CURID) .EQ. CI) THEN
                        GI = CURID
                        FOUND = .TRUE.
                        MORE = .FALSE.
                     END IF
                  END IF
               END IF

               IF (ATLOOP .GE. NATT__CAT1) THEN
                  MORE = .FALSE.
               END IF
            END DO

         END IF

*
*       If the identifier was not found then return the null identifier.

         IF (.NOT. FOUND) THEN
            GI = CAT__NOID
         END IF

      END IF

      END
