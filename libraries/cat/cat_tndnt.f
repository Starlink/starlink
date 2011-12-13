      SUBROUTINE CAT_TNDNT (CI, IDTYP, N, GI, STATUS)
*+
*  Name:
*     CAT_TNDNT
*  Purpose:
*     Get an identifier for the Nth (pre-existing) component of a
*     given type.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TNDNT (CI, IDTYP, N; GI; STATUS)
*  Description:
*     Get an identifier for the Nth (pre-existing) component of a
*     given type.  If N components of the required type could not be
*     found for the specified catalogue then the null identifier
*     (CAT__NOID is returned) and the running status remains ok.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     IDTYP  =  INTEGER (Given)
*        Type of identifier required, coded as follows:
*        CAT__CITYP - catalogue,
*        CAT__FITYP - column (or field),
*        CAT__QITYP - parameter,
*        CAT__EITYP - expression,
*        CAT__IITYP - index,
*        CAT__SITYP - selection,
*        CAT__JITYP - join.
*     N  =  INTEGER (Given)
*        Number of the component, of the specified type, that is
*        required.
*     GI  =  INTEGER (Returned)
*        Identifier to the required component.  If the required
*        component could not be found GI is set to CAT__NOID.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the number of components with the correct type and parentage
*     to zero.
*     Set the current element in the identifier arrays to zero.
*     Do while (continue searching)
*       Increment the current element in the identifier arrays.
*       If (the current element in the identifier arrays is less than
*       the total number of elements used) then
*         If (the type and parentage of the current element are equal
*         to the required values) then
*           Increment the number of components with the correct
*           parentage and type.
*           If (the number of components with the correct parentage and
*           type is equal the to required number) then
*             The required component has been found:
*             Set the termination flag,
*             Copy the current identifier to the returned identifier.
*           end if
*         end if
*       else
*         All the identifiers have been examined without finding the
*         required component:
*         Set the termination flag,
*         Set the returned identifier to null.
*       end if
*     end do
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
*     3/5/93 (ACD):  Prologue only.
*     17/7/93 (ACD): First implementation.
*     24/1/94 (ACD): Modified error reporting.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
*  Global Variables:
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
*  Arguments Given:
      INTEGER
     :  CI,
     :  IDTYP,
     :  N
*  Arguments Returned:
      INTEGER
     :  GI
*  Status:
      INTEGER STATUS      ! Global status
*  Local Variables:
      INTEGER
     :  CIDELM,  ! Current element in identifier arrays.
     :  NACCMP   ! No. of components with correct parentage and type.
      LOGICAL
     :  TERMIN   ! Flag: terminate search?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Search the identifier arrays until either N components of the
*       required type have been found (which corresponds to success)
*       of all the identifiers have been examined.  In the latter case
*       the requested component does not exist and the null identifier
*       is returned.  Note, however, that an error status is NOT raised.

         CIDELM = 0
         NACCMP = 0
         TERMIN = .FALSE.

         DO WHILE (.NOT. TERMIN)
            CIDELM = CIDELM + 1

            IF (CIDELM .LE. NIDS__CAT1) THEN

               IF (CI .EQ. IDPRN__CAT1(CIDELM)  .AND.
     :             IDTYP .EQ. IDTYP__CAT1(CIDELM) ) THEN
                  NACCMP = NACCMP + 1

                  IF (NACCMP .EQ. N) THEN
                     GI = IDVAL__CAT1(CIDELM)
                     TERMIN = .TRUE.
                  END IF

               END IF

            ELSE
               GI = CAT__NOID
               TERMIN = .TRUE.

            END IF

         END DO

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_TNDNT_ERR', 'CAT_TNDNT: error'/
     :        /'getting Nth identifier of a given type.', STATUS)
         END IF

      END IF

      END
