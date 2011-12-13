      SUBROUTINE CAT1_NMCHK (CI, NAME, EXIST, STATUS)
*+
*  Name:
*     CAT1_NMCHK
*  Purpose:
*     Check whether a column, parameter or expression exists in a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_NMCHK (CI, NAME; EXIST; STATUS)
*  Description:
*     Check whether a column, parameter or expression of a given name
*     exists in a given catalogue.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     NAME  =  CHARACTER*(*) (Given)
*        Name to be checked.
*     EXIST  =  LOGICAL (Returned)
*        Flag indicating whether the given name corresponds to the
*        name of a column, parameter or expression in the catalogue.
*        It is coded as follows:
*        .TRUE.  -  the name was found in the catalogue,
*        .FALSE. -  the name was NOT found in the catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the 'name exists' flag.
*     Initialise the identifier count.
*     Do while (there are more identifiers)
*       Increment the number of identifiers.
*       If the parent of the current identifier is the target catalogue
*       then
*         If the identifier corresponds to a column, parameter or
*         expression then
*           Get the name corresponding to the identifier
*           If the name matches the target name then
*             Set the 'name exists' flag.
*             Set the termination flag.
*           end if
*         end if
*       end if
*       If the last identifier has been examined then
*         Set the termination flag.
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     16/12/99 (ACD): Original version.
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
     :  CI
      CHARACTER
     :  NAME*(*)
*  Arguments Returned:
      LOGICAL
     :  EXIST
*  Status:
      INTEGER STATUS        ! Global status
*  Local Variables:
      LOGICAL
     :  MORE     ! Flag: more identifiers to examine?
      INTEGER
     :  CIDELM   ! Current element in identifier arrays.
      CHARACTER
     :  CURNAM*(CAT__SZCMP) ! Name corresponding to current identifier.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the 'name exists' flag and the identifier count.

         EXIST = .FALSE.
         CIDELM = 0

*
*       Search through the identifier arrays.

         MORE = .TRUE.

         DO WHILE (MORE)
            CIDELM = CIDELM + 1

*
*          Check whether the parent of the current identifier matches
*          the given target catalogue.  If it does then check whether
*          the identifier corresponds to a column, parameter or expression.

            IF (IDPRN__CAT1(CIDELM) .EQ. CI) THEN
               IF (IDTYP__CAT1(CIDELM) .EQ. CAT__FITYP  .OR.
     :             IDTYP__CAT1(CIDELM) .EQ. CAT__QITYP  .OR.
     :             IDTYP__CAT1(CIDELM) .EQ. CAT__EITYP) THEN

*
*                The current identifier corresponds to a column,
*                parameter or expression in the target catalogue.
*                Now get its name.

                  CALL CAT_TIQAC (IDVAL__CAT1(CIDELM), 'NAME', CURNAM,
     :              STATUS)

*
*                Check whether the current name matches the given
*                target name.  If so then set the 'name exists' flag
*                and the termination flag.

                  IF (CURNAM .EQ. NAME) THEN
                     EXIST = .TRUE.
                     MORE = .FALSE.
                  END IF
               END IF
            END IF

*
*          If the last identifier has been examined then set the
*          termination flag.

            IF (CIDELM .GE. NIDS__CAT1) THEN
               MORE = .FALSE.
            END IF

         END DO

      END IF

      END
