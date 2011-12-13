      SUBROUTINE CAT1_CRTID (IDTYPE, IDPRN, ID, STATUS)
*+
*  Name:
*     CAT1_CRTID
*  Purpose:
*     Attempt to create a new identifier.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_CRTID (IDTYPE, IDPRN; ID; STATUS)
*  Description:
*     Attempt to create a new identifier and add it to the list of
*     identifiers.
*  Arguments:
*     IDTYPE  =  INTEGER (Given)
*        Type of identifier to be created: catalogue, column etc.
*     IDPRN  =  INTEGER (Given)
*        Identifier to the parent of the identifier that is to be
*        created.
*     ID  =  INTEGER (Returned)
*        New identifier created.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is space for a new identifier at the end of the
*     identifiers list then
*       increment the number of identifiers.
*       Set the new identifier to the end of the list
*     else (the array is full)
*       Attempt to find a released location within the list
*     end if
*     If a suitable location was found then
*       Copy the details of the identifier to the common block.
*     else
*       Set the return argument containing the value of the identifier
*       to the null identifier.
*       Set the status.
*       Report the error.
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
*     23/6/93 (ACD): Original version.
*     23/1/94 (ACD): Modified error reporting.
*     6/3/95  (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     7/3/95  (ACD): Added initialisation of array IDCEL__CAT1.
*     11/4/95 (ACD): Changed the name of the null identifier.
*     27/5/98 (ACD): Properly implemented IDATT__CAT1 for speeding up
*        attribute searches and allow for re-use of released identifiers.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'          ! Standard CAT constants
      INCLUDE 'CAT1_PAR'
      INCLUDE 'CAT_ERR'
*  Global Variables:
      INCLUDE 'CAT1_IDS_CMN'
      INCLUDE 'CAT1_ATTRB_CMN'
*  Arguments Given:
      INTEGER
     :  IDTYPE,
     :  IDPRN
*  Arguments Returned:
      INTEGER
     :  ID
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  LOOP     ! Loop index.
      LOGICAL
     :  FOUND,   ! Flag: has a free identifier been found?
     :  MORE     ! Flag; continue hunting for an identifier?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check whether there is space for another identifier.  First
*       check if there is free space at the end of the list.  If not
*       then hunt for free (ie. previously released) identifiers
*       within the list.

         FOUND = .FALSE.

         IF (NIDS__CAT1 .LT. CAT1__MXIDS) THEN

*
*          Increment the number of identifiers and set the new
*          idenitifier to be at the end of the list.

            NIDS__CAT1 = NIDS__CAT1 + 1
            ID = NIDS__CAT1
            FOUND = .TRUE.

         ELSE

*
*          Hunt for a free identifier with the list and adopt the
*          first one found.  Such identifiers are identified by being
*          set to the null identifier.

            MORE = .TRUE.
            LOOP = 0

            DO WHILE (MORE)
               LOOP = LOOP + 1

               IF (IDVAL__CAT1(LOOP) .EQ. CAT__NOID) THEN
                  ID = LOOP
                  FOUND = .TRUE.
                  MORE = .FALSE.
               END IF

               IF (LOOP .GE. CAT1__MXIDS) THEN
                  MORE = .FALSE.
               END IF
            END DO
         END IF

*
*       If an identifier was found then store the details.  Otherwise
*       Set the return value to null, set the status and report an
*       error.

         IF (FOUND) THEN
            IDVAL__CAT1(ID) = ID
            IDTYP__CAT1(ID) = IDTYPE
            IDPRN__CAT1(ID) = IDPRN
            IDCEL__CAT1(ID) = 0

            IDATT__CAT1(ID) = NATT__CAT1 + 1

         ELSE
            ID = CAT__NOID
            STATUS = CAT__MAXID

            CALL CAT1_ERREP ('CAT1_CRTID_MID', 'Failed to create '/
     :        /'a new identifier.', STATUS)
         END IF

      END IF

      END
