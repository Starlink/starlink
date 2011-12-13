      SUBROUTINE CAT_TIDNT (CI, GNAME, GI, STATUS)
*+
*  Name:
*     CAT_TIDNT
*  Purpose:
*     Get an identifier for a named pre-existing component.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TIDNT (CI, GNAME; GI; STATUS)
*  Description:
*     Get an identifier for a named pre-existing component.  The
*     component may be of any type.
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
*     Attempt to get an existing identifier for the component.
*     If an identifier was not obtained then
*       Check whether the component could correspond to a vector
*       element.
*       If so then
*         Attempt to create a new identifier for the vector element.
*       end if
*     end if
*     If the identifier is null then
*       Set the return status.
*     end if
*     If an error occurred then
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
*     3/5/93  (ACD): Prologue only.
*     4/7/93  (ACD): First implementation.
*     27/7/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     2/2/94  (ACD): Re-written to handle vector elements.
*     11/4/95 (ACD): Changed the name of the null identifier.
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
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  LGNAME,    ! Length of GNAME  (excl. trail. blanks).
     :  ERRLEN     !   "    "  ERRMSG ( "  .   "  .   "   ).
      CHARACTER
     :  ERRMSG*75  ! Error message.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Attempt to get an existing identifier for the component.

         CALL CAT1_TIDNT (CI, GNAME, GI, STATUS)

*
*       If the attempt failed then check whether the name could
*       correspond to a vector element (that is, if it terminates with
*       a ']') and if so attempt to create an identifier for it.

*       Note that if CAT1_TIDNT could not get an identifier it
*       returns GI equal to the null identifier, not a bad status.

         IF (GI .EQ. CAT__NOID) THEN
            IF (GNAME .NE. ' ') THEN
               LGNAME = CHR_LEN (GNAME)

               IF (GNAME(LGNAME : LGNAME) .EQ. ']') THEN
                  CALL CAT1_CRTEL (CI, GNAME, GI, STATUS)

               ELSE
                  STATUS = CAT__NOCMP

               END IF
            ELSE
               STATUS = CAT__NOCMP

            END IF
         END IF

*
*       If the identifier is still null then set the return status.

         IF (GI .EQ. CAT__NOID) THEN
            STATUS = CAT__NOCMP
         END IF

*
*       If any error occurred the set the identifier to the null
*       identifier (it probably already had this value) and report the
*       error.

         IF (STATUS .NE. CAT__OK) THEN
            GI = CAT__NOID

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_TIDNT: error getting an identifier '/
     :        /'for component: ', ERRMSG, ERRLEN)

            IF (GNAME .NE. ' ') THEN
               LGNAME = CHR_LEN (GNAME)
               CALL CHR_PUTC (GNAME(1 : LGNAME), ERRMSG, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRMSG, ERRLEN)
            END IF

            CALL CAT1_ERREP ('CAT_TIDNT_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
