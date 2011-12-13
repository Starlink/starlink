      SUBROUTINE CAT1_FRESP (CI, STATUS)
*+
*  Name:
*     CAT1_FRESP
*  Purpose:
*     Release any dynamic arrays associated a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_FRESP (CI; STATUS)
*  Description:
*     Release any dynamic arrays associated a catalogue.  Such
*     arrays are created if there are any indices or selections for
*     the catalogue.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For every element in the attributes array
*       If the attribute is a pointer then
*         If the attribute corresponds to the catalogue being closed then
*           Free the array.
*         end if
*       end if
*     end for
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
*     27/5/98 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
*  Global Variables:
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
*  Arguments Given:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  CURAT,   ! Current attribute.
     :  PTR      ! Current pointer.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Examine all the attributes and free work space associated
*       with selections or indices on the specified catalogue.

         DO CURAT = 1, NATT__CAT1
            IF (ATTNM__CAT1(CURAT) .EQ. 'PTR') THEN
               IF (IDPRN__CAT1(ATTID__CAT1(CURAT)) .EQ. CI) THEN
                  PTR = ATTVI__CAT1(CURAT)
                  CALL CAT1_FREAR (PTR, STATUS)

                  ATTVI__CAT1(CURAT) = 0

c                 print2000
c2000             format(1x, 'Work space released.')
               END IF
            END IF
         END DO

      END IF

      END
