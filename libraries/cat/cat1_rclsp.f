      SUBROUTINE CAT1_RCLSP (CI, STATUS)
*+
*  Name:
*     CAT1_RCLSP
*  Purpose:
*     Reclaim space in common block arrays when a catalogue is closed.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_RCLSP (CI; STATUS)
*  Description:
*     Reclaim space in common block arrays when a catalogue is closed.
*     The common blocks where space is reclaimed are: ATTRB, IDS,
*     CATS.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For every element in the attributes array
*       If the element corresponds to the closing catalogue then
*         Shunt all the higher elements down by one.
*         Decrement the number of attributes.
*       end if
*     end for
*     Remake the identifier attributes array.
*     Set the values in the catalogue array to indicate the null
*     identifier.
*     For every element in the identifiers array
*       Set the values to indicate the null identifier.
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
*     28/5/98 (ACD): First stable version.
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
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
*  Arguments Given:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP,   ! Current attribute, identifier or catalogue.
     :  SHUNT,  ! Current element being shunted down.
     :  COUNT,  ! Current count of attributes.
     :  CIELM   ! Element in catalogues arrays for the catalogue.
*.

      IF (STATUS .EQ. CAT__OK) THEN

c        print2000, ci, ncats__cat1, nids__cat1, natt__cat1
c2000    format(1x, 'CAT1_RCLSP on entry... ' /
c    :     1x, 'ci, ncats__cat1, nids__cat1, natt__cat1: ',
c    :     i3, i5, i5, i5 / )

*
*       Examine every element in the attributes array.  If it
*       corresponds to the catalogue which is being closed then
*       shunt down all the higher elements and decrement the number
*       of attributes.  Note that the array is searched in reverse
*       order to reduce the number of shunts.

         COUNT = NATT__CAT1

         DO LOOP = NATT__CAT1, 1, -1
            IF (ATTID__CAT1(LOOP) .EQ. CI  .OR.
     :          IDPRN__CAT1(ATTID__CAT1(LOOP)) .EQ. CI) THEN
               DO SHUNT = LOOP+1, COUNT
                  ATTID__CAT1(SHUNT-1) = ATTID__CAT1(SHUNT)
                  ATTNM__CAT1(SHUNT-1) = ATTNM__CAT1(SHUNT)
                  ATTMU__CAT1(SHUNT-1) = ATTMU__CAT1(SHUNT)
                  ATTYP__CAT1(SHUNT-1) = ATTYP__CAT1(SHUNT)
                  ATTVV__CAT1(SHUNT-1) = ATTVV__CAT1(SHUNT)
                  ATTVB__CAT1(SHUNT-1) = ATTVB__CAT1(SHUNT)
                  ATTVU__CAT1(SHUNT-1) = ATTVU__CAT1(SHUNT)
                  ATTVW__CAT1(SHUNT-1) = ATTVW__CAT1(SHUNT)
                  ATTVI__CAT1(SHUNT-1) = ATTVI__CAT1(SHUNT)
                  ATTVR__CAT1(SHUNT-1) = ATTVR__CAT1(SHUNT)
                  ATTVD__CAT1(SHUNT-1) = ATTVD__CAT1(SHUNT)
                  ATTVL__CAT1(SHUNT-1) = ATTVL__CAT1(SHUNT)
                  ATTVC__CAT1(SHUNT-1) = ATTVC__CAT1(SHUNT)
               END DO

               COUNT = COUNT - 1

            END IF
         END DO

         NATT__CAT1 = COUNT

*
*       Remake the identifier attributes array, giving the start
*       position for each identifier in the attributes array.  Note
*       that the attributes array is scanned backwards so that the
*       final entry for each identifier corresponds to its first
*       attribute in the attributes arrays.

         DO LOOP = NATT__CAT1, 1, -1
            IDATT__CAT1(ATTID__CAT1(LOOP)) = LOOP
         END DO

*
*       Set the values in the catalogue array to indicate the null
*       identifier.

         CALL CAT1_CIELM (CI, CIELM, STATUS)

         CIDS__CAT1(CIELM) = CAT__NOID
         BKTYP__CAT1(CIELM) = 0
         MODE__CAT1(CIELM) = 0
         STATE__CAT1(CIELM) = 0
         EROW__CAT1(CIELM) = 0
         NROW__CAT1(CIELM) = 0
         NPCOL__CAT1(CIELM) = 0
         NVCOL__CAT1(CIELM) = 0
         NIND__CAT1(CIELM) = 0
         NPAR__CAT1(CIELM) = 0
         NSEL__CAT1(CIELM) = 0
         CROW__CAT1(CIELM) = 0
         FINSH__CAT1(CIELM) = .FALSE.

*
*       Examine every element in the identifiers array.  If it
*       corresponds to the catalogue which is being closed then
*       set the values to indicate the null identifier.  Note that
*       the values cannot be shunted because identifiers correspond
*       to elements in these arrays.

         DO LOOP = 1, NIDS__CAT1
            IF (IDVAL__CAT1(LOOP) .EQ. CI  .OR.
     :          IDPRN__CAT1(LOOP) .EQ. CI) THEN
               IDVAL__CAT1(LOOP) = CAT__NOID
               IDTYP__CAT1(LOOP) = 0
               IDATT__CAT1(LOOP) = 0
               IDPRN__CAT1(LOOP) = CAT__PRNUL
               IDCEL__CAT1(LOOP) = 0
            END IF
         END DO

c        print2001, ci, ncats__cat1, nids__cat1, natt__cat1
c2001    format(1x, 'CAT1_RCLSP on exit... '/
c    :     1x, 'ci, ncats__cat1, nids__cat1, natt__cat1: ',
c    :     i3, i5, i5, i5 / )

c         do loop = 1,  NIDS__CAT1
c            print3000, loop, IDVAL__CAT1(LOOP), IDTYP__CAT1(LOOP),
c     :        IDATT__CAT1(LOOP), IDPRN__CAT1(LOOP)
c 3000       format(3x, 'loop, idval, idtyp, idatt, idprn: ',
c     :        i5, i5, i5, i5, i5 )
c         end do

      END IF

      END
