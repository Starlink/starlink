      SUBROUTINE CAT1_GETLU (LU, STATUS)
*+
*  Name:
*     CAT1_GETLU
*  Purpose:
*     Return a free FORTRAN unit number.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_GETLU (LU; STATUS)
*  Description:
*     Return a free FORTRAN unit number.
*  Arguments:
*     LU  =  INTEGER (returned)
*           free logical unit number
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Use INQUIRE to find a free FORTRAN unit number.
*
*     Note that this versions starts at logical unit number 7, to
*     avoid the standard associations of units 5 and 6.
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
*     BDK: B.D.Kelly (ROE)
*     ACD: A C Davenhall (Leicester)
*  History:
*     25/8/92 (BDK): Original version (based on an idea by
*                    Alan Chipperfield).
*     2/7/93  (ACD): StarBase version.
*     11/9/93 (ACD): First stable version.
*     23/1/94 (ACD): Modified error reporting.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Returned:
      INTEGER LU       ! FORTRAN unit number.
*  Status:
      INTEGER STATUS   ! Global status.
*  Local Variables:
      LOGICAL
     :  OPEN           ! Flag: indicating whether unit number is in use.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Loop to find a free Fortran logical unit number.

         LU = 6
         OPEN = .TRUE.

         DO WHILE (OPEN  .AND.  (LU .LT. 99 ) )
            LU = LU + 1
            INQUIRE (UNIT=LU, OPENED=OPEN)
         END DO

*
*       If no free logical unit number could be found the set the
*       status and report an error.

         IF (OPEN) THEN
            STATUS = CAT__NOLUN

            CALL CAT1_ERREP ('CAT1_GETLU_NLU', 'Error obtaining a '/
     :        /'free Fortran logical unit number.', STATUS)

         END IF

      END IF

      END
