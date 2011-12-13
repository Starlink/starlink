*     SUBROUTINE CAT1_EQCK<t> (VAL1, VAL2, EQUAL, STATUS)
*+
*  Name:
*     CAT1_EQCK<t>
*  Purpose:
*     Check two values for equality.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL
*  Description:
*     Check two values of the same data type for equality.  Note that
*     though this routine is generic explicit code is included for each
*     data type.  This procedure is adopted in order to avoid an
*     illegal use of the relational operators revealed by the port to
*     Linux.
*  Arguments:
*     VAL1  =  <type> (Given)
*        First value to be checked.
*     VAL2  =  <type> (Given)
*        Second value to be checked.
*     EQUAL  =  LOGICAL (Returned)
*        Flag indicating whether the two values are equal.  It is .TRUE.
*        if they are; otherwise it is .FALSE.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Check the two values for equality.
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
*     13/12/96 (ACD): Original version.
*  Bugs:
*     None known
*-

      SUBROUTINE CAT1_EQCKUB (VAL1, VAL2, EQUAL, STATUS)
      IMPLICIT NONE

      INCLUDE 'CAT_PAR'           ! External CAT constants.

      BYTE    VAL1, VAL2
      LOGICAL EQUAL
      INTEGER STATUS

      IF (STATUS .EQ. CAT__OK) THEN

         IF (VAL1 .EQ. VAL2) THEN
            EQUAL = .TRUE.
         ELSE
            EQUAL = .FALSE.
         END IF

      END IF

      END

      SUBROUTINE CAT1_EQCKB (VAL1, VAL2, EQUAL, STATUS)
      IMPLICIT NONE

      INCLUDE 'CAT_PAR'           ! External CAT constants.

      BYTE    VAL1, VAL2
      LOGICAL EQUAL
      INTEGER STATUS

      IF (STATUS .EQ. CAT__OK) THEN

         IF (VAL1 .EQ. VAL2) THEN
            EQUAL = .TRUE.
         ELSE
            EQUAL = .FALSE.
         END IF

      END IF

      END

      SUBROUTINE CAT1_EQCKUW (VAL1, VAL2, EQUAL, STATUS)
      IMPLICIT NONE

      INCLUDE 'CAT_PAR'           ! External CAT constants.

      INTEGER*2  VAL1, VAL2
      LOGICAL EQUAL
      INTEGER STATUS

      IF (STATUS .EQ. CAT__OK) THEN

         IF (VAL1 .EQ. VAL2) THEN
            EQUAL = .TRUE.
         ELSE
            EQUAL = .FALSE.
         END IF

      END IF

      END

      SUBROUTINE CAT1_EQCKW (VAL1, VAL2, EQUAL, STATUS)
      IMPLICIT NONE

      INCLUDE 'CAT_PAR'           ! External CAT constants.

      INTEGER*2  VAL1, VAL2
      LOGICAL EQUAL
      INTEGER STATUS

      IF (STATUS .EQ. CAT__OK) THEN

         IF (VAL1 .EQ. VAL2) THEN
            EQUAL = .TRUE.
         ELSE
            EQUAL = .FALSE.
         END IF

      END IF

      END

      SUBROUTINE CAT1_EQCKI (VAL1, VAL2, EQUAL, STATUS)
      IMPLICIT NONE

      INCLUDE 'CAT_PAR'           ! External CAT constants.

      INTEGER VAL1, VAL2
      LOGICAL EQUAL
      INTEGER STATUS

      IF (STATUS .EQ. CAT__OK) THEN

         IF (VAL1 .EQ. VAL2) THEN
            EQUAL = .TRUE.
         ELSE
            EQUAL = .FALSE.
         END IF

      END IF

      END

      SUBROUTINE CAT1_EQCKR (VAL1, VAL2, EQUAL, STATUS)
      IMPLICIT NONE

      INCLUDE 'CAT_PAR'           ! External CAT constants.

      REAL    VAL1, VAL2
      LOGICAL EQUAL
      INTEGER STATUS

      IF (STATUS .EQ. CAT__OK) THEN

         IF (VAL1 .EQ. VAL2) THEN
            EQUAL = .TRUE.
         ELSE
            EQUAL = .FALSE.
         END IF

      END IF

      END

      SUBROUTINE CAT1_EQCKD (VAL1, VAL2, EQUAL, STATUS)
      IMPLICIT NONE

      INCLUDE 'CAT_PAR'           ! External CAT constants.

      DOUBLE PRECISION  VAL1, VAL2
      LOGICAL EQUAL
      INTEGER STATUS

      IF (STATUS .EQ. CAT__OK) THEN

         IF (VAL1 .EQ. VAL2) THEN
            EQUAL = .TRUE.
         ELSE
            EQUAL = .FALSE.
         END IF

      END IF

      END

      SUBROUTINE CAT1_EQCKL (VAL1, VAL2, EQUAL, STATUS)
      IMPLICIT NONE

      INCLUDE 'CAT_PAR'           ! External CAT constants.

      LOGICAL VAL1, VAL2
      LOGICAL EQUAL
      INTEGER STATUS

      IF (STATUS .EQ. CAT__OK) THEN

         IF ((VAL1 .AND. VAL2)  .OR.
     :       ((.NOT. VAL1) .AND. (.NOT. VAL2)) ) THEN
            EQUAL = .TRUE.
         ELSE
            EQUAL = .FALSE.
         END IF

      END IF

      END

      SUBROUTINE CAT1_EQCKC (VAL1, VAL2, EQUAL, STATUS)
      IMPLICIT NONE

      INCLUDE 'CAT_PAR'           ! External CAT constants.

      CHARACTER*(*)  VAL1, VAL2
      LOGICAL EQUAL
      INTEGER STATUS

      IF (STATUS .EQ. CAT__OK) THEN

         IF (VAL1 .EQ. VAL2) THEN
            EQUAL = .TRUE.
         ELSE
            EQUAL = .FALSE.
         END IF

      END IF

      END
