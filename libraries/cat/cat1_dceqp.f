      SUBROUTINE CAT1_DCEQP (EQPSTR, EQPSYS, EQPVAL, STATUS)
*+
*  Name:
*     CAT1_DCEQP
*  Purpose:
*     Decode a string representation of an equinox or epoch.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DCEQP (EQPSTR, EQPSYS; EQPVAL; STATUS)
*  Description:
*     Decode a string representation of an equinox or epoch.  The time
*     system (Besellian or Julian) and value of the equinox or epoch are
*     returned.
*
*     The input string should be of the form: time system code, followed
*     without spaces by the value.  Some valid examples are: 'B1950',
*     'J2000', 'B1987', 'J1943'.  The two valid codes for the time
*     system are 'B' and 'J'.  These codes are optional, so '1950',
*     '2000', '1987' and '1943' are also valid values.  If the code is
*     omitted a default code is determined from the value according to
*     to the following scheme:
*       value <= 1984.0  - Besellian ('B') adopted.
*       value >  1984.0  - Julian ('J') adopted.
*  Arguments:
*     EQPSTR  =  CHARACTER*(*) (Given)
*        Given string containing the equinox or epoch.
*     EQPSYS  =  CHARACTER*(*) (Returned)
*        Time system of the equinox or epoch.  The permitted values are
*        'B' (Besellian) and 'J' (Julian).
*     EQPVAL  =  DOUBLE PRECISION (Returned)
*        The value of the equinox or epoch (in years).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the string is not completely blank then
*       Take a working copy (which may be modified).
*       Remove any leading blanks.
*       Force the string into upper case.
*       If the first character is not a digit then
*         If the first character is 'B' or 'J' then
*           Set the flag - found system.
*           Set the system to the value found.
*         else
*           Set the illegal input flag.
*         end if
*         Replace the first character with a blank.
*         Remove this leading blank.
*       end if
*       Attempt to extract a value from the string.
*       If ok then
*         If the system has not yet been found then
*           If the value is less then 1984.0 then
*             Set the time system to Besellian.
*           else
*             Set the time system to Julian.
*           end if
*         end if
*       else
*         Set the illegal input flag.
*       end if
*     else (completely blank input)
*       Set the illegal input flag.
*     end if
*     If the illegal value flag has been set then
*       Set the status.
*       Report an error.
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     14/10/99 (ACD): Original version (from CAP_DCEQP).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      CHARACTER
     :  EQPSTR*(*)
*  Arguments Returned:
      CHARACTER
     :  EQPSYS*(*)
      DOUBLE PRECISION
     :  EQPVAL
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      LOGICAL CHR_ISDIG
*  Local Variables:
      CHARACTER
     :  EQPWRK*20 ! Working copy of EQPSTR (will be modified).
      LOGICAL
     :  FNDSYS,   ! Flag: found time system?
     :  BADINP,   ! Flag: invalid value input?
     :  ISDIG     ! Flag: is first character of EQPWRK a digit?
      INTEGER
     :  LSTAT     ! Local status decoding a value from EQPWRK.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         FNDSYS = .FALSE.
         BADINP = .FALSE.

*
*       Check that the given string is not completely blank.

         IF (EQPSTR .NE. ' ') THEN

*
*          Take a working copy of the given string.  (This copy will be
*          modified.)

            EQPWRK = EQPSTR

*
*          Remove any leading blanks and convert the string to upper case.

            CALL CHR_LDBLK (EQPWRK)
            CALL CHR_UCASE (EQPWRK)

*
*          Check whether the first character is a digit.

            ISDIG = CHR_ISDIG(EQPWRK(1 : 1))
            IF (.NOT. ISDIG) THEN

*
*             Check whether the first character is either 'B' or 'J'
*             (which are the two permitted alternatives).  If so then
*             set the 'system found' flag and set the return argument
*             for the time system.  Otherwise set the invalid input
*             flag.

               IF (EQPWRK(1 : 1) .EQ. 'B'  .OR.  EQPWRK(1 : 1) .EQ. 'J')
     :           THEN
                  FNDSYS = .TRUE.
                  EQPSYS(1 : 1) = EQPWRK(1 : 1)
               ELSE
                  BADINP = .TRUE.
               END IF

*
*             Replace the first character (which has been found to be
*             non-numeric) with a blank and then remove this leading
*             blank.

               EQPWRK(1 : 1) = ' '
               CALL CHR_LDBLK (EQPWRK)
            END IF

*
*          Attempt to extract a numeric value from the remaining string.

            LSTAT = CAT__OK
            CALL CHR_CTOD (EQPWRK, EQPVAL, LSTAT)

            IF (LSTAT .EQ. CAT__OK) THEN

*
*             If the time system has not yet been found then use the
*             value to determine it; years before 1984 are assumed to
*             be in the Besellian system, those after in the Julian
*             system.

               IF (.NOT. FNDSYS) THEN
                  IF (EQPVAL .LT. 1.984D3) THEN
                     EQPSYS = 'B'
                  ELSE
                     EQPSYS = 'J'
                  END IF
               END IF

            ELSE
               BADINP = .TRUE.

            END IF

         ELSE
            BADINP = .TRUE.

         END IF

*
*       If the invalid input flag has been set (that is, the given
*       string was invalid and could not be decoded) then set the status,
*       set the return arguments and report an error.

         IF (BADINP) THEN
            STATUS = CAT__ERROR

            EQPSYS = ' '
            EQPVAL = 0.0D0

            IF (EQPSTR .NE. ' ') THEN
               CALL MSG_SETC ('EQPSTR', EQPSTR)
               CALL ERR_REP ('CAT1_DCEQP_ERR', 'CAT1_DCEQP invalid '/
     :           /'equinox or epoch: ^EQPSTR.', STATUS)
            ELSE
               CALL ERR_REP ('CAT1_DCEQP_BLK', 'CAT1_DCEQP error: '/
     :           /'blank string given for equinox or epoch.', STATUS)
            END IF
         END IF

      END IF

      END
