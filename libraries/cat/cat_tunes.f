      SUBROUTINE CAT_TUNES (CATPRM, VALUE, STATUS)
*+
*  Name:
*     CAT_TUNES
*  Purpose:
*     Set a CAT tuning parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TUNES (CATPRM, VALUE; STATUS)
*  Description:
*     Set a CAT tuning parameter. Note that all these parameters are of
*     type CHARACTER.
*  Arguments:
*     CATPRM  =  CHARACTER*(*) (Given)
*        Name of the CAT tuning parameter which is to be set.  Currently
*        the tuning parameters supported are 'ANGLE_LIST' and 'QUIET'.
*     VALUE  =  CHARACTER*(*) (Given)
*        Value required for the catalogue parameter.
*
*        For parameter ANGLE_LIST the values may be as follows:
*          'RADIANS'     -  output angles in radians,
*          'SEXAGESIMAL' -  output angles in hours or degrees, formatted
*             as sexagesimal values.
*
*        For parameter QUIET the values may be as follows:
*          'YES' - do not issue informational messages (ie. be quiet),
*          'NO'  - issue informational messages (ie. not quiet).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Take a copy of the parameter.
*     Take a copy of the value.
*     Force the copies into upper case.
*     Check for each of the permitted parameters in turn and attempt
*     to set the internal parameter according to the value.
*     If any error occurred then
*       Report it.
*     end if
*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils
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
*     10/3/95 (ACD): Original version.
*     1/9/00  (ACD): Added tuning parameter QUIET.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_CTRL_CMN'     ! Flags to control CAT.
*  Arguments Given:
      CHARACTER
     :  CATPRM*(*),
     :  VALUE*(*)
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  COPPRM*75,  ! Local copy of CATPRM.
     :  COPVAL*75,  !   "    "   "  VALUE
     :  ERRTXT*75   ! Text for error message.
      LOGICAL
     :  BADPAR,     ! Flag: was a bad parameter input.
     :  BADVAL      !  "  :  "  "  "  value       "  .
      INTEGER
     :  ERRLEN,     ! Length of ERRTXT (excl. trail. blanks).
     :  LCATPR,     !   "    "  CATPRM ( "  .   "  .   "   ).
     :  LVALUE      !   "    "  VALUE  ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Take local copies (which may be modified) of the given parameter
*       and value and force them into upper case.

         COPPRM = CATPRM
         COPVAL = VALUE

         CALL CHR_UCASE (COPPRM)
         CALL CHR_UCASE (COPVAL)

*
*       Check each of the various supported parameters.

         BADPAR = .FALSE.
         BADVAL = .FALSE.

         IF (COPPRM .EQ. 'ANGLE_LIST') THEN
            IF (COPVAL(1 : 1) .EQ. 'R') THEN
               ANGCV__CAT1 = .FALSE.

            ELSE IF (COPVAL(1 : 1) .EQ. 'S') THEN
               ANGCV__CAT1 = .TRUE.

            ELSE
               STATUS = CAT__ERROR
               BADVAL = .TRUE.

            END IF

         ELSE IF (COPPRM .EQ. 'QUIET') THEN
            IF (COPVAL(1 : 1) .EQ. 'Y') THEN
               QUIET__CAT1 = .TRUE.

            ELSE IF (COPVAL(1 : 1) .EQ. 'N') THEN
               QUIET__CAT1 = .FALSE.

            ELSE
               STATUS = CAT__ERROR
               BADVAL = .TRUE.

            END IF

         ELSE
            STATUS = CAT__ERROR
            BADPAR = .TRUE.

         END IF

*
*       Report any error, assembling appropriate text depending on
*       whether the parameter or the value was bad.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            IF (BADPAR) THEN
               CALL CHR_PUTC ('CAT_TUNES: unknown parameter '/
     :           /'specified: ', ERRTXT, ERRLEN)

               IF (CATPRM .NE. ' ') THEN
                  LCATPR = CHR_LEN(CATPRM)
                  CALL CHR_PUTC (CATPRM(1 : LCATPR), ERRTXT, ERRLEN)
               ELSE
                  CALL CHR_PUTC ('<blank>', ERRTXT, ERRLEN)
               END IF

               CALL CHR_PUTC ('.', ERRTXT, ERRLEN)
            END IF

            IF (BADVAL) THEN
               CALL CHR_PUTC ('CAT_TUNES: parameter ', ERRTXT,
     :           ERRLEN)

               IF (CATPRM .NE. ' ') THEN
                  LCATPR = CHR_LEN(CATPRM)
                  CALL CHR_PUTC (CATPRM(1 : LCATPR), ERRTXT, ERRLEN)
               ELSE
                  CALL CHR_PUTC ('<blank>', ERRTXT, ERRLEN)
               END IF

               CALL CHR_PUTC (' has illegal value: ', ERRTXT,
     :           ERRLEN)

               IF (VALUE .NE. ' ') THEN
                  LVALUE = CHR_LEN(VALUE)
                  CALL CHR_PUTC (VALUE(1 : LVALUE), ERRTXT, ERRLEN)
               ELSE
                  CALL CHR_PUTC ('<blank>', ERRTXT, ERRLEN)
               END IF

               CALL CHR_PUTC ('.', ERRTXT, ERRLEN)
            END IF

            CALL CAT1_ERREP ('CAT_TUNES_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
