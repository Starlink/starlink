      SUBROUTINE CAT_TUNEG (CATPRM, VALUE, STATUS)
*+
*  Name:
*     CAT_TUNEG
*  Purpose:
*     Get a CAT tuning parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TUNEG (CATPRM; VALUE; STATUS)
*  Description:
*     Get a CAT tuning parameter. Note that all these parameters are of 
*     type CHARACTER.
*  Arguments:
*     CATPRM  =  CHARACTER*(*) (Given)
*        Name of the catalogue parameter which is to be got.  Currently
*        the only parameters supported are 'ANGLE_LIST' and 'QUIET'.
*     VALUE  =  CHARACTER*(*) (Given)
*        The current value of the specified parameter, expressed as a
*        CHARACTER string.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Take a copy of the parameter.
*     Force the copy into upper case.
*     Check for each of the permitted parameters in turn and return
*     a CHARACTER string determined by the current value.
*     If any error occurred then
*       Report it.
*     end if
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93  (ACD): Prologue only.
*     10/3/95 (ACD): Original version.
*     1/9/00  (ACD): Added tuning parameter QUIET.
*     4/4/01  (ACD): Corrected the prologue to mention parameter QUIET.
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
     :  CATPRM*(*)
*  Arguments Returned:
      CHARACTER
     :  VALUE*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  COPPRM*75,  ! Local copy of CATPRM.
     :  ERRTXT*75   ! Text for error message.
      INTEGER
     :  ERRLEN,     ! Length of ERRTXT (excl. trail. blanks).
     :  LCATPR      !   "    "  CATPRM ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Take a local copy (which may be modified) of the given parameter
*       and value and force it into upper case.

         COPPRM = CATPRM

         CALL CHR_UCASE (COPPRM)

*
*       Check each of the various supported parameters.

         IF (COPPRM .EQ. 'ANGLE_LIST') THEN
            IF (ANGCV__CAT1) THEN
               VALUE = 'SEXAGESIMAL'
            ELSE
               VALUE = 'RADIANS'
            END IF

         ELSE IF (COPPRM .EQ. 'QUIET') THEN
            IF (QUIET__CAT1) THEN
               VALUE = 'YES'
            ELSE
               VALUE = 'NO'
            END IF

         ELSE
            STATUS = CAT__ERROR

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_TUNES: unknown parameter '/
     :        /'specified: ', ERRTXT, ERRLEN)

            IF (CATPRM .NE. ' ') THEN
               LCATPR = CHR_LEN(CATPRM)
               CALL CHR_PUTC (CATPRM(1 : LCATPR), ERRTXT, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRTXT, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT_TUNES_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
