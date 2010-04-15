      SUBROUTINE CAP_RPFMT (NAME, NUNITS, NXFMT, STATUS)
*+
*  Name:
*     CAP_RPFMT
*  Purpose:
*     Replace the units and external display format for a column or parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_RPFMT (NAME, NXFMT; STATUS)
*  Description:
*     Replace the units and external display format for a column or
*     parameter.  The existing external display format is overwritten
*     with a new one.
*
*     The units and external display format are only modified if the
*     new valies are non-blank.
*
*     Note that no attempt is made to check that the new external
*     display format is a valid format specifier.
*  Arguments:
*     NAME  =  CHARACTER*(*) (Given)
*        Name of the column or parameter whose external display format
*        is to be replaced.
*     NUNITS  =  CHARACTER*(*) (Given)
*        The new units.
*     NXFMT  =  CHARACTER*(*) (Given)
*        The new external display format.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is a catalogue open then
*       Attempt to get an identifier for the named component.
*       If non-blank then
*         Set the units to the new value.
*       end if
*       If non-blank then
*         Set the external format to the new value.
*       end if
*       Set the flag saying that the details of the set of components
*       have changed.
*     else
*       Report warning: no catalogue open.
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     10/8/94 (ACD): Original version.
*     31/3/95 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! StarGaze common block.
*  Arguments Given:
      CHARACTER
     :  NAME*(*),
     :  NUNITS*(*),
     :  NXFMT*(*)
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      LOGICAL CHR_SIMLR
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  DELPOS,  ! Position of delimiter for an expression.
     :  LENGTH,  ! Length of column or expression name.
     :  PI,      ! Identifier to the column or parameter.
     :  LOOP     ! Loop index.
      LOGICAL
     :  FOUND    ! Flag; has the given name been found.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Attempt to get an identifier for the named component.

            DELPOS = INDEX(NAME, '{')

            IF (DELPOS .LE. 0) THEN
               IF (NAME .NE. ' ') THEN
                  LENGTH = CHR_LEN(NAME)
               ELSE
                  LENGTH = 1
               END IF
            ELSE
               LENGTH = DELPOS - 1
            END IF

            FOUND = .FALSE.

            DO LOOP = 1, CMPS__SGZ
               IF (CHR_SIMLR(NAME(1 : LENGTH), CMPNM__SGZ(LOOP) ) ) THEN
                  FOUND = .TRUE.
                  PI = CMPID__SGZ(LOOP)
               END IF
            END DO

            IF (.NOT. FOUND) THEN
               CALL CAT_TIDNT (CI__SGZ, NAME(1 : LENGTH), PI, STATUS)
            END IF

*
*          Set the units to the new value if it is non-blank.

            IF (NUNITS .NE. ' ') THEN
               CALL CAT_TATTC (PI, 'UNITS', NUNITS, STATUS)
            END IF

*
*          Set the external format to the new value if it is non-blank.

            IF (NXFMT .NE. ' ') THEN
               CALL CAT_TATTC (PI, 'EXFMT', NXFMT, STATUS)
            END IF

*
*          Set the flag to say that the details of the set of components
*          have changed.

            CMPCG__SGZ = .TRUE.

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETC ('NAME', NAME(1 : LENGTH))
            CALL ERR_REP ('CAP_RPFMT_ERR', 'Failed to modify the '/
     :        /'units or external format for column ^NAME.', STATUS)
         END IF

      END IF

      END
