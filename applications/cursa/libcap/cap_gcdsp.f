      SUBROUTINE CAP_GCDSP (COLNME, STATUS)
*+
*  Name:
*     CAP_GCDSP
*  Purpose:
*     Show the display details for a column.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GCDSP (COLNME; STATUS)
*  Description:
*     Show the display details for a column.  The details listed are:
*     data type, units and external format.
*  Arguments:
*     COLNME  =  CHARACTER*(*) (Given)
*        Name of the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is an open catalogue then
*       Get an identifier for the column.
*       Determine the data type of the column.
*       Format the data type for display.
*       Get the units for the column.
*       Get the external display format for the column.
*       Output the formatted data type, units and external format.
*     else
*       Report warning: catalogue not open.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     20/3/95 (ACD): Original version.
*     12/4/95 (ACD): First stable version.
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
     :  COLNME*(*)
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      LOGICAL CHR_SIMLR
*  Local Variables:
      INTEGER
     :  DELPOS,   ! Position of delimiter for an expression.
     :  LOOP,     ! Loop index.
     :  COLID,    ! Column identifier.
     :  IDTYPE,   ! Data type for the column.
     :  DTYPOS,   ! Length of DTYPE (excl. trail. blanks).
     :  CSIZE     ! Character size of column.
      CHARACTER
     :  ENAME*(CAT__SZCMP),  ! Name of an expression.
     :  DTYPE*70,            ! Column: formatted data type.
     :  UNITS*(CAT__SZUNI),  !   "   : units.
     :  EXFMT*(CAT__SZEXF)   !   "   : external display format.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Get an identifier for the column or expression.

            DELPOS = INDEX(COLNME, '{')

            IF (DELPOS .LE. 0) THEN
               CALL CAT_TIDNT (CI__SGZ, COLNME, COLID, STATUS)

            ELSE
               ENAME = ' '
               ENAME = COLNME(1 : DELPOS-1)

               COLID = CAT__NOID

               DO LOOP = 1, CMPS__SGZ
                  IF (CHR_SIMLR(ENAME, CMPNM__SGZ(LOOP) ) ) THEN
                     COLID = CMPID__SGZ(LOOP)
                  END IF
               END DO

            END IF

*
*          Get the data type of the column and format it for display.

            CALL CAT_TIQAI (COLID, 'DTYPE', IDTYPE, STATUS)
            CALL CAT_TIQAI (COLID, 'CSIZE', CSIZE, STATUS)

C           print3000, colnme(1 : 15), colid, idtype, csize
C3000       format(1x, 'cap_gcdsp, colnme, colid, idtype, csize: ',
C    :        a15, i4, i4, i4)

            DTYPOS = 0
            DTYPE = ' '

            CALL CAT_TYFMT (IDTYPE, CSIZE, DTYPE, DTYPOS, STATUS)

*
*          Get the units and external display format.

            CALL CAT_TIQAC (COLID, 'UNITS', UNITS, STATUS)
            CALL CAT_TIQAC (COLID, 'EXFMT', EXFMT, STATUS)

*
*          Output the formatted data type, units and external format.

            CALL CAP_OUT (GUI__SGZ, ' ', DTYPE, STATUS)
            CALL CAP_OUT (GUI__SGZ, ' ', UNITS, STATUS)
            CALL CAP_OUT (GUI__SGZ, ' ', EXFMT, STATUS)

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETC ('COLNME', COLNME)
            CALL ERR_REP ('CAP_GCDSP_ERR', 'Error getting display '/
     :        /'details for column ^COLNME.', STATUS)
         END IF

      END IF

      END
