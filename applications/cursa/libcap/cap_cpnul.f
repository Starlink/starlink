      SUBROUTINE CAP_CPNUL (NUMID, FIOUTE, FITYPE, STATUS)
*+
*  Name:
*     CAP_CPNUL
*  Purpose:
*     Copy null values to a set of fields in an output catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CPNUL (NUMID, FIOUTE, FITYPE; STATUS)
*  Description:
*     Copy null values to a set of fields in an output catalogue.
*     The routine works on the current row.
*  Arguments:
*     NUMID  =  INTEGER (Given)
*        Number of identifiers for the fields to be copied.
*     FIOUTE(NUMID)  =  INTEGER (Given)
*        Identifiers for the fields in the output catalogue.
*     FITYPE(NUMID)  =  INTEGER (Given)
*        Data types for the fields.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For every column
*       Check the data type of the column, and for the appropriate
*       data type:
*         Get the value for the current field from the input
*         catalogue.
*         Put the value to the current field in the output catalogue.
*       end case
*     end for
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     17/2/95 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
      INCLUDE 'CAT_PAR'     ! CAT symbolic constants.
      INCLUDE 'CAT_ERR'     ! Symbolic constants for CAT error codes.
*  Arguments Given:
      INTEGER
     :  NUMID,
     :  FIOUTE(NUMID),
     :  FITYPE(NUMID)
*  Status:
      INTEGER STATUS        ! Global status.
*  Local Constants:
      LOGICAL NULTRU  ! Flag indicating that the current field is null.
      PARAMETER (NULTRU = .TRUE.)
*  Local Variables:
      INTEGER
     :  CURID   ! Current identifier (inc. vector elements).

      BYTE       BVAL  ! Byte value.
      INTEGER*2  WVAL  ! Word value.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Set all the fields to null.

         DO CURID = 1, NUMID

*
*          Check the data type of the column, and for the appropriate
*          data type put a null value to the current field in the
*          output catalogue.

            IF (FITYPE(CURID) .EQ. CAT__TYPEB) THEN
               BVAL = 0
               CALL CAT_PUT0B (FIOUTE(CURID), BVAL, NULTRU,
     :           STATUS)

            ELSE IF (FITYPE(CURID) .EQ. CAT__TYPEW) THEN
               WVAL = 0
               CALL CAT_PUT0W (FIOUTE(CURID), WVAL, NULTRU,
     :           STATUS)

            ELSE IF (FITYPE(CURID) .EQ. CAT__TYPEI) THEN
               CALL CAT_PUT0I (FIOUTE(CURID), 0, NULTRU,
     :           STATUS)

            ELSE IF (FITYPE(CURID) .EQ. CAT__TYPER) THEN
               CALL CAT_PUT0R (FIOUTE(CURID), 0.0E0, NULTRU,
     :           STATUS)

            ELSE IF (FITYPE(CURID) .EQ. CAT__TYPED) THEN
               CALL CAT_PUT0D (FIOUTE(CURID), 0.0D0, NULTRU,
     :           STATUS)

            ELSE IF (FITYPE(CURID) .EQ. CAT__TYPEL) THEN
               CALL CAT_PUT0L (FIOUTE(CURID), .TRUE., NULTRU,
     :           STATUS)

            ELSE IF (FITYPE(CURID) .EQ. CAT__TYPEC) THEN
               CALL CAT_PUT0C (FIOUTE(CURID), ' ', NULTRU,
     :           STATUS)

            ELSE
               STATUS = CAT__INVDT

            END IF
         END DO

*
*       If the status is not ok then report an error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_CPNUL_ERR', 'CAP_CPNUL: Failure '/
     :        /'setting a set of fields to null.', STATUS)
         END IF

      END IF

      END
