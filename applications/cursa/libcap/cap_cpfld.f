      SUBROUTINE CAP_CPFLD (NUMID, FIINE, FIOUTE, FITYPE, STATUS)
*+
*  Name:
*     CAP_CPFLD
*  Purpose:
*     Copy a set of fields from an input to an output catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CPFLD (NUMID, FIINE, FIOUTE, FITYPE; STATUS)
*  Description:
*     Copy a set of fields from an input to an output catalogue.
*     The routine works on the current rows in both catalogues.
*  Arguments:
*     NUMID  =  INTEGER (Given)
*        Number of identifiers for the fields to be copied.
*     FIINE(NUMID)  =  INTEGER (Given)
*        Identifiers for the fields in the input catalogue.
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
*     17/2/95 (ACD): Original version (based on CAP_CPTAB).
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
     :  FIINE(NUMID),
     :  FIOUTE(NUMID),
     :  FITYPE(NUMID)
*  Status:
      INTEGER STATUS        ! Global status.
*  Local Variables:
      INTEGER
     :  CURID   ! Current identifier (inc. vector elements).
      LOGICAL
     :  NULFLG  ! Flag; is the current field null?

*
*    The following values hold the value read (and written) for the
*    current field.  There are values for each of the data types
*    supported by SCAR/ADC.

      BYTE             VALUEB
      INTEGER*2        VALUEW
      INTEGER          VALUEI
      REAL             VALUER
      DOUBLE PRECISION VALUED
      LOGICAL          VALUEL
      CHARACTER        VALUEC*(CAT__SZVAL)
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Copy all the fields.

         DO CURID = 1, NUMID

*
*          Check the data type of the column, and for the appropriate
*          data type:
*          Get the value for the current field from the input catalogue.
*          Put the value to the current field in the output catalogue.

            IF (FITYPE(CURID) .EQ. CAT__TYPEB) THEN
               CALL CAT_EGT0B (FIINE(CURID), VALUEB, NULFLG,
     :           STATUS)
               CALL CAT_PUT0B (FIOUTE(CURID), VALUEB, NULFLG,
     :           STATUS)

            ELSE IF (FITYPE(CURID) .EQ. CAT__TYPEW) THEN
               CALL CAT_EGT0W (FIINE(CURID), VALUEW, NULFLG,
     :           STATUS)
               CALL CAT_PUT0W (FIOUTE(CURID), VALUEW, NULFLG,
     :           STATUS)

            ELSE IF (FITYPE(CURID) .EQ. CAT__TYPEI) THEN
               CALL CAT_EGT0I (FIINE(CURID), VALUEI, NULFLG,
     :           STATUS)
               CALL CAT_PUT0I (FIOUTE(CURID), VALUEI, NULFLG,
     :           STATUS)

            ELSE IF (FITYPE(CURID) .EQ. CAT__TYPER) THEN
               CALL CAT_EGT0R (FIINE(CURID), VALUER, NULFLG,
     :           STATUS)
               CALL CAT_PUT0R (FIOUTE(CURID), VALUER, NULFLG,
     :           STATUS)

            ELSE IF (FITYPE(CURID) .EQ. CAT__TYPED) THEN
               CALL CAT_EGT0D (FIINE(CURID), VALUED, NULFLG,
     :           STATUS)
               CALL CAT_PUT0D (FIOUTE(CURID), VALUED, NULFLG,
     :           STATUS)

            ELSE IF (FITYPE(CURID) .EQ. CAT__TYPEL) THEN
               CALL CAT_EGT0L (FIINE(CURID), VALUEL, NULFLG,
     :           STATUS)
               CALL CAT_PUT0L (FIOUTE(CURID), VALUEL, NULFLG,
     :           STATUS)

            ELSE IF (FITYPE(CURID) .EQ. CAT__TYPEC) THEN
               CALL CAT_EGT0C (FIINE(CURID), VALUEC, NULFLG,
     :           STATUS)
               CALL CAT_PUT0C (FIOUTE(CURID), VALUEC, NULFLG,
     :           STATUS)

            ELSE
               STATUS = CAT__INVDT

            END IF
         END DO

*
*       If the status is not ok then report an error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_CPFLD_ERR', 'CAP_CPFLD: Failure '/
     :        /'copying a set of fields.', STATUS)
         END IF

      END IF

      END
