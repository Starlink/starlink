      SUBROUTINE CAP_ADPAR (CI, STATUS)
*+
*  Name:
*     CAP_ADPAR
*  Purpose:
*     Add one of more parameters to a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_ADPAR (CI; STATUS)
*  Description:
*     Add one of more parameters to a catalogue.  The details of the
*     parameters are obtained from the environment.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Identifier for the catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Do while (more parameters are to be added)
*       Get the name of the parameter.
*       If ok then
*         Set the error context.
*         Attempt to get an identifier for the parameter.
*         If an identifier is not obtained (ie. the parameter does not
*         exist) then
*           Annul the error.
*           Get the data type.
*           get the value.
*           Get the units.
*           Get the comments.
*           Convert the value to the appropriate data type.
*           Attempt to create the parameters.
*         else (an identifier obtained ok)
*           Report a message; a parameter or column with the given name
*           already exists.
*         end if
*         End the error context.
*       end if
*       Check if another parameter is to be added.
*       If the status is not ok then
*         Set the termination flag.
*       end if
*     end do
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     23/3/01 (ACD): Original version.
*     26/4/01 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
      INCLUDE 'CAT_PAR'     ! CAT symbolic constants.
      INCLUDE 'CAT_ERR'     ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS        ! Global status.
*  Local Variables:
      LOGICAL
     :  ADDPAR       ! Flag; add another new parameter?

*
*    The following variables represent the attributes of the current
*    parameter.

      INTEGER
     :  PID,         ! Identifier.
     :  PDTYPE,      ! Data type.
     :  PCSIZE       ! Size if a character string.
      CHARACTER
     :  PARTYP*8,              ! CHARACTER representation of data type.
     :  PNAME*(CAT__SZCMP),    ! Name.
     :  PUNITS*(CAT__SZUNI),   ! Units.
     :  PCOMM*(CAT__SZCOM),    ! Comments.
     :  PVALUE*(CAT__SZVAL)    ! Value (any data type, coded as CHAR).

*
*    Parameter value represented using the appropriate data type.

      REAL             PVALR
      DOUBLE PRECISION PVALD
      INTEGER          PVALI
      LOGICAL          PVALL
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Proceed whilst more new parameters are to be added.

         ADDPAR = .TRUE.

         DO WHILE (ADDPAR)

*
*          Get the name of the new parameter and proceed if all is ok.

            CALL PAR_GET0C ('PNAME', PNAME, STATUS)
            CALL PAR_CANCL ('PNAME', STATUS)

            CALL CHR_UCASE (PNAME)

            IF (STATUS .EQ. SAI__OK) THEN

*
*             Set the error context: mark the error stack, so that flushing
*             errors does not disturb any pre-existing errors in the error
*             stack.

               CALL ERR_MARK

*
*             Attempt to get an identifier for the parameter.

               CALL CAT_TIDNT (CI, PNAME, PID, STATUS)

*
*             Proceed if an identifier is not obtained (ie. the parameter
*             does not exist) then

               IF (PID .EQ. CAT__NOID  .AND.  STATUS .EQ. CAT__NOCMP)
     :           THEN

*
*                Annul the error.

                  CALL ERR_ANNUL (STATUS)

*
*                Get the details of the parameter: data type, value, units
*                and comments.

                  CALL PAR_CHOIC ('PARTYP', 'CHAR',
     :              'REAL,DOUBLE,INTEGER,LOGICAL,CHAR', .TRUE.,
     :              PARTYP, STATUS)
                  CALL PAR_CANCL ('PARTYP', STATUS)

                  IF (PARTYP .EQ. 'REAL') THEN
                     PDTYPE = CAT__TYPER

                  ELSE IF (PARTYP .EQ. 'DOUBLE') THEN
                     PDTYPE = CAT__TYPED

                  ELSE IF (PARTYP .EQ. 'INTEGER') THEN
                     PDTYPE = CAT__TYPEI

                  ELSE IF (PARTYP .EQ. 'LOGICAL') THEN
                     PDTYPE = CAT__TYPEL

                  ELSE
                     PDTYPE = CAT__TYPEC

                     CALL PAR_GET0I ('PCSIZE', PCSIZE, STATUS)
                     CALL PAR_CANCL ('PCSIZE', STATUS)

                     PCSIZE = MAX(1, PCSIZE)
                     PCSIZE = MIN(PCSIZE, CAT__SZVAL)

                  END IF

                  CALL PAR_GET0C ('PVALUE', PVALUE, STATUS)
                  CALL PAR_CANCL ('PVALUE', STATUS)

                  CALL PAR_GET0C ('PUNITS', PUNITS, STATUS)
                  CALL PAR_CANCL ('PUNITS', STATUS)

                  CALL PAR_GET0C ('PCOMM', PCOMM, STATUS)
                  CALL PAR_CANCL ('PCOMM', STATUS)

*
*                Convert the value to the appropriate data type and attempt
*                to create the parameter.

                  IF (PDTYPE .EQ. CAT__TYPER) THEN
                     CALL CHR_CTOR (PVALUE, PVALR, STATUS)
                     CALL CAT_PPTSR (CI, PNAME, PVALR, PCOMM, PID,
     :                 STATUS)

                  ELSE IF (PDTYPE .EQ. CAT__TYPED) THEN
                     CALL CHR_CTOD (PVALUE, PVALD, STATUS)
                     CALL CAT_PPTSD (CI, PNAME, PVALD, PCOMM, PID,
     :                 STATUS)

                  ELSE IF (PDTYPE .EQ. CAT__TYPEI) THEN
                     CALL CHR_CTOI (PVALUE, PVALI, STATUS)
                     CALL CAT_PPTSI (CI, PNAME, PVALI, PCOMM, PID,
     :                 STATUS)

                  ELSE IF (PDTYPE .EQ. CAT__TYPEL) THEN
                     CALL CHR_CTOL (PVALUE, PVALL, STATUS)
                     CALL CAT_PPTSL (CI, PNAME, PVALL, PCOMM, PID,
     :                 STATUS)

                  ELSE
                     CALL CAT_PPTSC (CI, PNAME, PVALUE, PCOMM, PID,
     :                 STATUS)
                     CALL CAT_TATTI (PID, 'CSIZE', PCSIZE, STATUS)

                  END IF

                  CALL CAT_TATTC (PID, 'UNITS', PUNITS, STATUS)

               ELSE

*
*                A parameter or column with the given name already exists:
*                report a message.  Note that no bad status has been raised.

                  CALL MSG_SETC ('PNAME', PNAME)
                  CALL MSG_OUT (' ', 'The catalogue already contains '/
     :              /'a column or parameter called ^PNAME.', STATUS)

                  CALL MSG_OUT (' ', 'Choose another name.', STATUS)

               END IF

*
*             Release the error stack.

               CALL ERR_RLSE
            END IF

*
*          Check if another parameter is to be added.

            CALL PAR_GET0L ('ADDPAR', ADDPAR, STATUS)
            CALL PAR_CANCL ('ADDPAR', STATUS)

*
*          Set the termination flag if all is not ok.

            IF (STATUS .NE. SAI__OK) THEN
               ADDPAR = .FALSE.
            END IF

         END DO

      END IF

      END
