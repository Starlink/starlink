      SUBROUTINE CAP_GCEXP (EXPR, STATUS)
*+
*  Name:
*     CAP_GCEXP
*  Purpose:
*     Create a new selection.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GCEXP (EXPR; STATUS)
*  Description:
*     Create a new selection.
*  Arguments:
*     EXPR  =  CHARACTER*(*) (Given)
*        Expression which rows must satisfy in order to be selected.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the catalogue is open then
*       If more selections are permitted then
*         Attempt to get an identifier for the expression.
*         If ok then
*           Attempt to generate a selection comprising rows which
*           satisfy this expression.
*           If ok then
*             If some rows were selected then
*               Increment the number of selections.
*               Store the details of the new selection.
*               Adopt this selection as the new current selection.
*               Set the current row to the first row in this selection.
*               Report details of the selection.
*             else
*               Display message: no rows selected.
*             end if
*           else
*             Report error: failure generating selection.
*           end if
*         else
*           Report error: failure getting expression identifier.
*         end if
*       else
*         Display message: no more selections permitted.
*       end if
*       If the status corresponds to an invalid expression then
*         Flush the error.
*         Annul the status.
*       end if
*     else
*       Display warning: no catalogue open.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     28/4/94  (ACD): Original version.
*     23/10/94 (ACD): First stable version.
*     6/3/95   (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! StarGaze common block.
*  Arguments Given:
      CHARACTER
     :  EXPR*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Variables:
      INTEGER
     :  SIOLD,   ! Identifier for old ('base') selection.
     :  SINEW,   ! Identifier for new selection (created here).
     :  EI,      ! Expression identifier.
     :  SIR,     ! Identifier to 'selection' of rejected rows.
     :  NUMSEL,  ! Number of rows selected.
     :  NUMREJ   !   "    "   "   rejected.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Check that there is space for more selections.

            IF (SELS__SGZ .LT. SGZ__MXSEL) THEN

*
*             Attempt to get an identifier for the expression and
*             proceed if all is ok.

               CALL CAT_EIDNT (CI__SGZ, EXPR, EI, STATUS)

               IF (STATUS .EQ. SAI__OK) THEN

*
*                Attempt to generate a selection comprising rows which
*                satisfy this expression and proceed if ok.  Note that the
*                selection operates on the current selection.

                  SIOLD = SELID__SGZ(CSEL__SGZ)

                  CALL CAT_SELCT (SIOLD, EI, .FALSE., SINEW, NUMSEL,
     :              SIR, NUMREJ, STATUS)

                  IF (STATUS .EQ. SAI__OK) THEN

*
*                   Check that some rows were selected.

                     IF (NUMSEL .GT. 0) THEN

*
*                      Increment the number of selections.

                        SELS__SGZ = SELS__SGZ + 1

*
*                      Store the details of the new selection in the
*                      common block.

                        CRIT__SGZ(SELS__SGZ) = EXPR
                        SELID__SGZ(SELS__SGZ) = SINEW
                        SELBS__SGZ(SELS__SGZ) = CSEL__SGZ
                        SELRW__SGZ(SELS__SGZ) = NUMSEL

*
*                      Adopt this selection as the new current
*                      selection.

                        CSEL__SGZ = SELS__SGZ

*
*                      Set the current row to the first row in this
*                      selection.

                        CROW__SGZ = 1

*
*                      Report details of the selection.

                        CALL MSG_SETI ('SELS', SELS__SGZ)
                        CALL MSG_SETI ('NUMSEL', NUMSEL)

                        CALL CAP_INFO (GUI__SGZ, ' ', 'Selection '/
     :                    /'^SELS: ^NUMSEL rows selected.', STATUS)
                     ELSE

*
*                      No rows satisfied the criteria; report a message.

                        CALL CAP_INFO (GUI__SGZ, ' ', 'No rows '/
     :                    /'satisfied the criteria; a selection was '/
     :                    /'not created.', STATUS)
                     END IF
                  ELSE

*
*                   A failure occurred generating the selection; report
*                   an error.

                     CALL ERR_REP ('CAP_GCEXP_SLCT', 'Failure '/
     :                 /'selecting rows from the catalogue.', STATUS)
                  END IF

               ELSE

*                A failure occurred getting an identifier for the
*                expression.  In practice this error will usuall be
*                caused by a failure parsing the expression.

                  CALL ERR_REP ('CAP_GCEXP_EI', 'Failed to parse '/
     :              /'expression.', STATUS)
               END IF

            ELSE

*
*             The maximum permitted number of selections already exist;
*             report a message.

               CALL MSG_SETI ('SZSEL', SGZ__MXSEL)
               CALL CAP_WARN (GUI__SGZ, ' ', 'The maximum permitted '/
     :           /'^SZSEL selections already exist; ', STATUS)

               CALL CAP_WARN (GUI__SGZ, ' ', 'no more can be created.',
     :           STATUS)
            END IF

*
*          If the routine is exiting with a bad status caused by a
*          failure to parse the given expression then flush the error
*          and annul the status.

            IF (STATUS .EQ. CAT__INVEX) THEN
               CALL ERR_FLUSH (STATUS)
               CALL ERR_ANNUL (STATUS)
            END IF

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

      END IF

      END
