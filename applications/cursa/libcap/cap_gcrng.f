      SUBROUTINE CAP_GCRNG (PNAME, MINRNG, MAXRNG, STATUS)
*+
*  Name:
*     CAP_GCRNG
*  Purpose:
*     Create a new range selection.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GCRNG (PNAME, MINRNG, MAXRNG; STATUS)
*  Description:
*     Create a new range selection.
*
*     This routine only works on sorted columns.
*  Arguments:
*     PNAME  =  CHARACTER*(*) (Given)
*        Name of the column for which the range of values is specified.
*     MINRNG  =  CHARACTER*(*) (Given)
*        Minimum permitted value in the range.
*     MAXRNG  =  CHARACTER*(*) (Given)
*        Maximum permitted value in the range.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the catalogue is open then
*       If more selections are permitted then
*         Attempt to get an identifier for the column.
*         If ok then
*           Get the data type of the column.
*           If the data type is 'CHARACTER' then
*             Select the rows in the range.
*           else if the data type is 'REAL' then
*             Convert the given range to data type 'REAL'.
*             Select the rows in the range.
*           else if the data type is 'DOUBLE PRECISION' then
*             Convert the given range to data type 'DOUBLE PRECISION'.
*             Select the rows in the range.
*           else if the data type is 'INTEGER', 'WORD' or 'BYTE' then
*             Convert the given range to data type 'INTEGER'.
*             Select the rows in the range.
*           else
*             Set the status: invalid data type.
*             Report an error.
*           end if
*           If all is ok then
*             If some rows were selected then
*               Increment the number of selections.
*               Store the details of the new selection.
*               Adopt this selection as the new current selection.
*               Set the current row to the first row in this selection.
*               Report details of the selection.
*             else
*               Display message: no rows selected.
*             end if
*           end if
*         end if
*       else
*         Display message: no more selections permitted.
*       end if
*     else
*       Display warning: no catalogue open.
*     end if
*     If an error occurred then
*       Report the error.
*     end if
*  Implementation Deficiencies:
*     The current version only works on sorted columns.  When indices
*     are added it should be made to work on indexed columns too.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     28/9/94  (ACD): Original version.
*     23/10/94 (ACD): First stable version.
*     6/3/95   (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     14/3/95  (ACD): Modified to accept ranges for angles specified as
*        sexagesimal values.
*     13/9/96  (ACD): Fixed a bug in reporting the error when the
*        data type is invalid.
*     19/9/96  (ACD): Changed wording of error message when there are
*        no rows in the specified range.
*     8/10/96  (ACD): Added extra argument for CAP_ANGDC.
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
     :  PNAME*(*),
     :  MINRNG*(*),
     :  MAXRNG*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  FI,      ! Column identifier.
     :  DTYPE,   ! Data type of the column.
     :  SIOLD,   ! Identifier for old ('base') selection.
     :  SINEW,   ! Identifier for new selection (created here).
     :  SIR,     ! Identifier to 'selection' of rejected rows.
     :  NUMSEL,  ! Number of rows selected.
     :  NUMREJ   !   "    "   "   rejected.
      CHARACTER
     :  BUFFER*(CAT__SZEXP),  ! Buffer for description of selection.
     :  ERRTXT*75             ! Error text.
      INTEGER
     :  LPNAME,  ! Length of PNAME  (excl. trail. blanks).
     :  BUFPOS,  !   "    "  BUFFER ( "  .   "  .   "   ).
     :  ERRLEN,  !   "    "  ERRTXT ( "  .   "  .   "   ).
     :  LMINRN,  !   "    "  MINRNG ( "  .   "  .   "   ).
     :  LMAXRN   !   "    "  MAXRNG ( "  .   "  .   "   ).
*
*    These variables hold the minimum and maximum extent of the range,
*    converted to various data types.

      REAL
     :  MINRNR,  ! Minimum of range.
     :  MAXRNR   ! Maximum "    "  .
      DOUBLE PRECISION
     :  MINRND,  ! Minimum of range.
     :  MAXRND   ! Maximum "    "  .
      INTEGER
     :  MINRNI,  ! Minimum of range.
     :  MAXRNI   ! Maximum "    "  .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Check that there is space for more selections.

            IF (SELS__SGZ .LT. SGZ__MXSEL) THEN

*
*             Attempt to get an identifier for the column and proceed
*             if it is ok.

               CALL CAT_TIDNT (CI__SGZ, PNAME, FI, STATUS)

               IF (STATUS .EQ. SAI__OK) THEN

*
*                Get the date type of the column.

                  CALL CAT_TIQAI (FI, 'DTYPE', DTYPE, STATUS)

*
*                Check for the various permitted data types and make a
*                range selection of the appropriate type.

                  SIOLD = SELID__SGZ(CSEL__SGZ)

                  IF (DTYPE .EQ. CAT__TYPEC) THEN
                     CALL CAT_SFNDC (SIOLD, FI, MINRNG, MAXRNG, .FALSE.,
     :                 SINEW, NUMSEL, SIR, NUMREJ, STATUS)

                  ELSE IF (DTYPE .EQ. CAT__TYPER) THEN
                     IF (INDEX(MINRNG, ':') .NE. 0) THEN
                        CALL CAP_ANGDC ('ANGLE', MINRNG, MINRND, STATUS)
                        MINRNR = REAL(MINRND)
                     ELSE
                        CALL CHR_CTOR (MINRNG, MINRNR, STATUS)
                     END IF

                     IF (INDEX(MAXRNG, ':') .NE. 0) THEN
                        CALL CAP_ANGDC ('ANGLE', MAXRNG, MAXRND, STATUS)
                        MAXRNR = REAL(MAXRND)
                     ELSE
                        CALL CHR_CTOR (MAXRNG, MAXRNR, STATUS)
                     END IF

C                    print5000, minrnr, maxrnr
C5000                format(1x, 'minrnr, maxrnr: ', 0pe12.3, 0pe12.3)

                     CALL CAT_SFNDR (SIOLD, FI, MINRNR, MAXRNR, .FALSE.,
     :                 SINEW, NUMSEL, SIR, NUMREJ, STATUS)

C                    print5001, sinew, numsel, status
C5001                format(1x, 'sinew, numsel, status: ',
C    :                 i5, i5, i20)

                  ELSE IF (DTYPE .EQ. CAT__TYPED) THEN
                     IF (INDEX(MINRNG, ':') .NE. 0) THEN
                        CALL CAP_ANGDC ('ANGLE', MINRNG, MINRND, STATUS)
                     ELSE
                        CALL CHR_CTOD (MINRNG, MINRND, STATUS)
                     END IF

                     IF (INDEX(MAXRNG, ':') .NE. 0) THEN
                        CALL CAP_ANGDC ('ANGLE', MAXRNG, MAXRND, STATUS)
                     ELSE
                        CALL CHR_CTOD (MAXRNG, MAXRND, STATUS)
                     END IF

                     CALL CAT_SFNDD (SIOLD, FI, MINRND, MAXRND, .FALSE.,
     :                 SINEW, NUMSEL, SIR, NUMREJ, STATUS)

                  ELSE IF (DTYPE .EQ. CAT__TYPEI  .OR.
     :              DTYPE .EQ. CAT__TYPEW  .OR.
     :              DTYPE .EQ. CAT__TYPEB) THEN
                     CALL CHR_CTOI (MINRNG, MINRNI, STATUS)
                     CALL CHR_CTOI (MAXRNG, MAXRNI, STATUS)

                     CALL CAT_SFNDI (SIOLD, FI, MINRNI, MAXRNI, .FALSE.,
     :                 SINEW, NUMSEL, SIR, NUMREJ, STATUS)

                  ELSE
                     STATUS = CAT__INVDT

                     ERRTXT = ' '
                     ERRLEN = 0

                     CALL CHR_PUTC ('Cannot perform a range selection '/
     :                 /'on column ', ERRTXT, ERRLEN)

                     IF (PNAME .NE. ' ') THEN
                        LPNAME = CHR_LEN(PNAME)
                        CALL CHR_PUTC (PNAME(1 : LPNAME), ERRTXT,
     :                    ERRLEN)
                     ELSE
                        CALL CHR_PUTC ('<unknown>', ERRTXT, ERRLEN)
                     END IF

                     CALL CHR_PUTC (' (', ERRTXT, ERRLEN)
                     CALL CAT_TYFMT (DTYPE, 0, ERRTXT, ERRLEN, STATUS)
                     CALL CHR_PUTC (').', ERRTXT, ERRLEN)

                     CALL ERR_REP ('CAP_GCRNG_IND',
     :                 ERRTXT(1 : ERRLEN), STATUS)
                     CALL ERR_REP ('CAP_GCRNG_IND', 'Invalid data '/
     :                 /'type.', STATUS)

                  END IF

*
*                Proceed if all is ok.

                  IF (STATUS .EQ. SAI__OK) THEN

*
*                   Check that some rows were selected.

                     IF (NUMSEL .GT. 0) THEN

*
*                      Increment the number of selections.

                        SELS__SGZ = SELS__SGZ + 1

*
*                      Assemble a textual description of the selection.
*                      Note that this description is intended to be read
*                      by humans, not interpreted by a machine.

                        BUFFER = ' '
                        BUFPOS = 0

                        IF (PNAME .NE. ' ') THEN
                           LPNAME = CHR_LEN(PNAME)
                           CALL CHR_PUTC (PNAME(1 : LPNAME), BUFFER,
     :                       BUFPOS)
                        ELSE
                           CALL CHR_PUTC ('<blank>', BUFFER, BUFPOS)
                        END IF

                        CALL CHR_PUTC (' range ', BUFFER, BUFPOS)

                        IF (MINRNG .NE. ' ') THEN
                           LMINRN = CHR_LEN(MINRNG)
                           CALL CHR_PUTC (MINRNG(1 : LMINRN), BUFFER,
     :                       BUFPOS)
                        ELSE
                           CALL CHR_PUTC ('<blank>', BUFFER, BUFPOS)
                        END IF

                        CALL CHR_PUTC (' to ', BUFFER, BUFPOS)

                        IF (MAXRNG .NE. ' ') THEN
                           LMAXRN = CHR_LEN(MAXRNG)
                           CALL CHR_PUTC (MAXRNG(1 : LMAXRN), BUFFER,
     :                       BUFPOS)
                        ELSE
                           CALL CHR_PUTC ('<blank>', BUFFER, BUFPOS)
                        END IF

*
*                      Store the details of the new selection in the
*                      common block.

                        CRIT__SGZ(SELS__SGZ) = BUFFER
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

                        CALL CAP_INFO (GUI__SGZ, ' ', 'Range '/
     :                    /'selection ^SELS: ^NUMSEL rows selected.',
     :                    STATUS)

                     ELSE

*
*                      No rows were found in the range; report a message.

                        CALL CAP_INFO (GUI__SGZ, ' ', 'No rows '/
     :                    /'in the specified range; a selection '/
     :                    /'was not created.', STATUS)

                     END IF
                  END IF
               END IF
            ELSE

*
*             The maximum permitted number of selections already exist;
*             report a message.

               CALL MSG_SETI ('MXSEL', SGZ__MXSEL)
               CALL CAP_WARN (GUI__SGZ, ' ', 'The maximum permitted '/
     :           /'^MXSEL selections already exist; ', STATUS)

               CALL CAP_WARN (GUI__SGZ, ' ', 'no more can be created.',
     :           STATUS)
            END IF

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_GCRNG_ERR', 'Error generating range '/
     :        /'selection.', STATUS)
         END IF

      END IF

      END
