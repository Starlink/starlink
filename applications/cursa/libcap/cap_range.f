      SUBROUTINE CAP_RANGE (CI, PNAME, MINRNG, MAXRNG, REJCAT, SI,
     :  NUMSEL, SIR, NUMREJ, CRIT, STATUS)
*+
*  Name:
*     CAP_RANGE
*  Purpose:
*     Create a new range selection.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_RANGE (CI, PNAME, MINRNG, MAXRNG, REJCAT; SI, NUMSEL,
*       SIR, NUMREJ, CRIT; STATUS)
*  Description:
*     Create a new range selection.
*
*     This routine only works on sorted columns.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Identifier to the catalogue from which the selection is to be
*        created.  This identifier may correspond to either a genuine
*        catalogue, a selection or an index.
*     PNAME  =  CHARACTER*(*) (Given)
*        Name of the column for which the range of values is specified.
*     MINRNG  =  CHARACTER*(*) (Given)
*        Minimum permitted value in the range.
*     MAXRNG  =  CHARACTER*(*) (Given)
*        Maximum permitted value in the range.
*     REJCAT  =  LOGICAL (Given)
*        Flag indicating whether a second optional selection of
*        rejected entries is also to be created.  It is coded as
*        follows:
*        .TRUE.  -  create selection of rejected objects,
*        .FALSE. -  do not create selection of rejected objects,
*     SI  =  INTEGER (Returned)
*        Identifier to list of selected objects.
*     NUMSEL  =  INTEGER (Returned)
*        Number of selected objects.
*     SIR  =  INTEGER (Returned)
*        Identifier to list of rejected objects.  If no list of rejected
*        objects has been specified it is returned set to null
*        (CAT__NOID).
*     CRIT  =  CHARACTER*(*) (Returned)
*        String summarising the selection.  This string is intended
*        to be read by a human, not interpretted by a machine.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get an identifier for the column.
*     If ok then
*       Get the data type of the column.
*       If the data type is 'CHARACTER' then
*         Select the rows in the range.
*       else if the data type is 'REAL' then
*         Convert the given range to data type 'REAL'.
*         Select the rows in the range.
*       else if the data type is 'DOUBLE PRECISION' then
*         Convert the given range to data type 'DOUBLE PRECISION'.
*         Select the rows in the range.
*       else if the data type is 'INTEGER', 'WORD' or 'BYTE' then
*         Convert the given range to data type 'INTEGER'.
*         Select the rows in the range.
*       else
*         Set the status: invalid data type.
*         Report an error.
*       end if
*       If all is ok then
*         If some rows were selected then
*           Assemble a description of the selection.
*         end if
*       end if
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     13/9/96 (ACD): Original version (based on CAP_GCRNG).
*     8/10/96 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
*     <...>
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  PNAME*(*),
     :  MINRNG*(*),
     :  MAXRNG*(*)
      LOGICAL
     :  REJCAT
*  Arguments Returned:
      INTEGER
     :  SI,
     :  NUMSEL,
     :  SIR,
     :  NUMREJ
      CHARACTER
     :  CRIT*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  FI,      ! Column identifier.
     :  DTYPE    ! Data type of the column.
      CHARACTER
     :  ERRTXT*75             ! Error text.
      INTEGER
     :  LPNAME,  ! Length of PNAME  (excl. trail. blanks).
     :  LCRIT,   !   "    "  CRIT   ( "  .   "  .   "   ).
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
*       Attempt to get an identifier for the column and proceed
*       if it is ok.

         CALL CAT_TIDNT (CI, PNAME, FI, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN

*
*          Get the data type of the column.

            CALL CAT_TIQAI (FI, 'DTYPE', DTYPE, STATUS)

*
*          Check for the various permitted data types and make a
*          range selection of the appropriate type.

            IF (DTYPE .EQ. CAT__TYPEC) THEN
               CALL CAT_SFNDC (CI, FI, MINRNG, MAXRNG, REJCAT,
     :           SI, NUMSEL, SIR, NUMREJ, STATUS)

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

               CALL CAT_SFNDR (CI, FI, MINRNR, MAXRNR, REJCAT,
     :           SI, NUMSEL, SIR, NUMREJ, STATUS)

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

               CALL CAT_SFNDD (CI, FI, MINRND, MAXRND, REJCAT,
     :           SI, NUMSEL, SIR, NUMREJ, STATUS)

            ELSE IF (DTYPE .EQ. CAT__TYPEI  .OR.
     :        DTYPE .EQ. CAT__TYPEW  .OR.
     :        DTYPE .EQ. CAT__TYPEB) THEN
               CALL CHR_CTOI (MINRNG, MINRNI, STATUS)
               CALL CHR_CTOI (MAXRNG, MAXRNI, STATUS)

               CALL CAT_SFNDI (CI, FI, MINRNI, MAXRNI, REJCAT,
     :           SI, NUMSEL, SIR, NUMREJ, STATUS)

            ELSE
               STATUS = CAT__INVDT

               ERRTXT = ' '
               ERRLEN = 0

               CALL CHR_PUTC ('Cannot perform a range selection '/
     :           /'on column ', ERRTXT, ERRLEN)

               IF (PNAME .NE. ' ') THEN
                  LPNAME = CHR_LEN(PNAME)
                  CALL CHR_PUTC (PNAME(1 : LPNAME), ERRTXT, ERRLEN)
               ELSE
                  CALL CHR_PUTC ('<unknown>', ERRTXT, ERRLEN)
               END IF

               CALL CHR_PUTC (' (', ERRTXT, ERRLEN)
               CALL CAT_TYFMT (DTYPE, 0, ERRTXT, ERRLEN, STATUS)
               CALL CHR_PUTC (').', ERRTXT, ERRLEN)

               CALL ERR_REP ('CAP_RANGE_IND', ERRTXT(1 : ERRLEN),
     :           STATUS)
               CALL ERR_REP ('CAP_RANGE_IND', 'Invalid data type.',
     :           STATUS)

            END IF

*
*          Proceed if all is ok.

            IF (STATUS .EQ. SAI__OK) THEN

*
*             Check that some rows were selected.

               IF (NUMSEL .GT. 0) THEN

*
*                Assemble a textual description of the selection.
*                Note that this description is intended to be read
*                by humans, not interpreted by a machine.

                  CRIT = ' '
                  LCRIT = 0

                  IF (PNAME .NE. ' ') THEN
                     LPNAME = CHR_LEN(PNAME)
                     CALL CHR_PUTC (PNAME(1 : LPNAME), CRIT,
     :                 LCRIT)
                  ELSE
                     CALL CHR_PUTC ('<blank>', CRIT, LCRIT)
                  END IF

                  CALL CHR_PUTC (' range ', CRIT, LCRIT)

                  IF (MINRNG .NE. ' ') THEN
                     LMINRN = CHR_LEN(MINRNG)
                     CALL CHR_PUTC (MINRNG(1 : LMINRN), CRIT, LCRIT)
                  ELSE
                     CALL CHR_PUTC ('<blank>', CRIT, LCRIT)
                  END IF

                  CALL CHR_PUTC (' to ', CRIT, LCRIT)

                  IF (MAXRNG .NE. ' ') THEN
                     LMAXRN = CHR_LEN(MAXRNG)
                     CALL CHR_PUTC (MAXRNG(1 : LMAXRN), CRIT, LCRIT)
                  ELSE
                     CALL CHR_PUTC ('<blank>', CRIT, LCRIT)
                  END IF
               END IF
            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_RANGE_ERR', 'Error generating range '/
     :        /'selection.', STATUS)
         END IF

      END IF

      END
