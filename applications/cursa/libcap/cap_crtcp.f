      SUBROUTINE CAP_CRTCP (CIIN, CIOUT, MXGRAT, NGRAT, FIGRAT, TYGRAT,
     :  STATUS)
*+
*  Name:
*     CAP_CRTCP
*  Purpose:
*     Create the columns and parameters to hold the graphics attributes.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CRTCP (CIIN, CIOUT, MXGRAT; NGRAT, FIGRAT, TYGRAT;
*       STATUS)
*  Description:
*     Create the columns and parameters to hold the graphics attributes.
*
*     Also, identifiers are obtained and stored internally for
*     expressions defining the clauses and SIZEn columns.
*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier for the input target list.
*     CIOUT  =  INTEGER (Given)
*        Identifier for the graphics attribute list.
*     MXGRAT  =  INTEGER (Given)
*        Maximum possible number of columns of graphics attributes.
*     NGRAT  =  INTEGER (Returned)
*        Number of columns of graphics attribute created.
*     FIGRAT  =  INTEGER (Returned)
*        Identifiers for the columns of graphics attributes.
*     TYGRAT  =  INTEGER (Returned)
*        The type (plotting symbol, colour, units etc.) of each column
*        of graphics attributes.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there are no blocks then
*       Create the symbol, colour and units as parameters.
*       For each SIZEn item
*         If it is not blank then
*           If it is a constant
*             Create as a parameter.
*           else
*             Create as a column.
*             Get identifier for expression.
*           end if
*         end if
*       end for
*     else
*       Set flag; symbol, colour, units, label are parameters.
*       Set flag; SIZEn columns do not exist.
*       For every IF block
*         For every clause in the current block
*           If there is a value for the symbol, colour, units or label
*           then
*             Set the flag saying it is a column.
*           end if
*           If there is a value for a SIZEn item then
*             Set the flag saying it exists.
*             Get identifier for expression.
*           end if
*           Get identifier for expression defining the clause.
*         end for
*       end for
*       Create the items as columns or parameters, as appropriate.
*     end if
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     12/8/96 (ACD): Original version.
*     13/8/96 (ACD): First stable version.
*     6/6/97  (ACD): Changed the subroutine prefix from CIO to CAP.
*     18/4/01 (ACD): Change the name of the symbol attribute from
*        'SYMBOL' to 'PSYMB'.  Also, ensure that the LABEL variable is
*        blank if no label is to be plotted.
*     19/4/01 (ACD): corrected bug in setting TYGRAT for symbol sizes
*        other than the first in the case where there are no IF blocks.
*        Also corrected the external format for REAL columns from E8.3
*        to E12.3.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'CIO_PAR'           ! CIO parametric constants.
*  Global Variables:
      INCLUDE 'CIO_CMN'           ! CIO common block.
*  Arguments Given:
      INTEGER
     :  CIIN,
     :  CIOUT,
     :  MXGRAT
*  Arguments Returned:
      INTEGER
     :  NGRAT,
     :  FIGRAT(MXGRAT),
     :  TYGRAT(MXGRAT)
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Variables:
      INTEGER
     :  QI,      ! Parameter identifier.
     :  FI,      ! Column        "     .
     :  LSTAT,   ! Local status.
     :  CURBLK,  ! Current IF block.
     :  CURCLS,  ! Current clause.
     :  NCLS     ! Number of clauses in the current IF block.
      REAL
     :  RVAL     ! REAL value decoded for current SIZEn item.
      LOGICAL
     :  CFSMB,   ! Flag; is the symbol a column?
     :  CFCOL,   !  "  ; "   "  colour "   "   ?
     :  CFUNT,   !  "  ; "   "  units  "   "   ?
     :  CFLBL,   !  "  ; "   "  label  "   "   ?
     :  ESIZ1,   ! Flag; does SIZE1 exist?
     :  ESIZ2,   !  "  ;  "   SIZE2   "  ?
     :  ESIZ3,   !  "  ;  "   SIZE3   "  ?
     :  ESIZ4    !  "  ;  "   SIZE4   "  ?
*.

      IF (STATUS .EQ. SAI__OK) THEN

         NGRAT = 0

*
*       Check whether there are any IF blocks.

         IF (NIFB__CIO .EQ. 0) THEN

*
*          There are no IF blocks.  Create the symbol, column and
*          units as parameters.  Create any SIZEn which is a constant
*          as a parameter; otherwise as a column.  In the latter case
*          obtain an identifier for the corresponding expression.

            CALL CAT_PPTSI (CIOUT, 'PSYMB', DSYMB__CIO,
     :        'Plotting symbol.', QI, STATUS)

            CALL CAT_PPTSI (CIOUT, 'COLOUR', DCOLR__CIO,
     :        'Plotting colour.', QI, STATUS)

            CALL CAT_PPTSI (CIOUT, 'SUNITS', DUNIT__CIO,
     :        'Plotting units code.', QI, STATUS)

            CALL CAT_PPTSC (CIOUT, 'LABEL', DLABL__CIO,
     :        'Plotting label.', QI, STATUS)

            IF (DSIZ1__CIO .NE. ' ') THEN
               LSTAT = SAI__OK
               CALL CHR_CTOR (DSIZ1__CIO, RVAL, LSTAT)

               IF (LSTAT .EQ. SAI__OK) THEN
                  CALL CAT_PPTSR (CIOUT, 'SIZE1', RVAL,
     :              'Plotting symbol size.', QI, STATUS)
               ELSE
                  CALL CAT_CNEWS (CIOUT, 'SIZE1', CAT__TYPER, 1,
     :              ' ', 'E12.3', 'Plotting symbol size.', FI, STATUS)

                  NGRAT = NGRAT + 1
                  FIGRAT(NGRAT) = FI
                  TYGRAT(NGRAT) = CIO__TYSZ1

                  CALL CAT_EIDNT (CIIN, DSIZ1__CIO, DEIS1__CIO, STATUS)
               END IF
            END IF

            IF (DSIZ2__CIO .NE. ' ') THEN
               LSTAT = SAI__OK
               CALL CHR_CTOR (DSIZ2__CIO, RVAL, LSTAT)

               IF (LSTAT .EQ. SAI__OK) THEN
                  CALL CAT_PPTSR (CIOUT, 'SIZE2', RVAL,
     :              'Second plotting symbol size.', QI, STATUS)
               ELSE
                  CALL CAT_CNEWS (CIOUT, 'SIZE2', CAT__TYPER, 1,
     :              ' ', 'E12.3', 'Second plotting symbol size.', FI,
     :              STATUS)

                  NGRAT = NGRAT + 1
                  FIGRAT(NGRAT) = FI
                  TYGRAT(NGRAT) = CIO__TYSZ2

                  CALL CAT_EIDNT (CIIN, DSIZ2__CIO, DEIS2__CIO, STATUS)
               END IF
            END IF

            IF (DSIZ3__CIO .NE. ' ') THEN
               LSTAT = SAI__OK
               CALL CHR_CTOR (DSIZ3__CIO, RVAL, LSTAT)

               IF (LSTAT .EQ. SAI__OK) THEN
                  CALL CAT_PPTSR (CIOUT, 'SIZE3', RVAL,
     :              'Third plotting symbol size.', QI, STATUS)
               ELSE
                  CALL CAT_CNEWS (CIOUT, 'SIZE3', CAT__TYPER, 1,
     :              ' ', 'E12.3', 'Third plotting symbol size.', FI,
     :              STATUS)

                  NGRAT = NGRAT + 1
                  FIGRAT(NGRAT) = FI
                  TYGRAT(NGRAT) = CIO__TYSZ3

                  CALL CAT_EIDNT (CIIN, DSIZ3__CIO, DEIS3__CIO, STATUS)
               END IF
            END IF

            IF (DSIZ4__CIO .NE. ' ') THEN
               LSTAT = SAI__OK
               CALL CHR_CTOR (DSIZ4__CIO, RVAL, LSTAT)

               IF (LSTAT .EQ. SAI__OK) THEN
                  CALL CAT_PPTSR (CIOUT, 'SIZE4', RVAL,
     :              'Fourth plotting symbol size.', QI, STATUS)
               ELSE
                  CALL CAT_CNEWS (CIOUT, 'SIZE4', CAT__TYPER, 1,
     :              ' ', 'E12.3', 'Fourth plotting symbol size.', FI,
     :              STATUS)

                  NGRAT = NGRAT + 1
                  FIGRAT(NGRAT) = FI
                  TYGRAT(NGRAT) = CIO__TYSZ4

                  CALL CAT_EIDNT (CIIN, DSIZ4__CIO, DEIS4__CIO, STATUS)
               END IF
            END IF

         ELSE

*
*          One of more IF blocks are present.  Examine every clause of
*          every IF block to determine which items can be parameters and
*          which must be columns.  Also get identifiers for the
*          expressions defining the SIZEn columns.

            CFSMB = .FALSE.
            CFCOL = .FALSE.
            CFUNT = .FALSE.
            CFLBL = .FALSE.

            IF (DSIZ1__CIO .NE. ' ') THEN
               ESIZ1 = .TRUE.
               CALL CAT_EIDNT (CIIN, DSIZ1__CIO, DEIS1__CIO, STATUS)
            ELSE
               ESIZ1 = .FALSE.
            END IF

            IF (DSIZ2__CIO .NE. ' ') THEN
               ESIZ2 = .TRUE.
               CALL CAT_EIDNT (CIIN, DSIZ2__CIO, DEIS2__CIO, STATUS)
            ELSE
               ESIZ2 = .FALSE.
            END IF

            IF (DSIZ3__CIO .NE. ' ') THEN
               ESIZ3 = .TRUE.
               CALL CAT_EIDNT (CIIN, DSIZ3__CIO, DEIS3__CIO, STATUS)
            ELSE
               ESIZ3 = .FALSE.
            END IF

            IF (DSIZ4__CIO .NE. ' ') THEN
               ESIZ4 = .TRUE.
               CALL CAT_EIDNT (CIIN, DSIZ4__CIO, DEIS4__CIO, STATUS)
            ELSE
               ESIZ4 = .FALSE.
            END IF

            DO CURBLK = 1, NIFB__CIO
               NCLS = NCLS__CIO(CURBLK)

               DO CURCLS = 1, NCLS

                  IF (SYMB__CIO(CURBLK, CURCLS) .NE. CIO__NULL) THEN
                     CFSMB = .TRUE.
                  END IF

                  IF (COLR__CIO(CURBLK, CURCLS) .NE. CIO__NULL) THEN
                     CFCOL = .TRUE.
                  END IF

                  IF (UNIT__CIO(CURBLK, CURCLS) .NE. CIO__NULL) THEN
                     CFUNT = .TRUE.
                  END IF

                  IF (LABL__CIO(CURBLK, CURCLS) .NE. ' ') THEN
                     CFLBL = .TRUE.
                  END IF

                  IF (SIZ1__CIO(CURBLK, CURCLS) .NE. ' ') THEN
                     ESIZ1 = .TRUE.
                     CALL CAT_EIDNT (CIIN, SIZ1__CIO(CURBLK, CURCLS),
     :                 EIS1__CIO(CURBLK, CURCLS), STATUS)
                  END IF

                  IF (SIZ2__CIO(CURBLK, CURCLS) .NE. ' ') THEN
                     ESIZ2 = .TRUE.
                     CALL CAT_EIDNT (CIIN, SIZ2__CIO(CURBLK, CURCLS),
     :                 EIS2__CIO(CURBLK, CURCLS), STATUS)
                  END IF

                  IF (SIZ3__CIO(CURBLK, CURCLS) .NE. ' ') THEN
                     ESIZ3 = .TRUE.
                     CALL CAT_EIDNT (CIIN, SIZ3__CIO(CURBLK, CURCLS),
     :                 EIS3__CIO(CURBLK, CURCLS), STATUS)
                  END IF

                  IF (SIZ4__CIO(CURBLK, CURCLS) .NE. ' ') THEN
                     ESIZ4 = .TRUE.
                     CALL CAT_EIDNT (CIIN, SIZ4__CIO(CURBLK, CURCLS),
     :                 EIS4__CIO(CURBLK, CURCLS), STATUS)
                  END IF

*
*                If the clause is not a default 'ELSE' (which has no
*                condition) then get an identifier for the defining
*                expression.

                  IF (CRIT__CIO(CURBLK, CURCLS) .NE. '<default>') THEN
                     CALL CAT_EIDNT (CIIN, CRIT__CIO(CURBLK, CURCLS),
     :                 EICT__CIO(CURBLK, CURCLS), STATUS)
                  ELSE
                     EICT__CIO(CURBLK, CURCLS) = CIO__NULL
                  END IF

               END DO
            END DO

*
*          Create the items as columns or parameters, as appropriate.

            IF (.NOT. CFSMB) THEN
               CALL CAT_PPTSI (CIOUT, 'PSYMB', DSYMB__CIO,
     :           'Plotting symbol.', QI, STATUS)
            ELSE
               CALL CAT_CNEWS (CIOUT, 'PSYMB', CAT__TYPEI, 1, ' ',
     :           'I4', 'Plotting symbol.', FI, STATUS)

               NGRAT = NGRAT + 1
               FIGRAT(NGRAT) = FI
               TYGRAT(NGRAT) = CIO__TYSMB
            END IF

            IF (.NOT. CFCOL) THEN
               CALL CAT_PPTSI (CIOUT, 'COLOUR', DCOLR__CIO,
     :           'Plotting colour.', QI, STATUS)
            ELSE
               CALL CAT_CNEWS (CIOUT, 'COLOUR', CAT__TYPEI, 1, ' ',
     :           'I4', 'Plotting colour.', FI, STATUS)

               NGRAT = NGRAT + 1
               FIGRAT(NGRAT) = FI
               TYGRAT(NGRAT) = CIO__TYCOL
            END IF

            IF (.NOT. CFUNT) THEN
               CALL CAT_PPTSI (CIOUT, 'SUNITS', DUNIT__CIO,
     :           'Plotting units code.', QI, STATUS)
            ELSE
               CALL CAT_CNEWS (CIOUT, 'SUNITS', CAT__TYPEI, 1, ' ',
     :           'I4', 'Plotting units code.', FI, STATUS)

               NGRAT = NGRAT + 1
               FIGRAT(NGRAT) = FI
               TYGRAT(NGRAT) = CIO__TYUNT
            END IF

            IF (.NOT. CFLBL) THEN
               CALL CAT_PPTSC (CIOUT, 'LABEL', DLABL__CIO,
     :           'Plotting label.', QI, STATUS)
            ELSE
               CALL CAT_CNEWS (CIOUT, 'LABEL', CAT__TYPEC, CAT__SZCMP,
     :           ' ', 'A15', 'Plotting label.', FI, STATUS)

               NGRAT = NGRAT + 1
               FIGRAT(NGRAT) = FI
               TYGRAT(NGRAT) = CIO__TYLBL
            END IF

            IF (ESIZ1) THEN
               CALL CAT_CNEWS (CIOUT, 'SIZE1', CAT__TYPER, 1, ' ',
     :           'E12.3', 'Plotting symbol size.', FI, STATUS)

               NGRAT = NGRAT + 1
               FIGRAT(NGRAT) = FI
               TYGRAT(NGRAT) = CIO__TYSZ1
            END IF

            IF (ESIZ2) THEN
               CALL CAT_CNEWS (CIOUT, 'SIZE2', CAT__TYPER, 1, ' ',
     :           'E12.3', 'Second plotting symbol size.', FI, STATUS)

               NGRAT = NGRAT + 1
               FIGRAT(NGRAT) = FI
               TYGRAT(NGRAT) = CIO__TYSZ2
            END IF

            IF (ESIZ3) THEN
               CALL CAT_CNEWS (CIOUT, 'SIZE3', CAT__TYPER, 1, ' ',
     :           'E12.3', 'Third plotting symbol size.', FI, STATUS)

               NGRAT = NGRAT + 1
               FIGRAT(NGRAT) = FI
               TYGRAT(NGRAT) = CIO__TYSZ3
            END IF

            IF (ESIZ4) THEN
               CALL CAT_CNEWS (CIOUT, 'SIZE4', CAT__TYPER, 1, ' ',
     :           'E12.3', 'Fourth plotting symbol size.', FI, STATUS)

               NGRAT = NGRAT + 1
               FIGRAT(NGRAT) = FI
               TYGRAT(NGRAT) = CIO__TYSZ4
            END IF

         END IF

      END IF

      END
