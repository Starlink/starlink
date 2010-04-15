      SUBROUTINE CAP_PLTGR (GRPLST, GAI, STATUS)
*+
*  Name:
*     CAP_PLTGR
*  Purpose:
*     Plot all the rows of a  graphics attributes list.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PLTGR (GRPLST, GAI; STATUS)
*  Description:
*     Plot all the rows of a  graphics attributes list.
*  Arguments:
*     GRPLST  =  CHARACTER*(*) (Given)
*        Name of the graphics attributes list.
*     GAI  =  INTEGER (Given)
*        Identifier to the graphics attributes list.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the number of rows in the list.
*     Get identifiers for the plotting attributes.
*     If the symbol is a parameter then
*       Get its value.
*       Set the flag saying that this symbol has been used.
*     end if
*     Do while (there are more rows to be processed)
*       Increment to the next row.
*       Attempt to read in the next row.
*       If ok then
*         Attempt to get the basic plotting attributes.
*         If the symbol is undefined then
*           Adopt the default symbol
*           Set the 'default symbol used' flag.
*         end if
*         If the symbol is not 'omit the object' then
*           Get the size attributes as required.
*           Convert the R.A. and Dec. to standard coordinates.
*           If ok then
*             If the object lies inside the chart window then
*               Plot the symbol.
*             else
*               Increment the number of points outside the window.
*             end if
*           else
*             Increment the number of points for which standard coordinates
*             could not be computed.
*           end if
*         else
*           Set the 'rows omitted' flag.
*         end if
*       end if
*       If all the rows have been read then
*         Set the termination flag.
*       end if
*       If the status is bad then
*         Set the termination flag.
*       end if
*     end do
*     If any rows were omitted then
*       Report warning as appropriate.
*     end if
*     Set the common block variable defining the symbol used.
*     If the default symbol was used then
*       Increment to the next default symbol.
*     end if
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     20/8/96 (ACD): Original version.
*     3/6/97  (ACD): Changed the subroutine prefix from CIO to CAP.
*     6/6/97  (ACD): First stable version.
*     18/7/00 (ACD): Made more robust against 'illegal' values of the
*        parameter.  Such values can arise in ESO ACL format catalogues.
*        The parameter value is checked and if it is not an INTEGER
*        value the default symbol is adopted.
*     18/4/01 (ACD): Change the name of the symbol attribute from
*        'SYMBOL' to 'PSYMB'.  Also, ensure that the LABEL variable is
*        blank if no label is to be plotted.
*     19/4/01 (ACD): Corrected a bug in obtaining the LABEL column.
*     20/4/01 (ACD): Improved the checking for, and reporting of, points
*        which could not be plotted.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CIO_PAR'           ! CIO parametric constants.
      INCLUDE 'CHART_PAR'         ! CATCHART constants.
*  Global Variables:
      INCLUDE 'CHART_CMN'         ! CATCHART common block.
*  Arguments Given:
      CHARACTER
     :  GRPLST*(*)
      INTEGER
     :  GAI
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Variables:

*
*    Flags defining whether the various optional columns are present
*    or not.

      LOGICAL
     :  EXSYMB,    ! Symbol.
     :  EXCOLR,    ! Colour.
     :  EXSUNT,    ! Units.
     :  EXLABL,    ! Label.
     :  EXSIZ1,    ! First  size.
     :  EXSIZ2,    ! Second  "  .
     :  EXSIZ3,    ! Third   "  .
     :  EXSIZ4     ! Fourth  "  .

*
*    Identifiers for the various plotting attributes.

      INTEGER
     :  RAI,      ! Right Ascension.
     :  DECI,     ! Declination.
     :  SYMBI,    ! Symbol.
     :  COLI,     ! Colour.
     :  SUNI,     ! Units.
     :  LABLI,    ! Label.
     :  SIZ1I,    ! First  size.
     :  SIZ2I,    ! Second  "  .
     :  SIZ3I,    ! Third   "  .
     :  SIZ4I,    ! Fourth  "  .
     :  LABLQI    ! Parameter holding the name of the label column.

*
*    Values for the various plotting attributes.

      DOUBLE PRECISION RA        ! Right Ascension.
      DOUBLE PRECISION DEC       ! Declination.
      INTEGER          SYMBOL    ! Symbol.
      INTEGER          COLOUR    ! Colour.
      INTEGER          SUNITS    ! Units.
      CHARACTER        LABEL*(CAT__SZVAL)    ! Label.
      REAL             SIZE1     ! First  size.
      REAL             SIZE2     ! Second  "  .
      REAL             SIZE3     ! Third   "  .
      REAL             SIZE4     ! Fourth  "  .

      CHARACTER
     :  SYMBUF*(CAT__SZVAL),     ! Buffer for the SYMBOL parameter.
     :  LABCOL*(CAT__SZCMP)      ! Name of the column holding the labels.
      INTEGER
     :  LSTAT,    ! Local status.
     :  NBADSC,   ! No. of points for which unable to compute standard coords.
     :  NOUTWD,   ! No. of points which lie outside the chart window.
     :  ROWS,     ! Number of rows in the graphics attributes list.
     :  ROW,      ! Current row in the graphics attributes list.
     :  SYMTYP,   ! Type of SYMBOL component: column or parameter?
     :  SLASTT    ! SLA status.
      LOGICAL
     :  GOTSYM,   ! Flag; got symbol?
     :  MORE,     ! Flag; more rows to process?
     :  NULFLG,   ! Null value flag.
     :  ANYOMT,   ! Flag; were any rows omitted (that is, not plotted)?
     :  DEFUSE    ! Flag; was the default symbol used.
      DOUBLE PRECISION
     :  XI,       ! } Standard (tangent plane) coordinates.
     :  ETA       ! }
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Determine the number of rows in the list.

         CALL CAT_TROWS (GAI, ROWS, STATUS)

*
*       Get identifiers for the plotting attributes.  Tests are made
*       to determine which optional columns are present.

         EXSYMB = .TRUE.
         EXCOLR = .TRUE.
         EXSUNT = .TRUE.
         EXLABL = .TRUE.
         EXSIZ1 = .TRUE.
         EXSIZ2 = .TRUE.
         EXSIZ3 = .TRUE.
         EXSIZ4 = .TRUE.

         GOTSYM = .FALSE.

         CALL CAT_TIDNT (GAI, 'RA', RAI, STATUS)
         CALL CAT_TIDNT (GAI, 'DEC', DECI, STATUS)

         CALL CAT_TIDNT (GAI, 'PSYMB', SYMBI, STATUS)
         IF (STATUS .EQ. CAT__NOCMP) THEN
            CALL ERR_ANNUL (STATUS)
            EXSYMB = .FALSE.
            SYMBOL = CIO__SUNDF
         END IF

         CALL CAT_TIDNT (GAI, 'COLOUR', COLI, STATUS)
         IF (STATUS .EQ. CAT__NOCMP) THEN
            CALL ERR_ANNUL (STATUS)
            EXCOLR = .FALSE.
            COLOUR = CIO__CDEF
         END IF

         CALL CAT_TIDNT (GAI, 'SUNITS', SUNI, STATUS)
         IF (STATUS .EQ. CAT__NOCMP) THEN
            CALL ERR_ANNUL (STATUS)
            EXSUNT = .FALSE.
            SUNITS = CIO__UFRAC
         END IF

         CALL CAT_TIDNT (GAI, 'LABEL', LABLQI, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
            CALL CAT_TIQAC (LABLQI, 'VALUE', LABCOL, STATUS)
            IF (STATUS .EQ. SAI__OK) THEN
               IF (LABCOL .NE. ' ') THEN
                  CALL CAT_TIDNT (GAI, LABCOL, LABLI, STATUS)
                  IF (STATUS .NE. SAI__OK) THEN
                     CALL MSG_SETC ('LABCOL', LABCOL)
                     CALL ERR_REP ('CAP_PLTGR_NLB', 'Label column '/
     :                 /'^LABCOL does not exist in the catalogue.',
     :                 STATUS)
                     CALL ERR_FLUSH (STATUS)
                     STATUS = CAT__NOCMP
                  END IF
               ELSE
                  EXLABL = .FALSE.
               END IF
            END IF
         END IF
         IF (STATUS .EQ. CAT__NOCMP) THEN
            CALL ERR_ANNUL (STATUS)
            EXLABL = .FALSE.
            LABEL = ' '
         END IF

         CALL CAT_TIDNT (GAI, 'SIZE1', SIZ1I, STATUS)
         IF (STATUS .EQ. CAT__NOCMP) THEN
            CALL ERR_ANNUL (STATUS)
            EXSIZ1 = .FALSE.
            SIZE1 = 1.0E0 / (2.0E0 * SQRT(REAL(ROWS) ) )
            SIZE1 = MIN(SIZE1, 5.0E-2)
         END IF

         CALL CAT_TIDNT (GAI, 'SIZE2', SIZ2I, STATUS)
         IF (STATUS .EQ. CAT__NOCMP) THEN
            CALL ERR_ANNUL (STATUS)
            EXSIZ2 = .FALSE.
            SIZE2 = 0.0E0
         END IF

         CALL CAT_TIDNT (GAI, 'SIZE3', SIZ3I, STATUS)
         IF (STATUS .EQ. CAT__NOCMP) THEN
            CALL ERR_ANNUL (STATUS)
            EXSIZ3 = .FALSE.
            SIZE3 = 0.0E0
         END IF

         CALL CAT_TIDNT (GAI, 'SIZE4', SIZ4I, STATUS)
         IF (STATUS .EQ. CAT__NOCMP) THEN
            CALL ERR_ANNUL (STATUS)
            EXSIZ4 = .FALSE.
            SIZE4 = 0.0E0
         END IF

*
*       If the plotting symbol is a parameter then get its value
*       and set the flag indicating that this symbol is not
*       available for use as a default.

         IF (EXSYMB) THEN
            CALL CAT_TIDTP (SYMBI, SYMTYP, STATUS)
            IF (SYMTYP .EQ. CAT__QITYP) THEN
               GOTSYM = .TRUE.

               CALL CAT_EGT0C (SYMBI, SYMBUF, NULFLG, STATUS)

               LSTAT = SAI__OK
               CALL CHR_CTOI (SYMBUF, SYMBOL, LSTAT)

               IF (LSTAT .EQ. SAI__OK) THEN
                  IF (SYMBOL .GE. CIO__SOPCR  .AND.
     :                SYMBOL .LE. CIO__SAST) THEN
                     AVSYM__CIO(SYMBOL) = .FALSE.
                  END IF
               ELSE
                  SYMBOL = CIO__SUNDF
               END IF
            END IF
         ELSE
            SYMTYP = CAT__QITYP
         END IF

*
*       Process all the row in the list (or until an error occurs).

         ANYOMT = .FALSE.
         DEFUSE = .FALSE.

         NBADSC = 0
         NOUTWD = 0

         ROW = 0
         MORE = .TRUE.

         DO WHILE (MORE)

*
*          Increment to the next row, attempt to read in this row and
*          proceed if ok.

            ROW = ROW + 1
            CALL CAT_RGET (GAI, ROW, STATUS)
            IF (STATUS .EQ. SAI__OK) THEN

*
*             Attempt to get the basic plotting attributes.

               CALL CAT_EGT0D (RAI, RA, NULFLG, STATUS)
               CALL CAT_EGT0D (DECI, DEC, NULFLG, STATUS)

               IF (EXSYMB) THEN
                  IF (.NOT. GOTSYM) THEN
                     CALL CAT_EGT0I (SYMBI, SYMBOL, NULFLG, STATUS)
                  END IF
               END IF
               IF (EXCOLR) THEN
                  CALL CAT_EGT0I (COLI, COLOUR, NULFLG, STATUS)
               END IF
               IF (EXSUNT) THEN
                  CALL CAT_EGT0I (SUNI, SUNITS, NULFLG, STATUS)
               END IF
               IF (EXLABL) THEN
                  CALL CAT_EGT0C (LABLI, LABEL, NULFLG, STATUS)
                  IF (NULFLG) THEN
                     LABEL = ' '
                  END IF
               ELSE
                  LABEL = ' '
               END IF

*
*             If the symbol is undefined then adopt the current default
*             symbol and set the 'default symbol used' flag.

               IF (SYMBOL .EQ. CIO__SUNDF) THEN
                  SYMBOL = DSYMB__CIO
                  DEFUSE = .TRUE.
               END IF

*
*             Check whether the plotting symbol is 'omit'; that is,
*             proceed if the object is to be plotted.

               IF (SYMBOL .NE. CIO__SOMIT) THEN

*
*                Get the size attributes as required for the type
*                of symbol.

                  IF (SYMBOL .NE. CIO__SDOT) THEN
                     IF (EXSIZ1) THEN
                        CALL CAT_EGT0R (SIZ1I, SIZE1, NULFLG, STATUS)
                     END IF
                  END IF

                  IF (SYMBOL .GE. CIO__SAST) THEN
                     IF (EXSIZ2) THEN
                        CALL CAT_EGT0R (SIZ2I, SIZE2, NULFLG, STATUS)
                     END IF
                  END IF

                  IF (SYMBOL .GE. CIO__SXYER) THEN
                     IF (EXSIZ3) THEN
                        CALL CAT_EGT0R (SIZ3I, SIZE3, NULFLG, STATUS)
                     END IF
                  END IF

                  IF (SYMBOL .EQ. CIO__SXYER  .OR.
     :                SYMBOL .EQ. CIO__SLOZG) THEN
                     IF (EXSIZ4) THEN
                        CALL CAT_EGT0R (SIZ4I, SIZE4, NULFLG, STATUS)
                     END IF
                  END IF

*
*                Convert the Right Ascension and Declination into standard
*                coordinates and proceed if ok.

                  CALL SLA_DS2TP (RA, DEC, RA__CIO, DEC__CIO, XI, ETA,
     :              SLASTT)
                  IF (SLASTT .EQ. 0) THEN

*
*                   Check that the point lies inside the chart window
*                   (remember that the X axis is flipped).

                     IF (XI  .GT. CWXMX__CIO  .AND.
     :                   XI  .LT. CWXMN__CIO  .AND.
     :                   ETA .LT. CWYMX__CIO  .AND.
     :                   ETA .GT. CWYMN__CIO) THEN

*
*                      Plot the symbol.

                        CALL CAP_PLTGS (XI, ETA, SYMBOL, COLOUR,
     :                    SUNITS, LABEL, SIZE1, SIZE2, SIZE3, SIZE4,
     :                    STATUS)

                     ELSE
                        NOUTWD = NOUTWD + 1

                     END IF

                  ELSE
                     NBADSC = NBADSC + 1

                  END IF
               ELSE

*
*                The current plotting symbol is 'omit (that is, do not
*                plot) this point'; set the flag indicating that points
*                have been omitted from the plot.

                  ANYOMT = .TRUE.

               END IF
            END IF

*
*          If all the rows have been read then set the termination flag.

            IF (ROW .GE. ROWS) THEN
               MORE = .FALSE.
            END IF

*
*          If the status is bad then set the termination flag.

            IF (STATUS .NE. SAI__OK) THEN
               MORE = .FALSE.
            END IF
         END DO

*
*       If any rows were omitted from the plot then report warnings
*       as appropriate.

         IF (ANYOMT) THEN
            CALL MSG_SETC ('GRPLST', GRPLST)
            CALL CAP_WARN (.TRUE., ' ', 'Some objects in list '/
     :        /'^GRPLST were deliberately not plotted.', STATUS)
         END IF

         IF (NOUTWD .GT. 0) THEN
            CALL MSG_SETI ('NOUTWD', NOUTWD)
            CALL MSG_SETC ('GRPLST', GRPLST)

            IF (NOUTWD .GT. 1) THEN
               CALL CAP_WARN (.TRUE., ' ', '^NOUTWD objects in '/
     :           /'list ^GRPLST fell outside the chart window.',
     :           STATUS)
            ELSE
               CALL CAP_WARN (.TRUE., ' ', '^NOUTWD object in '/
     :           /'list ^GRPLST fell outside the chart window.',
     :           STATUS)
            END IF
         END IF

         IF (NBADSC .GT. 0) THEN
            CALL MSG_SETI ('NBADSC', NBADSC)
            CALL MSG_SETC ('GRPLST', GRPLST)

            IF (NBADSC .GT. 1) THEN
               CALL CAP_WARN (.TRUE., ' ', 'Cannot compute '/
     :           /'standard coords. for ^NBADSC objects in list '/
     :           /'^GRPLST.', STATUS)
            ELSE
               CALL CAP_WARN (.TRUE., ' ', 'Cannot compute '/
     :           /'standard coords. for ^NBADSC object in list '/
     :           /'^GRPLST.', STATUS)
            END IF
         END IF

*
*       Set the common block variable indicating which symbol has been
*       used.  If the component PSYMB is a column rather than a
*       parameter then it is assumed that several symbols have been
*       used and the common block variable is set to the value
*       indicating 'various'.

         IF (SYMTYP .NE. CAT__QITYP) THEN
            CSYMB__CIO = CIO__SVAR
         ELSE
            CSYMB__CIO = SYMBOL
         END IF

*
*       If the default symbol was used then increment to the next default
*       symbol available for use.

         IF (DEFUSE) THEN
            IF (DSYMB__CIO .NE. CIO__SOMIT) THEN
               MORE = .TRUE.

               DO WHILE (MORE)
                  DSYMB__CIO = DSYMB__CIO + 1

                  IF (DSYMB__CIO .LE. CIO__SAST) THEN
                     IF (AVSYM__CIO(DSYMB__CIO) ) THEN
                        MORE = .FALSE.
                        AVSYM__CIO(DSYMB__CIO) = .FALSE.
                     END IF
                  ELSE
                     MORE = .FALSE.
                     DSYMB__CIO = CIO__SOMIT
                  END IF
               END DO
            END IF
         END IF

      END IF

      END
