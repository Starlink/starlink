      SUBROUTINE CAT5_GTCLD (CI, MAXCOL, ROWS, NUMCOL, FIA, FDTYPA,
     :  FCSIZA, FSCLFA, FSCALA, FZEROA, FFMTA, FANGLA, FNANGL, FPOSNA,
     :  FPTRA, FPTRNA, STATUS)
*+
*  Name:
*     CAT5_GTCLD
*  Purpose:
*     Get the details for all the columns in the description.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_GTCLD (CI, MAXCOL, ROWS; NUMCOL, FIA, FDTYPA, FCSIZA,
*       FSCLFA, FSCALA, FZEROA, FFMTA, FANGLA, FNANGL, FPOSNA, FPTRA,
*       FPTRNA; STATUS)
*  Description:
*     Get the details for all the columns in the description.
*
*     Array elements are treated as separate columns and have their
*     own details.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     MAXCOL  =  INTEGER (Given)
*        Maximum perimitted number of columns.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the small text list.
*     NUMCOL  =  INTEGER (Returned)
*        Total number of columns in the catalogue (treating vector
*        elements as separate columns).
*     FIA(MAXCOL)  =  INTEGER (Returned)
*        Column identifiers.
*     FDTYPA(MAXCOL)  =  INTEGER (Returned)
*        Data types of the columns.
*     FCSIZA(MAXCOL)  =  INTEGER (Returned)
*        Size of character columns.
*     FSCLFA(MAXCOL)  =  LOGICAL (Returned)
*        Flag indicating whether a scale factor and zero point are
*        to be applied to the column (.TRUE. if they are; othereise
*        .FALSE.).
*     FSCALA(MAXCOL)  =  DOUBLE PRECISION (Returned)
*        Scale factor for the column.
*     FZEROA(MAXCOL)  =  DOUBLE PRECISION (Returned)
*        Zero point for the column.
*     FFMTA(MAXCOL)  =  CHARACTER*(*) (Returned)
*        Format for the column.
*     FANGLA(MAXCOL)  =  INTEGER (Returned)
*        Flag indicating whether the column is an angle and if so
*        what its units are.
*     FNANGL(MAXCOL)  =  INTEGER (Returned)
*        If the column is an angle then the corresponding array element
*        contains the sequence number of the column amongst the columns
*        of angles.  Otherwise zero.
*     FPOSNA(MAXCOL)  =  INTEGER (Returned)
*        Position of the column in the table.
*     FPTRA(MAXCOL)  =  INTEGER (Returned)
*        Pointer to array to hold the column.
*     FPTRNA(MAXCOL)  =  INTEGER (Returned)
*        Pointer to array to hold the null value flags for the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the list of output columns.
*     For every input column
*       Get the attributes of the column.
*       If the column is a scalar then
*         If there is sufficient space then
*           Increment the number of output columns.
*           Copy the attributes to the output list.
*           Determine whether the column contains an angle.
*           Attempt to map workspace to hold the column.
*           Attempt to map workspace to hold the mull value flags for
*           the column.
*         else
*           Set the 'array full' flag.
*         end if
*       else the column is a vector
*         For every vector element
*           If there is sufficient space then
*             Attempt to get an identifier for the element.
*             If ok then
*               Increment the number of output columns.
*               Copy the attributes to the output list.
*               Determine whether the column contains an angle.
*               Attempt to map workspace to hold the column.
*               Attempt to map workspace to hold the mull value flags
*               for the column.
*             end if
*           else
*             Set the 'array full' flag.
*           end if
*         end for
*       end if
*     end for
*     If there was insufficient space for all the output columns then
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     11/7/96  (ACD): Original version.
*     30/7/96  (ACD): First stable version.
*     19/11/96 (ACD): Removed spurious deficiencies section claiming
*       that the routine will only handle free format tables (it will
*       handle both free and fixed format) and made string lengths
*       explicit in the checks for columns of angles.
*     24/1/97  (ACD): Fixed bug in computing positions of array elements
*       in the input record for fixed-format tables.
*     28/3/97  (ACD): Changed the definition of column and parameter
*        names to use the correct parametric contstant (CAT__SZCMP).
*     3/6/97   (ACD): Added options for minutes and seconds of arc
*        and time for TBLFMT.
*     4/8/98   (ACD): Added options to allow for complex in addition
*        to simple angles.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_DSDIR_CMN'    ! Description directives common block.
      INCLUDE 'CAT5_STL_CMN'      ! STL common block.
*  Arguments Given:
      INTEGER
     :  CI,
     :  MAXCOL,
     :  ROWS
*  Arguments Returned:
      INTEGER
     :  NUMCOL,
     :  FIA(MAXCOL),
     :  FDTYPA(MAXCOL),
     :  FCSIZA(MAXCOL),
     :  FANGLA(MAXCOL),
     :  FNANGL(MAXCOL),
     :  FPOSNA(MAXCOL),
     :  FPTRA(MAXCOL),
     :  FPTRNA(MAXCOL)
      LOGICAL
     :  FSCLFA(MAXCOL)
      DOUBLE PRECISION
     :  FSCALA(MAXCOL),
     :  FZEROA(MAXCOL)
      CHARACTER
     :  FFMTA(MAXCOL)*(*)
*  Status:
      INTEGERSTATUS             ! Global status
*  Exernal References:
      INTEGER CHR_LEN
*  Local Variables:
      LOGICAL
     :  ARFULL   ! Flag; is there space for another output column?
      INTEGER
     :  CURIN,   ! Count of current input column.
     :  CURELM,  ! Count of current vector element.
     :  CIOUT,   ! Catalogue identifier.
     :  FI,      ! Current column identifier.
     :  ENMPOS,  ! Current position in ENAME.
     :  FIE,     ! Current vector element idientifier.
     :  LTYPE,   ! Length of TYPE  (excl. trail. blanks).
     :  LFNAME,  !   "    "  FNAME ( "  .   "  .   "   ).
     :  FWIDTH,  ! Width of format specifier.
     :  FANGLE,  ! Angle (and angular units) flag.
     :  NANGLE   ! Sequence number of angle.
      CHARACTER
     :  ENAME*(CAT__SZCMP),  ! Name of the current vector element.
     :  TYPE*(CAT__SZTYP),   ! Character repn. of current data type.
     :  TBLFMT*(CAT__SZEXF)  ! Table format for the column.

*
*    Attributes for a single column.

      INTEGER
     :  FGENUS,  ! Genus attribute.
     :  FDTYPE,  ! Data type attribute.
     :  FCSIZE,  ! Character size attribute.
     :  FDIM,    ! Dimensionality attribute.
     :  FSIZE,   ! Size attribute.
     :  FNULL,   ! Null flag attribute.
     :  FORDER   ! Order attribute.
      CHARACTER
     :  FNAME*(CAT__SZCMP),  ! Name attribute.
     :  FEXP*(CAT__SZEXP),   ! Expression attribute.
     :  FXCEPT*(CAT__SZVAL), ! Exception value attribute.
     :  FUNIT*(CAT__SZUNI),  ! Units attribute.
     :  FXFMT*(CAT__SZEXF),  ! External format attribute.
     :  FCOMM*(CAT__SZCOM)   ! Comments attribute for the column.
      DOUBLE PRECISION
     :  FSCALE,  ! Scale factor attribute.
     :  FZERO,   ! Zero point attribute.
     :  FDATE    ! Modification date attribute.
      LOGICAL
     :  FPDISP   ! Preferential display flag attribute.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the list of output columns.

         NUMCOL = 0
         NANGLE = 0
         ARFULL = .FALSE.

*
*       Process every input column in the description common block.

         DO CURIN = 1, DCOLS__CAT1
C           print401, curin
C 401       format(1x, 'curin: ', I5)

*
*          Get all the attributes of the column.

            FI = DFID__CAT1(CURIN)
            CALL CAT_CINQ (FI, 1, CIOUT, FNAME, FGENUS, FEXP, FDTYPE,
     :        FCSIZE, FDIM, FSIZE, FNULL, FXCEPT, FSCALE, FZERO,
     :        FORDER, FUNIT, FXFMT, FPDISP, FCOMM, FDATE, STATUS)

*
*          Check whether the column is a scalar or a vector.

C           print402, fdim, cat__scalr
C 402       format(1x, 'fdim, cat__scalr: ', i5, i5)

            IF (FDIM .EQ. CAT__SCALR) THEN

*
*             The column is a scalar.  If there is space then increment
*             the number of output columns and add the detail.
*             Otherwise set the array full flag.

               IF (NUMCOL .LT. MAXCOL) THEN
                  NUMCOL = NUMCOL + 1

                  FIA(NUMCOL) = FI
                  FDTYPA(NUMCOL) = FDTYPE
                  FCSIZA(NUMCOL) = FCSIZE

                  IF (ABS(FSCALE - 1.0D0) .GT. CAT5__MNSCL  .OR.
     :                ABS(FZERO) .GT. CAT5__MNSCL) THEN
                     FSCLFA(NUMCOL) = .TRUE.
                  ELSE
                     FSCLFA(NUMCOL) = .FALSE.
                  END IF

                  FSCALA(NUMCOL) = FSCALE
                  FZEROA(NUMCOL) = FZERO
                  FPOSNA(NUMCOL) = DPOS__CAT1(CURIN)
                  FFMTA(NUMCOL) = DFMT__CAT1(CURIN)

*
*                Determine whether the column contains an angle.

                  TBLFMT = DFMT__CAT1(CURIN)

c                  call chr_clean (tblfmt)
                  CALL CHR_LDBLK (TBLFMT)
                  CALL CHR_UCASE (TBLFMT)

                  CALL CAT5_DCANG (FNAME, TBLFMT, NANGLE, FANGLE,
     :              STATUS)

                  FANGLA(NUMCOL) = FANGLE

                  IF (FANGLE .NE. CAT1__NANGL) THEN
                     FNANGL(NUMCOL) = NANGLE
                  ELSE
                     FNANGL(NUMCOL) = 0
                  END IF

*
*                Attempt to map workspace for the column and the
*                corresponding array of null value flags.

                  TYPE = ' '
                  LTYPE = 0

                  CALL CAT_TYFMT (FDTYPE, FCSIZE, TYPE, LTYPE, STATUS)
C                 print7000, rows, ltype, type, status
C7000             format(1x, 'rows, ltype, type, status: ',i5,
C    :              i5, 1x, a, i20)

                  CALL CAT1_CRTAR (ROWS, TYPE(1 : LTYPE),
     :              FPTRA(NUMCOL), STATUS)

                  CALL CAT1_CRTAR (ROWS, '_LOGICAL', FPTRNA(NUMCOL),
     :              STATUS)
               ELSE
                  ARFULL = .TRUE.

               END IF

            ELSE

*
*             The column is a vector.  Get the details for every
*             element if there is space.

               DO CURELM = 1, FSIZE
                  IF (NUMCOL .LT. MAXCOL) THEN
                     NUMCOL = NUMCOL + 1

                     ENAME = ' '
                     ENMPOS = 0

                     IF (FNAME .NE. ' ') THEN
                        LFNAME = CHR_LEN(FNAME)
                     ELSE
                        LFNAME = 1
                     END IF

                     CALL CHR_PUTC (FNAME(1 : LFNAME), ENAME, ENMPOS)
                     CALL CHR_PUTC ('[', ENAME, ENMPOS)
                     CALL CHR_PUTI (CURELM, ENAME, ENMPOS)
                     CALL CHR_PUTC (']', ENAME, ENMPOS)

                     CALL CAT_TIDNT (CI, ENAME, FIE, STATUS)

                     IF (STATUS .EQ. CAT__OK) THEN
                        FIA(NUMCOL) = FIE
                        FDTYPA(NUMCOL) = FDTYPE
                        FCSIZA(NUMCOL) = FCSIZE

                        IF (ABS(FSCALE - 1.0D0) .GT. CAT5__MNSCL  .OR.
     :                      ABS(FZERO) .GT. CAT5__MNSCL) THEN
                           FSCLFA(NUMCOL) = .TRUE.
                        ELSE
                           FSCLFA(NUMCOL) = .FALSE.
                        END IF

                        FSCALA(NUMCOL) = FSCALE
                        FZEROA(NUMCOL) = FZERO

                        IF (DPOSN__CAT1 .EQ. CAT1__PSCOL) THEN
                           FPOSNA(NUMCOL) = DPOS__CAT1(CURIN) +
     :                       CURELM - 1
                        ELSE
                           CALL CAT5_GLFMT (DFMT__CAT1(CURIN), FWIDTH,
     :                       STATUS)
                           FPOSNA(NUMCOL) = DPOS__CAT1(CURIN) +
     :                       (FWIDTH * (CURELM - 1) ) -1
                        END IF

                        FFMTA(NUMCOL) = DFMT__CAT1(CURIN)

*
*                      Determine whether the column contains an angle.

                        TBLFMT = DFMT__CAT1(CURIN)

                        CALL CHR_LDBLK (TBLFMT)
                        CALL CHR_UCASE (TBLFMT)

                        CALL CAT5_DCANG (FNAME, TBLFMT, NANGLE,
     :                    FANGLE, STATUS)

                        FANGLA(NUMCOL) = FANGLE

                        IF (FANGLE .NE. CAT1__NANGL) THEN
                           FNANGL(NUMCOL) = NANGLE
                        ELSE
                           FNANGL(NUMCOL) = 0
                        END IF

*
*                      Attempt to map workspace for the column and the
*                      corresponding array of null value flags.

                        TYPE = ' '
                        LTYPE = 0

                        CALL CAT_TYFMT (FDTYPE, FCSIZE, TYPE, LTYPE,
     :                    STATUS)

                        CALL CAT1_CRTAR (ROWS, TYPE(1 : LTYPE),
     :                    FPTRA(NUMCOL), STATUS)

                        CALL CAT1_CRTAR (ROWS, '_LOGICAL',
     :                    FPTRNA(NUMCOL), STATUS)
                     END IF

                  ELSE
                     ARFULL = .TRUE.

                  END IF

               END DO
            END IF

         END DO

*
*       If there was insufficient space for all the output columns
*       then set the status and report an error.

         IF (ARFULL) THEN
            STATUS = CAT__MAXID

            CALL CAT1_ERREP ('CAT5_GTCLD_MXID', 'Maximum number '/
     :        /'of columns exceeded.', STATUS)
         END IF

      END IF

      END
