      SUBROUTINE CAT6_FINCR (CI, STATUS)
*+
*  Name:
*     CAT6_FINCR
*  Purpose:
*     Finish the creation of a tab-separated table (TST).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_FINCR (CI; STATUS)
*  Description:
*     Finish the creation of a tab-separated table (TST).
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Get the array element for the catalogue.
*     If ok then
*       While there are more columns to be processed.
*         Attempt to get an identifier for the next column.
*         If ok then
*           Get the attributes for the column.
*           Generate the external format if it has not already been set.
*           Compute the position of the column in the table.
*           If the column is a scalar then
*             Map the work space array to hold the columns.
*             Map the work space array to hold the null value flags.
*             Set the null value flags for all the rows.
*             Save the common block details.
*           else (the column is a vector)
*             For every element
*               Generate an identifier for the element.
*               Map the work space array to hold the columns.
*               Map the work space array to hold the null value flags.
*               Set the null value flags for all the rows.
*               Save the common block details.
*             end for
*           end if
*         else
*           Set the termination flag.
*         end if
*       end do
*       Set the 'finished creation of catalogue' flag.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*     Copyright (C) 2009 Science and Technology Facilities Council
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
*     PWD: Peter W. Draper (Durham)
*  History:
*     16/6/99 (ACD): Original version (from CAT5_FINCR).
*     16/6/99 (ACD): First stable version.
*     28/8/09 (PWD): Don't use 'D' formats. TST are also read by C-based
*                    libraries which only understand 'E'.
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
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT6_TST_CMN'      ! Tab-separated table common block.
      INCLUDE 'CNF_PAR'           ! For CNF_PVAL function
*  Arguments Given:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS    ! Global status.
*  Exernal References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  ROWS,    ! Number of rows in the catalogue.
     :  TSUNIT,  ! Fortran unit no. for accessing the catalogue file.
     :  COLS,    ! Number of columns in the catalogue.
     :  CIELM,   ! Array element for the catalogue.
     :  FI,      ! Identifier for the current column.
     :  CIOUT,   ! Identifier to parent catalogue.
     :  SFXFMT,  ! Size of external format for character column.
     :  LFXFMT,  ! Length of FXFMT (excl. trail. blanks).
     :  PRVDIM,  ! Number of elements in the previous column.
     :  POSN     ! Position of the current column in the table.
      INTEGER
     :  PTR,     ! Pointer to array for current column.
     :  PTRN,    ! Pointer to null flag array for current column.
     :  LTYPE,   ! Length of TYPE  (excl. trail. blanks).
     :  LFNAME,  !   "    "  FNAME ( "  .   "  .   "   ).
     :  CURELM,  ! Count of current vector element.
     :  FIE,     ! Current vector element idientifier.
     :  ENMPOS   ! Current position in ENAME.
      CHARACTER
     :  TYPE*(CAT__SZTYP),   ! Character repn. of current data type.
     :  ENAME*(CAT__SZCMP)   ! Name of the current vector element.
      LOGICAL
     :  MORE     ! Flag; more columns to be processed?

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

C        print5000
C5000    format(1x, 'entering CAT6_FINCR......')

*
*       Attempt to get the array element corresponding to the catalogue
*       and proceed if ok.

         CALL CAT1_CIELM (CI, CIELM, STATUS)
         IF (STATUS .EQ. CAT__OK) THEN

*
*          Sequentially process all the columns.

            IF (EROW__CAT1(CIELM) .GT. 0) THEN
               ROWS = EROW__CAT1(CIELM)
            ELSE
               ROWS = CAT6__MXROW
               EROW__CAT1(CIELM) = CAT6__MXROW
            END IF

            PRVDIM = 1
            TSUNIT = TSUNT__CAT6(CIELM)
            COLS = 0
            POSN = 0

            MORE = .TRUE.

            DO WHILE (MORE)

*
*             Attempt to get an identifier for the next column and
*             proceed if ok.

               COLS = COLS + 1
C              print5001, cols
c5001          format(1x, 'cols: ', i5 )
               CALL CAT_TNDNT (CI, CAT__FITYP, COLS, FI, STATUS)
C              print5002, fi
C5002          format(5x, 'fi: ', i5 )
               IF (STATUS .EQ. CAT__OK  .AND.  FI .NE. CAT__NOID) THEN

*
*                Get the attributes for the column.

                  CALL CAT_CINQ (FI, 1, CIOUT, FNAME, FGENUS, FEXP,
     :              FDTYPE, FCSIZE, FDIM, FSIZE, FNULL, FXCEPT,
     :              FSCALE, FZERO, FORDER, FUNIT, FXFMT, FPDISP, FCOMM,
     :              FDATE, STATUS)

C                 print5003
C5003             format(1x, 'after CINQ')

*
*                Generate the external format if it has not already been
*                set.  Note that the DOUBLE PRECISION format should be
*                able represent angles to an accuracy of 0.01 second of arc.

                  IF (FXFMT .EQ. ' ') THEN
                     IF (FDTYPE .EQ. CAT__TYPEB) THEN
                        FXFMT = 'I4'
                     ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                        FXFMT = 'I6'
                     ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                        FXFMT = 'I6'
                     ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                        FXFMT = 'E12.3'
                     ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                        FXFMT = 'E19.10'
                     ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                        FXFMT = 'L5'
                     ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                        SFXFMT = FCSIZE + 2

                        LFXFMT = 0
                        FXFMT = ' '

                        CALL CHR_PUTC ('A', FXFMT, LFXFMT)
                        CALL CHR_PUTI (SFXFMT, FXFMT, LFXFMT)
                     END IF

                     LFXFMT = CHR_LEN(FXFMT)
                     CALL CAT_TATTC (FI, 'EXFMT', FXFMT(1 : LFXFMT),
     :                 STATUS)
                  END IF

*
*                Compute the position of the column in the table.

                  POSN = POSN + PRVDIM

*
*                Set the dimensionality of the 'previous column',
*                ready for processing the next column.

                  IF (FDIM .EQ. CAT__SCALR) THEN
                     PRVDIM = 1
                  ELSE
                     PRVDIM = FSIZE
                  END IF

*
*                Check whether the column is a scalar or vector.

                  IF (FDIM .EQ. CAT__SCALR) THEN

*
*                   The column is a scalar; map the work space array to
*                   hold the column and the null value flags.

                     TYPE = ' '
                     LTYPE = 0

                     CALL CAT_TYFMT (FDTYPE, FCSIZE, TYPE, LTYPE,
     :                 STATUS)
                     CALL CAT1_CRTAR (ROWS, TYPE(1 : LTYPE), PTR,
     :                 STATUS)

                     CALL CAT1_CRTAR (ROWS, '_LOGICAL', PTRN, STATUS)

*
*                   Set the null value flags for all the rows.

                     CALL CAT6_STNUL (.TRUE., ROWS,
     :                                %VAL(CNF_PVAL(PTRN)), STATUS)

*
*                   Save the common block details.

                     IF (FI .GT. 0  .AND.  FI .LE. NIDS__CAT1) THEN
                        FDTYP__CAT6(FI) = FDTYPE
                        FCSIZ__CAT6(FI) = FCSIZE
                        FPTR__CAT6(FI) = PTR
                        FPTRN__CAT6(FI) = PTRN
                     ELSE
                        STATUS = CAT__INVID

                        CALL CAT1_ERREP ('CAT6_FINCR_MID',
     :                    'Unknown column indentifier.', STATUS)
                     END IF

                  ELSE

*
*                   The column is a vector; for every element map
*                   workspace for the column and null values.

                     DO CURELM = 1, FSIZE
                        TYPE = ' '
                        LTYPE = 0

                        CALL CAT_TYFMT (FDTYPE, FCSIZE, TYPE, LTYPE,
     :                    STATUS)
                        CALL CAT1_CRTAR (ROWS, TYPE(1 : LTYPE), PTR,
     :                    STATUS)

                        CALL CAT1_CRTAR (ROWS, '_LOGICAL', PTRN, STATUS)

*
*                      Set the null value flags for all the rows.

                        CALL CAT6_STNUL (.TRUE., ROWS,
     :                                   %VAL(CNF_PVAL(PTRN)),
     :                    STATUS)

*
*                      Generate an identifier for the element.

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

*
*                      Save the common block details.

                        IF (FIE .GT. 0  .AND.  FIE .LE. NIDS__CAT1) THEN
                           FDTYP__CAT6(FIE) = FDTYPE
                           FCSIZ__CAT6(FIE) = FCSIZE
                           FPTR__CAT6(FIE) = PTR
                           FPTRN__CAT6(FIE) = PTRN
                        ELSE
                           STATUS = CAT__INVID

                           CALL CAT1_ERREP ('CAT6_FINCR_MID',
     :                       'Unknown column indentifier.', STATUS)
                        END IF
                     END DO
                  END IF

               ELSE

*
*                Failed to get an identifier; set the termination flag.

                  MORE = .FALSE.
               END IF
            END DO

*
*          Set the 'finished creation of catalogue' flag.

            FINSH__CAT1(CIELM) = .TRUE.

         END IF

      END IF

      END
