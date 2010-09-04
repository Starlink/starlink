      SUBROUTINE CAT5_FINCR (CI, STATUS)
*+
*  Name:
*     CAT5_FINCR
*  Purpose:
*     Finish the creation of a small text list (STL).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_FINCR (CI; STATUS)
*  Description:
*     Finish the creation of a small text list (STL).
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
*           Write the attribute details to the description file.
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
*       Write a blank line to the description file.
*       Set the 'finished creation of catalogue' flag.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     18/7/96  (ACD): Original version.
*     14/8/96  (ACD): First stable version.
*     10/12/96 (ACD): Added writing 'KAPPA format' STLs.
*     28/3/97  (ACD): Changed the definition of column and parameter
*        names to use the correct parametric contstant (CAT__SZCMP).
*     6/6/98   (ACD): Changed the way column details are manipulated.
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
      INCLUDE 'CAT5_STL_CMN'      ! STL back-end common block.
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
     :  DFUNIT,  ! Fortran unit no. for accessing the catalogue file.
     :  LSTAT,   ! Fortran I/I status.
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
     :  KFLAG,   ! Flag; is STL in 'KAPPA format' or standard.
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
C5000    format(1x, 'entering CAT5_FINCR......')

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
               ROWS = CAT5__MXROW
               EROW__CAT1(CIELM) = CAT5__MXROW
            END IF

            PRVDIM = 1
            DFUNIT = STUNT__CAT5(CIELM)
            KFLAG = KFLAG__CAT5(CIELM)
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
                        FXFMT = 'D19.10'
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
*                Write the appropriate attribute details to the
*                catalogue file.

                  CALL CAT5_WRCOL (DFUNIT, KFLAG, FNAME, FDIM, FSIZE,
     :              FDTYPE, FCSIZE, POSN, FORDER, FUNIT, FXFMT, FPDISP,
     :              FCOMM, STATUS)
C                 print5004
C5004             format(1x, 'after WRCOL')

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

                     CALL CAT5_STNUL (.TRUE., ROWS,
     :                                %VAL(CNF_PVAL(PTRN)), STATUS)

*
*                   Save the common block details.

                     IF (FI .GT. 0  .AND.  FI .LE. NIDS__CAT1) THEN
                        FPOSN__CAT5(FI) = POSN
                        FDTYP__CAT5(FI) = FDTYPE
                        FCSIZ__CAT5(FI) = FCSIZE
                        FFMT__CAT5(FI) = FXFMT
                        FPTR__CAT5(FI) = PTR
                        FPTRN__CAT5(FI) = PTRN
                     ELSE
                        STATUS = CAT__INVID

                        CALL CAT1_ERREP ('CAT5_FINCR_MID',
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

                        CALL CAT5_STNUL (.TRUE., ROWS,
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
                           FPOSN__CAT5(FIE) = POSN + CURELM - 1
                           FDTYP__CAT5(FIE) = FDTYPE
                           FCSIZ__CAT5(FIE) = FCSIZE
                           FFMT__CAT5(FIE) = FXFMT
                           FPTR__CAT5(FIE) = PTR
                           FPTRN__CAT5(FIE) = PTRN
                        ELSE
                           STATUS = CAT__INVID

                           CALL CAT1_ERREP ('CAT5_FINCR_MID',
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
*          Write a blank line to the description file.  This line serves
*          to separate the column definitions from any items which
*          follow.

            WRITE (DFUNIT, '(1X)', IOSTAT=LSTAT)
            IF (STATUS .EQ. CAT__OK) THEN
               CALL CAT1_IOERR (LSTAT, STATUS)
            END IF

*
*          Set the 'finished creation of catalogue' flag.

            FINSH__CAT1(CIELM) = .TRUE.

         END IF

      END IF

      END
