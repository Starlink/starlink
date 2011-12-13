      SUBROUTINE CAT5_OPNEX (CATNAM, CATFIL, EXTRA, MODE, CI,
     :  STATUS)
*+
*  Name:
*     CAT5_OPNEX
*  Purpose:
*     Open an existing small text list catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_OPNEX (CATNAM, CATFIL, EXTRA, MODE; CI; STATUS)
*  Description:
*     Open an existing small text list (STL) catalogue.
*  Arguments:
*     CATNAM  =  CHARACTER*(*) (Given)
*        The name of the catalogue.
*     CATFIL  =  CHARACTER*(*) (Given)
*        The file name for the catalogue.
*     EXTRA  =  CHARACTER*(*) (Given)
*        Any extra information needed to access the catalogue.  This
*        argument is not used in the STL back-end.
*     MODE  =  CHARACTER*(*) (Given)
*        The mode of accessing the catalogue; one of: 'READ', 'UPDATE',
*        'MODIFY' or 'WRITE'.
*     CI  =  INTEGER (Returned)
*        Identifier to the catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get a catalogue identifier.
*     If ok then
*       Initialise the directives to the default for a small text list.
*       Read and decode the description file and use the details to
*       create the columns and parameters of the catalogue.
*       If ok then
*         If all the mandatory directives have been set then
*           Determine the number of rows in the table by reading
*           the file.
*           If the number of rows is more than the maximum notionally
*           permitted then
*             Report a message.
*           end if
*           If the number of rows is greater than zero then
*             Obtain details of all the columns and create workspace
*             to hold the arrays of values.
*             Read in the table to values.
*             Ensure that the STL common block variables are set.
*             Open the description file, ready to to access textual
*             information.
*           else
*             Set the status.
*             Report an error.
*           end if
*         end if
*       end if
*     end if
*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils
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
*     2/7/96   (ACD): Original version.
*     16/9/96  (ACD): First stable version.
*     19/11/96 (ACD): Made to handle fixed-format tables, as well as
*        free-format ones.
*     6/6/98   (ACD): Changed the way the column details are handled.
*     4/8/98   (ACD): Added the ability to handle complex angles in
*        addition to simple ones.
*     19/7/00  (ACD): Added a check that the number of rows was greater
*        than zero.
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
      INCLUDE 'CAT1_DSDIR_CMN'    ! Description directives.
      INCLUDE 'CAT5_STL_CMN'      ! Small text list common block.
*  Arguments Given:
      CHARACTER
     :  CATNAM*(*),
     :  CATFIL*(*),
     :  EXTRA*(*),
     :  MODE*(*)
*  Arguments Returned:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      DOUBLE PRECISION
     :  CDATE     ! Catalogue modification date.
      INTEGER
     :  CIELM,    ! Common block element for the catalogue.
     :  TTROWS,   ! Total number of records in the table file (inc. header).
     :  ROWS,     ! Number of rows in the table.
     :  SKIP,     ! Number of records to skip before reading table.
     :  NUMCOL,   ! Number of columns in the catalogue.
     :  LOOP,     ! Loop index.
     :  FI        ! Identifier for the current column.
      INTEGER
     :  FIA(CAT5__MXCOL),     ! Column identifiers.
     :  FDTYPA(CAT5__MXCOL),  ! Data types of the columns.
     :  FCSIZA(CAT5__MXCOL),  ! Size of character columns.
     :  FPOSNA(CAT5__MXCOL),  ! Position of column in record.
     :  FANGLA(CAT5__MXCOL),  ! Code: is column an angle?
     :  FNANGL(CAT5__MXCOL),  ! Angle sequence numbers.
     :  FPTRA(CAT5__MXCOL),   ! Pointer to array to hold the columns.
     :  FPTRNA(CAT5__MXCOL),  ! Pointer to array to hold the column null flags.
     :  DFUNIT,   ! Fortran unit number for accessing the description file.
     :  LSTAT,    ! Local Fortran I/O status.
     :  MSGLEN    ! Length of MSGTXT (excl. trail. blanks).
      LOGICAL
     :  FSCLFA(CAT5__MXCOL)   ! Flag; is column to be scaled?
      DOUBLE PRECISION
     :  FSCALA(CAT5__MXCOL),  ! Scale factor for columns.
     :  FZEROA(CAT5__MXCOL)   ! Zero point for columns.
      CHARACTER
     :  FFMTA(CAT5__MXCOL)*(CAT__SZEXF), ! STL table formats for columns.
     :  CTNAME*(CAT__SZCNF),  ! Full name for STL table file.
     :  MSGTXT*75             ! Message text.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Attempt to create a new catalogue identifier and add it to the
*       list of identifiers.  Also store the attributes which pertain to
*       the entire catalogue, specifically its name and the modification
*       date.

         CALL CAT1_CRTCI (CI, STATUS)
         CALL CAT1_CIELM (CI, CIELM, STATUS)

         CALL CAT1_ADDAC (CI, 'NAME', .FALSE., CATNAM, STATUS)

         CALL CAT1_GTDAT (CDATE, STATUS)
         CALL CAT1_ADDAD (CI, 'DATE', .TRUE., CDATE, STATUS)

*
*       Proceed if ok.

         IF (STATUS .EQ. CAT__OK) THEN

*
*          Initialise the directives to the default for a small text list.

            DFLFG__CAT1 = CAT1__INDSC
            DFILE__CAT1 = ' '
            DPOSN__CAT1 = CAT1__PSCOL
            DRESZ__CAT1 = 0
            DROWS__CAT1 = 0
            DSKIP__CAT1 = 0

            DCOLS__CAT1 = 0
            DPARS__CAT1 = 0

*
*          Read and decode the description file and use the details to
*          create the columns and parameters of the catalogue: read and
*          decode the file and proceed if all ok.

            CALL CAT1_DDSCR (CATFIL, CI, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN

*
*             Check that all the mandatory directives have been
*             set.  For an STL the only condition to check for is
*             that if the table is not in the same file as the
*             description then the name of the table file must have been
*             specified.

               IF (DFLFG__CAT1 .EQ. CAT1__SEPFL) THEN
                  IF (DFILE__CAT1 .EQ. ' ') THEN
                     CALL CAT1_MSG (' ', 'error: location of '/
     :                 /'catalogue table not specified.', STATUS)

                     STATUS = CAT__INVDS
                  END IF
               END IF

               IF (STATUS .EQ. CAT__OK) THEN

*
*                Determine the number of rows in the table by reading
*                the file.

C                 print4999,
C    :              dflfg__cat1, dfile__cat1, ddesc__cat1, dskip__cat1
C4999             format(
C    :              1x, 'dflfg__cat1: ' i5 /
C    :              1x, 'dfile__cat1: ' a /
C    :              1x, 'ddesc__cat1: ' i5 /
C    :              1x, 'dskip__cat1: ' i5 )

                  IF (DFLFG__CAT1 .EQ. CAT1__INDSC) THEN
                     CTNAME = CATFIL
                     SKIP = DDESC__CAT1 + DSKIP__CAT1
                  ELSE
                     CTNAME = DFILE__CAT1
                     SKIP = DSKIP__CAT1
                  END IF

                  CALL CAT5_CNTRC (CTNAME, TTROWS, STATUS)
                  ROWS = TTROWS - SKIP

C                 print5000, ctname, rows, ttrows, skip
C5000             format(1x, 'ctname: 'a /
C    :              1x, 'rows, ttrows, skip: ', i5, i5, i5 )

*
*                If the number of rows is more than the maximum notionally
*                permitted then report a message.

                  IF (ROWS .GT. CAT5__MXROW) THEN
                     MSGTXT = ' '
                     MSGLEN = 0

                     CALL CHR_PUTC ('This small text list contains ',
     :                 MSGTXT, MSGLEN)
                     CALL CHR_PUTI (ROWS, MSGTXT, MSGLEN)
                     CALL CHR_PUTC (' rows.', MSGTXT, MSGLEN)

                     CALL CAT1_MSG (' ', MSGTXT(1 : MSGLEN), STATUS)

                     CALL CAT1_MSG (' ', 'It may take a while to '/
     :                 /'process.', STATUS)
                  END IF

*
*                Proceed if the number of rows is greater than zero.

                  IF (ROWS .GT. 0) THEN

*
*                   Obtain details of all the columns and create
*                   workspace to hold the arrays of values.

                     CALL CAT5_GTCLD (CI, CAT5__MXCOL, ROWS, NUMCOL,
     :                 FIA, FDTYPA, FCSIZA, FSCLFA, FSCALA, FZEROA,
     :                 FFMTA, FANGLA, FNANGL, FPOSNA, FPTRA, FPTRNA,
     :                 STATUS)

*
*                   Read in the table to values.  Different routines are
*                   called depending on whether the table is free-format
*                   or fixed format.

                     IF (DPOSN__CAT1 .EQ. CAT1__PSCOL) THEN
                        CALL CAT5_RDTBL (CTNAME, SKIP, ROWS, NUMCOL,
     :                    FDTYPA, FCSIZA, FSCLFA, FSCALA, FZEROA,
     :                    FPOSNA, FANGLA, FNANGL, FPTRA, FPTRNA,
     :                    STATUS)
                     ELSE
                        CALL CAT5_RDTBX (CTNAME, SKIP, ROWS, NUMCOL,
     :                    FDTYPA, FCSIZA, FSCLFA, FSCALA, FZEROA,
     :                    FPOSNA, FFMTA, FANGLA, FNANGL, FPTRA, FPTRNA,
     :                    STATUS)
                     END IF

*
*                   Ensure that the STL common block variables are set.

                     DO LOOP = 1, NUMCOL
                        FI = FIA(LOOP)

                        IF (FI .GT. 0  .AND.  FI .LE. NIDS__CAT1) THEN
                           FDTYP__CAT5(FI) = FDTYPA(LOOP)
                           FCSIZ__CAT5(FI) = FCSIZA(LOOP)
                           FPTR__CAT5(FI) = FPTRA(LOOP)
                           FPTRN__CAT5(FI) = FPTRNA(LOOP)
                           FFMT__CAT5(FI) = FFMTA(LOOP)
                        ELSE
                           STATUS = CAT__INVID

                           CALL CAT1_ERREP ('CAT5_OPNEX_MID',
     :                       'Unknown column identifier.', STATUS)
                        END IF
                     END DO

                     FNDSC__CAT5(CIELM) = .FALSE.

*
*                   Set the backend-independent catalogue common blocks
*                   containing the number of rows etc.

                     CIDS__CAT1(CIELM) = CI
                     BKTYP__CAT1(CIELM) = CAT1__BKSTL
                     EROW__CAT1(CIELM) = ROWS
                     NROW__CAT1(CIELM) = ROWS
                     NPCOL__CAT1(CIELM) = DCOLS__CAT1
                     NVCOL__CAT1(CIELM) = 0
                     NIND__CAT1(CIELM) = 0
                     NPAR__CAT1(CIELM) = DPARS__CAT1
                     NSEL__CAT1(CIELM) = 0
                     CROW__CAT1(CIELM) = 1
                     FINSH__CAT1(CIELM) = .TRUE.

*
*                   Open the description file, ready to to access
*                   textual information.

                     CALL CAT1_GETLU (DFUNIT, STATUS)
                     STUNT__CAT5(CIELM) = DFUNIT

                     OPEN(DFUNIT, STATUS='OLD', FILE=CATFIL,
     :                 IOSTAT=LSTAT)
                     IF (STATUS .EQ. CAT__OK) THEN
                        CALL CAT1_IOERR (LSTAT, STATUS)
                     END IF

                  ELSE

*
*                   The catalogue contained no rows; set the status
*                   and report an error.

                     STATUS = CAT__NOCAT

                     CALL CAT1_ERREP ('CAT5_OPNEX_ZRW',
     :                 'The catalogue contains zero rows.', STATUS)

                  END IF

               END IF

            END IF
         END IF

      END IF

      END
