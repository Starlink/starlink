      SUBROUTINE CAT6_OPNEX (CATNAM, CATFIL, EXTRA, MODE, CI,
     :  STATUS)
*+
*  Name:
*     CAT6_OPNEX
*  Purpose:
*     Open an existing tab-separated table catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_OPNEX (CATNAM, CATFIL, EXTRA, MODE; CI; STATUS)
*  Description:
*     Open an existing tab-separated table (TST) catalogue.
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
*       Read and decode the TST description and use the details to
*       create the columns and parameters of the catalogue.
*       If ok then
*         If the number of rows is more than the notional maximum then
*           Report a message.
*         end if
*         Obtain data types for the all the columns and create workspace
*         to hold the arrays of values.
*         Read in the table to values.
*         If ok then
*           Set the TST specific common block variables.
*           Set the backend independent common block variables.
*           Open the description file, ready to to access textual
*           information.
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
*     4/6/99  (ACD): Original version (from CAT5_OPNEX).
*     18/6/99 (ACD): First stable version.
*     12/7/00 (ACD): Removed the 'interpretation mode' added support
*        for recognising CURSA-specific column details.
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
      INTEGER STATUS          ! Global status.
*  Local Variables:
      DOUBLE PRECISION
     :  CDATE      ! Catalogue modification date.
      INTEGER
     :  CIELM,     ! Common block element for the catalogue.
     :  ROWS,      ! Number of rows in the table.
     :  SKIP,      ! Number of records to skip before reading table.
     :  PARS,      ! Number of parameters in the catalogue.
     :  RACOL,     ! Sequence no. of column RA  (0 = absent).
     :  DECCOL,    !    "     "   "    "    DEC (" =   "   ).
     :  RAUNIT,    ! Code for units of RA: hours or degrees?
     :  NUMCOL,    ! Number of columns in the catalogue.
     :  TSUNIT     ! Fortran unit number for accessing the TST file.
      INTEGER
     :  LOOP,      ! Loop index.
     :  FI,        ! Identifier for the current column.
     :  LSTAT,     ! Local Fortran I/O status.
     :  MSGLEN,    ! Length of MSGTXT (excl. trail. blanks).
     :  FIA(CAT6__MXCOL),     ! Column identifiers.
     :  FDTYPA(CAT6__MXCOL),  ! Data types of the columns.
     :  FCSIZA(CAT6__MXCOL),  ! Size of character columns.
     :  FPTRA(CAT6__MXCOL),   ! Pointer to array to hold the columns.
     :  FPTRNA(CAT6__MXCOL)   ! Pointer to array to hold the column null flags.
      CHARACTER
     :  MSGTXT*75  ! Message text.
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
*          Read and decode the description at the start of the TST,
*          create the corresponding columns and parameters and proceed
*          if ok.

            CALL CAT6_DDSCR (CATFIL, CI, SKIP, ROWS, PARS, RACOL,
     :        DECCOL, RAUNIT, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN

*
*             If the number of rows is more than the notional maximum
*             then report a message.

               IF (ROWS .GT. CAT6__MXROW) THEN
                  MSGTXT = ' '
                  MSGLEN = 0

                  CALL CHR_PUTC ('This tab-separated table contains ',
     :              MSGTXT, MSGLEN)
                  CALL CHR_PUTI (ROWS, MSGTXT, MSGLEN)
                  CALL CHR_PUTC (' rows.', MSGTXT, MSGLEN)

                  CALL CAT1_MSG (' ', MSGTXT(1 : MSGLEN), STATUS)

                  CALL CAT1_MSG (' ', 'It may take a while to '/
     :              /'process.', STATUS)
               END IF

*
*             Obtain data types for the columns and return work space
*             to hold the arrays of values.

               CALL CAT6_GTCLD (CI, ROWS, CAT6__MXCOL, NUMCOL,
     :           FIA, FDTYPA, FCSIZA, FPTRA, FPTRNA, STATUS)

*
*             Read in the table to values.

               CALL CAT6_RDTBL (CATFIL, SKIP, ROWS, RACOL, DECCOL,
     :           RAUNIT, NUMCOL, FDTYPA, FCSIZA, FPTRA, FPTRNA, STATUS)

*
*             Proceed if all is ok.

               IF (STATUS .EQ. CAT__OK) THEN

*
*                Set the TST specific common block variables.

                  DO LOOP = 1, NUMCOL
                     FI = FIA(LOOP)

                     IF (FI .GT. 0  .AND.  FI .LE. NIDS__CAT1) THEN
                        FDTYP__CAT6(FI) = FDTYPA(LOOP)
                        FCSIZ__CAT6(FI) = FCSIZA(LOOP)
                        FPTR__CAT6(FI) = FPTRA(LOOP)
                        FPTRN__CAT6(FI) = FPTRNA(LOOP)
                     ELSE
                        STATUS = CAT__INVID

                        CALL CAT1_ERREP ('CAT6_OPNEX_MID',
     :                    'Unknown column identifier.', STATUS)
                     END IF
                  END DO

                  HDREC__CAT6(CIELM) = SKIP
                  HDCUR__CAT6(CIELM) = 0

*
*                Set the backend-independent catalogue common blocks
*                containing the number of rows etc.

                  CIDS__CAT1(CIELM) = CI
                  BKTYP__CAT1(CIELM) = CAT1__BKTST
                  EROW__CAT1(CIELM) = ROWS
                  NROW__CAT1(CIELM) = ROWS
                  NPCOL__CAT1(CIELM) = NUMCOL
                  NVCOL__CAT1(CIELM) = 0
                  NIND__CAT1(CIELM) = 0
                  NPAR__CAT1(CIELM) = PARS
                  NSEL__CAT1(CIELM) = 0
                  CROW__CAT1(CIELM) = 1
                  FINSH__CAT1(CIELM) = .TRUE.

*
*                Open the description file, ready to to access
*                textual information.

                  CALL CAT1_GETLU (TSUNIT, STATUS)
                  TSUNT__CAT6(CIELM) = TSUNIT

                  OPEN(TSUNIT, STATUS='OLD', FILE=CATFIL, IOSTAT=LSTAT)
                  IF (STATUS .EQ. CAT__OK) THEN
                     CALL CAT1_IOERR (LSTAT, STATUS)
                  END IF
               END IF

            END IF
         END IF

      END IF

      END
