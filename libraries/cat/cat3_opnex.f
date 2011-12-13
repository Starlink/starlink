      SUBROUTINE CAT3_OPNEX (CATNAM, CATFIL, EXTRA, MODE, CI, STATUS)
*+
*  Name:
*     CAT3_OPNEX
*  Purpose:
*     Open an existing FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_OPNEX (CATNAM, CATFIL, EXTRA, MODE; CI; STATUS)
*  Description:
*     Open an existing FITS table.
*  Arguments:
*     CATNAM  =  CHARACTER*(*) (Given)
*        The name of the catalogue.
*     CATFIL  =  CHARACTER*(*) (Given)
*        The file name for the catalogue.
*     EXTRA  =  CHARACTER*(*) (Given)
*        Any extra information needed to access the catalogue.  For
*        a FITS table this is the extension number of the file within
*        the FITS table.
*     MODE  =  CHARACTER*(*) (Given)
*        The mode of accessing the catalogue; one of: 'READ', 'UPDATE',
*        'MODIFY' or 'WRITE'.
*     CI  =  INTEGER (Returned)
*        Identifier to the catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get a catalogue identifier.
*     Initialise the number of columns, indices etc. to zero.
*     If ok then
*       Attempt to get a free Fortran unit number.
*       Attempt to open the FITS file.
*       If ok then
*         Copy the FITS unit number to the common block.
*       else
*         Report an error.
*       end if
*       If all is ok then
*         Extract the required extension from passed argument 'EXTRA'.
*         Move to the required extension.
*         If the desired extension is a table then
*           Save the number of the extension in the common block.
*           Extract the keywords from the extension header.
*           Obtain the name and order of any column on which the
*           table is sorted.
*           For every column in the FITS table
*             Generate the default set of attributes for a column.
*             Read the details of the column from the FITS extension
*             header and replace the default values with the values
*             read.
*             If the table is sorted on the current column then
*               Set the sort order attribute for the column.
*             end if
*             Set the 'type of null value' attribute to indicate that
*             null values are explictly defined in the catalogue.
*             Add the column to the list of columns (that is, add its
*             attributes to the list of attributes).
*           end for
*           Set the number of rows. etc. in the catalogue common blocks.
*           Reset access to textual information in the header.
*         else (the extension is not a table)
*           Report an error
*         end if
*       end if
*     end if
*  Implementation Deficiencies:
*     The FITSIO status for 'file not found' is hard-coded into the
*     routine as a parametric constant.  I do not like this approach,
*     but can see no alternative as there are no symbolic constants
*     for the FITSIO error codes.
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/7/93  (ACD): Original version.
*     14/10/93 (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     27/1/94  (ACD): Modified obtaining FITS extension number from
*        argument EXTRA.
*     3/2/94   (ACD): Added handling of arrays.
*     11/2/94  (ACD): Added external reference to FITS block data
*       statement.
*     7/6/94   (ACD): Added '.fit' as an extra allowable file type (as
*        well as '.FIT'.
*     15/6/94  (ACD): Fixed bug: set the error status when reporting
*        the encounter of a FITS extension which is not a table.  Also
*        made to skip columns in binary tables which contain unsupported
*        data types: complex REAL and DOUBLE PRECiSION and variable
*        length arrays.
*     5/7/94   (ACD): Added handling of complex columns and proper
*        handling of scaled columns.
*     8/7/94   (ACD): Added reading of comments and the preferential
*        display flag for columns.
*     8/9/94   (ACD): Added reading of the standard keyword for
*        sorted tables to yield the name of the column on which the
*        table is sorted.  The sort attribute for the appropriate
*        column is set using this information.
*     22/9/94  (ACD): Added support for textual information.
*     25/10/94 (ACD): Set the 'type of null value' attribute for
*        every column to indicate that null values are explictly
*        defined in the catalogue.
*     6/3/95   (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     10/5/95  (ACD): Modified to handle HST GSC FITS tables with file
*        type 'gsc' or 'GSC'.
*     27/2/96  (ACD): Modified to handle file type '.FITS' for FITS
*        tables.
*     23/7/96  (ACD): Added argument for the catalogue file type.
*     4/6/98   (ACD): Changed the way the catalogue file name is
*        handled and which FITS keywords are converted to CAT parameters.
*     24/9/99  (ACD): Changed the way that the number of columns is
*        determined (to conform with CFITSIO).
*     26/10/99 (ACD): Modified to get a free unit number using FTGIOU
*       rather than CAT1_GETLU.  This change was necessary for CFITSIO.
*     19/7/00  (ACD): Modified to remove any spurious quotes in the
*       keyword containing the number of rows.
*     20/7/00  (ACD): Added checks that the column external formats
*       read from the table are valid.
*     21/7/00  (ACD): Moved deternining the number of rows in the table
*       into subroutine CAT3_GTPAR.
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
      INCLUDE 'CAT3_FIT_CMN'      ! FITS common block.
*  Arguments Given:
      CHARACTER
     :  CATNAM*(*),
     :  CATFIL*(*),
     :  MODE*(*),
     :  EXTRA*(*)
*  Arguments Returned:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS   ! Global status
*  External References:
      INTEGER CHR_LEN
*                        initialisation.
*  Local Constants:
       INTEGER FITOK   ! FITSIO Success status.
       PARAMETER (FITOK = 0)
*
       INTEGER FITNFD   ! FITSIO status for 'file not found'.
       PARAMETER (FITNFD = 103)
*
       INTEGER ASCTAB  ! FITSIO Code identifying an ASCII table.
       PARAMETER (ASCTAB = 1)
*
       INTEGER BINTAB  ! FITSIO Code identifying a binary table.
       PARAMETER (BINTAB = 2)
*  Local Variables:
      INTEGER
     :  EXTN,     ! Extension in the FITS file which holds the table.
     :  ROWS,     ! Number of rows in the catalogue.
     :  PCOLS,    ! Number of (physical) columns in the catalogue.
     :  INDS,     ! Number of indices.
     :  PARS,     ! Number of parameters in the catalogue.
     :  CIELM,    ! Common block element for the catalogue.
     :  FITUNT,   ! Fortran unit no. for access the FITS file.
     :  FITSTT,   ! FITSIO running status.
     :  BLKSIZ,   ! Blocksize of the FITS file.
     :  HDUTYP,   ! FITSIO code for Header and Data Unit type.
     :  FI,       ! Column identifier.
     :  NXCOL     ! Number of the next column.
      CHARACTER
     :  ERRMSG*80,   ! Buffer for error message.
     :  ONAME*(CAT__SZCMP)  ! Name of sort column for the table.
      INTEGER
     :  ORDER,    ! Code for the order of the sorted column.
     :  LCATFL,   ! Length of CATFIL (excl. trail. blanks).
     :  ERRPOS    !   "    "  ERRMSG ( "  .   "  .   "   ).
      LOGICAL
     :  MORCOL,   ! Flag: more columns in the FITS file?
     :  FMTOK     ! Flag; is external format ok?
      DOUBLE PRECISION
     :  CDATE     ! Catalogue modification date.

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
     :  FXCEPT*(10),         ! Exception value attribute.
     :  FUNIT*(CAT__SZUNI),  ! Units attribute.
     :  FXFMT*(CAT__SZEXF),  ! External format attribute.
     :  FCOMM*(CAT__SZCOM)   ! Comments attribute for the column.
      DOUBLE PRECISION
     :  FSCALE,  ! Scale factor attribute.
     :  FZERO,   ! Zero point attribute.
     :  FDATE    ! Modification date attribute.
      LOGICAL
     :  FPDISP   ! Preferential display flag attribute.

*
*    Additional variables pertaining to a single column.

      INTEGER
     :  FITYPE   ! FITS data type.
      LOGICAL
     :  SKIP,    ! Flag: skip the current column?
     :  SFLAG    ! Flag; is the column scaled?
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
*       Initialise the number of columns, parameters etc.

         PCOLS = 0
         PARS = 0
         INDS = 0

         IF (STATUS .EQ. CAT__OK) THEN

*
*          Attempt to get a free Fortran unit number and then to open
*          the FITS file.  If all is ok then copy the FITS unit number
*          to the common block; otherwise report an error.

            FITSTT = FITOK

            CALL FTGIOU (FITUNT, FITSTT)
C           print4000, fitunt, fitstt
C4000       format(1x, 'FTGIOU: fitunt, fitstt: ', i4, i4)
            CALL FTOPEN (FITUNT, CATFIL, 0, BLKSIZ, FITSTT)
C           print4001, fitstt
C4001       format(1x, 'FTOPEN:, fitstt: ', i4 )

            IF (FITSTT .EQ. FITOK) THEN
               FUNT__CAT3(CIELM) = FITUNT

            ELSE
               STATUS = CAT__NOCAT

               ERRMSG = ' '
               ERRPOS = 0

               CALL CHR_PUTC ('Failed to open FITS file ', ERRMSG,
     :           ERRPOS)

               IF (CATFIL .NE. ' ') THEN
                  LCATFL = CHR_LEN(CATFIL)
                  CALL CHR_PUTC (CATFIL(1 : LCATFL), ERRMSG, ERRPOS)
               ELSE
                  CALL CHR_PUTC ('<blank>', ERRMSG, ERRPOS)
               END IF

               CALL CHR_PUTC ('.', ERRMSG, ERRPOS)

               CALL CAT3_FITER ('CAT3_OPNEX_OPE', ERRMSG(1 : ERRPOS),
     :           FITSTT, STATUS)
            END IF

            IF (STATUS .EQ. CAT__OK) THEN

*
*             Extract the required extension from passed argument
*             'EXTRA'.  The default extension if EXTRA is blank is 1.

               IF (EXTRA .NE. ' ') THEN
                  CALL CHR_CTOI (EXTRA, EXTN, STATUS)

                  IF (STATUS .NE. CAT__OK) THEN
                     STATUS = CAT__INVCN

                     CALL CAT1_ERREP ('CAT3_OPNEX_EXN', 'Warning, '/
     :                 /'invalid FITS extension number; trying '/
     :                 /'extension one.', STATUS)

                     STATUS = CAT__OK
                     EXTN = 1
                  END IF

               ELSE
                  EXTN = 1

               END IF

C              print3000, extn, status
C3000          format(1x, 'before FTMRHD extn, status: ', i5, i7 )

*
*             Move to the required FITS extension.

               CALL FTMRHD (FITUNT, EXTN, HDUTYP, FITSTT)
C              print3001, hdutyp, fitstt
C3001          format(1x, 'FTMRHD: hdutyp, fitstt: ', i5, i5)

               IF (FITSTT .NE. FITOK) THEN
                  STATUS = CAT__NOCAT
                  CALL CAT3_FITER ('CAT3_OPNEX_ASE',
     :              'Failed to access specified FITS extension.',
     :              FITSTT, STATUS)
               END IF

*
*             Check that the move succeeded and the specified extension
*             corresponds to a FITS table (either ASCII or binary).

               IF (STATUS .EQ. CAT__OK  .AND.
     :            (HDUTYP .EQ. ASCTAB  .OR.  HDUTYP .EQ. BINTAB) ) THEN

*
*                Save the number of the extension in the common block.

                  EXTN__CAT3(CIELM) = EXTN

*
*                Extract the keywords from the extension header and
*                save them as CAT Parameters.

C                 print3499, status
C3499             format(1x, 'before CAT3_GTPAR, status: ', i5)
                  CALL CAT3_GTPAR (CI, EXTN, FITUNT, PARS, PCOLS,
     :              ROWS, STATUS)
C                 print3500, pcols, status
C3500             format(1x, 'after CAT3_GTPAR, pcols, status: ',
C    :              i5, i15)

*
*                Obtain the name and order of any column on which the
*                table is sorted.  Note that if the table is not sorted
*                on any column then ONAME is returned set to blank.

                  CALL CAT3_RDORD (FITUNT, ONAME, ORDER, STATUS)
C                 print3100, oname, order, status
C3100             format(1x, 'After CAT3_RDORD: ' /
C    :              3x, 'oname: ', a /
C    :              3x, 'order: ', i5 /
C    :              3x, 'status: ', i20 / )

*
*                Extract the details for all the columns in the FITS
*                table and save them as CAT columns.

                  MORCOL = .TRUE.
                  NXCOL = 0

                  DO WHILE (MORCOL)

*
*                   Generate the default attributes for a column.

                     CALL CAT1_DFATT (FNAME, FGENUS, FEXP, FDTYPE,
     :                 FCSIZE, FDIM, FSIZE, FNULL, FXCEPT, FSCALE,
     :                 FZERO, FORDER, FDATE, FUNIT, FXFMT, FPDISP,
     :                 FCOMM, STATUS)

*
*                   Read the details of the next column from the
*                   FITS file.

                     NXCOL = NXCOL + 1

*
*                   Check whether the table is ASCII or binary, and
*                   extract the appropriate details (note that tests
*                   above have established that HDUTYP is equal to
*                   either ASCTAB or BINTAB, so there is no need here to
*                   protect against it adopting other values).

                     IF (HDUTYP .EQ. ASCTAB) THEN

*
*                      The table is ASCII.

C                       print7000, 'before', nxcol, fname, status
C7000                   format(1x, a, ': nxcol, fname, status: ',
C    :                    i4, 1x, a, i20)
                        CALL CAT3_GTCLA (FITUNT, NXCOL, FNAME, FDTYPE,
     :                    FCSIZE, FUNIT, FSCALE, FZERO, FXCEPT, FXFMT,
     :                    FCOMM, FITYPE, SFLAG, STATUS)
C                       print7000, 'after', nxcol, fname, status
                        SKIP = .FALSE.

                     ELSE
*
*                      The table is binary.

                        CALL CAT3_GTCLB (FITUNT, NXCOL, SKIP, FNAME,
     :                    FDTYPE, FCSIZE, FDIM, FSIZE, FUNIT, FSCALE,
     :                    FZERO, FXCEPT, FXFMT, FPDISP, FCOMM,
     :                    FITYPE, SFLAG, STATUS)

                     END IF

                     IF (STATUS .EQ. CAT__OK  .AND.  FNAME .NE. ' '
     :                 .AND.  .NOT. SKIP) THEN

*
*                      Check that the external format read from the
*                      FITS table is ok.  If not then replace it with
*                      an appropriate one generated for the data type
*                      of the column.

                        CALL CAT1_CXFMT (FNAME, FXFMT, FDTYPE, FCSIZE,
     :                    FMTOK, STATUS)

                        IF (.NOT. FMTOK) THEN
                           CALL CAT1_DXFMT (FDTYPE, FCSIZE, FUNIT,
     :                       FXFMT, STATUS)
                        END IF

*
*                      If the table is sorted on the current column
*                      then set the sort order for the column.

C                       print3101, fname, oname
C3101                   format(3x, 'fname: ', a /
C    :                    3x, 'oname: ', a / )

                        IF (FNAME .EQ. ONAME) THEN
                           FORDER = ORDER
                        END IF

*
*                      Set the 'type of null value' attribute to
*                      indicate that null values are explictly defined
*                      in the catalogue.

                        FNULL = CAT__NULLS

*
*                      Add this column to the list of columns and get an
*                      identifier for it.

                        CALL CAT1_ADDCL (CI, FNAME, FGENUS, FEXP,
     :                    FDTYPE, FCSIZE, FDIM, FSIZE, FNULL, FXCEPT,
     :                    FSCALE, FZERO, FORDER, FDATE, FUNIT, FXFMT,
     :                    FPDISP, FCOMM, FI, STATUS)


C                       print7070, nxcol, fname, fi, fdtype
C7070                   format(1x, 'nxcol, fname, fdtype: ',
C    :                    i3, a15, i4, i4)

*
*                      Add the FITS specific details.

                        IF (FI .GT. 0 .AND.  FI .LE. NIDS__CAT1) THEN
                           COLNO__CAT3(FI) = NXCOL
                           FTYPE__CAT3(FI) = FITYPE
                           SFLAG__CAT3(FI) = SFLAG
                           SCALE__CAT3(FI) = FSCALE
                           ZERO__CAT3(FI) = FZERO

                        ELSE
                           STATUS = CAT__MAXID
                           CALL CAT1_ERREP ('CAT3_OPNEX_MID',
     :                       'Error with internal storage of '/
     :                       /'catalogue details.', STATUS)

                        END IF

*
*                      Reset the internal FITSIO scale factor and zero
*                      point for the column to 1.0 and 0.0 respectively.
*                      CAT takes care of the scaling itself and hence
*                      does not want FITSIO to apply any scaling.

                        FITSTT = 0
                        CALL FTTSCL (FITUNT, NXCOL, 1.0D0, 0.0D0,
     :                    FITSTT)

                     ELSE

*
*                      Check for error conditions.

                        IF (STATUS .NE. CAT__OK) THEN
                           MORCOL = .FALSE.

                           CALL CAT1_ERREP ('CAT3_OPNEX_RCD',
     :                       'Failure while reading FITS column '/
     :                       /'details.', STATUS)
                        END IF

                     END IF

*
*                   Check whether all the columns have been processed
*                   and set the termination flag if appropriate.

                     IF (NXCOL .GE. PCOLS) THEN
                        MORCOL = .FALSE.
                     END IF

                  END DO

*
*                Set the catalogue common blocks containing the number
*                of rows etc.

                  CIDS__CAT1(CIELM) = CI
                  BKTYP__CAT1(CIELM) = CAT1__BKFIT
                  NROW__CAT1(CIELM) = ROWS
                  NPCOL__CAT1(CIELM) = PCOLS
                  NVCOL__CAT1(CIELM) = 0
                  NIND__CAT1(CIELM) = 0
                  NPAR__CAT1(CIELM) = PARS
                  NSEL__CAT1(CIELM) = 0
                  CROW__CAT1(CIELM) = 1
                  FINSH__CAT1(CIELM) = .TRUE.

*
*                Reset access to textual information in the header.

                  CALL CAT3_RSTXT (CI, STATUS)

               ELSE

*
*                Check whether the abort was due to a bad status or
*                the FITS table being of type neither ASCII or binary.

                  IF (STATUS .EQ. CAT__OK) THEN

*
*                   The extension is not a table: set the status and report
*                   an error.

                     STATUS = CAT__NOCAT

                     CALL CAT1_ERREP ('CAT3_OPNEX_NTB',
     :                 'The specified FITS extension is not a table.',
     :                 STATUS)

                     ERRPOS = 0
                     ERRMSG = ' '

                     CALL CHR_PUTC ('It is ', ERRMSG, ERRPOS)

                     IF (HDUTYP .EQ. 0) THEN
                        CALL CHR_PUTC ('a primary array', ERRMSG,
     :                    ERRPOS)

                     ELSE IF (HDUTYP .GE. 3) THEN
                        CALL CHR_PUTC ('of unknown type', ERRMSG,
     :                    ERRPOS)

                     END IF

                     CALL CHR_PUTC (' (code: ', ERRMSG, ERRPOS)
                     CALL CHR_PUTI (HDUTYP, ERRMSG, ERRPOS)
                     CALL CHR_PUTC (').', ERRMSG, ERRPOS)

                     CALL CAT1_ERREP ('CAT3_OPNEX_NTT',
     :                 ERRMSG(1 : ERRPOS), STATUS)

                  ELSE
                     CALL CAT1_ERREP ('CAT3_OPNEX_ERR',
     :                 'Error processing FITS file.', STATUS)

                  END IF
               END IF
            END IF
         END IF

      END IF

      END
