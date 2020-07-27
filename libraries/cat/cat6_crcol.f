      SUBROUTINE CAT6_CRCOL (CI, NUMCOL, COLNAM, DTYPE, CWIDTH,  DECPL,
     :  EXPFMT, RACOL, DECCOL, GOTUNT, COLUNT, GOTTYP, COLTYP, COLCSZ,
     :  GOTFMT, COLFMT, STATUS)
*+
*  Name:
*     CAT6_CRCOL
*  Purpose:
*     Create the columns in a tab-separated table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_CRCOL (CI, NUMCOL, COLNAM, DTYPE, CWIDTH,  DECPL,
*       EXPFMT, RACOL, DECCOL, GOTUNT, COLUNT, GOTTYP, COLTYP, COLCSZ,
*       GOTFMT, COLFMT; STATUS)
*  Description:
*     Create the columns in a tab-separated table.
*
*     The units, data type and external format are also set.  These
*     quantities may have been read from CURSA-specific comments in the
*     TST.
*
*     If the data type and external format are not so specified they
*     are derived from the actual values in the table.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     NUMCOL  =  INTEGER (Given)
*        Number of columns.
*     COLNAM(NUMCOL)  =  CHARACTER*(*) (Given)
*        Column names.
*     DTYPE(NUMCOL)  =  INTEGER (Given)
*        Column data types.
*     CWIDTH(NUMCOL)  =  INTEGER (Given)
*        Widths for character columns.
*     DECPL(NCOL)  =  INTEGER (Given)
*        Accumulating numbers of decimal places for DOUBLE PRECISION
*        columns.
*     EXPFMT(NCOL)  =  LOGICAL (Given)
*        Accumulating number of Exponential format flags or DOUBLE
*        PRECISION columns.
*     RACOL  =  INTEGER (Given)
*        Sequence number of column of Right Ascension (name 'RA').
*        If the column is absent a value of zero is given.
*     DECCOL  =  INTEGER (Given)
*        Sequence number of column of Declination (name 'DEC').
*        If the column is absent a value of zero is given.
*     GOTUNT  =  LOGICAL (Given)
*        Flag; has a list of column units been specified, coded as
*        follows:
*        .TRUE.  - list specified,
*        .FALSE. - no list specified.
*     COLUNT(NUMCOL)  =  CHARACTER*(*) (Given)
*        List of column units, if specified.
*     GOTTYP  =  LOGICAL (Given)
*        Flag; has a list of column data types been specified, coded as
*        follows:
*        .TRUE.  - list specified,
*        .FALSE. - no list specified.
*     COLTYP(NUMCOL)  =  INTEGER (Given)
*        List of column data type codes, if specified.
*     COLCSZ(NUMCOL)  =  INTEGER (Given)
*        List of column character sizes, if specified.
*     GOTFMT  =  LOGICAL (Given)
*        Flag; has a list of column external formats been specified, coded
*        as follows:
*        .TRUE.  - list specified,
*        .FALSE. - no list specified.
*     COLFMT(NUMCOL)  =  CHARACTER*(*) (Given)
*        List of column external formats, if specified.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each column
*       Generate the default attributes for each column.
*       Set the column name and convert it to upper case.
*       If column units have been specified then
*         Set the units specified for the column.
*       end if
*       If data types and external formats have been specified then
*         Set the data type specified for the column.
*         Set the character size specified for the column.
*         Set the external format specified for the column.
*       else
*         Set the data type derived from the table of values.
*         Set the character width derived from the table of values.
*         Generate and set an external format.
*         If the column is RA then
*           Tweak the details.
*         end if
*         If the column is DEC then
*           Set the details.
*         end if
*       end if
*       Create the column.
*     end for
*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils
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
*     4/6/99  (ACD): Original version.
*     25/6/99 (ACD): First stable version.
*     24/5/00 (ACD): Fixed bug in calculating the number of decimal
*        places for floating point external formats.
*     12/7/00 (ACD): Added support for obtaining the units, data type
*        and external format from CURSA-specific comments in the TST.
*     20/7/00 (ACD): Added a check that any CURSA-specific external
*        format is valid for the specified data type.
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
*  Global Variables:
      INCLUDE 'CAT6_TST_CMN'      ! TST back-end common block.
*  Arguments Given:
      INTEGER
     :  CI,
     :  NUMCOL,
     :  DTYPE(NUMCOL),
     :  CWIDTH(NUMCOL),
     :  DECPL(NUMCOL),
     :  RACOL,
     :  DECCOL,
     :  COLTYP(NUMCOL),
     :  COLCSZ(NUMCOL)
      LOGICAL
     :  EXPFMT(NUMCOL),
     :  GOTUNT,
     :  GOTTYP,
     :  GOTFMT
      CHARACTER
     :  COLNAM(NUMCOL)*(*),
     :  COLUNT(NUMCOL)*(*),
     :  COLFMT(NUMCOL)*(*)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  LOOP,    ! Loop index.
     :  FI,      ! Column identifier.
     :  TOTAL,   ! Total width of the format.
     :  DECPLC,  ! Number of decimal places in the format.
     :  LFXFMT   ! Length of FXFMT (excl. trail. blanks).
      LOGICAL
     :  FMTOK    ! Flag; is external format ok?

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
*       Process each column.

         DO LOOP = 1, NUMCOL

*
*          Generate the default attributes for a column.

            CALL CAT1_DFATT (FNAME, FGENUS, FEXP, FDTYPE, FCSIZE, FDIM,
     :        FSIZE, FNULL, FXCEPT, FSCALE, FZERO, FORDER, FDATE, FUNIT,
     :        FXFMT, FPDISP, FCOMM, STATUS)

*
*          Set the column name and convert it to upper case.

            FNAME = COLNAM(LOOP)
            CALL CHR_UCASE (FNAME)

*
*          If units have been specified then set the units for the
*          column.

            IF (GOTUNT) THEN
               FUNIT = COLUNT(LOOP)
            END IF

*
*          Check whether data types and external formats have been
*          specified.  If so then adopt them for the column.  Otherwise
*          adopt values derived from the table.
*
*          Note that data types and external formats are only adopted
*          if both were specified as a hedge against generating invalid
*          tables where the external format does not correspond to the
*          data type.
*
*          A check is made that the external format given is valid for
*          the data type and if not an appropriate one is generated.

            IF (GOTTYP .AND. GOTFMT) THEN
               FDTYPE = COLTYP(LOOP)
               FCSIZE = COLCSZ(LOOP)
               FXFMT = COLFMT(LOOP)

               CALL CAT1_CXFMT (FNAME, FXFMT, FDTYPE, FCSIZE, FMTOK,
     :           STATUS)

               IF (.NOT. FMTOK) THEN
                  CALL CAT1_DXFMT (FDTYPE, FCSIZE, FUNIT, FXFMT,
     :              STATUS)
               END IF

            ELSE

*
*             Set the data type and character width derived from the table.

               FDTYPE = DTYPE(LOOP)
               FCSIZE = CWIDTH(LOOP)

*
*             Generate the external format from the data type and column
*             width.  Note that only the data types possible in a TST
*             need be considered.

               LFXFMT = 0
               FXFMT = ' '

               IF (FDTYPE .EQ. CAT__TYPEI) THEN
                  CALL CHR_PUTC ('I', FXFMT, LFXFMT)
                  CALL CHR_PUTI (FCSIZE, FXFMT, LFXFMT)

               ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                  IF (EXPFMT(LOOP)) THEN
                     DECPLC = DECPL(LOOP) - 2
                     DECPLC = MAX(1, DECPLC)
                     TOTAL = DECPLC + 7

                     CALL CHR_PUTC ('E', FXFMT, LFXFMT)
                     CALL CHR_PUTI (TOTAL, FXFMT, LFXFMT)
                     CALL CHR_PUTC ('.', FXFMT, LFXFMT)
                     CALL CHR_PUTI (DECPLC, FXFMT, LFXFMT)
                  ELSE
                     DECPLC = DECPL(LOOP)

                     IF (FCSIZE .GE. DECPLC + 2) THEN
                        TOTAL = FCSIZE
                     ELSE
                        TOTAL = DECPLC + 2
                     END IF

                     CALL CHR_PUTC ('F', FXFMT, LFXFMT)
                     CALL CHR_PUTI (TOTAL, FXFMT, LFXFMT)
                     CALL CHR_PUTC ('.', FXFMT, LFXFMT)
                     CALL CHR_PUTI (DECPLC, FXFMT, LFXFMT)
                  END IF

               ELSE
                  CALL CHR_PUTC ('A', FXFMT, LFXFMT)
                  CALL CHR_PUTI (FCSIZE, FXFMT, LFXFMT)

               END IF

*
*             If the current column is either the RA or DEC then tweak
*             the column details.
*
*             Note that the FXFMT format should be able represent angles
*             in radians to an accuracy of 0.01 second of arc.

               IF (LOOP .EQ. RACOL) THEN
                  FDTYPE = CAT__TYPED
                  FUNIT = 'RADIANS{HOURS}'
                  FXFMT = 'E19.10'
                  FCOMM = 'Right Ascension.'

               ELSE IF (LOOP .EQ. DECCOL) THEN
                  FDTYPE = CAT__TYPED
                  FUNIT = 'RADIANS{DEGREES}'
                  FXFMT = 'E19.10'
                  FCOMM = 'Declination.'

               END IF
            END IF

*          Must use LOCUM with logical columns.
            IF( FDTYPE .EQ. CAT__TYPEL ) FNULL = CAT__LOCUM

*          Create the column.

            CALL CAT1_ADDCL (CI, FNAME, FGENUS, FEXP, FDTYPE, FCSIZE,
     :        FDIM, FSIZE, FNULL, FXCEPT, FSCALE, FZERO, FORDER,
     :        FDATE, FUNIT, FXFMT, FPDISP, FCOMM, FI, STATUS)

C           print2001, fname, fdtype, fcsize, fxfmt
C2001       format(1x, 'fname, fdtype, fcsize, fxfmt: '
C    :        a15, i5, i5, a )
         END DO

      END IF

      END
