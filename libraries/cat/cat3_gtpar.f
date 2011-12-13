      SUBROUTINE CAT3_GTPAR (CI, EXTN, FITUNT, PARS, COLS, ROWS,
     :  STATUS)
*+
*  Name:
*     CAT3_GTPAR
*  Purpose:
*     Get FITS keywords from the current HDU as CAT parameters.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_GTPAR (CI, EXTN, FITUNT; PARS; COLS, ROWS; STATUS)
*  Description:
*     Get FITS keywords from the current HDU (Header and Data Unit)
*     and add them to the list of CAT parameters for the catalogue.
*     Standard FITS keywords, including those defining columns, are
*     ignored.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     EXTN  =  INTEGER (Given)
*        Extension number of the FITS header and data unit currently
*        being accessed (used to disambiguate the names of FITS keywords
*        when converting them into CAT parameters).
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing FITS file.
*     PARS  =  INTEGER (Given and Returned)
*        Current number of CAT parameters in the catalogue.
*     COLS  =  INTEGER (Returned)
*        Number of columns in the table.
*     ROWS  =  INTEGER (Returned)
*        Number of rows in the table.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the number of columns and rows.
*     Get the number of keywords in the header.
*     If ok then
*       For each keyword
*         Generate a default set of attributes for a parameter.
*         Attempt to read the keyword from the FITS file.
*         If ok then
*           Remove any enclosing quotes from the keyword value.
*           If the keyword is not the keyword for a column or some
*           other FITS-specific keyword then
*             Replace the default values with the values obtained.
*             If a keyword of this name already exists then
*               Disambiguate the keyword by appending the extension
*               number to it.
*             end if
*             Add the keyword as a parameter to the list of parameters.
*             If ok then
*               Increment the number of parameters.
*             end if
*           end if
*         end if
*         If the keyword is 'TFIELDS' then
*           Attempt to decode the number of columns.
*         end if
*         If the keyword is 'NAXIS2' then
*           Attempt to decode the number of rows.
*         end if
*       end for
*       If the number of columns was not obtained successfully then
*         Set the status.
*         Report an error.
*       end if
*       If the number of rows was not obtained successfully then
*         Set the status.
*         Report an error.
*       end if
*     else
*       Set the status.
*       Report an error obtaining the number of keywords.
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
*     ACD: A C Davenhall (Leicester)
*     PWD: Peter W. Draper (Durham)
*  History:
*     28/7/93  (ACD): Original version.
*     29/7/93  (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     25/1/94  (ACD): Changed the way in which the check that parameter
*       names are unamiguous is carried out.
*     9/7/94   (ACD): Extended the list of column specific keywords
*       which are excluded.
*     23/10/94 (ACD)): Set the external display format (previously it
*       had erroneously been left blank).
*     20/3/96  (ACD): Fixed bug; variable FITSTT was not being
*        initialised.
*     28/3/97  (ACD): Changed the definition of column and parameter
*        names to use the correct parametric contstant (CAT__SZCMP).
*     4/6/98   (ACD): Modified to ignore most standard FITS keywords,
*        not just those corresponding to table columns.
*     6/10/98  (PWD): Changed KWORD, KCOMM and KVALUE to use CAT__
*        constants. KVALUE was 30 and this truncated all parameter values.
*     24/9/99  (ACD): Modified to return the number of columns.
*     19/7/00  (ACD): Moved the removal of any enclosing quotes around
*        the keyword/parameter value to outside the check for FITS
*        specific keywords.  This change means that spurious quotes are
*        removed prior to attempting to obtain the value of TFIELDS.
*     20/7/00  (ACD): Changed the check for keyword TFIELDS to check
*        for precisely the right correct keyword (ie. not to ignore
*        subsequent characters).  Also added a warning if multiple
*        TFIELDS keywords are encountered.
*     21/7/00  (ACD): Added determining the number of rows from keyword
*        NAXIS2.
*     27/9/00  (ACD): Added keywords EXTNAME and EXTVER to the set that
*        are not included as CAT parameters.
*     11/12/00 (ACD): Added keyword HISTORY to the set that are not
*         included as CAT parameters.
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
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
*  Arguments Given:
      INTEGER
     :  CI,
     :  EXTN,
     :  FITUNT
*  Arguments Given and Returned:
      INTEGER
     :  PARS
*  Arguments Returned:
      INTEGER
     :  COLS,
     :  ROWS
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER FITOK   ! FITSIO Success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  FITSTT,   ! FITSIO status.
     :  NUMKEY,   ! Number of keywords.
     :  DUMMY,    ! Dummy argument for FITSIO routines.
     :  KOUNT,    ! Number of the current keyword.
     :  LPNAME,   ! Length of parameter name   (excl. trail. blanks).
     :  LPXFMT,   !   "    "  parameter format ( "  .   "  .   "   ).
     :  LKVAL,    !   "    "  keyword   value  ( "  .   "  .   "   ).
     :  QI        ! Parameter identifier.
      INTEGER
     :  ATLOOP,   ! Current attribute.
     :  IDLOOP,   ! Current identifier.
     :  NCHAR,    ! Number of characters in external display format.
     :  LSTAT     ! Local status decoding the number of columns.
      LOGICAL
     :  FOUND,    ! Flag: has requested component been found?
     :  GTROWS    ! Flag: determined the number of rows?
      CHARACTER
     :  KWORD*(CAT__SZCMP),  ! Name of keyword.
     :  KVALUE*(CAT__SZVAL), ! Value of keyword.
     :  KCOMM*(CAT__SZCOM)   ! Comments for keyword.

      INTEGER UNIQUE ! Unique value, increment after use
*
*    Attributes for a single parameter.

      CHARACTER
     :  PNAME*(CAT__SZCMP),  ! Name attribute.
     :  PUNIT*(CAT__SZUNI),  ! Units attribute.
     :  PXFMT*(CAT__SZEXF),  ! External format attribute.
     :  PCOMM*(CAT__SZCOM),  ! Comments attribute.
     :  PVALUE*(CAT__SZVAL)  ! Value attribute.
      INTEGER
     :  PDTYPE,  ! Data type attribute.
     :  PCSIZE,  ! Character size attribute.
     :  PDIM,    ! Dimensionality attribute.
     :  PSIZE    ! Size attribute.
      DOUBLE PRECISION
     :  PDATE    ! Modification date attribute.
      LOGICAL
     :  PPDISP   ! Preferential display flag attribute.

      SAVE UNIQUE
      DATA UNIQUE /1000/
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the number of columns and rows.

         COLS = -1
         ROWS = 0
         GTROWS = .FALSE.

*
*       Get the number of keywords in the header and proceed if ok.

         FITSTT = FITOK
         CALL FTGHSP (FITUNT, NUMKEY, DUMMY, FITSTT)
C        print4000, NUMKEY, FITSTT
C4000    format(1x, 'numkey, fitstt: ', i5, i15)

         IF (FITSTT .EQ. FITOK) THEN

*
*          Obtain each FITS keyword.

            DO KOUNT = 1, NUMKEY

*
*             Generate the default attributes for a parameter.

               CALL CAT1_DPATT (PNAME, PDTYPE, PCSIZE, PDIM, PSIZE,
     :           PDATE, PUNIT, PXFMT, PPDISP, PCOMM, PVALUE, STATUS)

*
*             Attempt to read the keyword.

C              print4001, kount
C4001          format(1x, 'kount: ', i4)
               CALL FTGKYN (FITUNT, KOUNT, KWORD, KVALUE, KCOMM,
     :           FITSTT)
C              print4002, kword, kvalue(1:20), kcomm(1:20), fitstt
C4002          format(3x, 'kword: ', a /
C    :           3x, 'kvalue: ', a20 /
C    :           3x, 'kcomm: ', a20 /
C    :           3x, 'fitstt: ', i15 )
               IF (FITSTT .NE. FITOK) THEN
                  STATUS = CAT__NOCAT
                  CALL CAT3_FITER ('CAT3_GTPAR_RKW',
     :              'Failed to read FITS keyword.', FITSTT, STATUS)
               END IF

               IF (STATUS .EQ. CAT__OK) THEN
                  CALL CHR_CLEAN (KWORD)
                  CALL CHR_CLEAN (KVALUE)
                  CALL CHR_CLEAN (KCOMM)

                  CALL CHR_LDBLK (KWORD)

*
*                Remove any enclosing quotes from around the keyword
*                value.  The quotes are removed here, rather than after
*                the test for FITS-specific keywords so that any quotes
*                around the value of keyword TFIELDS are also removed.

                  IF (KVALUE .NE. ' ') THEN
                     CALL CHR_LDBLK (KVALUE)

                     LKVAL = CHR_LEN(KVALUE)

                     IF (KVALUE(1 : 1) .EQ. '''' .AND.
     :                   KVALUE(LKVAL : LKVAL) .EQ. '''') THEN
                        KVALUE(1 : 1) = ' '
                        KVALUE(LKVAL : LKVAL) = ' '
                        CALL CHR_LDBLK (KVALUE)
                     END IF
                  END IF

*
*                Ignore the keyword if it is FITS-specific (FITS-specific
*                keywords do not convert into CAT parameters).

                  IF (KWORD(1 : 5) .NE. 'TBCOL'  .AND.
     :                KWORD(1 : 5) .NE. 'TFORM'  .AND.
     :                KWORD(1 : 5) .NE. 'TTYPE'  .AND.
     :                KWORD(1 : 5) .NE. 'TUNIT'  .AND.
     :                KWORD(1 : 5) .NE. 'TSCAL'  .AND.
     :                KWORD(1 : 5) .NE. 'TZERO'  .AND.
     :                KWORD(1 : 5) .NE. 'TNULL'  .AND.
     :                KWORD(1 : 5) .NE. 'TDISP'  .AND.
     :                KWORD(1 : 5) .NE. 'TCOMM'  .AND.
     :                KWORD(1 : 5) .NE. 'TPRFD'  .AND.
     :                KWORD(1 : 7) .NE. 'COMMENT' .AND.
     :                KWORD(1 : 7) .NE. 'HISTORY' .AND.
     :                KWORD(1 : 6) .NE. 'SIMPLE' .AND.
     :                KWORD(1 : 6) .NE. 'BITPIX' .AND.
     :                KWORD(1 : 5) .NE. 'NAXIS' .AND.
     :                KWORD(1 : 6) .NE. 'EXTEND' .AND.
     :                KWORD(1 : 8) .NE. 'XTENSION' .AND.
     :                KWORD(1 : 6) .NE. 'PCOUNT' .AND.
     :                KWORD(1 : 6) .NE. 'GCOUNT' .AND.
     :                KWORD(1 : 7) .NE. 'TFIELDS' .AND.
     :                KWORD(1 : 8) .NE. 'TSORTKEY' .AND.
     :                KWORD(1 : 7) .NE. 'EXTNAME' .AND.
     :                KWORD(1 : 6) .NE. 'EXTVER' .AND.
     :                KWORD        .NE. ' ') THEN

*
*                   Replace the default attributes with the values read
*                   (note that parameters read from FITS files are
*                   always treated as being of data type character).

                     PNAME = KWORD
                     PVALUE = KVALUE
                     PCOMM = KCOMM
                     PDTYPE = CAT__TYPEC
                     PCSIZE = CAT__SZVAL

*
*                   Work out an external display format for the value.

                     IF (PVALUE .NE. ' ') THEN
                        NCHAR = 1 + CHR_LEN(PVALUE)
                     ELSE
                        NCHAR = 1
                     END IF

                     LPXFMT = 0
                     PXFMT = ' '

                     CALL CHR_PUTC ('A', PXFMT, LPXFMT)
                     CALL CHR_PUTI (NCHAR, PXFMT, LPXFMT)

*
*                   Check if a parameter of this name already exists.
*                   If so, then invent an unambiguous name by appending
*                   the extension number to the parameter name.
*                   The check is carried out by searching the common
*                   block arrays for the required identifier.

                     FOUND = .FALSE.

                     DO ATLOOP = 1, NATT__CAT1
                        IF (ATTNM__CAT1(ATLOOP) .EQ. 'NAME'  .AND.
     :                      ATTYP__CAT1(ATLOOP) .EQ. CAT__TYPEC  .AND.
     :                      ATTVC__CAT1(ATLOOP) .EQ. PNAME)
     :                      THEN

                           DO IDLOOP = 1, NIDS__CAT1
                              IF (IDVAL__CAT1(IDLOOP) .EQ.
     :                          ATTID__CAT1(ATLOOP)  .AND.
     :                          IDPRN__CAT1(IDLOOP) .EQ. CI) THEN

                                 FOUND = .TRUE.

                              END IF
                           END DO

                        END IF
                     END DO

                     IF (FOUND) THEN
                        IF (PNAME .NE. ' ') THEN
                           LPNAME = CHR_LEN(PNAME)
                        ELSE
                           LPNAME = 1
                        END IF

                        CALL CHR_PUTC ('_', PNAME, LPNAME)
                        CALL CHR_PUTI (EXTN, PNAME, LPNAME)
                     END IF

*
*                   Add the keyword as a new parameter to the list of
*                   parameters.
                     CALL CAT1_ADDPR (CI, PNAME, PDTYPE, PCSIZE, PDIM,
     :                 PSIZE, PDATE, PUNIT, PXFMT, PPDISP, PCOMM,
     :                 PVALUE, QI, STATUS)
                     IF ( STATUS .EQ. CAT__DUPNM .AND. FOUND ) THEN

*                   Still not unique. Try once more with a unique integer.
                        CALL ERR_ANNUL( STATUS )
                        LPNAME = CHR_LEN(PNAME) - 4
                        CALL CHR_PUTI (UNIQUE, PNAME, LPNAME)
                        UNIQUE = UNIQUE + 1
                        CALL CAT1_ADDPR (CI, PNAME, PDTYPE, PCSIZE,
     :                                   PDIM ,PSIZE, PDATE, PUNIT,
     :                                   PXFMT, PPDISP, PCOMM,PVALUE,
     :                                   QI, STATUS)
                     END IF

*
*                   If ok then increment the number of parameters.

                     IF (STATUS .EQ. CAT__OK) THEN
                        PARS = PARS + 1
                     END IF
                  END IF

*
*                If the keyword is TFIELDS then attempt to get the
*                number of columns.

                  IF (KWORD .EQ. 'TFIELDS') THEN
                     IF (COLS .LE. 0) THEN
                        LSTAT = CAT__OK
                        CALL CHR_CTOI (KVALUE, COLS, LSTAT)

                        IF (LSTAT .NE. CAT__OK) THEN
                           COLS = -1
                        END IF
                     ELSE
                        CALL CAT1_MSG (' ', 'Duplicate TFIELDS '/
     :                    /'keyword ignored.', STATUS)
                     END IF
                  END IF

*
*                If the keyword is NAXIS2 then attempt to get the
*                number of rows.  Any negative value obtained is
*                forced to zero

                  IF (KWORD .EQ. 'NAXIS2') THEN
                     IF (.NOT. GTROWS) THEN
                        GTROWS = .TRUE.

                        LSTAT = CAT__OK
                        CALL CHR_CTOI (KVALUE, ROWS, LSTAT)
                        ROWS = MAX(ROWS, 0)

                        IF (LSTAT .NE. CAT__OK) THEN
                           ROWS = 0
                        END IF
                     ELSE
                        CALL CAT1_MSG (' ', 'Duplicate NAXIS2 '/
     :                    /'keyword ignored.', STATUS)
                     END IF
                  END IF

               END IF
            END DO

*
*          If the number of columns was not determined successfully
*          then set the status and report an error.

            IF (COLS .LE. 0) THEN
               STATUS = CAT__INVCD

               CALL CAT1_ERREP ('CAT3_GTPAR_TFL', 'Mandatory keyword '/
     :           /'TFIELDS is missing or present with an illegal '/
     :           /'value.', STATUS)
            END IF

*
*          If the number of rows was not determined successfully
*          then set the status and report an error.  If the number of
*          rows was zero then report a warning.

            IF (GTROWS) THEN
               IF (ROWS .LE. 0) THEN
                  CALL CAT1_MSG (' ', 'The number of rows (keyword '/
     :              /'NAXIS2) is zero.', STATUS)
               END IF

            ELSE
               STATUS = CAT__INVCD

               CALL CAT1_ERREP ('CAT3_GTPAR_NA2', 'Mandatory '/
     :           /'keyword NAXIS2 (number of rows) is missing.',
     :           STATUS)
            END IF

         ELSE

*
*          Failed to obtain the number of keywords; set the status
*          and report an error.

            STATUS = CAT__INVCD
            CALL CAT3_FITER ('CAT3_GTPAR_NKW', 'Failed to determine '/
     :        /'the number of keywords.', FITSTT, STATUS)
         END IF

      END IF

      END
