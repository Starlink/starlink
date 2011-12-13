      SUBROUTINE CAT3_FINCR (CI, STATUS)
*+
*  Name:
*     CAT3_FINCR
*  Purpose:
*     Finish the creation of a catalogue held as a binary FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_FINCR (CI; STATUS)
*  Description:
*     Finish the creation of a binary FITS table to hold a CAT
*     catalogue.
*
*     The FITS table is held in its own file.  The primary header and a
*     header for the binary table extension are created.  The columns
*     and parameters are written as FITS keywords.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get the array element corresponding to the catalogue.
*     If ok then
*       Obtain the Fortran unit number for accessing the FITS table.
*       Write the mandatory parameters to the primary header.
*       Define the structure of the primary (that is, there isn't one).
*       Create an extension to hold the binary table.
*       Set the number for the extension in the common block.
*       Write the mandatory FITS binary tables keywords corresponding to
*       the columns in the CAT catalogue.
*       Write the keywords to hold null values for integer columns.
*       Write the FITS keywords specific to CAT columns.
*       If the table is ordered (sorted) write the standard FITS
*       keyword defining the column on which it is sorted.
*       Define the structure of the binary FITS table.
*       Set the null values for Integer columns.
*       Write the CAT parameters as keywords in this extension.
*       Reset access to the textual information in the header.
*     end if
*  Implementation Deficiencies:
*     Only the absolute minimum information necessary to define the
*     FITS binary table is saved for each CAT parameter and column.
*     Thus, when (or if) the FITS table is reconsituted as a CAT
*     catalogue some information will have been lost.
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
*  History:
*     5/8/93   (ACD): Original version.
*     16/9/93  (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     31/1/94  (ACD): Added null values for integer columns.
*     4/2/94   (ACD): Added handling of vector columns.
*     18/2/94  (ACD): Fixed bug in handling vector columns of type
*        CHARACTER.
*     21/2/94  (ACD): Removed some unused variables.
*     7/7/94   (ACD): Added initialisation of FITS specific details.
*     9/7/94   (ACD): Added saving CAT comments and preferential
*        display flags for columns as FITS keywords.
*     11/7/94  (ACD): Made CAT BYTE columns correspond to FITS WORD
*        columns (because FITS binary tables have no signed byte
*        type).
*     13/7/94  (ACD): Made to save the external display format as the
*        Standard FITS column keyword TDISPn.
*     8/9/94   (ACD): Added standard keyword holding the name of the
*        column on which sorted tables are sorted.
*     23/9/94  (ACD): Added support for textual information.
*     25/10/94 (ACD): Changed the null value for INTEGER columns from
*        the appropriate Starlink/HDS bad value to 1 + the Starlink
*        value.  This kludge is necessary in order to avoid an error
*        in converting the value to and from a CHARACTER string on a
*        DEC alpha running OSF.
*     6/3/95   (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     10/3/95  (ACD): Corrected the defined size of variables to hold
*        values read from FITS CHARACTER keywords.
*     11/4/95  (ACD): Changed the name of the null identifier.
*     14/8/96  (ACD): Added external reference to routine FITB to
*        force initialisation.
*     28/3/97  (ACD): Changed the definition of column and parameter
*        names to use the correct parametric contstant (CAT__SZCMP).
*     4/6/98   (ACD): Changed all occurences of 'StarBase' to 'CAT'.
*     22/12/99 (ACD): Prevent single quotes being written to parameter
*        values and column or parameter comments.
*     1/9/00   (ACD): Added generation of the external format if it has
*        not already been set.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end specific comments.
*  Global Variables:
      INCLUDE 'CAT3_FIT_CMN'      ! FITS back-end common block.
*  Arguments Given:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS    ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER FITOK     ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  QI,       ! Parameter identifier.
     :  FI,       ! Column (or field) identifier.
     :  QCOUNT,   ! Number of the current parameter.
     :  FCOUNT,   !   "    "   "     "    columns.
     :  CIELM,    ! Common block array element for the catalogue.
     :  FITUNT,   ! Fortran unit number to access FITS file.
     :  FITSTT,   ! FITSIO status.
     :  NAXES(1)  ! Dummy array for FITS array size.
      INTEGER
     :  TSIZE,    ! Total size of field in CHARACTER array (bytes).
     :  LOOP,     ! Loop index.
     :  KEYLEN,   ! Length of KEYNUL (excl. trail. blanks).
     :  ERRLEN,   !   "    "  ERRMSG ( "  .   "  .   "   ).
     :  LBFORM,   !   "    "  BFORM  ( "  .   "  .   "   ).
     :  NULVAL,   ! Null value for byte, word and integer columns.
     :  FDTYPS(CAT__MXCOL),   ! CAT data types for all columns.
     :  STLOOP,   ! Current element of string.
     :  LENGTH,   ! Length of string (excl. trail. blanks).
     :  LQNAME    !   "    "  QNAME  ( "  .   "  .   "   ).
      INTEGER
     :  SFXFMT,   ! Size of external format for character column.
     :  LFXFMT    ! Length of FXTFMT (excl. trail. blanks).
      CHARACTER
     :  CNAME*(CAT__SZCNM),   ! Catalogue name.
     :  TTYPE(CAT__MXCOL)*15, ! FITS table column name.
     :  TFORM(CAT__MXCOL)*(CAT3__SZCVL), ! FITS table column data type.
     :  TUNIT(CAT__MXCOL)*(CAT3__SZCVL), ! FITS table column units.
     :  BFORM*10,             ! Current FITS table column data type.
     :  ERRMSG*75,  ! Error message.
     :  KEYNUL*9,   ! Name keyword for integer column null value.
     :  KEYNAM*9,   ! Name of FITS keyword.
     :  PRFCOM*68,  ! Comments for preferential display flag keyword.
     :  FCOMMS(CAT__MXCOL)*(CAT__SZCOM), ! Comments for all columns.
     :  FXTFMS(CAT__MXCOL)*(CAT__SZEXF)  ! External format for all columns.
      LOGICAL
     :  MORE,       ! Flag: more parameters or columns to access?
     :  INTCOL,     ! Flag: is the current column byte, word or integer?
     :  FPRFSS(CAT__MXCOL),  ! Preferential display flags for all columns.
     :  REMQOT      ! Flag: quote removed from current parameter value?

*
*    The following variables represent the attributes of the current
*    column (or field).

      INTEGER
     :  FCI,         ! Parent catalogue.
     :  FGENUS,      ! Genus.
     :  FDTYPE,      ! Data type.
     :  FCSIZE,      ! Size if a character string.
     :  FDIMS,       ! Dimensionality.
     :  FSIZEA(10),  ! Size of each array dimension.
     :  FNULL,       ! Null flag.
     :  FORDER       ! Order.
      CHARACTER
     :  FNAME*(CAT__SZCMP),    ! Name.
     :  FEXPR*(CAT__SZEXP),    ! Defining expression.
     :  FXCEPT*(CAT__SZVAL),   ! Exception value.
     :  FUNITS*(CAT__SZUNI),   ! Units.
     :  FXTFMT*(CAT__SZEXF),   ! External format.
     :  FCOMM*(CAT__SZCOM)     ! Comments.
      DOUBLE PRECISION
     :  FSCALEF,     ! Scale factor.
     :  FZEROP,      ! Zero point.
     :  FDATE        ! Modification date.
      LOGICAL
     :  FPRFDS       ! Preferential display flag.

*
*    Additional variables pertaining to a single column.

      INTEGER
     :  FITYPE       ! FITS data type.

*
*    The following variables represent the attributes of the current
*    parameter.

      INTEGER
     :  QCI,         ! Parent catalogue.
     :  QDTYPE,      ! Data type.
     :  QCSIZE,      ! Size if a character string.
     :  QDIMS,       ! Dimensionality.
     :  QSIZEA(10)   ! Size of each array dimension.
      CHARACTER
     :  QNAME*(CAT__SZCMP),    ! Name.
     :  QUNITS*(CAT__SZUNI),   ! Units.
     :  QXTFMT*(CAT__SZEXF),   ! External format.
     :  QCOMM*(CAT__SZCOM),    ! Comments.
     :  QVALUE*(CAT__SZVAL)    ! Value.
      LOGICAL
     :  QPRFDS      ! Preferential display flag.
      DOUBLE PRECISION
     :  QDATE        ! Modification date.

*.

C     write(17, 999) status
C 999 format(1x, 'cat3-fincr on entry: ', i10 )

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Attempt to get the array element corresponding to the catalogue.

         CALL CAT1_CIELM (CI, CIELM, STATUS)

         IF (STATUS .EQ. CAT__OK) THEN

*
*          Obtain the Fortran unit number for accessing the FITS table.

            FITUNT = FUNT__CAT3(CIELM)

*
*          Write the mandatory keywords for the primary header.

            NAXES(1) = 0
            FITSTT = 0

            CALL FTPHPR (FITUNT, .TRUE., 8, 0, NAXES, 0, 1, .TRUE.,
     :        FITSTT)
C           write(17, 1000) 'FTPHPR', fitstt, status
C1000       format(1x, 'after ', a, 3x, 'fitstt, status: ', i10, i10 )
            IF (FITSTT .NE. FITOK) THEN
               STATUS = CAT__ERROR
               CALL CAT3_FITER ('CAT3_FINCR_PHD',
     :           'Failed to write primary header.', FITSTT, STATUS)
            END IF

*
*          Define the structure of the primary array.

            CALL FTPDEF (FITUNT, 8, 0, NAXES, 0, 1, FITSTT)
C           write(17, 1000) 'FTPDEF', fitstt, status
            IF (FITSTT .NE. FITOK) THEN
               STATUS = CAT__ERROR
               CALL CAT3_FITER ('CAT3_FINCR_SPA',
     :           'Failed to define structure of the FITS primary '/
     :           /'array.', FITSTT, STATUS)
            END IF

*
*          Create an extension to hold the binary table.

            CALL FTCRHD (FITUNT, FITSTT)
C           write(17, 1000) 'FTCRHD', fitstt, status
            IF (FITSTT .NE. FITOK) THEN
               STATUS = CAT__ERROR
               CALL CAT3_FITER ('CAT3_FINCR_GET',
     :           'Failed to create extension for the FITS table.',
     :           FITSTT, STATUS)
            END IF

*
*          Set the number for the extension in the common block.

            EXTN__CAT3(CIELM) = 1

*
*          Construct the FITS binary tables keywords corresponding to
*          the CAT columns.

            FCOUNT = 0
            MORE = .TRUE.

            DO WHILE (MORE)
               FCOUNT = FCOUNT + 1

               CALL CAT_TNDNT (CI, CAT__FITYP, FCOUNT, FI, STATUS)

               IF (STATUS .EQ. CAT__OK  .AND.  FI .NE. CAT__NOID) THEN
                  CALL CAT_CINQ (FI, 10, FCI, FNAME, FGENUS, FEXPR,
     :              FDTYPE, FCSIZE, FDIMS, FSIZEA, FNULL, FXCEPT,
     :              FSCALEF, FZEROP, FORDER, FUNITS, FXTFMT, FPRFDS,
     :              FCOMM, FDATE, STATUS)

C                 print1010, fcount, fi, fname(1:10), fdtype,
C    :              fcsize, funits(1:10)
C1010             format(1x, 'fcount, fi, fname, fdtype, fcsize, ',
C    :              'funits: ', i3, i4, a10, i4, i4, a10)

*
*                Generate the external format if it has not already been
*                set.  Note that the DOUBLE PRECISION format should be
*                able represent angles to an accuracy of 0.01 second of
*                arc.

                  IF (FXTFMT .EQ. ' ') THEN
                     IF (FDTYPE .EQ. CAT__TYPEB) THEN
                        FXTFMT = 'I4'
                     ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                        FXTFMT = 'I6'
                     ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                        FXTFMT = 'I6'
                     ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                        FXTFMT = 'E12.3'
                     ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                        FXTFMT = 'D19.10'
                     ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                        FXTFMT = 'L5'
                     ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                        SFXFMT = FCSIZE + 2

                        LFXFMT = 0
                        FXTFMT = ' '

                        CALL CHR_PUTC ('A', FXTFMT, LFXFMT)
                        CALL CHR_PUTI (SFXFMT, FXTFMT, LFXFMT)
                     END IF

                     LFXFMT = CHR_LEN(FXTFMT)
                     CALL CAT_TATTC (FI, 'EXFMT', FXTFMT(1 : LFXFMT),
     :                 STATUS)
                  END IF

*
*                Strip any single quotes from the column comments.

                  IF (FCOMM .NE. ' ') THEN
                     LENGTH = CHR_LEN(FCOMM)

                     DO STLOOP = 1, LENGTH
                        IF (FCOMM(STLOOP : STLOOP) .EQ. '''') THEN
                           FCOMM(STLOOP : STLOOP) = '_'
                        END IF
                     END DO
                  END IF

*
*                Reset the 'type of null value' attribute to indicate
*                that null values are explicitly defined in the
*                catalogue.

                  CALL CAT_TATTI (FI, 'NULL', CAT__NULLS, STATUS)

*
*                Only some of the attributes for each column are saved;
*                in principle they could all be saved.

                  TTYPE(FCOUNT) = FNAME
                  FDTYPS(FCOUNT) = FDTYPE
                  FCOMMS(FCOUNT) = FCOMM
                  FPRFSS(FCOUNT) = FPRFDS
                  FXTFMS(FCOUNT) = FXTFMT

*
*                The following section assembles the FITS format
*                descriptors for the columns in the binary table.
*                The format of these column descriptors is as follows:
*
*                non-character scalar:   I   (for INTEGER, etc.)
*                non-character vector:  rI
*
*                character scalar:      wAw
*                character vector:      zAw
*
*                where:
*                    r = vector size,
*                    w = character size,
*                    z = w*r
*
*                Examples:
*                 9A9  - character scalar of size 9,
*                 27A9 - character vector of 3 elements, each of size 9.

                  BFORM = ' '
                  LBFORM = 0

                  IF (FDTYPE .NE. CAT__TYPEC) THEN
                     IF (FDIMS .EQ. CAT__VECTR) THEN
                        CALL CHR_PUTI (FSIZEA(1), BFORM, LBFORM)

                     END IF

                  ELSE
                     IF (FDIMS .EQ. CAT__VECTR) THEN
                        TSIZE = FSIZEA(1) * FCSIZE
                        CALL CHR_PUTI (TSIZE, BFORM, LBFORM)

                     ELSE
                        CALL CHR_PUTI (FCSIZE, BFORM, LBFORM)

                     END IF
                  END IF

                  IF (FDTYPE .EQ. CAT__TYPEB) THEN

*
*                   Note that CAT BYTE columns are stored as FITS
*                   WORD columns because FITS binary tables do not
*                   support a signed byte data type.

                     CALL CHR_PUTC ('I', BFORM, LBFORM)
                     FITYPE = CAT3__FTYPI

                  ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                     CALL CHR_PUTC ('I', BFORM, LBFORM)
                     FITYPE = CAT3__FTYPI

                  ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                     CALL CHR_PUTC ('J', BFORM, LBFORM)
                     FITYPE = CAT3__FTYPJ

                  ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                     CALL CHR_PUTC ('E', BFORM, LBFORM)
                     FITYPE = CAT3__FTYPE

                  ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                     CALL CHR_PUTC ('D', BFORM, LBFORM)
                     FITYPE = CAT3__FTYPD

                  ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                     CALL CHR_PUTC ('L', BFORM, LBFORM)
                     FITYPE = CAT3__FTYPL

                  ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                     CALL CHR_PUTC ('A', BFORM, LBFORM)
                     CALL CHR_PUTI (FCSIZE, BFORM, LBFORM)
                     FITYPE = CAT3__FTYPA

                  END IF

                  TFORM(FCOUNT) = BFORM

C                 print3000, fcount, fdims, fsizea(1), fdtype,
C    :              bform
C3000             format(1x, 'CAT3_FINCR' /
C    :              1x, 'fcount, fdims, fsizea(1), fdtype: ', i5, i5,
C    :              i5, i5 /
C    :              1x, 'bform: ', a10 / )

                  TUNIT(FCOUNT) = FUNITS

C                 print1011, ttype(fcount), tform(fcount),
C    :              tunit(fcount)
C1011             format(1x, a, 1x, a, 1x, a)

*
*                Add the FITS specific details to the common block.
*                These details are needed in order to subsequently
*                access the catalogue.

                  COLNO__CAT3(FI) = FCOUNT
                  FTYPE__CAT3(FI) = FITYPE
                  SFLAG__CAT3(FI) = .FALSE.
                  SCALE__CAT3(FI) = 1.0D0
                  ZERO__CAT3(FI) = 0.0D0

               ELSE
                  MORE = .FALSE.
                  FCOUNT = FCOUNT - 1

               END IF

            END DO

*
*          Write the FITS binary table extension keywords.

            CALL CAT_TIQAC (CI, 'NAME', CNAME, STATUS)

            CALL FTPHBN (FITUNT, 0, FCOUNT, TTYPE, TFORM, TUNIT,
     :        CNAME, 0, FITSTT)
C           write(17, 1000) 'FTPHBN', fitstt, status
            IF (FITSTT .NE. FITOK) THEN
               STATUS = CAT__ERROR
               CALL CAT3_FITER ('CAT3_FINCR_WBT',
     :           'Failed to write FITS binary table keywords.', FITSTT,
     :           STATUS)
            END IF

*
*          Write the keywords to hold null values for integer columns.

            DO LOOP = 1, FCOUNT
               INTCOL = .FALSE.

               IF (FDTYPS(LOOP) .EQ. CAT__TYPEB) THEN

*
*                Note that for CAT BYTE columns the WORD default null
*                value is used because CAT byte columns are stored as
*                FITS WORD columns.

                  INTCOL = .TRUE.
                  NULVAL = CAT1__DNULW

               ELSE IF (FDTYPS(LOOP) .EQ. CAT__TYPEW) THEN
                  INTCOL = .TRUE.
                  NULVAL = CAT1__DNULW

               ELSE IF (FDTYPS(LOOP) .EQ. CAT__TYPEI) THEN
                  INTCOL = .TRUE.
                  NULVAL = CAT1__DNULI + 1

               END IF

               IF (INTCOL) THEN
                  KEYNUL = ' '
                  KEYLEN = 0

                  CALL CHR_PUTC ('TNULL', KEYNUL, KEYLEN)
                  CALL CHR_PUTI (LOOP,  KEYNUL, KEYLEN)

                  CALL FTPKYJ (FITUNT, KEYNUL(1 : KEYLEN), NULVAL,
     :              'Null value.', FITSTT)
                  IF (FITSTT .NE. FITOK) THEN
                     STATUS = CAT__ERROR

                     ERRMSG = ' '
                     ERRLEN = 0

                     CALL CHR_PUTC ('Failure creating keyword ',
     :                 ERRMSG, ERRLEN)
                     CALL CHR_PUTC (KEYNUL(1 : KEYLEN), ERRMSG, ERRLEN)
                     CALL CHR_PUTC (' in FITS binary table.',
     :                 ERRMSG, ERRLEN)

                     CALL CAT3_FITER ('CAT3_FINCR_KPD',
     :                 ERRMSG(1 : ERRLEN), FITSTT, STATUS)
                  END IF
               END IF
            END DO

*
*          Write CAT external format as the standard FITS keyword
*          TDISPn and also the CAT specific FITS keywords to hold the
*          comments and preferential display flag for each column.

            DO LOOP = 1, FCOUNT

*
*             First the standard keyword TDISPn for the external
*             display format.

               KEYNAM = ' '
               KEYLEN = 0

               CALL CHR_PUTC ('TDISP', KEYNAM, KEYLEN)
               CALL CHR_PUTI (LOOP, KEYNAM, KEYLEN)

               CALL FTPKYS (FITUNT, KEYNAM(1 : KEYLEN), FXTFMS(LOOP),
     :           'External display format.', FITSTT)
               IF (FITSTT .NE. FITOK) THEN
                  STATUS = CAT__ERROR

                  ERRMSG = ' '
                  ERRLEN = 0

                  CALL CHR_PUTC ('Failure creating keyword ',
     :              ERRMSG, ERRLEN)
                  CALL CHR_PUTC (KEYNAM(1 : KEYLEN), ERRMSG, ERRLEN)
                  CALL CHR_PUTC (' in FITS binary table.',
     :              ERRMSG, ERRLEN)

                  CALL CAT3_FITER ('CAT3_FINCR_KNL',
     :              ERRMSG(1 : ERRLEN), FITSTT, STATUS)
               END IF

*
*             Then the preferential display flag.

               KEYNAM = ' '
               KEYLEN = 0

               CALL CHR_PUTC ('TPRFD', KEYNAM, KEYLEN)
               CALL CHR_PUTI (LOOP, KEYNAM, KEYLEN)

               IF (FPRFSS(LOOP) ) THEN
                  PRFCOM = 'Column chosen for preferential display.'
               ELSE
                  PRFCOM = 'Column rejected for preferential display.'
               END IF

               CALL FTPKYL (FITUNT, KEYNAM(1 : KEYLEN), FPRFSS(LOOP),
     :           PRFCOM, FITSTT)
               IF (FITSTT .NE. FITOK) THEN
                  STATUS = CAT__ERROR

                  ERRMSG = ' '
                  ERRLEN = 0

                  CALL CHR_PUTC ('Failure creating keyword ',
     :              ERRMSG, ERRLEN)
                  CALL CHR_PUTC (KEYNAM(1 : KEYLEN), ERRMSG, ERRLEN)
                  CALL CHR_PUTC (' in FITS binary table.',
     :              ERRMSG, ERRLEN)

                  CALL CAT3_FITER ('CAT3_FINCR_KCM',
     :              ERRMSG(1 : ERRLEN), FITSTT, STATUS)
               END IF

*
*             Finally the comments.

               KEYNAM = ' '
               KEYLEN = 0

               CALL CHR_PUTC ('TCOMM', KEYNAM, KEYLEN)
               CALL CHR_PUTI (LOOP, KEYNAM, KEYLEN)

               CALL FTPKYS (FITUNT, KEYNAM(1 : KEYLEN), FCOMMS(LOOP),
     :           ' ', FITSTT)
               IF (FITSTT .NE. FITOK) THEN
                  STATUS = CAT__ERROR

                  ERRMSG = ' '
                  ERRLEN = 0

                  CALL CHR_PUTC ('Failure creating keyword ',
     :              ERRMSG, ERRLEN)
                  CALL CHR_PUTC (KEYNAM(1 : KEYLEN), ERRMSG, ERRLEN)
                  CALL CHR_PUTC (' in FITS binary table.',
     :              ERRMSG, ERRLEN)

                  CALL CAT3_FITER ('CAT3_FINCR_KNL',
     :              ERRMSG(1 : ERRLEN), FITSTT, STATUS)
               END IF
            END DO

*
*          If the table is ordered (sorted) write the standard FITS
*          keyword defining the column on which it is sorted.

            CALL CAT3_WTORD (CI, FITUNT, STATUS)

*
*          Define the structure of the table.

            CALL FTBDEF (FITUNT, FCOUNT, TFORM, 0, 0, FITSTT)
C           write(17, 1000) 'FTBDEF', fitstt, status
            IF (FITSTT .NE. FITOK) THEN
               STATUS = CAT__ERROR
               CALL CAT3_FITER ('CAT3_FINCR_SBT',
     :           'Failed to define structure of FITS binary table.',
     :           FITSTT, STATUS)
            END IF

*
*          Set the null values for integer columns.

            DO LOOP = 1, FCOUNT
               INTCOL = .FALSE.

               IF (FDTYPS(LOOP) .EQ. CAT__TYPEB) THEN

*
*                Note that for CAT BYTE columns the WORD default null
*                value is used because CAT byte columns are stored as
*                FITS WORD columns.

                  INTCOL = .TRUE.
                  NULVAL = CAT1__DNULW

               ELSE IF (FDTYPS(LOOP) .EQ. CAT__TYPEW) THEN
                  INTCOL = .TRUE.
                  NULVAL = CAT1__DNULW

               ELSE IF (FDTYPS(LOOP) .EQ. CAT__TYPEI) THEN
                  INTCOL = .TRUE.
                  NULVAL = CAT1__DNULI + 1

               END IF

               IF (INTCOL) THEN
                  CALL FTTNUL (FITUNT, LOOP, NULVAL, FITSTT)
                  IF (FITSTT .NE. FITOK) THEN
                     STATUS = CAT__ERROR
                     CALL CAT3_FITER ('CAT3_FINCR_INL',
     :                 'Failure defining null value for FITS column.',
     :                 FITSTT, STATUS)
                  END IF
               END IF
            END DO


*
*          Write the CAT parameters for the catalogues as FITS
*          keywords in this extension.

            QCOUNT = 0
            MORE = .TRUE.

            DO WHILE (MORE)
               QCOUNT = QCOUNT + 1

               CALL CAT_TNDNT (CI, CAT__QITYP, QCOUNT, QI, STATUS)

               IF (STATUS .EQ. CAT__OK  .AND.  QI .NE. CAT__NOID) THEN
                  CALL CAT_PINQ (QI, 10, QCI, QNAME, QDTYPE, QCSIZE,
     :              QDIMS, QSIZEA, QUNITS, QXTFMT, QPRFDS, QCOMM,
     :              QVALUE, QDATE, STATUS)

C                 write(17, 1012) qcount, qname(1:10), qvalue(1:10),
C    :              qcomm(1:10)
C1012             format(1x, 'qcount, qname, qvalue, qcomm',
C    :              i5, a10, 1x, a10, 1x, a10)

*
*                Strip any single quotes from the parameter value and
*                comments.  A message is reported if the value is
*                modified.

                  IF (QVALUE .NE. ' ') THEN
                     LENGTH = CHR_LEN(QVALUE)
                     REMQOT = .FALSE.

                     DO STLOOP = 1, LENGTH
                        IF (QVALUE(STLOOP : STLOOP) .EQ. '''') THEN
                           QVALUE(STLOOP : STLOOP) = '_'
                           REMQOT = .TRUE.
                        END IF
                     END DO

                     IF (REMQOT) THEN
                        ERRMSG = ' '
                        ERRLEN = 0

                        CALL CHR_PUTC ('Quotation character(s) '/
     :                    /'removed from the value of parameter ',
     :                    ERRMSG, ERRLEN)

                        IF (QNAME .NE. ' ') THEN
                           LQNAME = CHR_LEN(QNAME)
                        ELSE
                           LQNAME = 1
                        END IF

                        CALL CHR_PUTC (QNAME(1 : LQNAME), ERRMSG,
     :                    ERRLEN)
                        CALL CHR_PUTC ('.', ERRMSG, ERRLEN)

                        CALL CAT1_MSG (' ', ERRMSG(1 :ERRLEN), STATUS)
                     END IF
                  END IF

                  IF (QCOMM .NE. ' ') THEN
                     LENGTH = CHR_LEN(QCOMM)

                     DO STLOOP = 1, LENGTH
                        IF (QCOMM(STLOOP : STLOOP) .EQ. '''') THEN
                           QCOMM(STLOOP : STLOOP) = '_'
                        END IF
                     END DO
                  END IF

C
C                Again the following is the minimum kludge necessary to
C                get the FITS back-end working.  In principle it is
C                possible to store more information for each parameter
C                than its name, value and comments.

                  CALL FTPKYS (FITUNT,  QNAME, QVALUE, QCOMM, FITSTT)
C                 write(17, 1000) 'FTPKYS', fitstt, status
                  IF (FITSTT .NE. FITOK) THEN
                     STATUS = CAT__ERROR
                     CALL CAT3_FITER ('CAT3_FINCR_WKW',
     :                 'Failed to write FITS keyword.', FITSTT, STATUS)
                  END IF

               ELSE
                  MORE = .FALSE.
                  QCOUNT = QCOUNT - 1


               END IF

            END DO

*
*          Reset access to the textual information in the header.

            CALL CAT3_RSTXT (CI, STATUS)

         END IF

C        write(17, 1001) fcount, qcount
C1001    format(1x, 'fcount, qcount: ', i10, i10)

      END IF

C     write(17, 998) status
C 998 format(1x, 'cat3-fincr on exit: ', i10 )

      END
