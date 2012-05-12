      SUBROUTINE CAT3_GTCLB (FITUNT, NXCOL, SKIP, FNAME, FDTYPE, FCSIZE,
     :  FDIM, FSIZE, FUNIT, FSCALE, FZERO, FXCEPT, FXFMT, FPDISP, FCOMM,
     :  FITYPE, SFLAG, STATUS)
*+
*  Name:
*     CAT3_GTCLB
*  Purpose:
*     Read details of a column from a binary FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_GTCLB (FITUNT, NXCOL; SKIP, FNAME, FDTYPE, FCSIZE, FDIM,
*       FSIZE, FUNIT, FSCALE, FZERO, FXCEPT, FXFMT, FPDISP, FCOMM,
*       FITYPE, SFLAG; STATUS)
*  Description:
*     Read details of a column from a binary FITS table.
*  Arguments:
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     NXCOL  =  INTEGER (Given)
*        Sequence number for the column to be accessed in the table.
*     SKIP  =  LOGICAL (Returned)
*        Flag indicating whether the column is to be skipped because
*        it contains an unsupported feature.  It is coded as follows:
*        .TRUE.  - skip the column,
*        .FALSE. - include the column.
*     FNAME  =  CHARACTER*(*) (Returned)
*        Name of the column.
*     FDTYPE  =  INTEGER (Returned)
*        CAT Data type of the column.
*     FCSIZE  =  INTEGER (Returned)
*        Size of columns of type character.
*     FDIM  =  INTEGER (Returned)
*        Dimensionality of the column.  Set to CAT__SCALR for a scalar
*        and CAT__VECTR for a vector (1-dimensional array).
*     FSIZE  =  INTEGER (Returned)
*        Number of elements in the column if it is an array.
*     FUNIT  =  CHARACTER*(*) (Returned)
*        Units of the column.
*     FSCALE  =  DOUBLE PRECISION (Returned)
*        Scale factor for the column.
*     FZERO  =  DOUBLE PRECISION (Returned)
*        Zero point for the column.
*     FXCEPT  =  CHARACTER*(*) (Returned)
*        Exception value for the column.
*     FXFMT  =  CHARACTER*(*) (Returned)
*        External format for the column.
*     FPDISP  =  LOGICAL (Returned)
*        Preferential display flag for the column.
*     FCOMM  =  CHARACTER*(*) (Returned)
*        Comments for the column.
*     FITYPE  =  INTEGER (Returned)
*        Integer code for the FITS data type.
*     SFLAG  =  LOGICAL (Returned)
*        Flag indicating whether or not the FITS column is scaled,
*        coded as follows:
*        .TRUE.  - the column is scaled,
*        .FALSE. - the column is not scaled.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Get the details of the column from the FITS table.
*     If the FITS table is ok and the name is not blank then
*       If the repeat count is greater than zero then
*         Set the 'skip column' flag to indicate it is not to be
*         skipped.
*         Replace the default values for each column with those read
*         from the FITS table.
*         Determine the dimensionality and size of the column.
*         Convert the FITS data type to the CAT data type, setting
*         the 'skip column' flag if the column has a FITS data type
*         which is not supported by CAT.
*         Convert the FITS null value into the CAT exception value.
*         Check if the column is scaled, and adjust the CAT data type
*         if necessary.
*         Attempt to get any comments for the column.
*         Attempt to get the preferential display flag for the column.
*       else
*         Set the 'skip column' flag to true.
*       end if
*       If the column is to be skipped then
*         Report a message.
*       end if
*       If any error occurred then
*         Report an error.
*       end if
*     else
*       Set the 'skip column' flag to true.
*       If the FITS status is not ok then
*         Report an error.
*       end if
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/8/93   (ACD): Original version.
*     24/1/94  (ACD): Modified error reporting.
*     3/2/94   (ACD): Added handling arrays.
*     16/6/94  (ACD): Fixed bug in handling binary columns and added
*        skipping of columns which contain unsupported features.  Also
*        made to skip columns which contain no data (the FITS repeat
*        count is less than 1).
*     5/7/94   (ACD): Added support for FITS complex columns (both REAL
*        and DOUBLE PRECISION) and proper handling of scaled columns.
*     9/7/94   (ACD): Added obtaining column comments and preferential
*        display flags from the FITS header.
*     10/3/95  (ACD): Corrected the defined size of variables to hold
*        values read from FITS CHARACTER keywords.
*     4/6/98   (ACD): Improved the error reporting.
*     27/10/99 (ACD): Modified for use with CFITSIO.  The determination
*        of the dimensionality of CHARACTER columns had to be revised.
*     15/12/99 (ACD): Fixed a bug in adopting the comments from the
*        TTYPEn keyword.
*     2012-05-11 (TIMJ):
*        Add K
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end symbolic constants.
*  Arguments Given:
      INTEGER
     :  FITUNT,
     :  NXCOL
*  Arguments Returned:
      LOGICAL
     :  SKIP,
     :  SFLAG
      CHARACTER
     :  FNAME*(*),
     :  FUNIT*(*),
     :  FXCEPT*(*),
     :  FXFMT*(*),
     :  FCOMM*(*)
      INTEGER
     :  FDTYPE,
     :  FCSIZE,
     :  FDIM,
     :  FSIZE,
     :  FITYPE
      DOUBLE PRECISION
     :  FSCALE,
     :  FZERO
      LOGICAL
     :  FPDISP
*  Status:
      INTEGER STATUS   ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
       INTEGER FITOK          ! FITSIO Success status.
       PARAMETER (FITOK = 0)
*  Local Variables:
      CHARACTER
     :  ERRMSG*75,            ! Error message.
     :  MESSGE*75,            ! Message to be reported to the user.
     :  SKPMSG*75,            ! Reason for skipping a column.
     :  KEYNAM*15,            ! Name of a FITS keyword.
     :  KEYVAL*(CAT3__SZCVL), ! Value of a FITS keyword.
     :  KEYCOM*70             ! Comments for a FITS keyword.
      INTEGER
     :  FITSTT,    ! FITSIO running status.
     :  LSTAT      ! Local ADAM status.
      INTEGER
     :  LXCEPT,    ! Length of FXCEPT (excl. trail. blanks).
     :  ERRLEN,    !   "    "  ERRMSG ( "  .   "  .   "   ).
     :  LTTYPE,    !   "    "  TTYPE  ( "  .   "  .   "   ).
     :  LDATYP,    !   "    "  DATYPE ( "  .   "  .   "   ).
     :  LTDISP,    !   "    "  TDISP  ( "  .   "  .   "   ).
     :  LMESSG,    !   "    "  MESSGE ( "  .   "  .   "   ).
     :  LSKPM,     !   "    "  SKPMSG ( "  .   "  .   "   ).
     :  KEYLEN,    !   "    "  KEYNAM ( "  .   "  .   "   ).
     :  LKEYVL     !   "    "  KEYVAL ( "  .   "  .   "   ).
      LOGICAL
     :  KEYVLL     ! Logical value for a FITS keyword.

*
*    Attributes for a column, as read from the FITS ASCII file.

      CHARACTER
     :  TTYPE*15,             ! Name.
     :  TUNIT*(CAT3__SZCVL),  ! Units.
     :  DATYPE*(CAT3__SZCVL), ! FITS data type of the column.
     :  TDISP*(CAT3__SZCVL)   ! External format.
      INTEGER
     :  REPEAT,    ! Length of element vector.
     :  TNULL      ! Integer value used to represent nulls.
      DOUBLE PRECISION
     :  TSCALE,    ! Scale factor.
     :  TZERO      ! Zero point.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Get the details of the column from the FITS table.

C        write(17, 3000) nxcol, fitunt
C3000    format(1x, 'nxcol, fitunt: ', i6, i6 )

         FITSTT = FITOK
         CALL FTGBCL (FITUNT, NXCOL, TTYPE, TUNIT, DATYPE, REPEAT,
     :     TSCALE, TZERO, TNULL, TDISP, FITSTT)

C        print3001, ttype, tunit, datype, repeat,
C    :     tscale, tzero, tnull, tdisp, fitstt
C3001    format(
C    :     3x, 'ttype: ', a15 /
C    :     3x, 'tunit: ', a10 /
C    :     3x, 'datype: ', a10 /
C    :     3x, 'repeat: ', i6 /
C    :     3x, 'tscale: ', 0pd20.8 /
C    :     3x, 'tzero: ', 0pd20.8 /
C    :     3x, 'tnull: ', i20 /
C    :     3x, 'tdisp: ', a10 /
C    :     3x, 'fitstt: ', i20 / )

         IF (FITSTT .EQ. FITOK  .AND.  TTYPE .NE. ' ') THEN

*
*          Check that the repeat count for the column is greater than
*          zero.

            IF (REPEAT .GT. 0) THEN

*
*             Set the flag saying the column is not to be skipped.

               SKIP = .FALSE.

*
*             Replace the default values for the column with those read
*             for the column.

               FNAME = TTYPE
               FUNIT = TUNIT
               FSCALE = TSCALE
               FZERO = TZERO
               FXFMT = TDISP

*
*             Determine the dimensionality and size of the column.

               IF (REPEAT .GT. 1) THEN
                  FDIM = CAT__VECTR
                  FSIZE = REPEAT

               ELSE
                  FDIM = CAT__SCALR
                  FSIZE = 0

               END IF

C              print4444, fdim, fsize
C4444          format(1x, 'Initial fdim, fsize: ', i4, i4)

*
*             Construct the CAT data type from the FITS data type.

               CALL CHR_LDBLK (DATYPE)

               IF (DATYPE .NE. ' ') THEN
                  LDATYP = CHR_LEN (DATYPE)
               ELSE
                  LDATYP = 1
               END IF

               IF (DATYPE(1 : 1) .EQ. 'P') THEN
                  SKIP = .TRUE.
                  SKPMSG = 'variable length arrays are not supported.'

               ELSE IF (DATYPE(LDATYP : LDATYP) .EQ. 'X') THEN
                  FDTYPE = CAT__TYPEB
                  FCSIZE = 0
                  FITYPE = CAT3__FTYPX

               ELSE IF (DATYPE(LDATYP : LDATYP) .EQ. 'B') THEN
                  FDTYPE = CAT__TYPEB
                  FCSIZE = 0
                  FITYPE = CAT3__FTYPB

               ELSE IF (DATYPE(LDATYP : LDATYP) .EQ. 'I') THEN
                  FDTYPE = CAT__TYPEW
                  FCSIZE = 0
                  FITYPE = CAT3__FTYPI

               ELSE IF (DATYPE(LDATYP : LDATYP) .EQ. 'J') THEN
                  FDTYPE = CAT__TYPEI
                  FCSIZE = 0
                  FITYPE = CAT3__FTYPJ

               ELSE IF (DATYPE(LDATYP : LDATYP) .EQ. 'K') THEN
                  FDTYPE = CAT__TYPEK
                  FCSIZE = 0
                  FITYPE = CAT3__FTYPK

               ELSE IF (DATYPE(LDATYP : LDATYP) .EQ. 'E') THEN
                  FDTYPE = CAT__TYPER
                  FCSIZE = 0
                  FITYPE = CAT3__FTYPE

               ELSE IF (DATYPE(LDATYP : LDATYP) .EQ. 'D') THEN
                  FDTYPE = CAT__TYPED
                  FCSIZE = 0
                  FITYPE = CAT3__FTYPD

               ELSE IF (DATYPE(LDATYP : LDATYP) .EQ. 'L') THEN
                  FDTYPE = CAT__TYPEL
                  FCSIZE = 0
                  FITYPE = CAT3__FTYPL

               ELSE IF (DATYPE(1 : 1) .EQ. 'A') THEN
                  FDTYPE = CAT__TYPEC
                  FITYPE = CAT3__FTYPA

*
*                For character columns the size is determined from
*                TDISP, not DATYPE.

                  IF (TDISP .NE. ' ') THEN
                     LTDISP = CHR_LEN(TDISP)

                     LSTAT = CAT__OK
                     CALL CHR_CTOI (TDISP(2 : LTDISP), FCSIZE, LSTAT)
                     IF (LSTAT .EQ. CAT__OK) THEN
                        IF (FCSIZE .GT. 0) THEN

*
*                         Because the column is of type CHARACTER the
*                         dimensionality and size must be re-determined.

C                          print6000, repeat, fcsize, status
C6000                      format(1x, 'repeat, fcsize, status: ',
C    :                       i4, i4, i20)

                           REPEAT = REPEAT / FCSIZE

                           IF (REPEAT .GT. 1) THEN
                              FDIM = CAT__VECTR
                              FSIZE = REPEAT

                           ELSE
                              FDIM = CAT__SCALR
                              FSIZE = 0

                           END IF

                        ELSE
                            STATUS = CAT__INVDT

                        END IF

                     ELSE
                        STATUS = CAT__INVDT

                     END IF

                  ELSE
                     FCSIZE = 1

                  END IF

               ELSE IF (DATYPE(LDATYP : LDATYP) .EQ. 'C') THEN

*
*                The FITS column is of data type complex single
*                precision.  CAT converts scalar complex single
*                precision column to 2-element single precision vector.
*                It converts a vector complex single precision
*                column to a single precision vector with twice the
*                original number of elements.

                  FDTYPE = CAT__TYPER
                  FCSIZE = 0
                  FITYPE = CAT3__FTYPC

                  IF (FDIM .EQ. CAT__SCALR) THEN
                     FDIM = CAT__VECTR
                     FSIZE = 2
                  ELSE
                     FSIZE = FSIZE * 2
                  END IF

               ELSE IF (DATYPE(LDATYP : LDATYP) .EQ. 'M') THEN

*
*                The FITS column is of data type complex double
*                precision.  CAT converts scalar complex double
*                precision column to 2-element doubleprecision vector.
*                It converts a vector complex double precision
*                column to a double precision vector with twice the
*                original number of elements.

                  FDTYPE = CAT__TYPED
                  FCSIZE = 0
                  FITYPE = CAT3__FTYPM

                  IF (FDIM .EQ. CAT__SCALR) THEN
                     FDIM = CAT__VECTR
                     FSIZE = 2
                  ELSE
                     FSIZE = FSIZE * 2
                  END IF

               ELSE
                  STATUS = CAT__INVDT

               END IF

*
*             Check if the column is scaled and adjust the CAT data type
*             if necessary.  The check is made by seeing if the scale
*             factor is significantly different from 1.0D0 or the zero
*             point is significantly different from 0.0D0.  CAT forces all
*             scaled columns to have data type DOUBLE PRECISION.

               IF (ABS(FSCALE - 1.0D0) .LT. CAT3__MINVL  .AND.
     :           ABS(FZERO) .LT. CAT3__MINVL) THEN
                  SFLAG = .FALSE.
               ELSE
                  SFLAG = .TRUE.
                  FDTYPE = CAT__TYPED
               END IF

*
*             Convert the FITS null value into the exception value.

               LXCEPT = 0
               FXCEPT = ' '

               CALL CHR_PUTI (TNULL, FXCEPT, LXCEPT)

*
*             Attempt to get any comments for the column.  Comments can
*             be obtained from one of two places; first a column
*             specific keyword call TCOMMn is tried.  If this attempt
*             fails, then any comments associated with the TTYPEn
*             keyword are tried.  Note that the TCOMMn keyword is a CAT
*             convention and likely to be found only in binary FITS tables
*             written with CAT.

               KEYNAM = ' '
               KEYLEN = 0

               CALL CHR_PUTC ('TCOMM', KEYNAM, KEYLEN)
               CALL CHR_PUTI (NXCOL, KEYNAM, KEYLEN)

               CALL FTGKEY (FITUNT, KEYNAM(1 : KEYLEN), KEYVAL, KEYCOM,
     :           FITSTT)

               IF (FITSTT .EQ. FITOK) THEN

*
*                The TCOMMn keyword was found.  Remove any enclosing
*                quotes and adopt the value.

                  IF (KEYVAL .NE. ' ') THEN
                     LKEYVL = CHR_LEN(KEYVAL)

                     IF (KEYVAL(1 : 1) .EQ. ''''  .AND.
     :                   KEYVAL(LKEYVL : LKEYVL) .EQ. '''') THEN
                       KEYVAL(1 : 1) = ' '
                       KEYVAL(LKEYVL : LKEYVL) = ' '

                       CALL CHR_LDBLK (KEYVAL)
                     END IF
                  END IF

                  FCOMM = KEYVAL

               ELSE

*
*                Failed to find the TCOMMn keyword.  Reset the FITS
*                status and adopt any comments associated with the
*                TTYPEn keyword.

                  FITSTT = FITOK

                  KEYNAM = ' '
                  KEYLEN = 0

                  CALL CHR_PUTC ('TTYPE', KEYNAM, KEYLEN)
                  CALL CHR_PUTI (NXCOL, KEYNAM, KEYLEN)

                  CALL FTGKEY (FITUNT, KEYNAM(1 : KEYLEN), KEYVAL,
     :              KEYCOM, FITSTT)

                  IF (FITSTT .EQ. FITOK) THEN
                     IF (KEYCOM .NE. ' ') THEN
                        FCOMM = KEYCOM
                     ELSE
                        FCOMM = ' '
                     END IF
                  ELSE
                     FCOMM = ' '
                  END IF
               END IF

*
*             Attempt to get the preferential display flag for the column,
*             from FITS keyword TPRFDn.  Note that the TPRFDn keyword is a
*             CAT convention and is likely to be found only in binary FITS
*             tables written with CAT.

               KEYNAM = ' '
               KEYLEN = 0

               CALL CHR_PUTC ('TPRFD', KEYNAM, KEYLEN)
               CALL CHR_PUTI (NXCOL, KEYNAM, KEYLEN)

               CALL FTGKYL (FITUNT, KEYNAM(1 : KEYLEN), KEYVLL, KEYCOM,
     :           FITSTT)

               IF (FITSTT .EQ. FITOK) THEN
                  FPDISP = KEYVLL
               ELSE
                  FPDISP = .TRUE.
               END IF

            ELSE

*
*             The repeat count for the FITS column is zero or negative;
*             it contains no data.  Set the skip column flag.

               FNAME = TTYPE

               SKIP = .TRUE.

               SKPMSG = ' '
               LSKPM = 0

               CALL CHR_PUTC ('the column contains no data (the '/
     :           /'repeat count is ', SKPMSG, LSKPM)
               CALL CHR_PUTI (REPEAT, SKPMSG, LSKPM)
               CALL CHR_PUTC (').', SKPMSG, LSKPM)
            END IF

*
*          If the column is to be skipped then report a message.

            IF (SKIP) THEN
               MESSGE = ' '
               LMESSG = 0

               CALL CHR_PUTC ('FITS column ', MESSGE, LMESSG)

               IF (TTYPE .NE. ' ') THEN
                  LTTYPE = CHR_LEN(TTYPE)
                  CALL CHR_PUTC (TTYPE(1 : LTTYPE), MESSGE, LMESSG)

               ELSE
                  CALL CHR_PUTC ('<blank>', MESSGE, LMESSG)

               END IF

               CALL CHR_PUTC (' (sequence no. ', MESSGE, LMESSG)
               CALL CHR_PUTI (NXCOL, MESSGE, LMESSG)
               CALL CHR_PUTC (') skipped;', MESSGE, LMESSG)

               CALL CAT1_MSG (' ', MESSGE(1 : LMESSG), STATUS)
               CALL CAT1_MSG (' ', '  ' // SKPMSG, STATUS)
            END IF

*
*          Report any error.

            IF (STATUS .NE. CAT__OK) THEN
               SKIP = .TRUE.

               ERRMSG = ' '
               ERRLEN = 0

               CALL CHR_PUTC ('FITS column ', ERRMSG, ERRLEN)

               IF (TTYPE .NE. ' ') THEN
                  LTTYPE = CHR_LEN(TTYPE)
                  CALL CHR_PUTC (TTYPE(1 : LTTYPE), ERRMSG, ERRLEN)

               ELSE
                  CALL CHR_PUTC ('<blank>', ERRMSG, ERRLEN)

               END IF

               CALL CHR_PUTC (' (sequence no. ', ERRMSG, ERRLEN)
               CALL CHR_PUTI (NXCOL, ERRMSG, ERRLEN)
               CALL CHR_PUTC (') has illegal format: ', ERRMSG, ERRLEN)

               IF (DATYPE.NE. ' ') THEN
                  CALL CHR_PUTC (DATYPE(1 : LDATYP), ERRMSG, ERRLEN)

               ELSE
                  CALL CHR_PUTC ('<blank>', ERRMSG, ERRLEN)

               END IF

               CALL CAT1_ERREP ('CAT3_FTCLB_ILF', ERRMSG(1 : ERRLEN),
     :           STATUS)

            END IF

         ELSE
            SKIP = .TRUE.

            IF (FITSTT .NE. FITOK) THEN

*
*             Error getting the FITS column details.

               STATUS = CAT__NOCAT

               ERRMSG = ' '
               ERRLEN = 0

               CALL CHR_PUTC ('Failure reading details for FITS '/
     :           /'column number ', ERRMSG, ERRLEN)
               CALL CHR_PUTI (NXCOL, ERRMSG, ERRLEN)
               CALL CHR_PUTC ('.', ERRMSG, ERRLEN)

               CALL CAT3_FITER ('CAT3_GTCLB_ERR', ERRMSG(1 : ERRLEN),
     :           FITSTT, STATUS)

            END IF

         END IF

C        print4445, fdim, fsize
C4445    format(1x, 'Final fdim, fsize: ', i4, i4)

      END IF

      END
