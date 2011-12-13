      SUBROUTINE CAT3_GTCLA (FITUNT, NXCOL, FNAME, FDTYPE, FCSIZE,
     :  FUNIT, FSCALE, FZERO, FXCEPT, FXFMT, FCOMM, FITYPE, SFLAG,
     :  STATUS)
*+
*  Name:
*     CAT3_GTCLA
*  Purpose:
*     Read details of a column from an ASCII FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_GTCLA (FITUNT, NXCOL; FNAME, FDTYPE, FCSIZE, FUNIT,
*       FSCALE, FZERO, FXCEPT, FXFMT, FCOMM, FITYPE, SFLAG; STATUS)
*  Description:
*     Read details of a column from an ASCII FITS table.
*  Arguments:
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     NXCOL  =  INTEGER (Given)
*        Sequence number for the column to be accessed in the table.
*     FNAME  =  CHARACTER*(*) (Returned)
*        Name of the column.
*     FDTYPE  =  INTEGER (Returned)
*        Data type of the column (using the CAT integer codes).
*     FCSIZE  =  INTEGER (Returned)
*        Size of columns of type character.
*     FUNIT  =  CHARACTER*(*) (Returned)
*        Units of the column.
*     FSCALE  =  DOUBLE PRECISION (Returned)
*        Scale factor for the column.
*     FZERO  =  DOUBLE PRECISION (Returned)
*        Zero pointfor the column.
*     FXCEPT  =  CHARACTER*(*) (Returned)
*        Exception value for the column.
*     FXFMT  =  CHARACTER*(*) (Returned)
*        External format for the column.
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
*     Attempt to get details of the column from the FITS table.
*     If ok then
*       Convert the FITS details to the equivalent CAT details.
*     end if
*     If any error occurred then
*       Report the error.
*       Set the status.
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
*     14/10/93 (ACD): First stable version.
*     23/1/94  (ACD): Modified error reporting.
*     15/6/94  (ACD): Corrected mistake in the prologue comments.
*     5/7/94   (ACD): Added arguments for FITS data type and 'scaled
*        column' flag.
*     8/7/94   (ACD): Added getting comments for the CAT column from the
*        comments for the FITS TTYPEn keyword.
*     10/3/95  (ACD): Corrected the defined size of variables to hold
*        values read from FITS CHARACTER keywords.
*     10/5/95  (ACD): Changed the interpretation of 'F' formats from
*        REAL to DOUBLE PRECISION CAT columns.
*     20/3/96  (ACD): Fixed bug; variable FITSTT was not being
*        initialised.
*     4/6/98   (ACD): Improved the error reporting.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT3_FIT_PAR'      ! Internal FITS back-end comments.
*  Arguments Given:
      INTEGER
     :  FITUNT,
     :  NXCOL
*  Arguments Returned:
      CHARACTER
     :  FNAME*(*),
     :  FUNIT*(*),
     :  FXCEPT*(*),
     :  FXFMT*(*),
     :  FCOMM*(*)
      INTEGER
     :  FDTYPE,
     :  FCSIZE,
     :  FITYPE
      DOUBLE PRECISION
     :  FSCALE,
     :  FZERO
      LOGICAL
     :  SFLAG
*  Status:
      INTEGER STATUS   ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
       INTEGER FITOK   ! FITSIO Success status.
       PARAMETER (FITOK = 0)
*  Local Variables:
      CHARACTER
     :  ERRMSG*75,          ! Error message.
     :  TTYPEN*15,          ! Name of the keyword for the nth FITS column.
     :  KVAL*(CAT3__SZCVL), ! Keyword value   for the nth FITS column.
     :  KCOMM*70            !    "    comment  "   "   "   "     "   .
      INTEGER
     :  FITSTT,    ! FITSIO running status.
     :  LSTAT,     ! Local status.
     :  ERRLEN,    ! Length of ERRMSG (excl. trail. blanks).
     :  TFMLEN,    !   "    "  TFORM  ( "  .   "  .   "   ).
     :  TTYLEN,    !   "    "  TTYPEN ( "  .   "  .   "   ).
     :  LFNAME     !   "    "  FNAME  ( "  .   "  .   "   ).
*
*    Attributes for a column, as read from the FITS ASCII file.

      CHARACTER
     :  TTYPE*15,             ! Name.
     :  TUNIT*(CAT3__SZCVL),  ! Units.
     :  TFORM*(CAT3__SZCVL),  ! Internal format in the FITS table.
     :  TNULL*(CAT3__SZCVL),  ! Null value.
     :  TDISP*(CAT3__SZCVL)   ! External format.
      INTEGER
     :  TBCOL      ! Start position of the colunm in each record.
      DOUBLE PRECISION
     :  TSCALE,    ! Scale factor.
     :  TZERO      ! Zero point.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Get the details of the column from the FITS table.

         FITSTT = FITOK
         CALL FTGACL (FITUNT, NXCOL, TTYPE, TBCOL, TUNIT, TFORM,
     :     TSCALE, TZERO, TNULL, TDISP, FITSTT)

         IF (FITSTT .EQ. FITOK  .AND.  TTYPE .NE. ' ') THEN

*
*          Replace the default values for the column with those read
*          for the column.

C           write(17, 3000) pcols, ttype
C3000       format(1x, 'pcols, ttype: ', I4, 1x, a15 )

            FNAME = TTYPE
            FUNIT = TUNIT
            FSCALE = TSCALE
            FZERO = TZERO
            FXCEPT = TNULL

            IF (TDISP .NE. ' ') THEN
               FXFMT = TDISP
            ELSE
               FXFMT = TFORM
            END IF

*
*          Construct the CAT data type from the FITS data type.

            CALL CHR_LDBLK (TFORM)

            IF (TFORM(1 : 1) .EQ. 'I') THEN
               FDTYPE = CAT__TYPEI
               FCSIZE = 0
               FITYPE = CAT3__FTYPJ

            ELSE IF (TFORM(1 : 1) .EQ. 'A') THEN
               FDTYPE = CAT__TYPEC
               FITYPE = CAT3__FTYPA

               LSTAT = CAT__OK
               CALL CHR_CTOI (TFORM(2 : 10), FCSIZE, LSTAT)
               IF (LSTAT .NE. CAT__OK) THEN
                  STATUS = CAT__INVDT

                  ERRMSG = ' '
                  ERRLEN = 0

                  CALL CHR_PUTC ('Internal column format invalid: ',
     :              ERRMSG, ERRLEN)

                  TFMLEN = CHR_LEN (TFORM)
                  CALL CHR_PUTC (TFORM(1 : TFMLEN), ERRMSG, ERRLEN)

                  CALL CAT1_ERREP ('CAT3_GTCLA_ICF', ERRMSG(1 : ERRLEN),
     :              STATUS)
               END IF


            ELSE IF (TFORM(1 : 1) .EQ. 'F') THEN
               FDTYPE = CAT__TYPED
               FCSIZE = 0
               FITYPE = CAT3__FTYPD

            ELSE IF (TFORM(1 : 1) .EQ. 'E') THEN
               FDTYPE = CAT__TYPER
               FCSIZE = 0
               FITYPE = CAT3__FTYPE

            ELSE IF (TFORM(1 : 1) .EQ. 'D') THEN
               FDTYPE = CAT__TYPED
               FCSIZE = 0
               FITYPE = CAT3__FTYPD

            ELSE
               STATUS = CAT__INVDT

               ERRMSG = ' '
               ERRLEN = 0

               CALL CHR_PUTC ('FITS column has illegal format: ',
     :           ERRMSG, ERRLEN)

               TFMLEN = CHR_LEN (TFORM)
               CALL CHR_PUTC (TFORM(1 : TFMLEN), ERRMSG, ERRLEN)

               CALL CAT1_ERREP ('CAT3_GTCLA_FIC', ERRMSG(1 : ERRLEN),
     :           STATUS)

            END IF

*
*          Check if the column is scaled and adjust the CAT data type
*          if necessary.  The check is made by seeing if the scale
*          factor is significantly different from 1.0D0 or the zero
*          point is significantly different from 0.0D0.  CAT forces all
*          scaled columns to have data type DOUBLE PRECISION.

            IF (ABS(FSCALE - 1.0D0) .LT. CAT3__MINVL  .AND.
     :        ABS(FZERO) .LT. CAT3__MINVL) THEN
               SFLAG = .FALSE.
            ELSE
               SFLAG = .TRUE.
               FDTYPE = CAT__TYPED
            END IF

*
*          Get any comments that were included with the TTYPEn keyword.

            TTYLEN = 0
            TTYPEN = ' '

            CALL CHR_PUTC ('TTYPE', TTYPEN, TTYLEN)
            CALL CHR_PUTI (NXCOL, TTYPEN, TTYLEN)

            CALL FTGKEY (FITUNT, TTYPEN(1 : TTYLEN), KVAL, KCOMM,
     :        FITSTT)

            IF (FITSTT .EQ. FITOK) THEN
               FCOMM = KCOMM
            ELSE
               FCOMM = ' '
            END IF

         END IF

*
*       Report any error.

         IF (FITSTT .NE. FITOK) THEN
            STATUS = CAT__NOCAT

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Failure reading details for FITS '/
     :        /'column ', ERRMSG, ERRLEN)

            IF (FNAME .NE. ' ') THEN
               LFNAME = CHR_LEN(FNAME)
               CALL CHR_PUTC (FNAME(1 : LFNAME), ERRMSG, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRMSG, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRMSG, ERRLEN)

            CALL CAT3_FITER ('CAT3_GTCLA_ERR', ERRMSG(1 : ERRLEN),
     :        FITSTT, STATUS)
         END IF

      END IF

      END
