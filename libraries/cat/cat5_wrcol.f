      SUBROUTINE CAT5_WRCOL (DFUNIT, KFLAG, FNAME, FDIM, FSIZE, FDTYPE,
     :  FCSIZE, POSN, FORDER, FUNIT, FXFMT, FPDISP, FCOMM, STATUS)
*+
*  Name:
*     CAT5_WRCOL
*  Purpose:
*     Write details of a column to the description file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_WRCOL (DFUNIT, KFLAG, FNAME, FDIM, FSIZE, FDTYPE,
*       FCSIZE, POSN, FORDER, FUNIT, FXFMT, FPDISP, FCOMM, STATUS)
*  Description:
*     Write details of a column to the description file.
*  Arguments:
*     DFUNIT  =  INTEGER (Given)
*        Fortran unit number for writing to the description file.
*     KFLAG  =  LOGICAL
*        Flag indicating whether the catalogue is to be written as in
*        'KAPPA format' or as a standard STL, coded as follows:
*        .TRUE.  -  KAPPA format,
*        .FALSE. -  standard.
*     FNAME  =  CHARACTER*(*) (Given)
*        Name of the column.
*     FDIM  =  INTEGER (Given)
*        Dimensionality of the column.
*     FSIZE  =  INTEGER (Given)
*        Number of elements of a vector column.
*     FDTYPE  =  INTEGER (Given)
*        Data type of the column.
*     FCSIZE  =  INTEGER (Given)
*        Size of a CHARACTER column.
*     POSN  =  INTEGER (Given)
*        Position of the column in the table.
*     FORDER  =  INTEGER (Given)
*        Order of the column.
*     FUNIT  =  CHARACTER*(*) (Given)
*        Units of the column.
*     FXFMT  =  CHARACTER*(*) (Given)
*        External format for the column.
*     FPDISP  =  LOGICAL (Given)
*        Preferential display flag for the column.
*     FCOMM  =  CHARACTER*(*) (Given)
*        Comments describing the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Write the details to the file.
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     18/7/96  (ACD): Original version.
*     19/7/96  (ACD): First stable version.
*     10/12/96 (ACD): Added writing 'KAPPA format' STLs.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  DFUNIT,
     :  FDIM,
     :  FSIZE,
     :  FDTYPE,
     :  FCSIZE,
     :  POSN,
     :  FORDER
      CHARACTER
     :  FNAME*(*),
     :  FUNIT*(*),
     :  FXFMT*(*),
     :  FCOMM*(*)
      LOGICAL
     :  KFLAG,
     :  FPDISP
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  BUFFER*70, ! output buffer.
     :  TYPE*(CAT__SZTYP)  ! Character repn. of current data type.
      INTEGER
     :  LSTAT,     ! Local Fortran I/O status.
     :  BUFLEN,    ! Length of BUFFER (excl. trail. blanks).
     :  LFNAME,    !   "    "  FNAME  ( "  .   "  .   "   ).
     :  LTYPE,     !   "    "  TYPE   ( "  .   "  .   "   ).
     :  LFXFMT,    !   "    "  FXFMT  ( "  .   "  .   "   ).
     :  LFUNIT,    !   "    "  FUNIT  ( "  .   "  .   "   ).
     :  LFCOMM     !   "    "  FCOMM  ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Write the first line containing the mandatory items and
*       the external display format.

         BUFFER = ' '
         BUFLEN = 0

         IF (.NOT. KFLAG) THEN
            CALL CHR_PUTC ('C', BUFFER, BUFLEN)
         ELSE
            CALL CHR_PUTC ('#C', BUFFER, BUFLEN)
         END IF

         IF (FNAME .NE. ' ') THEN
            LFNAME = CHR_LEN(FNAME)
         ELSE
            LFNAME = 1
         END IF

         BUFLEN = 3
         CALL CHR_PUTC (FNAME(1 : LFNAME), BUFFER, BUFLEN)

         IF (FDIM .NE. CAT__SCALR) THEN
            CALL CHR_PUTC ('[', BUFFER, BUFLEN)
            CALL CHR_PUTI (FSIZE, BUFFER, BUFLEN)
            CALL CHR_PUTC (']', BUFFER, BUFLEN)
         END IF

         BUFLEN = BUFLEN + 2
         BUFLEN = MAX(BUFLEN, 12)

         TYPE = ' '
         LTYPE = 0

         CALL CAT_TYFMT (FDTYPE, FCSIZE, TYPE, LTYPE, STATUS)
         CALL CHR_PUTC (TYPE(2 : LTYPE), BUFFER, BUFLEN)

         BUFLEN = BUFLEN + 2
         BUFLEN = MAX(BUFLEN, 22)

         CALL CHR_PUTI (POSN, BUFFER, BUFLEN)

         BUFLEN = BUFLEN + 3
         BUFLEN = MAX(BUFLEN, 28)

         CALL CHR_PUTC ('EXFMT=', BUFFER, BUFLEN)

         IF (FXFMT .NE. ' ') THEN
            LFXFMT = CHR_LEN(FXFMT)
         ELSE
            LFXFMT = 1
         END IF

         CALL CHR_PUTC (FXFMT(1 : LFXFMT), BUFFER, BUFLEN)

         WRITE(DFUNIT, 2000, IOSTAT=LSTAT) BUFFER(1 : BUFLEN)
 2000    FORMAT(A)
         CALL CAT1_IOERR (LSTAT, STATUS)

*
*       If the units are not blank then write them as a continuation
*       line.

         IF (FUNIT .NE. ' ') THEN
            LFUNIT = CHR_LEN(FUNIT)

            IF (.NOT. KFLAG) THEN
               WRITE(DFUNIT, 2001, IOSTAT=LSTAT) FUNIT(1 : LFUNIT)
 2001          FORMAT(':    UNITS=''', A, '''')
            ELSE
               WRITE(DFUNIT, 2002, IOSTAT=LSTAT) FUNIT(1 : LFUNIT)
 2002          FORMAT('#:    UNITS=''', A, '''')
            END IF

            IF (STATUS .EQ. CAT__OK) THEN
               CALL CAT1_IOERR (LSTAT, STATUS)
            END IF
         END IF

*
*       If the comments are not blank then write them as a continuation
*       line.

         IF (FCOMM .NE. ' ') THEN
            LFCOMM = CHR_LEN(FCOMM)

            IF (.NOT. KFLAG) THEN
               WRITE(DFUNIT, 2003, IOSTAT=LSTAT) FCOMM(1 : LFCOMM)
 2003          FORMAT(':    COMMENTS=''', A, '''')
            ELSE
               WRITE(DFUNIT, 2004, IOSTAT=LSTAT) FCOMM(1 : LFCOMM)
 2004          FORMAT('#:    COMMENTS=''', A, '''')
            END IF

            IF (STATUS .EQ. CAT__OK) THEN
               CALL CAT1_IOERR (LSTAT, STATUS)
            END IF
         END IF

*
*       If order is not default (unordered) then write it as a
*       continuation line.

         IF (FORDER.EQ. CAT__ASCND  .OR.  FORDER .EQ. CAT__DSCND) THEN
            IF (FORDER .EQ. CAT__ASCND) THEN
               IF (.NOT. KFLAG) THEN
                  WRITE(DFUNIT, 2005, IOSTAT=LSTAT)
 2005             FORMAT(':    ORDER=ASCENDING')
               ELSE
                  WRITE(DFUNIT, 2006, IOSTAT=LSTAT)
 2006             FORMAT('#:    ORDER=ASCENDING')
               END IF
            ELSE IF (FORDER .EQ. CAT__DSCND) THEN
               IF (.NOT. KFLAG) THEN
                  WRITE(DFUNIT, 2007, IOSTAT=LSTAT)
 2007             FORMAT(':    ORDER=DESCENDING')
               ELSE
                  WRITE(DFUNIT, 2008, IOSTAT=LSTAT)
 2008             FORMAT('#:    ORDER=DESCENDING')
               END IF
            END IF

            IF (STATUS .EQ. CAT__OK) THEN
               CALL CAT1_IOERR (LSTAT, STATUS)
            END IF
         END IF

*
*       If preferential display flag is not default (true) then write it
*       as a continuation line.

         IF (.NOT. FPDISP) THEN
            IF (.NOT. KFLAG) THEN
               WRITE(DFUNIT, 2009, IOSTAT=LSTAT)
 2009          FORMAT(':    PREFDISP=FALSE')
            ELSE
               WRITE(DFUNIT, 2010, IOSTAT=LSTAT)
 2010          FORMAT('#:    PREFDISP=FALSE')
            END IF

            IF (STATUS .EQ. CAT__OK) THEN
               CALL CAT1_IOERR (LSTAT, STATUS)
            END IF
         END IF

      END IF

      END
