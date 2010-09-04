      SUBROUTINE CAT5_WRPAR (DFUNIT, KFLAG, QNAME, QDTYPE, QCSIZE,
     :  QVALUE, QUNIT, QXFMT, QPDISP, QCOMM, STATUS)
*+
*  Name:
*     CAT5_WRPAR
*  Purpose:
*     Write details of a parameter to the description file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_WRPAR (DFUNIT, KFLAG, QNAME, QDTYPE, QCSIZE, QVALUE,
*       QUNIT, QXFMT, QPDISP, QCOMM; STATUS)
*  Description:
*     Write details of a parameter to the description file.
*  Arguments:
*     DFUNIT  =  INTEGER (Given)
*        Fortran unit number for writing to the description file.
*     KFLAG  =  LOGICAL
*        Flag indicating whether the catalogue is to be written as in
*        'KAPPA format' or as a standard STL, coded as follows:
*        .TRUE.  -  KAPPA format,
*        .FALSE. -  standard.
*     QNAME  =  CHARACTER*(*) (Given)
*        Name of the parameter.
*     QDTYPE  =  INTEGER (Given)
*        Data type of the parameter.
*     QCSIZE  =  INTEGER (Given)
*        Size of a CHARACTER parameter.
*     QVALUE =  CHARACTER*(*) (Given)
*        Value of the parameter.
*     QUNIT  =  CHARACTER*(*) (Given)
*        Units of the parameter.
*     QXFMT  =  CHARACTER*(*) (Given)
*        External format for the parameter.
*     QPDISP  =  LOGICAL (Given)
*        Preferential display flag for the parameter.
*     QCOMM  =  CHARACTER*(*) (Given)
*        Comments describing the parameter.
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     19/7/96  (ACD): Original version.
*     28/8/96  (ACD): First stable version.
*     10/12/96 (ACD): Added writing 'KAPPA format' STLs.
*     6/6/98   (ACD): Correct mistake in the prologue comments.
*     22/12/99 (ACD): Improved the handling of long CHARACTER parameters.
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
     :  QDTYPE,
     :  QCSIZE
      CHARACTER
     :  QNAME*(*),
     :  QVALUE*(*),
     :  QUNIT*(*),
     :  QXFMT*(*),
     :  QCOMM*(*)
      LOGICAL
     :  KFLAG,
     :  QPDISP
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER BUFSIZ  ! Size of the output line buffer.
      PARAMETER( BUFSIZ = 80)
*  Local Variables:
      CHARACTER
     :  BUFFER*(BUFSIZ),   ! Output buffer.
     :  TYPE*(CAT__SZTYP)  ! Character repn. of current data type.
      INTEGER
     :  LSTAT,     ! Local Fortran I/O status.
     :  BUFLEN,    ! Length of BUFFER (excl. trail. blanks).
     :  LQNAME,    !   "    "  FNAME  ( "  .   "  .   "   ).
     :  LTYPE,     !   "    "  TYPE   ( "  .   "  .   "   ).
     :  LQVAL,     !   "    "  QVALUE ( "  .   "  .   "   ).
     :  LQXFMT,    !   "    "  QXFMT  ( "  .   "  .   "   ).
     :  LQUNIT,    !   "    "  QUNIT  ( "  .   "  .   "   ).
     :  LQCOMM,    !   "    "  QCOMM  ( "  .   "  .   "   ).
     :  LCMAX      ! Maximum permitted length for a comment.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Write the first line containing the mandatory items and
*       the external display format.  The external display format
*       is written on a continuation line of there is insufficient
*       space on the intial line.

         BUFFER = ' '
         BUFLEN = 0

         IF (.NOT. KFLAG) THEN
            CALL CHR_PUTC ('P', BUFFER, BUFLEN)
         ELSE
            CALL CHR_PUTC ('#P', BUFFER, BUFLEN)
         END IF

         IF (QNAME .NE. ' ') THEN
            LQNAME = CHR_LEN(QNAME)
         ELSE
            LQNAME = 1
         END IF

         BUFLEN = 3
         CALL CHR_PUTC (QNAME(1 : LQNAME), BUFFER, BUFLEN)

         BUFLEN = BUFLEN + 2
         BUFLEN = MAX(BUFLEN, 12)

         TYPE = ' '
         LTYPE = 0

         CALL CAT_TYFMT (QDTYPE, QCSIZE, TYPE, LTYPE, STATUS)
         CALL CHR_PUTC (TYPE(2 : LTYPE), BUFFER, BUFLEN)

         BUFLEN = BUFLEN + 2
         BUFLEN = MAX(BUFLEN, 22)

         IF (QDTYPE .EQ. CAT__TYPEC) THEN
            CALL CHR_PUTC ('''', BUFFER, BUFLEN)
         END IF

         IF (QVALUE .NE. ' ') THEN
            LQVAL = CHR_LEN(QVALUE)
            CALL CHR_PUTC (QVALUE(1 : LQVAL), BUFFER, BUFLEN)
         ELSE
            IF (QDTYPE .EQ. CAT__TYPEC) THEN
               BUFLEN = BUFLEN + 1
            ELSE
               CALL CHR_PUTI (0, BUFFER, BUFLEN)
            END IF
         END IF

         IF (BUFLEN .GE. BUFSIZ) THEN
            BUFLEN = BUFSIZ - 1
         END IF

         IF (QDTYPE .EQ. CAT__TYPEC) THEN
            CALL CHR_PUTC ('''', BUFFER, BUFLEN)
         END IF

         IF (BUFLEN .GE. BUFSIZ - 9) THEN
            WRITE(DFUNIT, 2000, IOSTAT=LSTAT) BUFFER(1 : BUFLEN)
 2000       FORMAT(A)
            CALL CAT1_IOERR (LSTAT, STATUS)

            BUFFER = ' '
            BUFLEN = 0

            CALL CHR_PUTC (':    ', BUFFER, BUFLEN)

         ELSE
            BUFLEN = BUFLEN + 3
            BUFLEN = MAX(BUFLEN, 28)

         END IF

         CALL CHR_PUTC ('EXFMT=', BUFFER, BUFLEN)

         IF (QXFMT .NE. ' ') THEN
            LQXFMT = CHR_LEN(QXFMT)
         ELSE
            LQXFMT = 1
         END IF

         CALL CHR_PUTC (QXFMT(1 : LQXFMT), BUFFER, BUFLEN)

         WRITE(DFUNIT, 2000, IOSTAT=LSTAT) BUFFER(1 : BUFLEN)
         CALL CAT1_IOERR (LSTAT, STATUS)

*
*       If the units are not blank then write them as a continuation
*       line.

         IF (QUNIT .NE. ' ') THEN
            LQUNIT = CHR_LEN(QUNIT)

            IF (.NOT. KFLAG) THEN
               WRITE(DFUNIT, 2001, IOSTAT=LSTAT) QUNIT(1 : LQUNIT)
 2001          FORMAT(':    UNITS=''', A, '''')
            ELSE
               WRITE(DFUNIT, 2002, IOSTAT=LSTAT) QUNIT(1 : LQUNIT)
 2002          FORMAT('#:    UNITS=''', A, '''')
            END IF

            IF (STATUS .EQ. CAT__OK) THEN
               CALL CAT1_IOERR (LSTAT, STATUS)
            END IF
         END IF

*
*       If the comments are not blank then write them as a continuation
*       line.

         IF (QCOMM .NE. ' ') THEN
            LQCOMM = CHR_LEN(QCOMM)

            IF (.NOT. KFLAG) THEN
               LCMAX = BUFSIZ - 16
               LQCOMM = MIN(LQCOMM, LCMAX)
               WRITE(DFUNIT, 2003, IOSTAT=LSTAT) QCOMM(1 : LQCOMM)
 2003          FORMAT(':    COMMENTS=''', A, '''')
            ELSE
               LCMAX = BUFSIZ - 17
               LQCOMM = MIN(LQCOMM, LCMAX)
               WRITE(DFUNIT, 2004, IOSTAT=LSTAT) QCOMM(1 : LQCOMM)
 2004          FORMAT('#:    COMMENTS=''', A, '''')
            END IF

            IF (STATUS .EQ. CAT__OK) THEN
               CALL CAT1_IOERR (LSTAT, STATUS)
            END IF
         END IF

*
*       If preferential display flag is not default (true) then write it
*       as a continuation line.

         IF (.NOT. QPDISP) THEN
            IF (.NOT. KFLAG) THEN
               WRITE(DFUNIT, 2005, IOSTAT=LSTAT)
 2005          FORMAT(':    PREFDISP=FALSE')
            ELSE
               WRITE(DFUNIT, 2006, IOSTAT=LSTAT)
 2006          FORMAT('#:    PREFDISP=FALSE')
            END IF

            IF (STATUS .EQ. CAT__OK) THEN
               CALL CAT1_IOERR (LSTAT, STATUS)
            END IF
         END IF

      END IF

      END
