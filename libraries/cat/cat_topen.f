      SUBROUTINE CAT_TOPEN (CNAME, STATE, MODE, CI, STATUS)
*+
*  Name:
*     CAT_TOPEN
*  Purpose:
*       Open a catalogue and obtain an identifier to it.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TOPEN (CNAME, STATE, MODE; CI; STATUS)
*  Description:
*     Open a catalogue and obtain an identifier to it.
*
*     The matrix of possibilities for STATE and MODE is:
*
*     STATE = 'NEW', then
*       MODE = 'WRITE' is ok
*       and MODE = 'READ' if forbidden.
*
*     STATE = 'OLD' then
*       MODE = 'WRITE' is ok
*       and MODE = 'READ' is ok.
*
*     Notes:
*
*     * If a catalogue already exists and an attempt is made to open
*       it with STATE = 'NEW', CAT_TOPEN will fail with an error.
*
*     * If an existing catalogue is opened with STATE = 'OLD' and
*       MODE = 'WRITE' it will be overwritten.
*
*  Arguments:
*     CNAME  =  CHARACTER*(*) (Given)
*        Catalogue name.
*     STATE  =  CHARACTER*(*) (Given)
*        Required state of the catalogue.  One of:
*        NEW  -  A new catalogue is to be created,
*        OLD  -  An existing (that is, old) catalogue is to be opened.
*     MODE  =  CHARACTER*(*) (Given)
*        Mode in which the catalogue will be accessed.  One of:
*        READ   -  the catalogue may only be read from,
*        WRITE  -  an new catalogue is to be written.
*     CI  =  INTEGER (Returned)
*        Catalogue identifier.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to open the catalogue.
*     Report any error.
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     17/11/98 (ACD): New version which merely calls an internal
*        routine and reports any error.  The previous CAT_TOPEN
*        became subroutine CAT1_TOPEN.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      CHARACTER
     :  CNAME*(*),
     :  STATE*(*),
     :  MODE*(*)
*  Arguments Returned:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS   ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  ERRBUF*75      ! Text of error message.
      INTEGER
     :  ERRLEN,        ! Length of ERRBUF (excl. trail. blanks).
     :  LCNAME         ! Length of CNAME.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Attempt to open the catalogue.

         CALL CAT1_TOPEN (CNAME, STATE, MODE, CI, STATUS)

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRBUF = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_TOPEN: failed to open catalogue ',
     :        ERRBUF, ERRLEN)

            IF (CNAME .NE. ' ') THEN
               LCNAME = CHR_LEN(CNAME)
               CALL CHR_PUTC (CNAME(1 : LCNAME), ERRBUF, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRBUF, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRBUF, ERRLEN)

            CALL CAT1_ERREP ('CAT_TOPEN_ERR', ERRBUF(1 : ERRLEN),
     :        STATUS)
         END IF
      END IF

      END
