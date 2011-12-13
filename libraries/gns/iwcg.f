      SUBROUTINE GNS_IWCG ( IWKID, CHAR, VALUE, STATUS )

*+
*  Name:
*     GNS_IWCG

*  Purpose:
*     Inquire workstation characteristic

*  Invocation:
*     CALL GNS_IWCG( IWKID, CHAR, VALUE, STATUS )

*  Description:
*     The specified characteristic is returned as a character string,
*     blank filled or truncated as necessary. If the characteristic does
*     not exist then a blank string is returned.
*
*     Any characteristic with a keyword value (see appendix A) can be
*     inquired with this routine.

*  Arguments:
*     IWKID = INTEGER (Given)
*        GKS workstation identifier
*     CHAR = CHARACTER*(*) (Given)
*        Characteristic name
*     VALUE = CHARACTER*(GNS__SZKEY) (Returned)
*        The value of the specified characteristic
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Side Effects:
*     The GNS database may be opened.

*  Copyright:
*     Copyright (C) 1988, 1990, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     DLT: D.L. Terrett (STARLINK)
*     NE: Nick Eaton (Durham University)

*  History:
*     16-MAY-1988 (DLT):
*        Original version.
*      2-APR-1990 (NE):
*        Added OPEN keyword
*      9-JUL-1990 (NE):
*        Added error reporting
*      3-JUN-1992 (NE):
*        Added WINDOW_OVERLAY
*      1-SEP-1992 (NE):
*        Updated prologue.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'GNS_ERR'
      INCLUDE 'GNS_PAR'
      INCLUDE 'gns.cmn'
      INCLUDE 'chars.par'

*  Arguments Given:
      INTEGER IWKID
      CHARACTER*(*) CHAR

*  Arguments Returned:
      CHARACTER*(*) VALUE

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAXCLA, MAXOUT, MAXERA, MAXOPN
      PARAMETER( MAXCLA = 10, MAXOUT = 2, MAXERA = 1, MAXOPN = 1)

*  Local Variables:
      INTEGER IERR, ICON, ITYPE, L

      CHARACTER*(GNS__SZKEY) TCHAR
      CHARACTER*(GNS__SZKEY) CLANAM(0:10), OUTNAM(0:2), ERANAM(0:1),
     :                       OPENAM(0:1)

*  Local Data:
      DATA CLANAM /' ','GRAPHICS_OVERLAY', 'IMAGE_DISPLAY',
     :             'IMAGE_OVERLAY', 'MATRIX_PRINTER', 'METAFILE_INPUT',
     :             'METAFILE_OUTPUT', 'PEN_PLOTTER', 'TERMINAL',
     :             'WINDOW', 'WINDOW_OVERLAY'/
      DATA OUTNAM /' ', 'DIRECT', 'FILE'/
      DATA ERANAM /' ', 'SELECTIVE' /
      DATA OPENAM /' ', 'NORESET' /
*.

      IF (STATUS.EQ.0) THEN

*     Convert the workstation id to a type
         CALL GQWKC(IWKID,IERR,ICON,ITYPE)
         IF (IERR.EQ.0) THEN

*        Make sure that the common block contains data for this type
            CALL GNS_1RDWST(ITYPE, STATUS)
            IF (STATUS.EQ.0) THEN

*           Trim blanks from the characteristic name
               CALL GNS_1TRIM(CHAR,TCHAR,L)

               IF (TCHAR.EQ.'CLASS'.OR.TCHAR.EQ.'class') THEN
                  IF (CLASS.LE.MAXCLA) THEN
                     VALUE = CLANAM(CLASS)
                  ELSE
                     VALUE = ' '
                  ENDIF
               ELSE IF (TCHAR.EQ.'OUTPUT'.OR.TCHAR.EQ.'output') THEN
                  IF (OUTPUT.LE.MAXOUT) THEN
                     VALUE = OUTNAM(OUTPUT)
                  ELSE
                     VALUE = ' '
                  ENDIF
               ELSE IF (TCHAR.EQ.'CLEAR'.OR.TCHAR.EQ.'clear') THEN
                  IF (CLEAR.LE.MAXERA) THEN
                     VALUE = ERANAM(CLEAR)
                  ELSE
                     VALUE = ' '
                  ENDIF
               ELSE IF (TCHAR.EQ.'OPEN'.OR.TCHAR.EQ.'open') THEN
                  IF (IOPEN.LE.MAXOPN) THEN
                     VALUE = OPENAM(IOPEN)
                  ELSE
                     VALUE = ' '
                  ENDIF
               ELSE
                  STATUS = GNS__UNKCHA
                  CALL EMS_REP( 'GNS_IWCG_UNKCHA',
     :                          'Unknown workstation characteristic',
     :                          STATUS )
               END IF
            END IF
         ELSE
            STATUS = GNS__INWKID
            CALL EMS_REP( 'GNS_IWCG_INWKID',
     :                    'Invalid GKS workstation identifier', STATUS )
         END IF
      END IF
      END

