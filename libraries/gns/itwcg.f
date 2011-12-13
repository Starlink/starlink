      SUBROUTINE GNS_ITWCG ( IWKTYP, CHAR, VALUE, STATUS )

*+
*  Name:
*     GNS_ITWCG

*  Purpose:
*     Inquire workstation characteristic from its type

*  Invocation:
*     CALL GNS_ITWCG( IWKTYP, CHAR, VALUE, STATUS )

*  Description:
*     The specified characteristic is returned as a character string,
*     blank filled or truncated as necessary. If the characteristic does
*     not exist then a blank string is returned.
*
*     Any characteristic with a keyword value (see appendix A) can be
*     inquired with this routine.

*  Arguments:
*     IWKTYP = INTEGER (Given)
*        GKS workstation type
*     CHAR = CHARACTER*(*) (Given)
*        Characteristic name
*     VALUE = CHARACTER*(GNS__SZKEY) (Returned)
*        The value of the requested characteristic
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Notes:
*     This performs the same function as GNS_IWCG except that the device
*     is specified by its GKS type and therefore the characteristics can
*     be queried before the device is opened by GKS.

*  Side Effects:
*     The GNS database may be opened.

*  Copyright:
*     Copyright (C) 1990, 1992 Science & Engineering Research Council.
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
*     NE: Nick Eaton (Durham University)

*  History:
*     10-JUL-1990 (NE):
*        Original version.
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
      INTEGER IWKTYP
      CHARACTER*(*) CHAR

*  Arguments Returned:
      CHARACTER*(*) VALUE

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER L, MAXCLA, MAXOUT, MAXERA, MAXOPN
      PARAMETER( MAXCLA = 10, MAXOUT = 2, MAXERA = 1, MAXOPN = 1)

*  Local Variables:
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

*   Make sure that the common block contains data for this type
         CALL GNS_1RDWST(IWKTYP, STATUS)
         IF (STATUS.EQ.0) THEN

*   Trim blanks from the characteristic name and convert to upper case
            CALL GNS_1TRIM(CHAR,TCHAR,L)
            CALL CHR_UCASE(TCHAR)

            IF (TCHAR.EQ.'CLASS') THEN
               IF (CLASS.LE.MAXCLA) THEN
                  VALUE = CLANAM(CLASS)
               ELSE
                  VALUE = ' '
               ENDIF
            ELSEIF (TCHAR.EQ.'OUTPUT') THEN
               IF (OUTPUT.LE.MAXOUT) THEN
                  VALUE = OUTNAM(OUTPUT)
               ELSE
                  VALUE = ' '
               ENDIF
            ELSEIF (TCHAR.EQ.'CLEAR') THEN
               IF (CLEAR.LE.MAXERA) THEN
                  VALUE = ERANAM(CLEAR)
               ELSE
                  VALUE = ' '
               ENDIF
            ELSEIF (TCHAR.EQ.'OPEN') THEN
               IF (IOPEN.LE.MAXOPN) THEN
                  VALUE = OPENAM(IOPEN)
               ELSE
                  VALUE = ' '
               ENDIF
            ELSE
               STATUS = GNS__UNKCHA
               CALL EMS_REP( 'GNS_ITWCG_UNKCH',
     :                       'Unknown workstation characteristic',
     :                       STATUS )
            END IF
         ELSE
            STATUS = GNS__WKNDEF
            CALL EMS_REP( 'GNS_ITWCG_WKNDF',
     :          'Specified workstation type is not in the GNS database',
     :           STATUS )
         END IF
      END IF
      END

