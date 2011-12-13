      SUBROUTINE gns_1FNDF (PACK,FILE,NAME)
*+
*  Name:
*     gns_1FNDF

*  Purpose:
*     Locates a GNS data file.

*  Language:
*     Starlink Fortran 77

*  Description:
*     The GNS data file for the specified package is located by checking
*     for the existence of the following files:
*
*        ${GNS_<PACK><FILE>} (eg. translation of the environment variable
*                             GNS_GKSNAMES).
*        ${GNS_DIR}/gns_<pack><file>
*        ${PATH}/etc/gns_<pack>_<file>

*  Arguments:
*     PACK = CHAR (Given)
*         Package name (eg GKS or IDI)
*     FILE = CHAR (Given)
*         File to be found (eg NAMES or DEVICES)
*     NAME = CHAR (Returned)
*         Full file name

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DLT: D L Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     25-JUN-1995 (DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Implicit Inputs:
*     none

*  Implicit Outputs:
*     none

*-
      IMPLICIT NONE

      INCLUDE 'SAE_PAR'

      CHARACTER*(*) PACK, FILE, NAME

      CHARACTER*3 UPACK, LPACK
      CHARACTER*7 UFILE, LFILE
      CHARACTER*15 ENV
      CHARACTER*2048 DIR
      INTEGER STATUS, I1, I2, LDIR
      LOGICAL FOUND
      INTEGER CHR_LEN

      CALL EMS_MARK

*  Try file specific environment variable.
      UPACK = PACK
      CALL CHR_UCASE(UPACK)
      UFILE = FILE
      CALL CHR_UCASE(UFILE)

      ENV = 'GNS_' // UPACK(:LEN(PACK)) // UFILE
      STATUS = SAI__OK
      CALL PSX_GETENV(ENV, NAME, STATUS)
      IF (STATUS.EQ.SAI__OK) THEN
         INQUIRE (FILE=NAME, EXIST=FOUND)
         IF (FOUND) GOTO 999
      ENDIF

*  Try package directory environment variable.
      LPACK = PACK
      CALL CHR_LCASE(LPACK)
      LFILE = FILE
      CALL CHR_LCASE(LFILE)

      STATUS = SAI__OK
      CALL PSX_GETENV('GNS_DIR', DIR, STATUS)
      IF (STATUS.EQ.SAI__OK) THEN
         NAME = DIR(:CHR_LEN(DIR)) // '/gns_' // LPACK(:LEN(PACK)) //
     :          LFILE
         INQUIRE (FILE=NAME, EXIST=FOUND)
         IF (FOUND) GOTO 999
      ENDIF

*  Try directories on PATH.
      STATUS = SAI__OK
      CALL PSX_GETENV('PATH', DIR, STATUS)
      IF (STATUS.EQ.SAI__OK) THEN
         LDIR = CHR_LEN(DIR)
         I1 = 1
   10    CONTINUE
         I2 = INDEX(DIR(I1:),':')
         IF (I2.EQ.0) THEN
            NAME = DIR(I1:CHR_LEN(DIR)) // '/../etc/gns_' //
     :             LPACK(:LEN(PACK)) // LFILE
         ELSE
            NAME = DIR(I1:I1+I2-2) // '/../etc/gns_' //
     :             LPACK(:LEN(PACK)) // LFILE
            I1 = I1 + I2
         ENDIF
         INQUIRE (FILE=NAME, EXIST=FOUND)
         IF (FOUND) GOTO 999

         IF (I2.NE.0 .AND. I1.LE. LDIR) GO TO 10
      END IF
      NAME = ' '
  999 CONTINUE
      CALL EMS_ANNUL(STATUS)
      CALL EMS_RLSE
      END
