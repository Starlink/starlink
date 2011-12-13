      PROGRAM GNSRUN
*+
*  Name:
*     GNSRUN

*  Description:
*     Exercises the Graphics Workstation Name System as a
*     demonstration for a user and to test the installation of the GNS
*     system.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
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
*     DLT: D. L. Terrett (Starlink)
*     NE: Nick Eaton (Durham University)
*     MJB: Martin Bly (Starlink)

*  History:
*     17-MAY-1989 (DLT):
*        Original Version.
*     23-JUL-1990 (NE):
*        Removed calls to GNS_MSG.
*     14-OCT-1997 (MJB):
*        Added initial STATUS.

*-
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'GNS_ERR'
      INCLUDE 'GNS_PAR'

      INTEGER STATUS, ICNTX, LD, IERR, IWKS

      INTEGER IDISP, ISIZE(2), LUTDEP, MAXCOL, NVAL, LEN
      CHARACTER IDITYP*3, NAM*10

      CHARACTER MSG*(GNS__SZMSG)
      CHARACTER DESCR*(GNS__SZDES),
     :          NAME*(GNS__SZNAM)
      CHARACTER*72 WKSTN

      LOGICAL GNS_FILTI
      EXTERNAL GNS_FILTI
*
      STATUS = SAI__OK

*   Start the system for IDI
      CALL GNS_START('IDI',STATUS)
      IF (STATUS.NE.0) THEN
         PRINT *, '*Error* The GNS system for IDI could not be started'
         PRINT *, 'Please see your system manager'
         GO TO 9999
      END IF

*   List all the available IDI workstation names and their descriptions
      WRITE (*,'(28X,A)') 'IDI Workstation names'
      WRITE (*,'(28X,A)') '---------------------'

      IWKS = 0
      icntx = 0
  200 CONTINUE
      CALL GNS_GWNI(GNS_FILTI,ICNTX,NAME,DESCR,LD,STATUS)
      IF (STATUS.NE.0) THEN
         GO TO 9999
      END IF
      IF (ICNTX.EQ.0) GO TO 220
      PRINT *,'  ', NAME, DESCR(:LD)
      IWKS = IWKS + 1
      GO TO 200

  220 CONTINUE
      PRINT *
      IF (IWKS.EQ.0) THEN
         PRINT *, 'There are no IDI devices available on this system'
         PRINT *
         GO TO 9999
      END IF

*   Ask for a workstation name for a test plot
  160 CONTINUE
      PRINT *,'Enter a workstation name to generate a test plot'
      PRINT *,'(or control D to exit):'
      READ (*,'(A)',END=9999) WKSTN

*   Translate it to an IDI type and device name
      CALL GNS_TNI(WKSTN,IDITYP,NAM,STATUS)
      IF (STATUS.NE.0) THEN
         IF (STATUS.EQ.GNS__NAMNR.OR.STATUS.EQ.GNS__AMBNAM) THEN
            PRINT *,'Please try another name'
            STATUS = 0
            GO TO 160
         END IF
         PRINT *, 'Please see your system manager'
         GO TO 9999
      END IF

*   Open the device
      PRINT *
      WRITE(*,'(1X,5A)') 'The IDI device ', NAM,
     :    '  of type ',IDITYP,' will be opened'
      CALL IIDOPN(WKSTN,IDISP,IERR)
      IF (IERR.NE.0) THEN
         CALL IIDERR(IERR,MSG,LEN)
         PRINT *,'*Error* The selected device could not be opened'
         PRINT *, MSG(:LEN)
         PRINT *, 'Please see your system manager'
         GO TO 9999
      END IF

*   Reset the device
      CALL IIDRST(IDISP,IERR)
      IF (IERR.NE.0) GO TO 9000

*   Inquire the size of the display
      CALL IIDQCI(IDISP,12,2,ISIZE,NVAL,IERR)
      IF (IERR.NE.0) GO TO 9000

*   Inquire the maximum depth of the LUT
      CALL IIDQCI(IDISP,14,1,LUTDEP,NVAL,IERR)
      MAXCOL=2**LUTDEP-1
      IF (IERR.NE.0) GO TO 9000

*   Plot a message on the screen
      CALL IIGTXT(IDISP,0,'Successful test of IDI',ISIZE(1)/4,
     :                                  ISIZE(2)/2,0,0,MAXCOL,0,IERR)
      IF (IERR.NE.0) GO TO 9000
      CALL IIMSMV(IDISP,0,1,1,IERR)
      IF (IERR.NE.0) GO TO 9000

 9000 CONTINUE
      IF (IERR.NE.0) THEN
         CALL IIDERR(IERR,MSG,LEN)
         PRINT *,'*Error* Call to IDI failed'
         PRINT *, MSG(:LEN)
         PRINT *, 'Please see your system manager'
         GO TO 9999
      END IF

*   Close down
      CALL IIDCLO(IDISP,IERR)
      CALL GNS_STOP('IDI', STATUS)

 9999 CONTINUE
      END
