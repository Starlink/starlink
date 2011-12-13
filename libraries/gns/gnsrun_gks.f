      PROGRAM GNSRUN
*+
*  Name:
*     GNSRUN

*  Purpose:
*     Test GNS

*  Language:
*     Starlink Fortran 77

*  Description:
*     Exercises the Graphics Workstation Name System as a
*     demonstration for a user and to test the installation of the GNS
*     system.

*  Copyright:
*     Copyright (C) 1989-1990 Science & Engineering Research Council.
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
*     NE: Nick Eaton (Starlink)
*     {enter_new_authors_here}

*  History:
*     17-MAY-1989 (DLT):
*        Modified.
*     23-JUL-1990 (NE):
*        Removed calls to GNS_MSG.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE
      INCLUDE 'GNS_ERR'
      INCLUDE 'GNS_PAR'
      INCLUDE 'GKS_PAR'

      INTEGER STATUS, ICNTX, LD, ITYPE, ICON, IERR, ISTATE, LDEV,
     : IUNIT, LX, LY, ASF(13), LTERM, LERTXT, IWKS
      REAL RX, RY, SCALE, XSIZE, YSIZE, XBOX(5), YBOX(5), X(2), Y(2)

      CHARACTER DESCR*(GNS__SZDES),
     :          DEVICE*(GNS__SZDEV), ERTXT*(GNS__SZTXT),
     :          NAME*(GNS__SZNAM), OUTPUT*(GNS__SZKEY),
     :          TERM*(GNS__SZTER)
      CHARACTER*72 WKSTN

      LOGICAL GNS_FILTG, GNS_FILTI
      EXTERNAL GNS_FILTG, GNS_FILTI

      REAL XLEFT, XRIGHT, YBOT, YTOP
      PARAMETER (XLEFT = -10.0, XRIGHT = 45.0,
     :            YBOT = -10.0, YTOP = 25.0)

      REAL XDUCK(43), YDUCK(43)
      DATA XDUCK / 0.0,2.0,4.0,6.0,8.0,10.0,12.0,14.0,16.4,17.0,17.3,
     1          17.8, 18.5, 20.0, 22.0, 24.0, 26.0, 28.0, 29.0,
     2          28.8,27.2,25.0,23.0,21.5,21.1,21.5,22.8, 24.1, 25.1,
     3          25.2, 24.2, 22.1, 20.0, 18.0, 16.0, 14.0, 12.0,
     4          10.0,  8.0,  6.1,  4.2,  3.0,  1.3 /
      DATA YDUCK / 8.8, 7.6, 7.1, 7.4, 8.0, 8.9, 9.6, 9.9, 9.4,
     1          9.7, 12.0, 14.0, 16.1, 17.0, 17.0, 16.0, 13.9,
     2          13.1, 13.2, 12.3, 11.5, 11.5, 11.5, 11.2, 10.5,
     3          9.0, 8.0, 7.0, 5.1, 3.6, 1.9, 1.1, 0.9, 0.7,
     4          0.8, 1.0, 1.0, 1.2, 1.8, 2.1, 2.9, 4.1, 6.0 /

      DATA STATUS, ICNTX /0, 0/
      DATA XBOX /XLEFT,XRIGHT,XRIGHT,XLEFT,XLEFT/
      DATA YBOX /YBOT,YBOT,YTOP,YTOP,YBOT/

*   Start the system for GKS
      CALL GNS_START('GKS',STATUS)
      IF (STATUS.NE.0) THEN
         PRINT *, '*Error* The GNS system for GKS could not be started'
         PRINT *, 'Please see your system manager'
         GO TO 8888
      END IF

*   Open GKS so that GNS_FILTG can remove devices not supported by GKS.
      CALL GOPKS(6, -1)

*   List all the available GKS workstation names and their descriptions
      WRITE (*,'(28X,A)') 'GKS Workstation names'
      WRITE (*,'(28X,A)') '---------------------'

      IWKS = 0
  100 CONTINUE
      CALL GNS_GWNG(GNS_FILTG,ICNTX,NAME,DESCR,LD,STATUS)
      IF (STATUS.NE.0) THEN
         GO TO 8888
      END IF
      IF (ICNTX.EQ.0) GO TO 120
      PRINT *,'  ', NAME, DESCR(:LD)
      IWKS = IWKS + 1
      GO TO 100

  120 CONTINUE
      PRINT *
      IF (IWKS.EQ.0) THEN
         PRINT *, 'There are no GKS devices available on this system'
         PRINT *
         GO TO 8888
      END IF

*   Ask for a workstation name for a test plot
  140 CONTINUE
      PRINT *,'Enter a workstation name to generate a test plot'
      PRINT *,'(or control D to exit):'
      READ (*,'(A)',END=8888) WKSTN

*   Translate it to a GKS type and connection id
      CALL GNS_TNG(WKSTN,ITYPE,ICON,STATUS)
      IF (STATUS.NE.0) THEN
         IF (STATUS.EQ.GNS__NAMNR.OR.STATUS.EQ.GNS__AMBNAM) THEN
            PRINT *,'Please try another name'
            STATUS = 0
            GO TO 140
         END IF
         PRINT *, 'Please see your system manager'
         GO TO 8888
      END IF

*   Set the Fill area style ASF to individual
      CALL GQASF(IERR, ASF)
      ASF(11) = GINDIV
      ASF(12) = GINDIV
      CALL GSASF(ASF)

*   Open the selected workstation
      PRINT *
      WRITE(*,'(1X,A,I5,A,I3,A)') 'The GKS workstation type ', ITYPE,
     : ' connection identifier ', ICON, ' will be opened'
      CALL GOPWK(1, ICON, ITYPE)

*   Check that the workstation is now open
      CALL GQWKS(1, IERR, ISTATE)
      IF (IERR.NE.0) THEN
         PRINT *,'*Error* The selected workstation could not be opened'
         GO TO 8888
      END IF

*   Clear the text screen if the device is the same as the command
*   terminal
      CALL GNS_GTN(TERM, LTERM, STATUS)
      IF (STATUS.NE.0) THEN
         IF (STATUS.NE.GNS__NOTINT) THEN
            PRINT *, 'Please see your system manager'
            GO TO 8888
         ELSE
            STATUS = 0
            LTERM = 0
         END IF
      END IF

*   Get the name of the plotting device
      CALL GNS_IDNG(1, DEVICE, LDEV, STATUS)
      IF (STATUS.NE.0) THEN
         PRINT *, 'Please see your system manager'
         GO TO 8888
      END IF

*   If they match then get the text string that will erase the text
*   screen
      IF (LTERM.GT.0) THEN
         IF (TERM(:LTERM).EQ.DEVICE(:LDEV)) THEN
            CALL GNS_IETG(1, ERTXT, LERTXT, STATUS)
            IF (STATUS.NE.0) THEN
               PRINT *, 'Please see your system manager'
               GO TO 8888
            END IF
            IF (LERTXT.GT.0) PRINT *,ERTXT(:LERTXT)
         END IF
      END IF

*   Report the device or file name being used
      CALL GNS_IWCG(1, 'OUTPUT', OUTPUT, STATUS)
      IF (STATUS.NE.0) THEN
         PRINT *, 'Please see your system manager'
         GO TO 8888
      END IF
      IF (OUTPUT.EQ.'DIRECT') THEN
         PRINT *,'Plotting on ',DEVICE(:LDEV)
      ELSE IF (OUTPUT.EQ.'FILE') THEN
         PRINT *,'Writing plot file ',DEVICE(:LDEV)
      ELSE
         PRINT *,
     :'*Error* Unexpected output type - Please report to system manager'
         GO TO 8888
      END IF

*   Modify the workstation transformation so that the entire display
*   surface is accessable and set up two transformation, one with world
*   coordinates suitable for drawing the duck and the other with metres
*   as the coordinates with 0,0 at the bottom centre of the display
*   surface.
      CALL GQDSP(ITYPE, IERR, IUNIT, RX, RY, LX, LY)
      CALL GSWKVP(1, 0.0, RX, 0.0, RY)
      CALL GSWKWN(1, 0.0, MIN(1.0,RX/RY), 0.0, MIN(1.0,RY/RX))
      CALL GSVP(1, 0.0, MIN(1.0,RX/RY), 0.0, MIN(1.0,RY/RX))
      CALL GSVP(2, 0.0, MIN(1.0,RX/RY), 0.0, MIN(1.0,RY/RX))
      CALL GSWN(1, XLEFT, XRIGHT, YBOT, YTOP)
      CALL GNS_IWSG(1, SCALE, STATUS)
      IF (STATUS.NE.0) THEN
         PRINT *, 'Please see your system manager'
      END IF
      IF (SCALE.EQ.0) THEN
         PRINT *,'Workstation scale not available'
      ELSE
         XSIZE = RX * SCALE
         YSIZE = RY * SCALE
         CALL GSWN(2, -(XSIZE/2.0), XSIZE/2.0, 0.0, YSIZE)
      END IF

*   Draw a border around the display surface
      CALL GACWK(1)
      CALL GSELNT(1)
      CALL GPL(5, XBOX, YBOX)

*   Draw a 10cm bar at the bottom of the display
      CALL GSELNT(2)
      X(1) = -0.05
      X(2) = 0.05
      Y(1) = 0.02
      Y(2) = 0.02
      CALL GPL(2,X,Y)
      X(2) = -0.05
      Y(1) = 0.015
      Y(2) = 0.025
      CALL GPL(2,X,Y)
      X(1) = 0.05
      X(2) = 0.05
      CALL GPL(2,X,Y)

*   Label it
      CALL GSTXFP(1, 2)
      CALL GSCHH(0.002)
      CALL GSTXAL(GACENT, GACENT)
      CALL GTX(0.0, 0.025, '10 cm')

*   Draw the wretched duck
      CALL GSELNT(1)
      CALL GSFAIS(GHATCH)
      CALL GSFAI(1)
      CALL GFA(43, XDUCK, YDUCK)

*   Close down
      CALL GDAWK(1)
      CALL GCLWK(1)
      CALL GCLKS
      CALL GNS_STOP('GKS', STATUS)

 8888 CONTINUE
      END
