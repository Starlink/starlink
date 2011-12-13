************************************************************************

      SUBROUTINE AGI_1DEFBA ( WKNAME, AGINAM, DEVICE, NDC, WORLD,
     :                        STATUS )

*+
*  Name:
*     AGI_1DEFBA
*
*  Purpose:
*     Define the coordinates of the base picture (Native PGPLOT)
*
*  Invocation:
*     CALL AGI_1DEFBA( WKNAME, AGINAM, DEVICE, NDC, WORLD, STATUS )
*
*  Description:
*     This defines the coordinate system of the base picture for the
*     given workstation. The coordinate system is calculated to be the
*     same as that used by GKS to enable interoperability of AGI database
*     entries. The PGPLOT view surface is defined to be represented by NDC
*     coodinates of (0,1.,0,1.) even on rectangular devices while GKS
*     always maintains a square aspect ratio until explicitly altered.
*
*     GKS compatible representations of the device, normalised device and
*     world coordinates of the base picture are returned.

*  Arguments:
*     WKNAME = CHARACTER * ( * ) (Given)
*        Workstation name
*     AGINAM = CHARACTER * ( * ) (Given but not used)
*        AGI name
*     DEVICE = REAL ( 4 ) (Returned)
*        Device coordinates of base picture
*     NDC = REAL ( 4 ) (Returned)
*        Normalised device coordinates of base picture
*     WORLD = REAL ( 4 ) (Returned)
*        World coordinates of base picture
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*     All Rights Reserved.
*
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
*     BKM: Brian McIlwrath (Starlink, RAL)
*     DSB: David Berry (Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*
*  History:
*     Dec 1999 (BKM):
*        Original version
*     1-NOV-2001 (DSB):
*        Open PGPLOT using AGP1_PGBEG.
*     21-APR-2009 (TIMJ):
*        Check status after calling AGP1_PGBEG and before inquiring the plotting
*        dimensions. Remove unsued label 99 (and corresponding call to ERR_RLSE)
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'AGI_ERR'

*  Arguments Given :
      CHARACTER * ( * ) WKNAME
      CHARACTER * ( * ) AGINAM

*  Arguments Returned :
      REAL DEVICE( 4 )
      REAL NDC( 4 )
      REAL WORLD( 4 )

*  Status :
      INTEGER STATUS

*  Local variables :
      INTEGER I
      REAL XN, YN, D
      REAL VSURSZ(4)
*.

*   Initialise if the device fails to open
      DO I = 1, 4
         DEVICE(I) = 0.0
         NDC(I) = 0.0
         WORLD(I) = 0.0
      END DO

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Ensure PGPLOT is open
         CALL AGP1_PGBEG( ' ', WKNAME, STATUS )

         IF (STATUS .EQ. SAI__OK) THEN

*   Get view surface size in inches
            CALL PGQVSZ( 2, VSURSZ(1), VSURSZ(2), VSURSZ(3), VSURSZ(4) )

*   Calculate the normalised device coordinates for this display
            D = MAX( ABS( VSURSZ(2) ), ABS( VSURSZ(4) ) )
            XN = VSURSZ(2) / D
            YN = VSURSZ(4) / D

*   Define the NDC coordinates for this area
            NDC( 1 ) = 0.0
            NDC( 2 ) = XN
            NDC( 3 ) = 0.0
            NDC( 4 ) = YN

*   Define the world coordinates for this area
            WORLD( 1 ) = 0.0
            WORLD( 2 ) = D / VSURSZ(4)
            WORLD( 3 ) = 0.0
            WORLD( 4 ) = D / VSURSZ(2)

*   Get full view surface size in device units (dots/pixels)
            CALL PGQVSZ( 3, VSURSZ(1), VSURSZ(2), VSURSZ(3), VSURSZ(4) )

*   Define the device coordinates for this area
            DEVICE( 1 ) = 0.0
            DEVICE( 2 ) = REAL( VSURSZ(2) )
            DEVICE( 3 ) = 0.0
            DEVICE( 4 ) = REAL( VSURSZ(4) )

         ENDIF

      ENDIF

      END
