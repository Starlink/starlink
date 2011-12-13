************************************************************************

      SUBROUTINE AGI_1DEFBA ( WKNAME, AGINAM, DEVICE, NDC, WORLD,
     :                        STATUS )

*+
*  Name:
*     AGI_1DEFBA
*
*  Purpose:
*     Define the coordinates of the base picture
*
*  Invocation:
*     CALL AGI_1DEFBA( WKNAME, AGINAM, DEVICE, NDC, WORLD, STATUS )
*
*  Description:
*     This defines the coordinate system of the base picture for the
*     given workstation. The coordinate system is obtained from the
*     GKS desciption of the device and therfore the workstation name
*     or the AGI name must translate into a GKS type.
*     The device, normalised device and world coordinates of the base
*     picture are returned.
*
*  Arguments:
*     WKNAME = CHARACTER * ( * ) (Given)
*        Workstation name
*     AGINAM = CHARACTER * ( * ) (Given)
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
*  Algorithm:
*     Check status on entry.
*     Get the GKS workstation identifier for the current picture.
*     If GKS is closed then open it.
*     If the device is an input/output device then
*        Inquire the device size.
*     Else
*        Assume a square display for a metafile or segment.
*     Endif
*     Close GKS if it was opened in this routine.
*     Define the device, ndc and world coordinates for the base picture.
*
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
*     NE: Nick Eaton (Durham University)
*
*  History:
*     Oct 1988 (NE):
*        Original version
*     Jun 1990 (NE):
*        Added AGINAM to argument list
*     Dec 1990 (NE):
*        Use raster units from GQDSP
*     Feb 1992 (NE):
*        GKS 7.4 version
*     Jul 1992 (NE):
*        Test status from all GKS inquiry routines.
*        Calculate NDC's from display size rather than raster units.
*     Oct 1992 (NE):
*        Improve error reporting
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'AGI_ERR'
      INCLUDE 'GKS_PAR'

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
      LOGICAL GKSOP

      CHARACTER DEVNAM * 64

      INTEGER CONID, ISTAT, LNAME, OPERS, UNITS, WKCAT, WKTY, XR, YR

      REAL D, XN, YN, XU, YU
*.

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Use a local status value for this section
         ISTAT = SAI__OK
         CALL ERR_MARK

*   Get the GKS workstation identifier for the current picture
         CALL SGS_WIDEN( WKNAME, WKTY, CONID, ISTAT )

*   If this has failed then try the AGI name
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( ISTAT )
            CALL GNS_IGAG( AGINAM, DEVNAM, ISTAT )
            CALL SGS_WIDEN( DEVNAM, WKTY, CONID, ISTAT )

*   If this has also failed then give up
            IF ( ISTAT .NE. SAI__OK ) THEN
               CALL ERR_RLSE
               STATUS = AGI__NAMNR
               LNAME = INDEX( WKNAME, ' ' ) - 1
               CALL MSG_SETC( 'NAME', WKNAME(:LNAME) )
               CALL ERR_REP( 'AGI_1DEFBA_NMNR',
     :                      'Device name ^NAME not recognised', STATUS )
               GOTO 99
            ENDIF
         ENDIF
         CALL ERR_RLSE

*   Inquire GKS state
         CALL GQOPS( OPERS )

*   If GKS is closed then open it, and remember that it has been opened
         IF ( OPERS .EQ. GGKCL ) THEN
            CALL GOPKS( 6, -1 )
            GKSOP = .TRUE.
         ELSE
            GKSOP = .FALSE.
         ENDIF

*   Inquire the category of the workstation
         CALL GQWKCA( WKTY, ISTAT, WKCAT )

*   Check that there have not been any errors from GKS
         IF ( ISTAT .NE. 0 ) THEN
            STATUS = AGI__GKERR
            CALL MSG_SETI( 'ISTAT', ISTAT )
            CALL ERR_REP( 'AGI_1DEFBA_EROR',
     :                    'Error ^ISTAT returned by GQWKCA', STATUS )
            LNAME = INDEX( WKNAME, ' ' ) - 1
            CALL MSG_SETC( 'NAME', WKNAME(:LNAME) )
            CALL ERR_REP( 'AGI_1DEFBA_EROR',
     :                    'Unable to query device ^NAME', STATUS )
            GOTO 99
         ENDIF

*   If it is an input or output device then get its size
         IF ( ( WKCAT .EQ. GOUTPT ) .OR.
     :        ( WKCAT .EQ. GINPUT ) .OR.
     :        ( WKCAT .EQ. GOUTIN ) ) THEN
            CALL GQDSP( WKTY, ISTAT, UNITS, XU, YU, XR, YR )

*   Check that there have not been any errors from GKS
            IF ( ISTAT .NE. 0 ) THEN
               STATUS = AGI__GKERR
               CALL MSG_SETI( 'ISTAT', ISTAT )
               CALL ERR_REP( 'AGI_1DEFBA_EROR',
     :                       'Error ^ISTAT returned by GQDSP', STATUS )
               LNAME = INDEX( WKNAME, ' ' ) - 1
               CALL MSG_SETC( 'NAME', WKNAME(:LNAME) )
               CALL ERR_REP( 'AGI_1DEFBA_EROR',
     :                       'Unable to query device ^NAME', STATUS )
               GOTO 99
            ENDIF

*   Workstation is a metafile or segment so assume a square display
         ELSE
            XU = 1.0
            YU = 1.0
         ENDIF

*   Can close GKS again if it was opened above
         IF ( GKSOP ) THEN
            CALL GCLKS
         ENDIF

*   Calculate the normalised device coordinates for this display
*   This is the same as CALL SGS_1NORM( XU, YU, XN, YN )
*   The only reason for not doing this is that SGS_1NORM is not in
*   the ADAM shareable library.
         D = MAX( ABS( XU ), ABS( YU ) )
         XN = XU / D
         YN = YU / D

*   Define the device coordinates for this area
         DEVICE( 1 ) = 0.0
         DEVICE( 2 ) = REAL( XR )
         DEVICE( 3 ) = 0.0
         DEVICE( 4 ) = REAL( YR )

*   Define the NDC coordinates for this area
         NDC( 1 ) = 0.0
         NDC( 2 ) = XN
         NDC( 3 ) = 0.0
         NDC( 4 ) = YN

*   Define the world coordinates for this area
         WORLD( 1 ) = 0.0
         WORLD( 2 ) = D / YU
         WORLD( 3 ) = 0.0
         WORLD( 4 ) = D / XU

      ENDIF

  99  CONTINUE

      END

