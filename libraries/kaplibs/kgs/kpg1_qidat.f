      SUBROUTINE KPG1_QIDAT( PNDEV, PKG, NINTS, LX, LY, STATUS )
*+
*  Name:
*     KPG1_QIDAT

*  Purpose:
*     Inquires the number of colour indices and pixel dimensions of
*     the current workstation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_QIDAT( PNDEV, PKG, NINTS, LX, LY, STATUS )

*  Description:
*     This routine makes inquires of a graphics system to determine
*     the maximum number of greyscale intensities, and the maximum
*     display-surface in pixels.

*  Arguments:
*     PNDEV = CHARACTER * ( * ) (Given)
*        The ADAM parameter associated with the current graphics
*        workstation.  It is used to generate error messages.
*     PKG = CHARACTER * ( * ) (Given)
*        The graphics system.  It must be 'SGS' or 'GKS'.
*     NINTS = INTEGER (Returned)
*        The maximum number of intensities or colour indices available
*        on the current workstation.  It is initialised to 1.
*     LX = INTEGER (Returned)
*        The maximum display-surface size in the x direction, given
*        as device (raster) pixels.  It is initialised to 1.
*     LY = INTEGER (Returned)
*        The maximum display-surface size in the y direction, given
*        as device (raster) pixels.  It is initialised to 1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  An SGS workstation must be open.
*     -  An error is reported if the device is not of the raster type.
*     Thus the device should first be checked to be a valid workstation,
*     i.e. is a raster device, or appropriate error handling added to
*     the calling routine.

*  Notes:
*     -  This routine calls a GKS inquiry to find the size of
*     the display surface.  The name of this routine changed between
*     GKS 7.2 and GKS 7.4 from GDMDS to GQDSP.  Therefore this
*     routine has two variants. This is the GKS 7.4 version.

*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 May 10 (MJC):
*        Original version.
*     1992 June 3 (MJC):
*        GKS 7.4 version.
*     2011 May 10 (MJC):
*        Set mandatory bad status before calling ERR_REP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GKS_PAR'          ! GKS parameter definitions

*  Arguments Given:
      CHARACTER * ( * ) PNDEV
      CHARACTER * ( * ) PKG

*  Arguments Returned:
      INTEGER NINTS
      INTEGER LX
      INTEGER LY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CONID              ! Connection identifier
      INTEGER GSTAT              ! Graphics status
      INTEGER JDUNIT             ! Type of units of pixel area
      INTEGER MTX                ! Max.no. of text  bundle table entries
      INTEGER MPL                ! Max.no. of polyline   "    "     "
      INTEGER MPM                ! Max.no. of polymarker "    "     "
      INTEGER MFA                ! Max.no. of fill-area  "    "     "
      INTEGER MPI                ! Max.no. of pattern indices
      CHARACTER * ( 3 ) PACKAGE  ! Graphics system
      REAL SIZX                  ! x size of the pixel area (in metres
                                 ! when JDUNIT is 0)
      REAL SIZY                  ! y size of the pixel area (in metres
                                 ! when JDUNIT is 0)
      INTEGER WKID               ! GKS workstation identifier
      INTEGER WKCLA              ! Workstation class
      INTEGER WSTYPE             ! Workstation type

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the input parameters.
*  ==============================

*  First the graphics system.  Later IDI will be added.
      PACKAGE = PKG
      CALL CHR_UCASE( PACKAGE )
      IF ( PACKAGE .NE. 'GKS' .AND. PACKAGE .NE. 'SGS' ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'PKG', PKG )
         CALL ERR_REP( 'KPG1_QIDAT',
     :     'Unable to assess the image-display characteristics due to '/
     :     /'an invalid graphics system (^PKG) being specified.',
     :     STATUS )
         GOTO 999
      END IF

*  Initialise values.
*  ==================
      LX = 1
      LY = 1
      NINTS = 1

*  Start of the section to handle the GKS system.
*  ==============================================
      IF ( PACKAGE .EQ. 'GKS' .OR. PACKAGE .EQ. 'SGS' ) THEN

*  Start the GNS system for GKS.
         CALL GNS_START( 'GKS', STATUS )

*  Inquire the workstation identifier for GKS inquiries.
         CALL SGS_ICURW( WKID )

*  Get the workstation type and class.
         CALL GQWKC( WKID, GSTAT, CONID, WSTYPE )
         CALL GQWKCL( WSTYPE, GSTAT, WKCLA )

*  Inquire whether GKS has reported an error.
         CALL GKS_GSTAT( STATUS )

*  Inquire the number of greyscale intensities that are available on
*  the specified device.
         CALL GQLWK( WSTYPE, GSTAT, MPL, MPM, MTX, MFA, MPI, NINTS )

*  Inquire whether GKS has reported an error.
         CALL GKS_GSTAT( STATUS )

*  Check that the workstation supports raster.
         IF ( WKCLA .EQ. GRASTR ) THEN

*  Find the number of pixels in x and y.
            CALL GQDSP( WSTYPE, GSTAT, JDUNIT, SIZX, SIZY, LX, LY )

*  Inquire whether GKS has reported an error
            CALL GKS_GSTAT( STATUS )

         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', PNDEV )
            CALL ERR_REP( 'KPG1_QIDAT_NORASTER',
     :        '$^NAME workstation is not a raster device, hence the '/
     :        /'size in pixels cannot be derived.', STATUS )
         END IF

*  Report the error context.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'NAME', PNDEV )
            CALL ERR_REP( 'KPG1_QIDAT_QWKST',
     :        'Error while trying to determine the maximum number of '/
     :        /'greyscale intensities or the maximum display-surface '/
     :        /'size in pixels for workstation $^NAME.', STATUS )
         END IF

*  Stop the GNS system for GKS.
         CALL GNS_STOP( 'GKS', STATUS )
      END IF

  999 CONTINUE

      END
