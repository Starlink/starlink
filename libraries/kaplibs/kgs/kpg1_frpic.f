      SUBROUTINE KPG1_FRPIC( PNXSIZ, PNYSIZ, LABEL, SQUARE, ZONE,
     :                       PICID, STATUS )
*+
*  Name:
*     KPG1_FRPIC

*  Purpose:
*     Creates a frame picture of a given size within the current
*     picture.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_FRPIC( PNXSIZ, PNYSIZ, LABEL, SQUARE, ZONE, PICID,
*    :                 STATUS )

*  Description:
*     This routine obtains from the environment the size of the frame
*     zone in metres up to the size of the current picture.  A new zone
*     is created, optionally selecting the largest square zone
*     within it and normalising the world co-ordinates.  This frame
*     picture is saved with the specified label into the graphics
*     database.

*  Arguments:
*     PNXSIZ = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter which is to be used to obtain
*        the x size of the frame picture in metres.
*     PNYSIZ = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter which is to be used to obtain
*        the y size of the frame picture in metres.
*     LABEL = CHARACTER * ( * ) (Given)
*        The label for the frame picture.
*     SQUARE = LOGICAL (Given)
*        If true the largest square within the sized zone becomes the
*        frame picture, and it will have world co-ordinates from 0.0 to
*        1.0 in both x and y.
*     ZONE = INTEGER (Returned)
*        The SGS zone identifier of the frame.
*     PICID = INTEGER (Returned)
*        The picture identifier of the frame picture.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  The AGI SGS interface must be active.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 25 (MJC):
*        Original version.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      CHARACTER * ( * )
     :  LABEL,
     :  PNXSIZ,
     :  PNYSIZ

      LOGICAL
     :  SQUARE

*  Arguments Returned:
      INTEGER
     :  PICID,
     :  ZONE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL
     :  PXSIZE,                  ! Physical size of the x axis in metres
                                 ! to scale the plot
     :  PYSIZE,                  ! Physical size of the y axis in metres
     :  X1, X2,                  ! The x bounds in world co-ordinates
                                 ! of the input zone
     :  Y1, Y2,                  ! The y bounds in world co-ordinates
                                 ! of the input zone
     :  XM, YM                   ! The x-y size in metres of the input
                                 ! zone

      INTEGER
     :  ZONET                    ! Temporary zone for a square picture
*.

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Inquire zone the size.

      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*    Get the size of the plot.

      CALL PAR_GDR0R( PNXSIZ, ( 1.0 - 4.0 * VAL__EPSR ) * XM, 0.004, XM,
     :                .TRUE., PXSIZE, STATUS )
      CALL PAR_GDR0R( PNYSIZ, ( 1.0 - 4.0 * VAL__EPSR ) * YM, 0.004, YM,
     :                .TRUE., PYSIZE, STATUS )

*    Create a zone of the specified size.

      CALL SGS_ZSIZE( PXSIZE, PYSIZE, 'BL', ZONE, STATUS )

      IF ( SQUARE ) THEN

*       Create the largest square zone in which to plot say to keep
*       square pixels.

         CALL SGS_ZSHAP( 1.0, 'BL', ZONET, STATUS )

*       Normalised the world co-ordinates.

         CALL SGS_SW( 0.0, 1.0, 0.0, 1.0, STATUS )

*       Release the unwanted zone identifier.

         CALL SGS_RELZ( ZONE )

*       Make the current the one to export.

         ZONE = ZONET
      END IF

*    Record the base picture in the database

      CALL AGS_SZONE( 'FRAME', LABEL, PICID, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'KPG1_FRPIC_DBSF',
     :     'Error while storing the frame picture in the graphics '/
     :     /'database.', STATUS )
      END IF

      END
