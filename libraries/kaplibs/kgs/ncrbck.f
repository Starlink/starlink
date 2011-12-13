      SUBROUTINE NCRBCK( XMIN, XMAX, YMIN, YMAX, TITLE, XTITLE, YTITLE,
     :                   MINTIC, MAJTIC, OUTTIC, NICE, COMMNT, PIC0,
     :                   PIC1, STATUS )
*+
*  Name:
*     NCRBCK

*  Purpose:
*     Draw a standard set of axes for an AUTOGRAPH plot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NCRBCK( XMIN, XMAX, YMIN, YMAX, TITLE, XTITLE, YTITLE,
*    :             MINTIC, MAJTIC, OUTTIC, NICE, COMMNT, PIC0, PIC1,
*    :             STATUS )

*  Description:
*     This routine gives a standard background of labelled and annotated
*     axes, within the contraints and options.  This requires a slightly
*     smaller grid region, however it is reset to the input size on
*     exit. The plotting is done within the current SGS zone. The
*     AUTOGRAPH grid window (in user coordinates) is saved as a DATA
*     picture in the AGI database. The current picture on entry to this
*     routine is reinstated before returning.  The SGS zone on exit is
*     the grid window, with the defined bounds as its world
*     co-ordinates.

*  Arguments:
*     XMIN = REAL (Given)
*        Lower limit of the X axis in user coordinates.
*     XMAX = REAL (Given)
*        Upper limit of the X axis in user coordinates.
*     YMIN = REAL (Given)
*        Lower limit of the y axis in user coordinates.
*     YMAX = REAL (Given)
*        Upper limit of the y axis in user coordinates.
*     TITLE = CHARACTER * ( * ) (Given)
*        A title for the plot.
*     XTITLE = CHARACTER * ( * ) (Given)
*        A title for the X axis.
*     YTITLE = CHARACTER * ( * ) (Given)
*        A title for the Y axis.
*     MINTIC( 2 ) = REAL (Given)
*        The number of minor tick marks between each major tick mark
*        for the x and y axes.  A negative value forces the graphics
*        package to compute appropriate values.
*     MAJTIC( 2 ) = REAL (Given)
*        The parameter controlling the numbers of major tick marks
*        for the x and y axes.  (Number used is between MAJTIC+2 and
*        5*MAJTIC/2+4.) A negative value forces the graphics package
*        to compute appropriate values.
*     OUTTIC = LOGICAL (Given)
*        If true the axis tick marks are drawn outside the box.
*     NICE = LOGICAL (Given)
*         If true, a major tick mark will appear at the ends of each
*         axis.
*     COMMNT = CHARACTER * ( * ) (Given)
*        A comment to store in the AGI database with the new DATA
*        picture.
*     PIC0 = INTEGER (Given)
*        The identifier of the current AGI picture.
*     PIC1 = INTEGER (Returned)
*        The identifier of the new AGI DATA picture.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-JUN-1990 (DSB):
*        Original version.
*     1990 October 4 (MJC):
*        Incorporated KAPPA standard style control and resetting of the
*        AUTOGRAPH parameters via a subroutine.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:

      IMPLICIT NONE              ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants


*  Arguments Given:
      REAL        XMIN
      REAL        XMAX
      REAL        YMIN
      REAL        YMAX
      CHARACTER   TITLE*(*)
      CHARACTER   XTITLE*(*)
      CHARACTER   YTITLE*(*)
      CHARACTER   COMMNT*(*)
      REAL        MINTIC( 2 )
      REAL        MAJTIC( 2 )
      LOGICAL     NICE
      LOGICAL     OUTTIC
      INTEGER     PIC0

*  Arguments Returned:
      INTEGER     PIC1


*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      REAL      SNX_AGGUX        ! Function to convert grid X units to
                                 ! user X units.
      REAL      SNX_AGGUY        ! Function to convert grid Y units to
                                 ! user Y units.
      EXTERNAL SNX_AGGUX
      EXTERNAL SNX_AGGUY

*  Local Constants:
      REAL
     :  ANCLP1, ANCLP2,          ! Fraction of the frame zone in which
     :  ANCLP3, ANCLP4           ! the image will appear when there are
                                 ! axes. Note aspect ratio is preserved.
      PARAMETER ( ANCLP1 = 0.19, ANCLP2 = 0.95,
     :            ANCLP3 = 0.15, ANCLP4 = 0.91 )

*  Local Variables:
      REAL      ANCLIP( 4 )      ! Fraction of the frame zone in which
                                 ! the graph itself will appear (can't
                                 ! give an array directly as parameter)
      REAL      GRID( 4 )        ! Current AUTOGRAPH grid offsets
      INTEGER   GRIDZN           ! Identifier for SGS zone covering the
                                 ! AUTOGRAPH grid window.
      INTEGER   TSTAT            ! Temporary status

*.

*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the parameters that define the grid region.

      ANCLIP( 1 ) = ANCLP1
      ANCLIP( 2 ) = ANCLP2
      ANCLIP( 3 ) = ANCLP3
      ANCLIP( 4 ) = ANCLP4

*  Get AUTOGRAPH to use the SGS zone.

      CALL SNX_AGWV

*  Store the current NCAR grid values.

      CALL AGGETF( 'GRID/LEFT.', GRID( 1 ) )
      CALL AGGETF( 'GRID/RIGHT.', GRID( 2 ) )
      CALL AGGETF( 'GRID/BOTTOM.', GRID( 3 ) )
      CALL AGGETF( 'GRID/TOP.', GRID( 4 ) )

*  Store the current NCAR grid values.

      CALL AGSETF( 'GRID/LEFT.', ANCLIP( 1 ) )
      CALL AGSETF( 'GRID/RIGHT.', ANCLIP( 2 ) )
      CALL AGSETF( 'GRID/BOTTOM.', ANCLIP( 3 ) )
      CALL AGSETF( 'GRID/TOP.', ANCLIP( 4 ) )

*  Draw the annotated axes, with unit line thickness.

      CALL NCRAXS( XMIN, YMIN, XMAX, YMAX, TITLE, XTITLE, YTITLE,
     :             MINTIC, MAJTIC, OUTTIC, 1.0, NICE, STATUS )

*  Create and select an SGS zone which just encompasses the AUTOGRAPH
*  grid window for future plotting of the graph itself. The world
*  co-ordinates will be 0.0 to 1.0 on both axes.

      CALL SNX_AGCS
      CALL SGS_ZONE( 0.0, 1.0, 0.0, 1.0, GRIDZN, STATUS )

*  Set the world co-ordinates of the new zone to be user co-ordinates.

      CALL SGS_SW( SNX_AGGUX( 0.0 ), SNX_AGGUX( 1.0 ),
     :             SNX_AGGUY( 0.0 ), SNX_AGGUY( 1.0 ), STATUS )

*  Restore the input NCAR grid values.

      CALL AGSETF( 'GRID/LEFT.', GRID( 1 ) )
      CALL AGSETF( 'GRID/RIGHT.', GRID( 2 ) )
      CALL AGSETF( 'GRID/BOTTOM.', GRID( 3 ) )
      CALL AGSETF( 'GRID/TOP.', GRID( 4 ) )

*  If either SGS routine failed, report the error and set the global
*  status.

      IF ( STATUS .NE. SAI__OK ) THEN

         CALL ERR_REP( 'NCRBCK_ERR1',
     :     'NCRBCK: Error matching SGS zone and AUTOGRAPH grid window.',
     :      STATUS )

      END IF


*  Save the grid window zone as a DATA picture in AGI.

      CALL AGS_SZONE( 'DATA', COMMNT, PIC1, STATUS )


*  Re-instate the original current picture.  Allow for a bad status
*  which would otherwise prevent this from happening.

      IF ( STATUS .NE. SAI__OK ) THEN
         TSTAT = STATUS
         STATUS = SAI__OK

         CALL AGI_SELP( PIC0, STATUS )
         IF ( STATUS .EQ. SAI__OK ) STATUS = TSTAT
      ELSE
         CALL AGI_SELP( PIC0, STATUS )
      END IF


      END
