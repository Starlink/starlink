      SUBROUTINE LUTSAVE( STATUS )
*+
*  Name:
*     LUTSAVE

*  Purpose:
*     Saves the current colour table of an image-display device in an
*     NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL LUTSAVE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine saves the colour table of a nominated image display to
*     an NDF LUT file and/or a text file.

*  Usage:
*     lutsave lut [device]

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The name of the image-display device whose colour table is to
*        be saved.  [Current image display]
*     FULL = _LOGICAL (Read)
*        If TRUE the whole colour-table for the device is stored
*        including the reserved pens.  This is necessary to save a
*        colour table written by another package that does not reserve
*        colour indices.  For colour tables produced by KAPPA this
*        should be FALSE. [FALSE]
*     LOGFILE = FILENAME (Write)
*        The name of a text file to receive the formatted values in the
*        colour table. Each line i the file contains the red, green and
*        blue intensities for a single pen, separated by spaces. A null
*        string (!) means that no file is created.  [!]
*     LUT = NDF (Write)
*        The output NDF into which the colour table is to be stored.
*        Its second dimension equals the number of colour-table
*        entries that are stored.  This will be less than the
*        total number of colour indices on the device if FULL is FALSE.
*        No NDF is created if a null (!) value is given.
*     TITLE = LITERAL (Read)
*       The title for the output NDF. ["KAPPA - Lutsave"]

*  Examples:
*     lutsave pizza
*        This saves the current colour table on the current
*        image-display device to an NDF called pizza.
*     lutsave redshift full
*        This saves in full the current colour table on the current
*        image-display device to an NDF called redshift.

*  Related Applications:
*     KAPPA: LUTREAD, LUTABLE, LUTEDIT, LUTVIEW.

*  Copyright:
*     Copyright (C) 2001, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-OCT-2001 (DSB):
*        Original PGPLOT version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CTM_PAR'          ! Colour-table-management constants
      INCLUDE 'PAR_ERR'          ! Parameter-system error codes
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIMS              ! Dimensionality of the LUT
      PARAMETER ( NDIMS = 2 )

*  Local Variables:
      INTEGER DIMS( NDIMS )      ! LUT dimensions
      INTEGER EL                 ! No. of mapped elements
      INTEGER FDI                ! File descriptor
      INTEGER IPIC1              ! Original AGI picture
      INTEGER LBND( NDIMS )      ! LUT lower bounds
      INTEGER LP                 ! Lowest pen to be saved
      INTEGER NDF                ! NDF identifier
      INTEGER NINTS              ! No of pens
      INTEGER PNTR               ! Pointer to mapped data array
      INTEGER UP                 ! Highest pen to be saved
      LOGICAL FULL               ! Full colour table to be saved?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Full colour table to be saved?
      CALL PAR_GTD0L( 'FULL', .FALSE., .TRUE., FULL, STATUS )

*  Open workstation without clearing the screen.
      CALL KPG1_PGOPN( 'DEVICE', 'UPDATE', IPIC1, STATUS )

*  Check whether chosen device is an 'image display' with a suitable
*  minimum number of colour indices, and will not reset when opened.
      IF( .NOT. FULL ) THEN
         CALL KPG1_PQVID( 'DEVICE', 'IMAGE_DISPLAY,'/
     :                   /'IMAGE_OVERLAY,WINDOW,MATRIX_PRINTER',
     :                   'RESET', 8 + CTM__RSVPN, NINTS, STATUS )
         LP = CTM__RSVPN
         UP = NINTS
      ELSE
         CALL KPG1_PQVID( 'DEVICE', 'IMAGE_DISPLAY,'/
     :                   /'IMAGE_OVERLAY,WINDOW,MATRIX_PRINTER',
     :                   'RESET', 8, NINTS, STATUS )
         LP = 0
         UP = NINTS
      END IF

*  Increment the number of colours is one more than the highest colour
*  index (because the background colour index is zero).
      NINTS = NINTS + 1

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Create the output simple NDF structure.
      LBND( 1 ) = 1
      LBND( 2 ) = 1
      DIMS( 1 ) = 3
      DIMS( 2 ) = UP - LP + 1
      CALL LPG_CREAT( 'LUT', '_REAL', NDIMS, LBND, DIMS, NDF, STATUS )

*  If a null value was given...
      IF( STATUS .EQ. PAR__NULL ) THEN

*  Annull the error
         CALL ERR_ANNUL( STATUS )

*  Allocate memory and store the LUT in the memory.
         CALL PSX_CALLOC( DIMS( 1 )*DIMS( 2 ), '_REAL', PNTR, STATUS )
         CALL KPG1_PLPUT( LP, UP, LP, UP, %VAL( CNF_PVAL( PNTR ) ),
     :                    STATUS )

*  Write it to a log file.
         CALL KPG1_LSTAR( DIMS( 1 ), DIMS( 2 ),
     :                    %VAL( CNF_PVAL( PNTR ) ), 1, 1,
     :                    DIMS( 1 ), DIMS( 2 ), .TRUE., FDI,
     :                    'LOGFILE', STATUS )

*  Free the memory.
         CALL PSX_FREE( PNTR, STATUS )

*  If an output NDF was specified...
      ELSE

*  Map the NDF's data component for WRITE access.
         CALL NDF_MAP( NDF, 'DATA', '_REAL', 'WRITE', PNTR, EL, STATUS )

*  Save the LUT to the NDF data component.
         CALL KPG1_PLPUT( LP, UP, LP, UP, %VAL( CNF_PVAL( PNTR ) ),
     :                    STATUS )

*  Also write it to a log file.
         CALL KPG1_LSTAR( DIMS( 1 ), DIMS( 2 ),
     :                    %VAL( CNF_PVAL( PNTR ) ), 1, 1,
     :                    DIMS( 1 ), DIMS( 2 ), .TRUE., FDI,
     :                    'LOGFILE', STATUS )

*  Set the title component of the NDF.
         CALL NDF_CINP( 'TITLE', NDF, 'TITLE', STATUS )

      END IF

 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  PGPLOT closedown sequence.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'LUTSAVE_ERR', 'LUTSAVE: Unable to save the '//
     :                 'current image-display colour table.', STATUS )
      END IF

      END
