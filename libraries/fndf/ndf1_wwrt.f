      SUBROUTINE NDF1_WWRT( IWCS, IDCB, STATUS )
*+
*  Name:
*     NDF1_WWRT

*  Purpose:
*     Write WCS information to an entry in the DCB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_WWRT( IWCS, IDCB, STATUS )

*  Description:
*     This routine stores new WCS (World Coordinate System) information
*     in a data object with an entry in the DCB, over-writing any
*     information that may already be present.

*  Arguments:
*     IWCS = INTEGER (Given)
*        A pointer to an AST_ object which contains the coordinate
*        system information to be written. This should be a pointer to
*        a FrameSet satisfying all the requirements imposed by the NDF_
*        library, but this routine makes no check on this and any valid
*        AST_ pointer will be accepted and used. The AST_ object itself
*        is not modified by this routine.
*     IDCB = INTEGER (Given)
*        Index to the data object entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine stores a cloned pointer to the AST_ object
*     supplied (not a copy) in the DCB.

*  Copyright:
*     Copyright (C) 1997 Rutherford Appleton Laboratory.

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     24-JUN-1997 (RFWS):
*        Original version.
*     14-JUL-1997 (RFWS):
*        Use AST_EXEMPT.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'AST_PAR'          ! AST_ public interface

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ASTLC = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to HDS _CHAR array holding AST_ data.
*        DCB_ASTLN = INTEGER (Write)
*           Next element to use in HDS _CHAR array holding AST_ data.
*        DCB_ASTPT = INTEGER (Write)
*           Pointer to mapped HDS _CHAR array holding AST_ data.
*        DCB_IWCS( NDF__MXDCB ) = INTEGER (Read and Write)
*           Pointer to DCB world coordinate system information.
*        DCB_KW( NDF__MXDCB ) = LOGICAL (Write)
*           Whether DCB world coordinate system information is
*           available.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

*  Arguments Given:
      INTEGER IWCS
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL NDF1_WRAST        ! Write AST_ data to an HDS object

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) WCSLOC ! Locator to WCS component
      INTEGER CHAN               ! Pointer to AST_ Channel
      INTEGER DIM( 1 )           ! Dimension size of HDS object
      INTEGER NWRITE             ! Number of AST_ objects written
      LOGICAL THERE              ! Component present?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that WCS information is available in the DCB.
      CALL NDF1_DW( IDCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If a pointer to WCS information is held in the DCB, then annul it.
         IF ( DCB_IWCS( IDCB ) .NE. AST__NULL ) THEN
            CALL AST_ANNUL( DCB_IWCS( IDCB ), STATUS )
         END IF

*  If a 'WCS' component exists in the NDF data structure, then erase it.
         THERE = .FALSE.
         CALL DAT_THERE( DCB_LOC( IDCB ), 'WCS', THERE, STATUS )
         IF ( THERE ) THEN
            CALL DAT_ERASE( DCB_LOC( IDCB ), 'WCS', STATUS )
         END IF

*  Clone an AST_ pointer to the WCS information and store it in the
*  DCB. Exempt the new pointer from AST_ context handling (so it is not
*  annulled if AST_END is called).
         DCB_IWCS( IDCB ) = AST_CLONE( IWCS, STATUS )
         CALL AST_EXEMPT( DCB_IWCS( IDCB ), STATUS )

*  Create a new WCS component (a scalar structure).
         DIM( 1 ) = 0
         CALL DAT_NEW( DCB_LOC( IDCB ), 'WCS', 'WCS', 0, DIM, STATUS )

*  Obtain a locator to this structure and create a DATA component (a
*  1-dimensional _CHAR array) within it to hold the information.
         WCSLOC = DAT__NOLOC
         CALL DAT_FIND( DCB_LOC( IDCB ), 'WCS', WCSLOC, STATUS )
         CALL DAT_NEW1C( WCSLOC, 'DATA', NDF__SZAST, NDF__INAST,
     :                   STATUS )

*  Obtain a locator to the DATA component, storing this in the DCB and
*  annul the WCS structure locator.
         DCB_ASTLC = DAT__NOLOC
         CALL DAT_FIND( WCSLOC, 'DATA', DCB_ASTLC, STATUS )
         CALL DAT_ANNUL( WCSLOC, STATUS )

*  Map the new object for write access and store the resulting pointer
*  in the DCB.
         DIM( 1 ) = NDF__INAST
         CALL DAT_MAP( DCB_ASTLC, '_CHAR', 'WRITE', 1, DIM, DCB_ASTPT,
     :                 STATUS )

*  Create an AST_ Channel to write the supplied data to the new
*  component.  Supply the NDF1_WRAST routine as the "sink" routine for
*  storing the data, and specify that only essential information be
*  included.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CHAN = AST_CHANNEL( AST_NULL, NDF1_WRAST,
     :                          'Full=-1,Comment=0', STATUS )

*  Initialise the index of the first element in the _CHAR array to be
*  used by the sink function.
            DCB_ASTLN = 1

*  Write the copy of the supplied AST_ object to the Channel, thus
*  transferring the data to the HDS component.
            NWRITE = AST_WRITE( CHAN, DCB_IWCS( IDCB ), STATUS )

*  If an error occurred during data transfer, report a contextual error
*  message.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL DAT_MSG( 'OBJECT', DCB_ASTLC )
               CALL ERR_REP( 'NDF1_WWRT_WRT',
     :                       'Error while writing AST_ data to ' //
     :                       'the HDS object ^OBJECT.', STATUS )
            END IF

*  Annul the Channel pointer, thus deleting the Channel.
            CALL AST_ANNUL( CHAN, STATUS )
         END IF

*  Unmap the DATA component.
         CALL DAT_UNMAP( DCB_ASTLC, STATUS )

*  Obtain the number of elements used in the component and alter its
*  size to eliminate the unused elements.
         DIM( 1 ) = DCB_ASTLN - 1
         CALL DAT_ALTER( DCB_ASTLC, 1, DIM, STATUS )

*  Annul the DATA component locator.
         CALL DAT_ANNUL( DCB_ASTLC, STATUS )

*  If an error occurred, clean up by erasing any new HDS component that
*  was created.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_BEGIN( STATUS )
            CALL DAT_ERASE( DCB_LOC( IDCB ), 'WCS', STATUS )
            CALL ERR_END( STATUS )

*  Also annul the pointer to any WCS information held in the DCB.
            CALL AST_ANNUL( DCB_IWCS( IDCB ), STATUS )
         END IF

*  Note whether WCS informatiomn is up to date in the DCB.
         DCB_KW( IDCB ) = STATUS .EQ. SAI__OK
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_WWRT', STATUS )

      END
