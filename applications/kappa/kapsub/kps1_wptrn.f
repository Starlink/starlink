      SUBROUTINE KPS1_WPTRN( TR, LOC, NAME, COMM, STATUS )
*+
*  Name:
*     KPS1_WPTRN

*  Purpose:
*     Creates a transform structure for a two-dimensional polar
*     transform.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_WPTRN( TR, LOC, NAME, COMM, STATUS )

*  Description:
*     The routine creates the forward and inverse mappings for a
*     two-dimensional polar transform whose coefficients are given by
*     the TR array.  The mappings are stored using TRANSFORM in the
*     object pointed to by LOC.

*  Arguments:
*     TR( 3 ) = DOUBLE PRECISION (Given)
*        The coefficients of the polar transformation.  In order these
*        are the Cartesian origin (x then y), and the orientation origin
*        measured from the x-axis anticlockwise.
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to structure in which transformation is to be stored.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the transformation structure.  It must not bbe
*        longer than DAT__SZNAM characters.
*     COMM = CHARACTER * ( * ) (Given)
*        Comment to be added to transform structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 March 8 (MJC):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT constants
      INCLUDE 'TRN_PAR'          ! TRANSFORM parameterisations

*  Arguments Given:
      DOUBLE PRECISION TR( 6 )
      CHARACTER * ( * ) LOC
      CHARACTER * ( * ) NAME
      CHARACTER * ( * ) COMM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER EXPLEN             ! Maximum expression length
      PARAMETER ( EXPLEN = 512 )

*  Local Variables:
      CHARACTER * ( EXPLEN ) FOR( 2 ) ! Forward mappings
      CHARACTER * ( EXPLEN ) INV( 2 ) ! Inverse mappings
      CHARACTER * ( DAT__SZLOC ) LOCTR ! Locator to transform
      INTEGER NSUBS              ! Dummy
      LOGICAL VALID              ! Locator is valid

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the forward and inverse transformations.
      FOR( 1 ) = 'R=SQRT((X-TR1)*(X-TR1)+(Y-TR2)*(Y-TR2))'
      FOR( 2 ) = 'THETA=ATAN2D((Y-TR2),(X-TR1))+TR3'
      INV( 1 ) = 'X=TR1+R*COSD(THETA-TR3)'
      INV( 2 ) = 'Y=TR2+R*SIND(THETA-TR3)'

*  Replace the parameter tokens with values.
      CALL TRN_STOKD( 'TR1', TR( 1 ), FOR( 1 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR2', TR( 2 ), FOR( 1 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR1', TR( 1 ), FOR( 2 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR2', TR( 2 ), FOR( 2 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR3', TR( 3 ), FOR( 2 ), NSUBS, STATUS )

      CALL TRN_STOKD( 'TR1', TR( 1 ), INV( 1 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR3', TR( 3 ), INV( 1 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR2', TR( 2 ), INV( 2 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR3', TR( 3 ), INV( 2 ), NSUBS, STATUS )

*  Create the transformation.
      CALL TRN_NEW( 2, 2, FOR, INV, '_DOUBLE', COMM, LOC, NAME,
     :              LOCTR, STATUS )

*  Annul the locator to the transformation structure.
      CALL DAT_VALID( LOCTR, VALID, STATUS )
      IF ( VALID ) CALL DAT_ANNUL( LOCTR, STATUS )

      END
