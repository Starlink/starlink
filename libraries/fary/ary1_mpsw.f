      SUBROUTINE ARY1_MPSW( IACB, LOC, TYPE, INOPT, MLOC, COPY, PNTR,
     :                      STATUS )
*+
*  Name:
*     ARY1_MPSW

*  Purpose:
*     Map a simple array component for WRITE access.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_MPSW( IACB, LOC, TYPE, INOPT, MLOC, COPY, PNTR, STATUS )

*  Description:
*     The routine maps a component (non-imaginary or imaginary) of a
*     simple array for WRITE access and returns a locator and a pointer
*     to the mapped data. The mapped data region may also be
*     initialised to zero or to "bad" values according to the value
*     supplied for the INOPT argument.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to an entry in the ACB specifying the array whose data
*        are to be mapped.
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to the array data component (non-imaginary or
*        imaginary) to be accessed.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type for access; an HDS primitive numeric data type
*        string (case insensitive).
*     INOPT = CHARACTER * ( * ) (Given)
*        The initialisation option; one of ' ', 'ZERO' or 'BAD' (case
*        insensitive).
*     MLOC = CHARACTER * ( * ) (Returned)
*        Locator associated with the object which is mapped to provide
*        memory locations for the data.
*     COPY = LOGICAL (Returned)
*        Whether the mapped data region is a "copy" of the actual data
*        (as opposed to being a mapped slice of the data object itself).
*     PNTR = INTEGER (Returned)
*        Pointer to the mapped data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine may also be used to map scaled or primitive arrays.

*  Prior requirements:
*     -  Appropriate entries relating to the region of data to be
*     mapped must already have been set up in the MCB before this
*     routine is called.
*     -  Type, bounds and dimensionality information for the data object
*     being mapped must already be available in the DCB.

*  Algorithm:
*     -  Set an initial value for the MLOC argument before checking the
*     inherited status.
*     -  Obtain indices into the DCB and MCB and initialise.
*     -  Calculate the size of the mapping region in each dimension and
*     the total number of data elements to be mapped.
*     -  If appropriate, map the whole data component directly using
*     HDS.
*     -  Otherwise, if appropriate, locate a slice containing the
*     required subregion of the data component and map this directly
*     using HDS.
*     -  Otherwise, create and map a temporary object to contain a copy
*     of the data.
*     -  Initialise the mapped data region as required.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     All Rights Reserved.
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. All Rights Reserved.
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-JUN-1989 (RFWS):
*        Original version.
*     2-MAR-1990 (RFWS):
*        Added initialisation of the MLOC argument.
*     24-APR-2006 (DSB):
*        Add support for scaled arrays.
*     1-NOV-2010 (DSB):
*        Include support for delta compressed arrays.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_LBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read)
*           Lower bounds of data object.
*        DCB_NDIM( ARY__MXDCB ) = INTEGER (Read)
*           Number of data object dimensions.
*        DCB_TYP( ARY__MXDCB ) = CHARACTER * ( ARY__SZTYP ) (Read)
*           Array data type.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_IMCB( ARY__MXACB ) = INTEGER (Read)
*           Index to mapping entry in the MCB.
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Read)
*           Number of access dimensions.

      INCLUDE 'ARY_MCB'          ! ARY_ Mapping Control Block
*        MCB_LMRB( ARY__MXDIM, ARY__MXMCB ) = INTEGER (Read)
*           Lower bounds of mapping region.
*        MCB_MRFUL( ARY_MXMCB ) = LOGICAL (Read)
*           Whether the mapping transfer region completely fills the
*           mapping region.
*        MCB_UMRB( ARY__MXDIM, ARY__MXMCB ) = INTEGER (Read)
*           Upper bounds of mapping region.
*        MCB_WHOLE( ARY__MXMCB ) = LOGICAL (Read)
*           Whether the whole data component is to be accessed.

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) LOC
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) INOPT

*  Arguments Returned:
      CHARACTER * ( * ) MLOC
      LOGICAL COPY
      INTEGER PNTR

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local variables:
      INTEGER DIM( ARY__MXDIM )  ! Dimensions of mapping region
      INTEGER DIML( ARY__MXDIM ) ! Lower bounds of slice
      INTEGER DIMU( ARY__MXDIM ) ! Upper bounds of slice
      INTEGER EL                 ! Number of data elements to be mapped
      INTEGER IDCB               ! Index to entry in DCB
      INTEGER IMCB               ! Index to entry in MCB
      INTEGER NDIMA              ! Number of access dimensions
      INTEGER NDIMD              ! Number of data object dimensions
      INTEGER I                  ! Loop counter for dimensions

*.

*  Set an initial value for the MLOC argument.
      MLOC = ARY__NOLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain indices into the DCB and MCB.
      IDCB = ACB_IDCB( IACB )
      IMCB = ACB_IMCB( IACB )

*  Obtain the number of access dimensions and the number of actual data
*  object dimensions.
      NDIMA = ACB_NDIM( IACB )
      NDIMD = DCB_NDIM( IDCB )

*  Calculate the size of the mapping region in each dimension and the
*  total number of data elements to be mapped.
      EL = 1
      DO 1 I = 1, MAX( NDIMA, NDIMD )
         DIM( I ) = MCB_UMRB( I, IMCB ) - MCB_LMRB( I, IMCB ) + 1
         EL = EL * DIM( I )
1     CONTINUE

*  If the mapping region (and mapping transfer region) comprises the
*  whole data object and no data type conversion or scaling is required,
*  then clone a locator to the entire data component and map it for WRITE
*  access directly using HDS.
      IF ( MCB_WHOLE( IMCB ) .AND.
     :     CHR_SIMLR( TYPE, DCB_TYP( IDCB ) ) .AND.
     :     DCB_FRM( IDCB ) .NE. 'SCALED' .AND.
     :     DCB_FRM( IDCB ) .NE. 'DELTA' ) THEN
         CALL DAT_CLONE( LOC, MLOC, STATUS )
         CALL DAT_MAP( MLOC, TYPE, 'WRITE', NDIMD, DIM, PNTR, STATUS )
         COPY = .FALSE.

*  Otherwise, if the mapping transfer region fills the mapping region
*  and the data component dimensionality does not exceed the maximum
*  dimensionality of an HDS slice and no data type conversion or scaling is
*  required, then a slice of the data can be mapped directly using HDS.
      ELSE IF ( MCB_MRFUL( IMCB ) .AND.
     :          ( NDIMD .LE. ARY__MXHSL ) .AND.
     :          CHR_SIMLR( TYPE, DCB_TYP( IDCB ) ) .AND.
     :          DCB_FRM( IDCB ) .NE. 'SCALED' .AND.
     :          DCB_FRM( IDCB ) .NE. 'DELTA' ) THEN

*  Calculate the bounds of the data component slice required.
         DO 2 I = 1, NDIMD
            DIML( I ) = MCB_LMRB( I, IMCB ) - DCB_LBND( I, IDCB ) + 1
            DIMU( I ) = MCB_UMRB( I, IMCB ) - DCB_LBND( I, IDCB ) + 1
2        CONTINUE

*  Locate the slice and map it for WRITE access.
         CALL DAT_SLICE( LOC, NDIMD, DIML, DIMU, MLOC, STATUS )
         CALL DAT_MAP( MLOC, TYPE, 'WRITE', NDIMD, DIM, PNTR, STATUS )
         COPY = .FALSE.

*  In all other cases, a separate copy of the mapped data must be
*  maintained, so create and map a temporary object to contain it.
      ELSE
         CALL ARY1_CMTMP( TYPE, NDIMA, DIM, MLOC, PNTR, STATUS )
         COPY = .TRUE.
      END IF

*  Initialise the mapped data, as required.
      CALL ARY1_IOBW( TYPE, INOPT, EL, PNTR, STATUS )

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_MPSW', STATUS )

      END
