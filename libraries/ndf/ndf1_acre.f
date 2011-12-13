      SUBROUTINE NDF1_ACRE( IDCB, STATUS )
*+
*  Name:
*     NDF1_ACRE

*  Purpose:
*     Ensure that an NDF axis component exists, creating one if
*     necessary.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ACRE( IDCB, STATUS )

*  Description:
*     The routine ensures that an axis component exists for an NDF data
*     object with an entry in the DCB. If the associated data object
*     does not currently have an axis component, then one is created
*     and filled with axis data values describing the default axis
*     coordinate system.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the data object entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Ensure that axis structure information is available in the
*     DCB.
*     -  See if the first axis structure element locator is valid. If
*     so, then an axis structure already exists, so there is nothing to
*     do.
*     -  Obtain the NDF bounds and number of dimensions from the ARY_
*     system identifier for the data array held in the DCB.
*     -  Loop to ensure that DCB information is available for each NDF
*     axis data array. These arrays do not exist yet, but this process
*     establishes their default attributes in the DCB (which are then
*     used by NDF1_ADCRE when creating the arrays).
*     -  Ensure that an axis normalisation flag value is available for
*     each NDF dimension.
*     -  Create a 1-dimensional axis structure in the NDF with the
*     necessary number of elements.
*     -  Obtain a locator to the axis structure.
*     -  Loop to create an data array for each axis.  Obtain a locator
*     to each axis structure element (cell) and store this in the DCB.
*     -  Use HDS_TUNE to set the optimum number of components in the
*     HDS structure and create and initialise an axis data array within
*     the structure element.
*     -  If the axis normalisation flag value is .TRUE., then create a
*     new NORMALISED component and set its value.
*     -  Annul the locator to the entire axis structure.
*     -  If an error occurred, then annul any ARY_ system identifiers
*     and locators which may have been acquired.
*     -  Erase any axis structure which may have been created, ignoring
*     any error which may occur.
*     -  Note if the axis structure and axis data array information in
*     the DCB is valid.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-JUN-1990 (RFWS):
*        Original version.
*     2-JUL-1990 (RFWS):
*        Fixed bug in call to NDF1_ADINI and in status initialisation.
*     2-AUG-1990 (RFWS):
*        Rationalised internal status checking.
*     9-OCT-1990 (RFWS):
*        Changed to call NDF1_ADINI (name change).
*     15-OCT-1990 (RFWS):
*        Changed to take a DCB index as first argument instead of an ACB
*        index.
*     19-OCT-1990 (RFWS):
*        Changed to call NDF1_ADCRE to perform the axis data array
*        creation and initialisation.
*     23-OCT-1990 (RFWS):
*        Updated prologue.
*     27-NOV-1990 (RFWS):
*        Added call to HDS_TUNE to set the optimum number of components
*        in an axis structure element.
*     29-NOV-1990 (RFWS):
*        Changed to ensure that default attributes are established in
*        the DCB for the axis data arrays before creating them. Also
*        changed to process only those NDF axes which exist, rather
*        than all possible axes.
*     29-NOV-1990 (RFWS):
*        Added creation of axis NORMALISED component.
*     2-JAN-1991 (RFWS):
*        Removed unnecessary use of NDF_ERR include file.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ADID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Write)
*           ARY_ system identifiers for axis data arrays.
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read and Write)
*           Locators to axis structure elements.
*        DCB_ANRM( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read)
*           Axis normalisation value.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        DCB_KA( NDF__MXDCB ) = LOGICAL (Write)
*           Whether axis component information is available.
*        DCB_KAD( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Write)
*           Whether information about axis data arrays is available.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) ALOC ! Axis structure locator
      INTEGER CELL( 1 )          ! Axis structure cell subscript
      INTEGER DIMA( 1 )          ! Axis structure dimension size
      INTEGER IAX                ! Loop counter for axes
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER TSTAT              ! Temporary status variable
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that axis structure information is available in the DCB.
      CALL NDF1_DA( IDCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  See if the first axis structure element locator is valid. If so,
*  then an axis structure already exists, so there is nothing to do.
         IF ( DCB_ALOC( 1, IDCB ) .EQ. DAT__NOLOC ) THEN

*  Obtain the NDF bounds and number of dimensions from the ARY_ system
*  identifier for the data array held in the DCB.
            CALL ARY_BOUND( DCB_DID( IDCB ), NDF__MXDIM, LBND, UBND,
     :                      NDIM, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop to ensure that DCB information is available for each NDF axis
*  data array. These arrays do not exist yet, but this process
*  establishes their default attributes in the DCB (which are then used
*  by NDF1_ADCRE when creating the arrays).
               DO 1 IAX = 1, NDIM
                  CALL NDF1_DAD( IAX, IDCB, STATUS )

*  Ensure that an axis normalisation flag value is available for each
*  NDF dimension.
                  CALL NDF1_DAN( IAX, IDCB, STATUS )
 1             CONTINUE

*  Create a 1-dimensional axis structure in the NDF with the necessary
*  number of elements.
               DIMA( 1 ) = NDIM
               CALL DAT_NEW( DCB_LOC( IDCB ), 'AXIS', 'AXIS', 1, DIMA,
     :                       STATUS )

*  Obtain a locator to the axis structure.
               CALL DAT_FIND( DCB_LOC( IDCB ), 'AXIS', ALOC, STATUS )

*  Loop to create an data array for each axis.  Obtain a locator to
*  each axis structure element (cell) and store this in the DCB.
               DO 2 IAX = 1, NDIM
                  CELL( 1 ) = IAX
                  CALL DAT_CELL( ALOC, 1, CELL, DCB_ALOC( IAX, IDCB ),
     :                           STATUS )

*  Use HDS_TUNE to set the optimum number of components in the HDS
*  structure and create and initialise an axis data array within the
*  structure element.
                  CALL HDS_TUNE( 'NCOMP', 8, STATUS )
                  CALL NDF1_ADCRE( LBND( IAX ), UBND( IAX ), IAX, IDCB,
     :                             STATUS )

*  If the axis normalisation flag value is .TRUE., then create a new
*  NORMALISED component and set its value.
                  IF ( DCB_ANRM( IAX, IDCB ) ) THEN
                     CALL DAT_NEW0L( DCB_ALOC( IAX, IDCB ),
     :                               'NORMALISED', STATUS )
                     CALL CMP_PUT0L( DCB_ALOC( IAX, IDCB ),
     :                               'NORMALISED', .TRUE., STATUS )
                  END IF
 2             CONTINUE

*  Annul the locator to the entire axis structure.
               CALL DAT_ANNUL( ALOC, STATUS )

*  If an error occurred, then annul any ARY_ system identifiers and
*  locators which may have been acquired.
               IF ( STATUS .NE. SAI__OK ) THEN
                  DO 3 IAX = 1, NDIM
                     CALL ARY_ANNUL( DCB_ADID( IAX, IDCB ), STATUS )
                     CALL DAT_ANNUL( DCB_ALOC( IAX, IDCB ), STATUS )
 3                CONTINUE

*  Erase any axis structure which may have been created, ignoring any
*  error which may occur.
                  CALL ERR_MARK
                  TSTAT = SAI__OK
                  CALL DAT_ERASE( DCB_LOC( IDCB ), 'AXIS', TSTAT )
                  CALL ERR_ANNUL( TSTAT )
                  CALL ERR_RLSE
               END IF

*  Note if the axis structure and axis data array information in the DCB
*  is valid.
               DCB_KA( IDCB ) = STATUS .EQ. SAI__OK
               DO 4 IAX = 1, NDIM
                  DCB_KAD( IAX, IDCB ) = STATUS .EQ. SAI__OK
 4             CONTINUE
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ACRE', STATUS )

      END
