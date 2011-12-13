      SUBROUTINE NDF1_ASBND( NDIM, LBND, UBND, IACB, STATUS )
*+
*  Name:
*     NDF1_ASBND

*  Purpose:
*     Set new pixel-index bounds for an NDF's axis component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ASBND( NDIM, LBND, UBND, IACB, STATUS )

*  Description:
*     The routine sets new pixel index bounds for an NDF's axis
*     component, including possible changes in the number of NDF
*     dimensions. Existing values held in axis arrays are retained or
*     extrapolated, as appropriate.

*  Arguments:
*     NDIM = INTEGER (Given)
*        New number of NDF dimensions.
*     LBND( NDIM ) = INTEGER (Given)
*        New lower pixel-index bounds.
*     UBND( NDIM ) = INTEGER (Given)
*        New upper pixel-index bounds.
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine can only be used to change the bounds of a base
*     NDF. It will return without action if an NDF section is supplied.
*     -  The NDF's axis structure need not exist.
*     -  This routine should be invoked prior to changing the bounds of
*     the NDF's main data array, upon whose shape the process of
*     extrapolating axis arrays depends.

*  Algorithm:
*     -  Check whether the NDF is a section. There is nothing to do if
*     it is.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Determine the initial number of dimensions of the NDF data
*     object from the ARY_ system identifier for its main data array,
*     held in the DCB.
*     -  Loop through all the NDF dimensions which are being retained.
*     -  Set new pixel-index bounds for the axis width, variance and
*     data arrays.
*     -  Loop through existing NDF dimensions which are not being
*     retained (if any).
*     -  Reset the axis character components and the data, variance and
*     width arrays.
*     -  If axis extension information is available and the extension
*     locator is valid, then annul it.
*     -  Ensure that axis normalisation information is available (to be
*     retained in the DCB in case the axis is re-created).
*     -  If an axis structure exists, then empty the appropriate
*     element of all remaining components so that the structure may be
*     contracted to match the new number of NDF dimensions. Annul the
*     DCB locator for the structure element.
*     -  Loop to process new NDF dimensions not initially present (if
*     any).
*     -  Ensure that DCB information is available for all the new axis
*     arrays.  These arrays do not yet exist, but this establishes
*     their default attributes.
*     -  Convert the default axis array storage forms to take account
*     of the new NDF bounds, if necessary.
*     -  Ensure that an axis normalisation flag value is available for
*     each new dimension.
*     -  If the new number of dimensions differs from the old number
*     and an axis structure exits, then obtain a locator to the axis
*     structure array and alter its size appropriately.
*     -  Loop to initialise any new NDF dimensions.
*     -  Obtain a DCB locator for the axis structure element of each
*     new dimension.
*     -  Use HDS_TUNE to set the optimum number of components in the
*     HDS structure and create and initialise a new axis data array.
*     -  If the axis normalisation flag value is .TRUE., then create a
*     new NORMALISED component and set its value.
*     -  Annul the locator to the axis structure.

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     19-OCT-1990 (RFWS):
*        Original version.
*     2-NOV-1990 (RFWS):
*        Added conversion of default storage form, to take account of
*        new axis bounds.
*     6-NOV-1990 (RFWS):
*        Changed to call ARY_NDIM instead of NDF_BOUND (a bug) and added
*        an additional status check.
*     27-NOV-1990 (RFWS):
*        Added call to HDS_TUNE to optimise the number of components in
*        an axis structure cell.
*     29-NOV-1990 (RFWS):
*        Substantial re-write to ensure that the default attributes of
*        all axis arrays (the storage form is the important attribute)
*        are established in the DCB so that their values are not lost
*        if they should diverge from the NDF default attributes due to
*        the change in bounds.
*     29-NOV-1990 (RFWS):
*        Added creation of new axis NORMALISED components.
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

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ADFRM( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM
*        ) (Read and Write)
*           Storage form of axis data arrays.
*        DCB_ANRM( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read)
*           Axis normalisation value.
*        DCB_AVFRM( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM
*        ) (Read and Write)
*           Storage form of axis variance arrays.
*        DCB_AWFRM( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM
*        ) (Read and Write)
*           Storage form of axis width arrays.
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read and Write)
*           Locators to axis structure elements.
*        DCB_AXLOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC
*        ) (Read and Write)
*           Locators to axis extension components.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        DCB_KAX( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read)
*           Whether axis extension information is available.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) ALOC ! Axis structure locator
      INTEGER CELL( 1 )          ! Axis structure cell index
      INTEGER IAX                ! Loop counter for axes/dimensions
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER NDIMI              ! Initial number of dimensions
      INTEGER NEWSIZ( 1 )        ! New axis structure size

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check whether the NDF is a section. There is nothing to do if it is.
      IF ( .NOT. ACB_CUT( IACB ) ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Determine the initial number of dimensions of the NDF data object
*  from the ARY_ system identifier for its main data array, held in the
*  DCB.
         CALL ARY_NDIM( DCB_DID( IDCB ), NDIMI, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop through all the NDF dimensions which are being retained.
            DO 1 IAX = 1, MIN( NDIMI, NDIM )

*  Set new pixel-index bounds for the axis width, variance and data
*  arrays.
               CALL NDF1_AWSBN( LBND( IAX ), UBND( IAX ), IAX, IACB,
     :                          STATUS )
               CALL NDF1_AVSBN( LBND( IAX ), UBND( IAX ), IAX, IACB,
     :                          STATUS )
               CALL NDF1_ADSBN( LBND( IAX ), UBND( IAX ), IAX, IACB,
     :                          STATUS )
 1          CONTINUE

*  Loop through existing NDF dimensions which are not being retained
*  (if any).
            DO 2 IAX = NDIM + 1, NDIMI

*  Reset the axis character components and the data, variance and width
*  arrays.
               CALL NDF1_ACRST( IAX, NDF__ALAB, IACB, STATUS )
               CALL NDF1_ACRST( IAX, NDF__AUNI, IACB, STATUS )
               CALL NDF1_ADRST( IAX, IACB, STATUS )
               CALL NDF1_AVRST( IAX, IACB, STATUS )
               CALL NDF1_AWRST( IAX, IACB, STATUS )

*  If axis extension information is available and the extension locator
*  is valid, then annul it.
               IF ( DCB_KAX( IAX, IDCB ) ) THEN
                  IF ( DCB_AXLOC( IAX, IDCB ) .NE. DAT__NOLOC ) THEN
                     CALL DAT_ANNUL( DCB_AXLOC( IAX, IDCB ), STATUS )
                  END IF
               END IF

*  Ensure that axis normalisation information is available (to be
*  retained in the DCB in case the axis is re-created).
               CALL NDF1_DAN( IAX, IDCB, STATUS )

*  If an axis structure exists, then empty the appropriate element of
*  all remaining components so that the structure may be contracted to
*  match the new number of NDF dimensions. Annul the DCB locator for
*  the structure element.
               IF ( DCB_ALOC( IAX, IDCB ) .NE. DAT__NOLOC ) THEN
                  CALL NDF1_HRST( DCB_ALOC( IAX, IDCB ), STATUS )
                  CALL DAT_ANNUL( DCB_ALOC( IAX, IDCB ), STATUS )
               END IF
 2          CONTINUE

*  Loop to process new NDF dimensions not initially present (if any).
            IF ( STATUS .EQ. SAI__OK ) THEN
               DO 3 IAX = NDIMI + 1, NDIM

*  Ensure that DCB information is available for all the new axis
*  arrays.  These arrays do not yet exist, but this establishes their
*  default attributes.
                  CALL NDF1_DAD( IAX, IDCB, STATUS )
                  CALL NDF1_DAV( IAX, IDCB, STATUS )
                  CALL NDF1_DAW( IAX, IDCB, STATUS )

*  Convert the default axis array storage forms to take account of the
*  new NDF bounds, if necessary.
                  CALL NDF1_CBFRM( 1, LBND( IAX ), UBND( IAX ),
     :                             DCB_ADFRM( IAX, IDCB ), STATUS )
                  CALL NDF1_CBFRM( 1, LBND( IAX ), UBND( IAX ),
     :                             DCB_ADFRM( IAX, IDCB ), STATUS )
                  CALL NDF1_CBFRM( 1, LBND( IAX ), UBND( IAX ),
     :                             DCB_AWFRM( IAX, IDCB ), STATUS )

*  Ensure that an axis normalisation flag value is available for each
*  new dimension.
                  CALL NDF1_DAN( IAX, IDCB, STATUS )
 3             CONTINUE

*  If the new number of dimensions differs from the old number and an
*  axis structure exits, then obtain a locator to the axis structure
*  array and alter its size appropriately.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( ( NDIM .NE. NDIMI ) .AND.
     :                 ( DCB_ALOC( 1, IDCB ) .NE. DAT__NOLOC ) ) THEN
                     CALL DAT_FIND( DCB_LOC( IDCB ), 'AXIS', ALOC,
     :                              STATUS )
                     NEWSIZ( 1 ) = NDIM
                     CALL DAT_ALTER( ALOC, 1, NEWSIZ, STATUS )

*  Loop to initialise any new NDF dimensions.
                     DO 4 IAX = NDIMI + 1, NDIM

*  Obtain a DCB locator for the axis structure element of each new
*  dimension.
                        CELL( 1 ) = IAX
                        CALL DAT_CELL( ALOC, 1, CELL,
     :                                 DCB_ALOC( IAX, IDCB ), STATUS )

*  Use HDS_TUNE to set the optimum number of components in the HDS
*  structure and create and initialise a new axis data array.
                        CALL HDS_TUNE( 'NCOMP', 8, STATUS )
                        CALL NDF1_ADCRE( LBND( IAX ), UBND( IAX ), IAX,
     :                                   IDCB, STATUS )

*  If the axis normalisation flag value is .TRUE., then create a new
*  NORMALISED component and set its value.
                        IF ( DCB_ANRM( IAX, IDCB ) ) THEN
                           CALL DAT_NEW0L( DCB_ALOC( IAX, IDCB ),
     :                                     'NORMALISED', STATUS )
                           CALL CMP_PUT0L( DCB_ALOC( IAX, IDCB ),
     :                                     'NORMALISED', .TRUE.,
     :                                     STATUS )
                        END IF
 4                   CONTINUE

*  Annul the locator to the axis structure.
                     CALL DAT_ANNUL( ALOC, STATUS )
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ASBND', STATUS )

      END
