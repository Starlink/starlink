      SUBROUTINE NDF1_ADMAP( IAX, IACB, TYPE, MODE, PNTR, EL, STATUS )
*+
*  Name:
*     NDF1_ADMAP

*  Purpose:
*     Map an NDF's axis data array for access.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADMAP( IAX, IACB, TYPE, MODE, PNTR, EL, STATUS )

*  Description:
*     The routine maps a specified NDF axis data array (containing the
*     coordinates of the pixel centres in the nominated dimension) for
*     access. The NDF is identified by its ACB entry. Account is taken
*     of the possibility that the required axis data array may not
*     exist (either because it has not yet been created or because the
*     NDF's ACB entry contains dimensions which do not exist in the
*     actual data object described in the DCB) and suitable default
*     values are provided. Extrapolated values may also be returned if
*     access to an NDF section extending outside the bounds of the
*     actual data object is required. A new axis structure (including
*     an axis data array) may be created by this routine if necessary.

*  Arguments:
*     IAX = INTEGER (Given)
*        Number of the axis whose data array is to be mapped.
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric type to be used to access the mapped values (case
*        insensitive).
*     MODE = CHARACTER * ( * ) (Given)
*        Mapping mode to be used to access the values: 'READ', 'UPDATE'
*        or 'WRITE' (case insensitive).
*     PNTR = INTEGER (Returned)
*        Pointer to the mapped array of values. A value of zero will be
*        returned if the routine is called with STATUS set, or if it
*        should fail for any reason.
*     EL = INTEGER (Returned)
*        Number of axis array elements mapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The array mapped by this routine will be initialised to
*     contain valid axis centre positions, even if WRITE access is
*     specified. This is necessary because mapping of the associated
*     axis width array may itself require use of the mapped axis centre
*     positions for initialisation.

*  Algorithm:
*     -  Initialise the returned pointer value before checking the
*     inherited status.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Check that the required axis data array is not already mapped.
*     Report an error if it is.
*     -  Obtain the pixel index bounds of the NDF's data array and of
*     the data array of the actual data object (whose ARY_ system
*     identifier is stored in the DCB).
*     -  Obtain the pixel offsets between the actual data object and
*     the NDF which the ACB entry describes.
*     -  Ensure that axis data array information is available in the
*     DCB.
*     -  Calculate the data object pixel index bounds for the required
*     axis array (by transforming from the NDF pixel index system to
*     the data object pixel index system).
*     -  See if the required axis data array exists and whether the NDF
*     is a section.
*     -  Set logical read, update or write flags.
*     -  Determine the numeric type of the axis data array.
*     -  If the axis data array exists and the NDF is not a section,
*     then clone an ARY_ system identifier for it.
*     -  Otherwise, if it exists and the NDF is a section, then obtain
*     an ARY_ system identifier for the appropriate section of the axis
*     data array.
*     -  If modifications may be made to the section's mapped values,
*     then make a temporary copy of the section so that the original
*     data object will not be affected.
*     -  Otherwise, if the axis data array does not exist and the NDF
*     is a base NDF and values are to be written to the array and the
*     axis can be created (i.e. lies within the dimensionality of the
*     actual data object), then create an NDF axis structure containing
*     a new axis data array. Clone an ARY_ system identifier for the
*     array.
*     -  In all other cases, the axis data array will not exist and
*     cannot be created, so a temporary array must be created for the
*     purposes of mapping. This will later be discarded when the axis
*     data array is unmapped.
*     -  Map the array, initialise it, and then unmap it.
*     -  It is possible that a section of an existing axis data array
*     may extend beyond the bounds of the associated data object. Test
*     if it extends beyond the lower bound and derive extrapolation
*     parameters if it does.
*     -  Similarly derive extrapolation parameters if the section
*     extends beyond the array's upper bound.
*     -  Map the array (or derived temporary array) for access. Use
*     UPDATE access if WRITE was specified, to ensure that the array's
*     values are initialised. Otherwise use the access mode as
*     specified.
*     -  If necessary, extrapolate the mapped values to lower pixel
*     indices.  Similarly extrapolate to higher pixel indices if
*     necessary.
*     -  If an error occurred, then annul any ARY_ system identifier
*     which may have been acquired.
*     -  Return a null pointer under error conditions.
*     -  Otherwise, store the ARY_ system identifier and pointer for
*     the mapped array, also store the numeric type used for mapping
*     (in upper case) and mark the axis data array as mapped.
*     -  Increment the appropriate DCB mapping counts.

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-JUL-1990 (RFWS):
*        Original version.
*     2-AUG-1990 (RFWS):
*        Fixed passing of wrong array bounds to the extrapolation
*        routines.
*     10-OCT-1990 (RFWS):
*        Fixed bad index used to access ACB_ADMAP array.
*     10-OCT-1990 (RFWS):
*        Fixed wrong array identifier passed to NDF1_GAEXT and wrong
*        PIX0 argument to NDF1_ADEXT.
*     5-NOV-1990 (RFWS):
*        Changed to ensure that the mapped array contains valid axis
*        centre coordinates, even if WRITE access is specified.
*     5-NOV-1990 (RFWS):
*        Added initialisation of the returned pointer value.
*     9-NOV-1990 (RFWS):
*        Changed to call NDF1_GADEX instead of NDF1_GAEXT.
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
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ADID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifiers for axis data arrays.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        DCB_NADMP( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings to each axis data array.
*        DCB_NMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Total number of current mappings to each data object.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ADMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether NDF axis data arrays are currently mapped for
*           access.
*        ACB_ADMID( NDF__MXDIM, NDF__MXACB ) = INTEGER (Write)
*           ARY_ system identifier for axis data array mappings.
*        ACB_ADMPT( NDF__MXDIM, NDF__MXACB ) = INTEGER (Write)
*           Pointers to mapped axis data arrays.
*        ACB_ADMTP( NDF__MXDIM, NDF__MXACB ) = CHARACTER * ( NDF__SZTYP
*        ) (Write)
*           Numeric type used to map axis data arrays.
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IAX
      INTEGER IACB
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      INTEGER PNTR
      INTEGER EL

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) ADTYPE ! Axis data array numeric type
      DOUBLE PRECISION LSCALE    ! Lower extrapolation scale factor
      DOUBLE PRECISION LZERO     ! Lower extrapolation zero point
      DOUBLE PRECISION USCALE    ! Upper extrapolation scale factor
      DOUBLE PRECISION UZERO     ! Upper extrapolation zero point
      INTEGER ID                 ! ARY_ identifier for mapped axis array
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER LBND( NDF__MXDIM ) ! NDF upper pixel index bounds
      INTEGER LBNDD( NDF__MXDIM ) ! Data object lower bounds
      INTEGER LBNDS( 1 )         ! Lower section bounds
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER NDIMD              ! Number of data object dimensions
      INTEGER OFFS( NDF__MXDIM ) ! Pixel offset of NDF entry in the ACB
      INTEGER OLDID              ! Old ARY_ system identifier
      INTEGER PLACE              ! ARY_ system placeholder
      INTEGER UBND( NDF__MXDIM ) ! NDF lower pixel index bounds
      INTEGER UBNDD( NDF__MXDIM ) ! Data object upper bounds
      INTEGER UBNDS( 1 )         ! Upper section bounds
      LOGICAL EXIST              ! Does axis data array exist?
      LOGICAL LOWER              ! Extrapolate to lower pixel indices?
      LOGICAL MODER              ! Read access?
      LOGICAL MODEU              ! Update access?
      LOGICAL MODEW              ! Write access?
      LOGICAL SECT               ! Is the NDF a section?
      LOGICAL UPPER              ! Extrapolate to upper pixel indices?

*.

*  Initialise the returned pointer value.
      PNTR = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an index to the data object entry in the DCB.
      IDCB = ACB_IDCB( IACB )

*  Check that the required axis data array is not already mapped. Report
*  an error if it is.
      IF ( ACB_ADMAP( IAX, IACB ) ) THEN
         STATUS = NDF__ISMAP
         CALL MSG_SETI( 'AXIS', IAX )
         CALL NDF1_AMSG( 'NDF', IACB )
         CALL ERR_REP( 'NDF1_ADMAP_MAP',
     :   'The centre array for axis ^AXIS of the NDF structure ^NDF ' //
     :   'is already mapped for access through the specified ' //
     :   'identifier (possible programming error).', STATUS )

*  Obtain the pixel index bounds of the NDF's data array and of the
*  data array of the actual data object (whose ARY_ system identifier
*  is stored in the DCB).
      ELSE
         CALL ARY_BOUND( ACB_DID( IACB ), NDF__MXDIM, LBND, UBND, NDIM,
     :                   STATUS )
         CALL ARY_BOUND( DCB_DID( IDCB ), NDF__MXDIM, LBNDD, UBNDD,
     :                   NDIMD, STATUS )

*  Obtain the pixel offsets between the actual data object and the NDF
*  which the ACB entry describes.
         CALL ARY_OFFS( ACB_DID( IACB ), DCB_DID( IDCB ), NDF__MXDIM,
     :                  OFFS, STATUS )

*  Ensure that axis data array information is available in the DCB.
         CALL NDF1_DAD( IAX, IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Calculate the data object pixel index bounds for the required axis
*  array (by transforming from the NDF pixel index system to the data
*  object pixel index system).
            LBNDS( 1 ) = LBND( IAX ) + OFFS( IAX )
            UBNDS( 1 ) = UBND( IAX ) + OFFS( IAX )

*  See if the required axis data array exists and whether the NDF is a
*  section.
            EXIST = DCB_ADID( IAX, IDCB ) .NE. ARY__NOID
            SECT = ACB_CUT( IACB )

*  Set logical read, update or write flags.
            MODER = .FALSE.
            MODEU = .FALSE.
            MODEW = .FALSE.
            IF ( CHR_SIMLR( MODE, 'READ' ) ) THEN
               MODER = .TRUE.
            ELSE IF ( CHR_SIMLR( MODE, 'UPDATE' ) ) THEN
               MODEU = .TRUE.
            ELSE IF ( CHR_SIMLR( MODE, 'WRITE' ) ) THEN
               MODEW = .TRUE.
            END IF

*  Determine the numeric type of the axis data array.
            CALL NDF1_ADTYP( IAX, IACB, ADTYPE, STATUS )

*  If the axis data array exists and the NDF is not a section, then
*  clone an ARY_ system identifier for it.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( EXIST .AND. ( .NOT. SECT ) ) THEN
                  CALL ARY_CLONE( DCB_ADID( IAX, IDCB ), ID, STATUS )

*  Otherwise, if it exists and the NDF is a section, then obtain an
*  ARY_ system identifier for the appropriate section of the axis data
*  array.
               ELSE IF ( EXIST .AND. SECT ) THEN
                  CALL ARY_SECT( DCB_ADID( IAX, IDCB ), 1, LBNDS, UBNDS,
     :                           ID, STATUS )

*  If modifications may be made to the section's mapped values, then
*  make a temporary copy of the section so that the original data
*  object will not be affected.
                  IF ( MODEU .OR. MODEW ) THEN
                     CALL ARY_TEMP( PLACE, STATUS )
                     OLDID = ID
                     CALL ARY_COPY( OLDID, PLACE, ID, STATUS )
                     CALL ARY_ANNUL( OLDID, STATUS )
                  END IF

*  Otherwise, if the axis data array does not exist and the NDF is a
*  base NDF and values are to be written to the array and the axis can
*  be created (i.e. lies within the dimensionality of the actual data
*  object), then create an NDF axis structure containing a new axis
*  data array. Clone an ARY_ system identifier for the array.
               ELSE IF ( ( .NOT. EXIST ) .AND.
     :                   ( .NOT. SECT ) .AND.
     :                   ( MODEW .OR. MODEU ) .AND.
     :                   ( IAX .LE. NDIMD ) ) THEN
                  CALL NDF1_ACRE( IDCB, STATUS )
                  CALL ARY_CLONE( DCB_ADID( IAX, IDCB ), ID, STATUS )

*  In all other cases, the axis data array will not exist and cannot be
*  created, so a temporary array must be created for the purposes of
*  mapping. This will later be discarded when the axis data array is
*  unmapped.
               ELSE
                  CALL ARY_TEMP( PLACE, STATUS )
                  CALL ARY_NEW( ADTYPE, 1, LBNDS, UBNDS, PLACE, ID,
     :                          STATUS )

*  Map the array, initialise it, and then unmap it.
                  CALL ARY_MAP( ID, ADTYPE, 'WRITE', PNTR, EL, STATUS )
                  CALL NDF1_ADINI( ADTYPE, LBNDS( 1 ), UBNDS( 1 ), PNTR,
     :                             STATUS )
                  CALL ARY_UNMAP( ID, STATUS )
               END IF

*  It is possible that a section of an existing axis data array may
*  extend beyond the bounds of the associated data object. Test if it
*  extends beyond the lower bound and derive extrapolation parameters
*  if it does.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  LOWER = .FALSE.
                  UPPER = .FALSE.
                  IF ( EXIST .AND. SECT ) THEN
                     IF ( LBNDS( 1 ) .LT. LBNDD( IAX ) ) THEN
                        LOWER = .TRUE.
                        CALL NDF1_GADEX( LBNDD( IAX ), UBNDD( IAX ),
     :                                   DCB_ADID( IAX, IDCB ), .FALSE.,
     :                                   LSCALE, LZERO, STATUS )
                     END IF

*  Similarly derive extrapolation parameters if the section extends
*  beyond the array's upper bound.
                     IF ( UBNDS( 1 ) .GT. UBNDD( IAX ) ) THEN
                        UPPER = .TRUE.
                        CALL NDF1_GADEX( LBNDD( IAX ), UBNDD( IAX ),
     :                                   DCB_ADID( IAX, IDCB ), .TRUE.,
     :                                   USCALE, UZERO, STATUS )
                     END IF
                  END IF
               END IF

*  Map the array (or derived temporary array) for access. Use UPDATE
*  access if WRITE was specified, to ensure that the array's values are
*  initialised. Otherwise use the access mode as specified.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( MODEW ) THEN
                     CALL ARY_MAP( ID, TYPE, 'UPDATE', PNTR, EL,
     :                             STATUS )
                  ELSE
                     CALL ARY_MAP( ID, TYPE, MODE, PNTR, EL, STATUS )
                  END IF

*  If necessary, extrapolate the mapped values to lower pixel indices.
                  IF ( LOWER ) THEN
                     CALL NDF1_ADEXT( TYPE, LSCALE, LZERO, .FALSE.,
     :                                MIN( LBNDD( IAX ) - 1,
     :                                     UBNDS( 1 ) ),
     :                                LBNDS( 1 ), UBNDS( 1 ), PNTR,
     :                                STATUS )
                  END IF

*  Similarly extrapolate to higher pixel indices if necessary.
                  IF ( UPPER ) THEN
                     CALL NDF1_ADEXT( TYPE, USCALE, UZERO, .TRUE.,
     :                                MAX( UBNDD( IAX ) + 1,
     :                                     LBNDS( 1 ) ),
     :                                LBNDS( 1 ), UBNDS( 1 ), PNTR,
     :                                STATUS )
                  END IF
               END IF

*  If an error occurred, then annul any ARY_ system identifier which may
*  have been acquired.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ARY_ANNUL( ID, STATUS )
               END IF
            END IF
         END IF
      END IF

*  Return a null pointer under error conditions.
      IF ( STATUS .NE. SAI__OK ) THEN
         PNTR = 0

*  Otherwise, store the ARY_ system identifier and pointer for the
*  mapped array, also store the numeric type used for mapping (in upper
*  case) and mark the axis data array as mapped.
      ELSE
         ACB_ADMID( IAX, IACB ) = ID
         ACB_ADMPT( IAX, IACB ) = PNTR
         ACB_ADMTP( IAX, IACB ) = TYPE
         CALL CHR_UCASE( ACB_ADMTP( IAX, IACB ) )
         ACB_ADMAP( IAX, IACB ) = .TRUE.

*  Increment the appropriate DCB mapping counts.
         DCB_NADMP( IAX, IDCB ) = DCB_NADMP( IAX, IDCB ) + 1
         DCB_NMAP( IDCB ) = DCB_NMAP( IDCB ) + 1
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADMAP', STATUS )

      END
