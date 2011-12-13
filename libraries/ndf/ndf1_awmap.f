      SUBROUTINE NDF1_AWMAP( IAX, IACB, TYPE, MODE, PNTR, EL, STATUS )
*+
*  Name:
*     NDF1_AWMAP

*  Purpose:
*     Map an NDF's axis width array for access.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWMAP( IAX, IACB, TYPE, MODE, PNTR, EL, STATUS )

*  Description:
*     The routine maps a specified NDF axis width array for access. The
*     NDF is identified by its ACB entry. Account is taken of the
*     possibility that the required axis width array may not exist
*     (either because it has not yet been created or because the NDF's
*     ACB entry contains dimensions which do not exist in the actual
*     data object described in the DCB) and suitable default values are
*     provided. Extrapolated values may also be returned if access to
*     an NDF section extending outside the bounds of the actual data
*     object is required. A new axis structure (including an axis width
*     array) may be created by this routine if necessary.

*  Arguments:
*     IAX = INTEGER (Given)
*        Number of the axis whose width array is to be mapped.
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

*  Algorithm:
*     -  Initiallise the returned pointer value.
*     -  Check inherited global status.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Check that the required axis width array is not already
*     mapped.
*     -  Report an error if it is.
*     -  Obtain the pixel index bounds of the NDF's data array and of
*     the data array of the actual data object (whose ARY_ system
*     identifier is stored in the DCB).
*     -  Obtain the pixel offsets between the actual data object and
*     the NDF which the ACB entry describes.
*     -  Ensure that axis width array information is available in the
*     DCB.
*     -  Calculate the data object pixel index bounds for the required
*     axis array (by transforming from the NDF pixel index system to
*     the data object pixel index system).
*     -  See if the required axis width array exists and whether the
*     NDF is a section.
*     -  Set logical read, update or write flags.
*     -  Determine the numeric type of the axis width array.
*     -  If the axis width array exists and is not a section, then
*     clone an ARY_ system identifier for it.
*     -  Otherwise, if it exists and is a section and values must be
*     obtained from it, then obtain an ARY_ system identifier for the
*     appropriate section of the array.
*     -  If modifications may be made to the section's mapped values,
*     then make a temporary copy of the section so that the original
*     data object will not be affected.
*     -  Otherwise, if the axis width array does not exist and the NDF
*     is a base NDF and values are to be written to the array and the
*     axis can be created (i.e. lies within the dimensionality of the
*     actual data object), then create a new axis width array. Clone an
*     ARY_ system identifier for it.
*     -  In all other cases, values will not be written back to the
*     axis width array, so a temporary array must be created for the
*     purposes of mapping. This will later be discarded when the axis
*     width array is unmapped. Create the array and note that a new
*     temporary array exists.
*     -  It is possible that a section of an existing axis width array
*     may extend beyond the bounds of the associated data object. Test
*     if it extends beyond the lower bound and derive a width value to
*     be used for extrapolation if it does.
*     -  Similarly derive a width value to use for extrapolation if it
*     extends beyond the array's upper bound.
*     -  Map the axis width array (or derived temporary array) for
*     access. Use the access mode supplied unless a new (i.e.
*     un-initialised) array is being accessed, in which case 'WRITE' is
*     needed.
*     -  If necessary, extrapolate the mapped values to lower pixel
*     indices.
*     -  Similarly, extrapolate to higher pixel indices if necessary.
*     -  If initialisation of the mapped values is required, then new
*     width values must derived from the values in the associated axis
*     data array. Test if these are currently mapped and note this
*     fact.  If so, then create and map a temporary array to hold a
*     double precision copy. Transfer the mapped axis data array values
*     to the new array, converting them to double precision.
*     -  If the axis data array is not mapped, then map it as an array
*     of double precision values.
*     -  If an attempt to access the axis data array fails, then report
*     context information.
*     -  Initialise the mapped width values.
*     -  Annul the temporary copy of the axis data array or unmap the
*     axis data array itself, as appropriate.
*     -  If an error occurred, then annul any ARY_ system identifier
*     which may have been acquired.
*     -  If no error has occurred, then store the ARY_ system
*     identifier for the mapped array and mark the array as mapped.
*     -  Increment the appropriate DCB mapping counts.
*     -  Return a null pointer under error conditions.

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
*     16-OCT-1990 (RFWS):
*        Original version, derived from the NDF1_ADMAP routine.
*     9-NOV-1990 (RFWS):
*        Re-structured to handle initialisation and extrapolation of the
*        mapped values separately.
*     1-MAR-1991 (RFWS):
*        Fixed incorrect use of IDCB where IACB was required.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_AWID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifiers for axis width arrays.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        DCB_NAWMP( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings to each axis width array.
*        DCB_NMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Total number of current mappings to each data object.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ADMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read)
*           Whether NDF axis data arrays are currently mapped for
*           access.
*        ACB_ADMPT( NDF__MXDIM, NDF__MXACB ) = INTEGER (Read)
*           Pointers to mapped axis data arrays.
*        ACB_ADMTP( NDF__MXDIM, NDF__MXACB ) = CHARACTER * ( NDF__SZTYP
*        ) (Read)
*           Numeric type used to map axis data arrays.
*        ACB_AWMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether NDF axis width arrays are currently mapped for
*           access.
*        ACB_AWMID( NDF__MXDIM, NDF__MXACB ) = INTEGER (Write)
*           ARY_ system identifier for axis width array mappings.
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
      CHARACTER * ( NDF__SZTYP ) AWTYPE ! Axis width numeric type
      DOUBLE PRECISION LWIDTH ! Lower extrapolation width value
      DOUBLE PRECISION UWIDTH ! Upper extrapolation width value
      INTEGER DPNTR              ! Temporary data array pointer
      INTEGER ID                 ! ARY_ identifier for mapped axis array
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IDD                ! Temporary data array identifier
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
      LOGICAL DATMAP             ! Axis data array mapped?
      LOGICAL DCE                ! Data conversion error?
      LOGICAL EXIST              ! Does axis width array exist?
      LOGICAL LOWER              ! Extrapolate to lower pixel indices?
      LOGICAL MODER              ! Read access?
      LOGICAL MODEU              ! Update access?
      LOGICAL MODEW              ! Write access?
      LOGICAL NEW                ! New (or temporary) array created?
      LOGICAL SECT               ! Is the NDF a section?
      LOGICAL UPPER              ! Extrapolate to upper pixel indices?

*.

*  Initiallise the returned pointer value.
      PNTR = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an index to the data object entry in the DCB.
      IDCB = ACB_IDCB( IACB )

*  Check that the required axis width array is not already mapped.
*  Report an error if it is.
      IF ( ACB_AWMAP( IAX, IACB ) ) THEN
         STATUS = NDF__ISMAP
         CALL MSG_SETI( 'AXIS', IAX )
         CALL NDF1_AMSG( 'NDF', IACB )
         CALL ERR_REP( 'NDF1_AWMAP_MAP',
     :   'The width array for axis ^AXIS of the NDF structure ^NDF ' //
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

*  Ensure that axis width array information is available in the DCB.
         CALL NDF1_DAW( IAX, IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Calculate the data object pixel index bounds for the required axis
*  array (by transforming from the NDF pixel index system to the data
*  object pixel index system).
            LBNDS( 1 ) = LBND( IAX ) + OFFS( IAX )
            UBNDS( 1 ) = UBND( IAX ) + OFFS( IAX )

*  See if the required axis width array exists and whether the NDF is a
*  section.
            EXIST = DCB_AWID( IAX, IDCB ) .NE. ARY__NOID
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

*  Determine the numeric type of the axis width array.
            CALL NDF1_AWTYP( IAX, IACB, AWTYPE, STATUS )
         END IF

*  If the axis width array exists and is not a section, then clone an
*  ARY_ system identifier for it.
         IF ( STATUS .EQ. SAI__OK ) THEN
            NEW = .FALSE.
            IF ( EXIST .AND. ( .NOT. SECT ) ) THEN
               CALL ARY_CLONE( DCB_AWID( IAX, IDCB ), ID, STATUS )

*  Otherwise, if it exists and is a section and values must be obtained
*  from it, then obtain an ARY_ system identifier for the appropriate
*  section of the array.
            ELSE IF ( EXIST .AND. SECT .AND. ( MODER .OR. MODEU ) ) THEN
               CALL ARY_SECT( DCB_AWID( IAX, IDCB ), 1, LBNDS, UBNDS,
     :                        ID, STATUS )

*  If modifications may be made to the section's mapped values, then
*  make a temporary copy of the section so that the original data object
*  will not be affected.
               IF ( MODEU ) THEN
                  CALL ARY_TEMP( PLACE, STATUS )
                  OLDID = ID
                  CALL ARY_COPY( OLDID, PLACE, ID, STATUS )
                  CALL ARY_ANNUL( OLDID, STATUS )
               END IF

*  Otherwise, if the axis width array does not exist and the NDF is a
*  base NDF and values are to be written to the array and the axis can
*  be created (i.e. lies within the dimensionality of the actual data
*  object), then create a new axis width array. Clone an ARY_ system
*  identifier for it.
            ELSE IF ( ( .NOT. EXIST ) .AND.
     :                ( .NOT. SECT ) .AND.
     :                ( MODEW .OR. MODEU ) .AND.
     :                ( IAX .LE. NDIMD ) ) THEN
               CALL NDF1_AWCRE( IAX, IDCB, STATUS )
               CALL ARY_CLONE( DCB_AWID( IAX, IDCB ), ID, STATUS )
               NEW = .TRUE.

*  In all other cases, values will not be written back to the axis
*  width array, so a temporary array must be created for the purposes
*  of mapping. This will later be discarded when the axis width array
*  is unmapped. Create the array and note that a new temporary array
*  exists.
            ELSE
               CALL ARY_TEMP( PLACE, STATUS )
               CALL ARY_NEW( AWTYPE, 1, LBNDS, UBNDS, PLACE, ID,
     :                       STATUS )
               NEW = .TRUE.
            END IF
         END IF

*  It is possible that a section of an existing axis width array may
*  extend beyond the bounds of the associated data object. Test if it
*  extends beyond the lower bound and derive a width value to be used
*  for extrapolation if it does.
         IF ( STATUS .EQ. SAI__OK ) THEN
            LOWER = .FALSE.
            UPPER = .FALSE.
            IF ( EXIST .AND. SECT .AND. ( MODER .OR. MODEU ) ) THEN
               IF ( LBNDS( 1 ) .LT. LBNDD( IAX ) ) THEN
                  LOWER = .TRUE.
                  CALL NDF1_GAWEX( LBNDD( IAX ), UBNDD( IAX ),
     :                             DCB_AWID( IAX, IDCB ), .FALSE.,
     :                             LWIDTH, STATUS )
               END IF

*  Similarly derive a width value to use for extrapolation if it
*  extends beyond the array's upper bound.
               IF ( UBNDS( 1 ) .GT. UBNDD( IAX ) ) THEN
                  UPPER = .TRUE.
                  CALL NDF1_GAWEX( LBNDD( IAX ), UBNDD( IAX ),
     :                             DCB_AWID( IAX, IDCB ), .TRUE.,
     :                             UWIDTH, STATUS )
               END IF
            END IF

*  Map the axis width array (or derived temporary array) for access.
*  Use the access mode supplied unless a new (i.e. un-initialised)
*  array is being accessed, in which case 'WRITE' is needed.
            IF ( .NOT. NEW ) THEN
               CALL ARY_MAP( ID, TYPE, MODE, PNTR, EL, STATUS )
            ELSE
               CALL ARY_MAP( ID, TYPE, 'WRITE', PNTR, EL, STATUS )
            END IF

*  If necessary, extrapolate the mapped values to lower pixel indices.
            IF ( LOWER ) THEN
               CALL NDF1_AWEXT( TYPE, .FALSE.,
     :                          MIN( LBNDD( IAX ) - 1, UBNDS( 1 ) ),
     :                          LWIDTH, LBNDS( 1 ), UBNDS( 1 ),
     :                          PNTR, STATUS )
            END IF

*  Similarly, extrapolate to higher pixel indices if necessary.
            IF ( UPPER ) THEN
               CALL NDF1_AWEXT( TYPE, .TRUE.,
     :                          MAX( UBNDD( IAX ) + 1, LBNDS( 1 ) ),
     :                          UWIDTH, LBNDS( 1 ), UBNDS( 1 ),
     :                          PNTR, STATUS )
            END IF

*  If initialisation of the mapped values is required, then new width
*  values must derived from the values in the associated axis data
*  array. Test if these are currently mapped and note this fact.
            IF ( NEW .AND. ( MODER .OR. MODEU ) ) THEN
               IF ( ACB_ADMAP( IAX, IACB ) ) THEN
                  DATMAP = .TRUE.

*  If so, then create and map a temporary array to hold a double
*  precision copy. Transfer the mapped axis data array values to the
*  new array, converting them to double precision.
                  CALL ARY_TEMP( PLACE, STATUS )
                  CALL ARY_NEW( '_DOUBLE', 1, LBNDS, UBNDS, PLACE,
     :                          IDD, STATUS )
                  CALL ARY_MAP( IDD, '_DOUBLE', 'WRITE', DPNTR, EL,
     :                          STATUS )
                  CALL NDF1_CVTD( .TRUE., EL, ACB_ADMTP( IAX, IACB ),
     :                            ACB_ADMPT( IAX, IACB ),
     :                            %VAL( CNF_PVAL( DPNTR ) ), DCE,
     :                            STATUS )

*  If the axis data array is not mapped, then map it as an array of
*  double precision values.
               ELSE
                  DATMAP = .FALSE.
                  CALL NDF1_ADMAP( IAX, IACB, '_DOUBLE', 'READ', DPNTR,
     :                             EL, STATUS )

*  If an attempt to access the axis data array fails, then report
*  context information.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETI( 'AXIS', IAX )
                  CALL NDF1_AMSG( 'NDF', IACB )
                  CALL ERR_REP( 'NDF1_AWMAP_ACCESS',
     :                          'Unable to access the axis centre ' //
     :                          'array for axis ^AXIS of the NDF ' //
     :                          'structure ^NDF in order to ' //
     :                          'initialise the associated width ' //
     :                          'array.', STATUS )
                  END IF
               END IF


*  Initialise the mapped width values.
               CALL NDF1_AWINI( TYPE, LBNDS( 1 ), UBNDS( 1 ),
     :                          %VAL( CNF_PVAL( DPNTR ) ), PNTR,
     :                          STATUS )

*  Annul the temporary copy of the axis data array or unmap the axis
*  data array itself, as appropriate.
               IF ( DATMAP ) THEN
                  CALL ARY_ANNUL( IDD, STATUS )
               ELSE
                  CALL NDF1_ADUMP( IAX, IACB, STATUS )
               END IF
            END IF
         END IF

*  If an error occurred, then annul any ARY_ system identifier which may
*  have been acquired.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ARY_ANNUL( ID, STATUS )
         END IF
      END IF

*  If no error has occurred, then store the ARY_ system identifier for
*  the mapped array and mark the array as mapped.
      IF ( STATUS .EQ. SAI__OK ) THEN
         ACB_AWMID( IAX, IACB ) = ID
         ACB_AWMAP( IAX, IACB ) = .TRUE.

*  Increment the appropriate DCB mapping counts.
         DCB_NAWMP( IAX, IDCB ) = DCB_NAWMP( IAX, IDCB ) + 1
         DCB_NMAP( IDCB ) = DCB_NMAP( IDCB ) + 1

*  Return a null pointer under error conditions.
      ELSE
         PNTR = 0
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWMAP', STATUS )

      END
