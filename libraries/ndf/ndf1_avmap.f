      SUBROUTINE NDF1_AVMAP( IAX, IACB, TYPE, MODE, STDEV, PNTR, EL,
     :                       STATUS )
*+
*  Name:
*     NDF1_AVMAP

*  Purpose:
*     Map an NDF's axis variance array for access.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AVMAP( IAX, IACB, TYPE, MODE, PNTR, EL, STATUS )

*  Description:
*     The routine maps a specified NDF axis variance array for access.
*     The NDF is identified by its ACB entry. Account is taken of the
*     possibility that the required axis variance array may not exist
*     (either because it has not yet been created or because the NDF's
*     ACB entry contains dimensions which do not exist in the actual
*     data object described in the DCB) and suitable default values are
*     provided. Extrapolated values may also be returned if access to
*     an NDF section extending outside the bounds of the actual data
*     object is required. A new axis structure (including an axis
*     variance array) may be created by this routine if necessary.

*  Arguments:
*     IAX = INTEGER (Given)
*        Number of the axis whose variance array is to be mapped.
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric type to be used to access the mapped values (case
*        insensitive).
*     MODE = CHARACTER * ( * ) (Given)
*        Mapping mode to be used to access the values: 'READ', 'UPDATE'
*        or 'WRITE' (case insensitive).
*     STDEV = LOGICAL (Given)
*        Whether conversion of the mapped variance values to standard
*        deviations is required (as opposed to accessing the variance
*        values directly).
*     PNTR = INTEGER (Returned)
*        Pointer to the mapped array of values. A value of zero will be
*        returned if the routine is called with STATUS set, or if it
*        should fail for any reason.
*     EL = INTEGER (Returned)
*        Number of axis array elements mapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise the returned pointer value before checking the
*     inherited global status.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Check that the required axis variance array is not already
*     mapped.  Report an error if it is.
*     -  Obtain the pixel index bounds of the NDF's data array and of
*     the data array of the actual data object (whose ARY_ system
*     identifier is stored in the DCB).
*     -  Obtain the pixel offsets between the actual data object and
*     the NDF which the ACB entry describes.
*     -  Ensure that axis variance array information is available in
*     the DCB.
*     -  Calculate the data object pixel index bounds for the required
*     axis array (by transforming from the NDF pixel index system to
*     the data object pixel index system).
*     -  See if the required axis variance array exists and whether the
*     NDF is a section.
*     -  Set logical read, update or write flags.
*     -  Determine the numeric type of the axis variance array.
*     -  If the axis variance array exists and is not a section, then
*     clone an ARY_ system identifier for it.
*     -  Otherwise, if it exists and is a section and values must be
*     obtained from it, then obtain an ARY_ system identifier for the
*     appropriate section of the array.
*     -  Otherwise, if the axis variance array does not exist and the
*     NDF is a base NDF and values are to be written to the array and
*     the axis can be created (i.e. lies within the dimensionality of
*     the actual data object), then create a new axis variance array.
*     Clone an ARY_ system identifier for it. Note if the array's
*     values must be initialised.
*     -  In all other cases the axis variance array will not exist and
*     cannot be created (or exists and cannot be modified), so a
*     temporary array must be created for the purposes of mapping. This
*     will later be discarded when the axis variance array is unmapped.
*     Note if the array's values must be initialised.
*     -  The case where update access is required to a section of an
*     existing axis variance array needs special attention. In this
*     case values must be read from the array, but modifications will
*     later be discarded. A similar problem also exists when mapping a
*     pre-existing variance array as standard deviation values in READ
*     mode; in this case a temporary array is required to allow the
*     mapped values to be converted in situ to standard deviations.
*     Copy the array into a temporary array and annul the original
*     identifier.
*     -  If values are to be read from a section of an existing axis
*     variance array, then it is possible that the section being
*     accessed may extend beyond the bounds of the array. Test if it
*     extends beyond the lower bound.
*     -  Similarly see if the section extends beyond the array's upper
*     bound.
*     -  Map the array (or derived temporary array) for access. Use
*     READ/ZERO access if READ was specified to ensure that the mapped
*     values are initialised.
*     -  Otherwise, if initialisation is required, then use WRITE
*     access and perform explicit initialisation (this is used instead
*     of /ZERO on the access mode to prevent the storage form being
*     implicitly changed).
*     -  Otherwise, use the access mode as supplied.
*     -  If necessary, extrapolate the mapped values to lower pixel
*     indices by filling with zeros.
*     -  Similarly extrapolate to higher pixel indices if necessary.
*     -  If an error occurred, then annul any ARY_ system identifier
*     which may have been acquired.
*     -  If the variance values were mapped successfully, then see
*     whether conversion to standard deviations is required. Before
*     doing so, also check that pre-defined values are to be read from
*     the array and that the array's contents will not be zero,
*     otherwise conversion is superfluous.
*     -  If a conversion error occurs, then report context information.
*     -  If no error has occurred (or the only error was to encounter a
*     negative standard deviation), then store the ARY_ system
*     identifier and pointer for the mapped array. Also store the
*     numeric type used for mapping and the access mode (in upper case)
*     along with the standard deviation conversion flag.  Mark the axis
*     variance array as mapped.
*     -  Increment the appropriate DCB mapping counts.
*     -  Return a null pointer under error conditions.

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
*     16-OCT-1990 (RFWS):
*        Original version, derived from the NDF1_ADMAP routine.
*     5-NOV-1990 (RFWS):
*        Changed mapped value initialisation arrangememt to avoid
*        changing the storage form.
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
*        DCB_AVID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifiers for axis variance arrays.
*        DCB_AVTYP( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP
*        ) (Read)
*           Numeric data type of axis variance arrays.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        DCB_NAVMP( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings to each axis variance array.
*        DCB_NMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Total number of current mappings to each data object.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_AVMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether NDF axis variance arrays are currently mapped for
*           access.
*        ACB_AVMID( NDF__MXDIM, NDF__MXACB ) = INTEGER (Write)
*           ARY_ system identifier for axis variance array mappings.
*        ACB_AVMMD( NDF__MXDIM, NDF__MXACB ) = CHARACTER * ( NDF__SZMOD
*        ) (Write)
*           Access mode used to map axis variance arrays.
*        ACB_AVMPT( NDF__MXDIM, NDF__MXACB ) = INTEGER (Write)
*           Pointers to mapped axis variance arrays.
*        ACB_AVMST( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Write)
*           Whether mapped axis variance values are standard deviations.
*        ACB_AVMTP( NDF__MXDIM, NDF__MXACB ) = CHARACTER * ( NDF__SZTYP
*        ) (Write)
*           Numeric type used to map axis variance arrays.
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

*  Arguments Given:
      INTEGER IAX
      INTEGER IACB
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) MODE
      LOGICAL STDEV

*  Arguments Returned:
      INTEGER PNTR
      INTEGER EL

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) AVTYPE ! Axis variance numeric type
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
      LOGICAL DCE                ! Data conversion error?
      LOGICAL EXIST              ! Does axis variance array exist?
      LOGICAL INIT               ! Array must be initialised?
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

*  Check that the required axis variance array is not already mapped.
*  Report an error if it is.
      IF ( ACB_AVMAP( IAX, IACB ) ) THEN
         STATUS = NDF__ISMAP
         CALL MSG_SETI( 'AXIS', IAX )
         CALL NDF1_AMSG( 'NDF', IACB )
         CALL ERR_REP( 'NDF1_AVMAP_MAP',
     :   'The variance array for axis ^AXIS of the NDF structure ' //
     :   '^NDF is already mapped for access through the specified ' //
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

*  Ensure that axis variance array information is available in the DCB.
         CALL NDF1_DAV( IAX, IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Calculate the data object pixel index bounds for the required axis
*  array (by transforming from the NDF pixel index system to the data
*  object pixel index system).
            LBNDS( 1 ) = LBND( IAX ) + OFFS( IAX )
            UBNDS( 1 ) = UBND( IAX ) + OFFS( IAX )

*  See if the required axis variance array exists and whether the NDF
*  is a section.
            EXIST = DCB_AVID( IAX, IDCB ) .NE. ARY__NOID
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

*  Determine the numeric type of the axis variance array.
            CALL NDF1_AVTYP( IAX, IACB, AVTYPE, STATUS )
         END IF

*  If the axis variance array exists and is not a section, then clone
*  an ARY_ system identifier for it.
         IF ( STATUS .EQ. SAI__OK ) THEN
            INIT = .FALSE.
            IF ( EXIST .AND. ( .NOT. SECT ) ) THEN
               CALL ARY_CLONE( DCB_AVID( IAX, IDCB ), ID, STATUS )

*  Otherwise, if it exists and is a section and values must be obtained
*  from it, then obtain an ARY_ system identifier for the appropriate
*  section of the array.
            ELSE IF ( EXIST .AND. SECT .AND. ( MODER .OR. MODEU ) ) THEN
               CALL ARY_SECT( DCB_AVID( IAX, IDCB ), 1, LBNDS, UBNDS,
     :                        ID, STATUS )

*  Otherwise, if the axis variance array does not exist and the NDF is
*  a base NDF and values are to be written to the array and the axis
*  can be created (i.e. lies within the dimensionality of the actual
*  data object), then create a new axis variance array. Clone an ARY_
*  system identifier for it.
            ELSE IF ( ( .NOT. EXIST ) .AND.
     :                ( .NOT. SECT ) .AND.
     :                ( MODEW .OR. MODEU ) .AND.
     :                ( IAX .LE. NDIMD ) ) THEN
               CALL NDF1_AVCRE( IAX, IDCB, STATUS )
               CALL ARY_CLONE( DCB_AVID( IAX, IDCB ), ID, STATUS )

*  Note if the array's values must be initialised.
               INIT = .NOT. MODEW

*  In all other cases the axis variance array will not exist and cannot
*  be created (or exists and cannot be modified), so a temporary array
*  must be created for the purposes of mapping. This will later be
*  discarded when the axis variance array is unmapped.
            ELSE
               CALL ARY_TEMP( PLACE, STATUS )
               CALL ARY_NEW( AVTYPE, 1, LBNDS, UBNDS, PLACE, ID,
     :                       STATUS )

*  Note if the array's values must be initialised.
               INIT = .NOT. MODEW
            END IF

*  The case where update access is required to a section of an existing
*  axis variance array needs special attention. In this case values
*  must be read from the array, but modifications will later be
*  discarded. A similar problem also exists when mapping a pre-existing
*  variance array as standard deviation values in READ mode; in this
*  case a temporary array is required to allow the mapped values to be
*  converted in situ to standard deviations.  Copy the array into a
*  temporary array and annul the original identifier.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( EXIST .AND.
     :              ( ( SECT .AND. MODEU ) .OR.
     :                ( STDEV .AND. MODER ) ) ) THEN
                  CALL ARY_TEMP( PLACE, STATUS )
                  OLDID = ID
                  CALL ARY_COPY( OLDID, PLACE, ID, STATUS )
                  CALL ARY_ANNUL( OLDID, STATUS )
               END IF
            END IF

*  If values are to be read from a section of an existing axis variance
*  array, then it is possible that the section being accessed may
*  extend beyond the bounds of the array. Test if it extends beyond the
*  lower bound.
            IF ( STATUS .EQ. SAI__OK ) THEN
               LOWER = .FALSE.
               UPPER = .FALSE.
               IF ( EXIST .AND. SECT .AND. ( MODER .OR. MODEU ) ) THEN
                  LOWER = LBNDS( 1 ) .LT. LBNDD( IAX )

*  Similarly see if the section extends beyond the array's upper bound.
                  UPPER = UBNDS( 1 ) .GT. UBNDD( IAX )
               END IF

*  Map the array (or derived temporary array) for access. Use READ/ZERO
*  access if READ was specified to ensure that the mapped values are
*  initialised.
               IF ( MODER ) THEN
                  CALL ARY_MAP( ID, TYPE, 'READ/ZERO', PNTR, EL,
     :                          STATUS )

*  Otherwise, if initialisation is required, then use WRITE access and
*  perform explicit initialisation (this is used instead of /ZERO on
*  the access mode to prevent the storage form being implicitly
*  changed).
               ELSE IF ( INIT ) THEN
                  CALL ARY_MAP( ID, TYPE, 'WRITE', PNTR, EL, STATUS )
                  CALL NDF1_AVEXT( TYPE, .TRUE.,
     :                             LBNDS( 1 ), LBNDS( 1 ), UBNDS( 1 ),
     :                             PNTR, STATUS )

*  Otherwise, use the access mode as supplied.
               ELSE
                  CALL ARY_MAP( ID, TYPE, MODE, PNTR, EL, STATUS )
               END IF

*  If necessary, extrapolate the mapped values to lower pixel indices by
*  filling with zeros.
               IF ( LOWER ) THEN
                  CALL NDF1_AVEXT( TYPE, .FALSE.,
     :                             MIN( LBNDD( IAX ) - 1, UBNDS( 1 ) ),
     :                             LBNDS( 1 ), UBNDS( 1 ), PNTR,
     :                             STATUS )
               END IF

*  Similarly extrapolate to higher pixel indices if necessary.
               IF ( UPPER ) THEN
                  CALL NDF1_AVEXT( TYPE, .TRUE.,
     :                             MAX( UBNDD( IAX ) + 1, LBNDS( 1 ) ),
     :                             LBNDS( 1 ), UBNDS( 1 ), PNTR,
     :                             STATUS )
               END IF
            END IF

*  If an error occurred, then annul any ARY_ system identifier which may
*  have been acquired.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ARY_ANNUL( ID, STATUS )
            END IF
         END IF
      END IF

*  If the variance values were mapped successfully, then see whether
*  conversion to standard deviations is required. Before doing so, also
*  check that pre-defined values are to be read from the array and that
*  the array's contents will not be zero, otherwise conversion is
*  superfluous.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( STDEV .AND. EXIST .AND. ( .NOT. MODEW ) ) THEN
            CALL NDF1_V2S( .TRUE., TYPE, EL, PNTR, DCE, STATUS )

*  If a conversion error occurs, then report context information.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETI( 'AXIS', IAX )
               CALL NDF1_AMSG( 'NDF', IACB )
               CALL ERR_REP( 'NDF1_AVMAP_CONV',
     :                       'Error converting axis variance values ' //
     :                       'into standard deviations (errors) for ' //
     :                       'axis ^AXIS of the NDF structure ^NDF',
     :                       STATUS )
            END IF
         END IF
      END IF

*  If no error has occurred (or the only error was to encounter a
*  negative standard deviation), then store the ARY_ system identifier
*  and pointer for the mapped array. Also store the numeric type used
*  for mapping and the access mode (in upper case) along with the
*  standard deviation conversion flag.  Mark the axis variance array as
*  mapped.
      IF ( ( STATUS .EQ. SAI__OK ) .OR.
     :     ( STATUS .EQ. NDF__NGVAR ) ) THEN
         ACB_AVMID( IAX, IACB ) = ID
         ACB_AVMPT( IAX, IACB ) = PNTR
         ACB_AVMTP( IAX, IACB ) = TYPE
         CALL CHR_UCASE( ACB_AVMTP( IAX, IACB ) )
         ACB_AVMMD( IAX, IACB ) = MODE
         CALL CHR_UCASE( ACB_AVMMD( IAX, IACB ) )
         ACB_AVMST( IAX, IACB ) = STDEV
         ACB_AVMAP( IAX, IACB ) = .TRUE.

*  Increment the appropriate DCB mapping counts.
         DCB_NAVMP( IAX, IDCB ) = DCB_NAVMP( IAX, IDCB ) + 1
         DCB_NMAP( IDCB ) = DCB_NMAP( IDCB ) + 1

*  Return a null pointer under error conditions.
      ELSE
         PNTR = 0
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AVMAP', STATUS )

      END
