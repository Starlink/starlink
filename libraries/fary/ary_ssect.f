      SUBROUTINE ARY_SSECT( IARY1, IARY2, IARY3, STATUS )
*+
*  Name:
*     ARY_SSECT

*  Purpose:
*     Create a similar array section to an existing one.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_SSECT( IARY1, IARY2, IARY3, STATUS )

*  Description:
*     The routine creates a new array section, using an existing
*     section as a template. The new section bears the same
*     relationship to its base array as the template section does to
*     its own base array. Allowance is made for pixel-index shifts
*     which may have been applied so that the pixel-indices of the new
*     section match those of the template.  The number of dimensions of
*     the input and template arrays may differ.

*  Arguments:
*     IARY1 = INTEGER (Given)
*        Identifier for the input array from which the section is to be
*        derived. This may be a base array or an array section.
*     IARY2 = INTEGER (Given)
*        Identifier for the template section (this may also be a base
*        array or an array section).
*     IARY3 = INTEGER (Returned)
*        Identifier for the new array section.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine normally generates an array section.  However, if
*     both input arrays are base arrays with identical pixel-index
*     bounds, then there is no need to create a section in order to
*     access the required part of the first array. In this case a base
*     array identifier will be returned instead.
*     -  The new section created by this routine will have the same
*     number of dimensions as the array (or array section) from which
*     it is derived. If the template (IARY2) array has fewer dimensions
*     than this, then the bounds of any additional input dimensions are
*     preserved unchanged in the new array. If the template (IARY2)
*     array has more dimensions, then the excess ones are ignored.
*     -  This routine takes account of the regions of each base array
*     to which the input array sections have access. It may therefore
*     restrict the region accessible to the new section (and pad with
*     "bad" pixels) so as not to grant access to regions of the base
*     array which were not previously accessible through the input
*     arrays.
*     -  If this routine is called with STATUS set, then a value of
*     ARY__NOID will be returned for the IARY3 argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The ARY__NOID
*     constant is defined in the include file ARY_PAR.

*  Algorithm:
*     -  Set an initial value for the IARY3 argument before checking the
*     inherited status.
*     -  Import the identifiers for the input arrays.
*     -  Initialise the ACB index for the output array and obtain DCB
*     indices for the data objects associated with the input arrays.
*     -  Determine the number of dimensions in common between the two
*     input arrays.
*     -  Derive the differential pixel index shifts which relate pixel
*     indices in one input array to the equivalent pixel indices in the
*     other array ("equivalent" here means having the same pixel indices
*     in the actual data object or base array).
*     -  Shift the bounds of the IARY2 array to determine the equivalent
*     bounds in the IARY1 array pixel index system.
*     -  If the IARY1 array has additional dimensions, then set the
*     bounds of these dimensions to be unchanged.
*     -  Check to see if the bounds of the region to be extracted from
*     the IARY1 array match its existing bounds.
*     -  If either input array is an array section or the bounds do not
*     match, then cut an appropriate section from the input array,
*     generating a new ACB entry to describe it.
*     -  At this point, the new array section's data transfer window
*     takes account of the bounds of the two input arrays and the data
*     transfer window of the first input array. The effect of the data
*     transfer window of the second input array must now be included.
*     This is only necessary if this second array is not a base array
*     and if the output array currently has a data transfer window.
*     -  If the second input array has no data transfer window, then
*     neither does the output array.
*     -  Otherwise, find the intersection region between the current
*     output array data transfer window and that of the second input
*     array (both in the reference frame pixel index system). Note
*     whether this intersection region exists in the ACB entry for the
*     output array.
*     -  If the intersection region exists, then transfer its bounds to
*     the new ACB entry as the data transfer window bounds of the
*     output array.
*     -  Shift the resulting section so that its pixel index system
*     matches that of the template array.
*     -  If both input arrays are base arrays whose bounds match, then
*     simply clone the input ACB base array entry.
*     -  Export an identifier for the new array section.
*     -  If an error has occurred, but a new ACB entry has been
*     allocated, then clean up by annulling it.
*     -  If an error occurred, then report context information.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-NOV-1989 (RFWS):
*        Original version.
*     13-MAR-1990 (RFWS):
*        Added code to account for the extent and possible
*        non-existence of the data transfer window of the template
*        array section.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_SFT( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read)
*           Accumulated pixel index shifts since the data object was
*           first imported/created.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_CUT( ARY__MXACB ) = LOGICAL (Read)
*           Whether the ACB entry describes a cut (i.e. array section).
*        ACB_DTWEX( ARY__MXACB ) = LOGICAL (Read and Write)
*           Whether a data transfer window exists.
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_LBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Lower bounds of array.
*        ACB_LDTW( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Lower data transfer window bounds.
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Read)
*           Number of dimensions for each array described by an ACB
*           entry.
*        ACB_SFT( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Accumulated pixel index shifts for each ACB entry.
*        ACB_UBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Upper bounds of array.
*        ACB_UDTW( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Upper data transfer window bounds.

*  Arguments Given:
      INTEGER IARY1
      INTEGER IARY2

*  Arguments Returned:
      INTEGER IARY3

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IACB1              ! ACB index for the IARY1 array
      INTEGER IACB2              ! ACB index for the IARY2 array
      INTEGER IACB3              ! ACB index for the IARY3 array
      INTEGER IDCB1              ! DCB index for the IARY1 data object
      INTEGER IDCB2              ! DCB index for the IARY2 data object
      INTEGER LBND( ARY__MXDIM ) ! Lower bound of region to be accessed
      INTEGER LX( ARY__MXDIM )   ! Lower bounds of data transfer window
      INTEGER NDIM               ! Number of array dimensions in common
      INTEGER SFT( ARY__MXDIM )  ! Differential pixel index shifts
      INTEGER UBND( ARY__MXDIM ) ! Upper bound of region to be accessed
      INTEGER UX( ARY__MXDIM )   ! Upper bounds of data tranfer window
      LOGICAL MATCH              ! Whether array bounds match

*.

*  Set an initial value for the IARY3 argument.
      IARY3 = ARY__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the identifiers for the input arrays.
      CALL ARY1_IMPID( IARY1, IACB1, STATUS )
      CALL ARY1_IMPID( IARY2, IACB2, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the ACB index for the output array.
         IACB3 = 0

*  Obtain indices to the data object entries in the DCB for the two
*  input arrays.
         IDCB1 = ACB_IDCB( IACB1 )
         IDCB2 = ACB_IDCB( IACB2 )

*  Determine the number of dimensions in common between the two input
*  arrays.
         NDIM = MIN( ACB_NDIM( IACB1 ), ACB_NDIM( IACB2 ) )

*  Loop through the common dimensions, deriving the pixel index shift
*  necessary in each dimension to relate pixel indices in one array
*  section to those in the other. This is formed by finding the shift
*  needed to convert from pixel indices in the IARY2 section to pixels
*  in its base array, then subtracting the equivalent quantity for the
*  IARY1 section.
         DO 1 I = 1, NDIM
            SFT( I ) = ( ACB_SFT( I, IACB2 ) - DCB_SFT( I, IDCB2 ) ) -
     :                 ( ACB_SFT( I, IACB1 ) - DCB_SFT( I, IDCB1 ) )

*  Subtract the shifts from the template section (IARY2) pixel index
*  bounds to derive the bounds in the pixel index system of the IARY1
*  section which correspond to the same region of its base array.
            LBND( I ) = ACB_LBND( I, IACB2 ) - SFT( I )
            UBND( I ) = ACB_UBND( I, IACB2 ) - SFT( I )
1        CONTINUE

*  If the IARY1 array has more dimensions than the template, then its
*  bounds in the additional dimensions are preserved.
         DO 2 I = ACB_NDIM( IACB2 ) + 1, ACB_NDIM( IACB1 )
            SFT( I ) = 0
            LBND( I ) = ACB_LBND( I, IACB1 )
            UBND( I ) = ACB_UBND( I, IACB1 )
2        CONTINUE

*  Check to see if the bounds of the section to be extracted from the
*  IARY1 array match its existing bounds (if so, then there may be no
*  need to generate a new section in order to access the required part
*  of the base array).
         MATCH = .TRUE.
         DO 3 I = 1, NDIM
            IF ( ( LBND( I ) .NE. ACB_LBND( I, IACB1 ) ) .OR.
     :           ( UBND( I ) .NE. ACB_UBND( I, IACB1 ) ) ) THEN
               MATCH = .FALSE.
               GO TO 4
             END IF
3        CONTINUE
4        CONTINUE

*  If either input array is not a base array, or if the bounds do not
*  match (above), then a new section must be created. Cut the
*  appropriate section from the IARY1 array, generating a new ACB entry
*  to describe it.
         IF ( ACB_CUT( IACB1 ) .OR. ACB_CUT( IACB2 ) .OR.
     :        ( .NOT. MATCH ) ) THEN
            CALL ARY1_CUT( IACB1, ACB_NDIM( IACB1 ), LBND, UBND, IACB3,
     :                     STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  At this point, the new array section's data transfer window takes
*  account of the bounds of the two input arrays and the data transfer
*  window of the first input array. The effect of the data transfer
*  window of the second input array must now be included. This is only
*  necessary if this second array is not a base array and if the output
*  array currently has a data transfer window.
               IF ( ACB_CUT( IACB2 ) .AND. ACB_DTWEX( IACB3 ) ) THEN

*  If the second input array has no data transfer window, then neither
*  does the output array.
                  IF ( .NOT. ACB_DTWEX( IACB2 ) ) THEN
                     ACB_DTWEX( IACB3 ) = .FALSE.

*  Otherwise, find the intersection region between the current output
*  array data transfer window and that of the second input array (both
*  in the reference frame pixel index system). Note whether this
*  intersection region exists in the ACB entry for the output array.
                  ELSE
                     CALL ARY1_XSBND( ARY__MXDIM, ACB_LDTW( 1, IACB3 ),
     :                                            ACB_UDTW( 1, IACB3 ),
     :                                ARY__MXDIM, ACB_LDTW( 1, IACB2 ),
     :                                            ACB_UDTW( 1, IACB2 ),
     :                                ARY__MXDIM, LX, UX,
     :                                ACB_DTWEX( IACB3 ), STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  If the intersection region exists, then transfer its bounds to the
*  new ACB entry as the data transfer window bounds of the output
*  array.
                        IF ( ACB_DTWEX( IACB3 ) ) THEN
                           DO 5 I = 1, ARY__MXDIM
                              ACB_LDTW( I, IACB3 ) = LX( I )
                              ACB_UDTW( I, IACB3 ) = UX( I )
5                          CONTINUE
                        END IF
                     END IF
                  END IF
               END IF
            END IF

*  Apply the differential pixel index shifts to the new array section
*  to convert its pixel indices to match the template (IARY2) array.
            CALL ARY1_SFT( ACB_NDIM( IACB1 ), SFT, IACB3, STATUS )

*  If both input arrays are base arrays with the same bounds, then
*  access to the entire array is required, so the IARY1 base array entry
*  in the ACB can simply be cloned.
         ELSE
            CALL ARY1_CLN( IACB1, IACB3, STATUS )
         END IF

*  Export an identifier for the new array section.
         CALL ARY1_EXPID( IACB3, IARY3, STATUS )

*  If an error occurred, but a new ACB entry was allocated, then clean
*  up by annulling it.
         IF ( ( STATUS .NE. SAI__OK ) .AND. ( IACB3 .NE. 0 ) ) THEN
            CALL ARY1_ANL( IACB3, STATUS )
         END IF
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_SSECT_ERR',
     :   'ARY_SSECT: Error obtaining an array section using an ' //
     :   'existing section as a template.', STATUS )
         CALL ARY1_TRACE( 'ARY_SSECT', STATUS )
      END IF

      END
