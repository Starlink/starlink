      SUBROUTINE ARY1_GMRB( IACB, MTREX, MRFULL, WHOLE, LMRB, UMRB,
     :                      LMTR, UMTR, STATUS )
*+
*  Name:
*     ARY1_GMRB

*  Purpose:
*     Get mapping region bounds.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_GMRB( IACB, MTREX, MRFULL, WHOLE, LMRB, UMRB, LMTR,
*     UMTR, STATUS )

*  Description:
*     The routine determines the extent of the mapping region
*     associated with access to all or part of an array. It also
*     determines whether a mapping transfer region exists, whether it
*     fills the mapping region and whether the whole of the data object
*     should be mapped. The results are returned using the pixel index
*     system of the actual data object.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the entry in the ACB for which mapping region
*        information is required.
*     MTREX = LOGICAL (Returned)
*        Whether a mapping transfer region exists.
*     MRFULL = LOGICAL (Returned)
*        Whether the mapping transfer region completely fills the
*        mapping region.
*     WHOLE = LOGICAL (Returned)
*        Whether the mapping transfer region and mapping region both
*        consist of the entire data object, indicating that the entire
*        object can be mapped.
*     LMRB( ARY__MXDIM ) = INTEGER (Returned)
*        Lower mapping region bounds.
*     UMRB( ARY__MXDIM ) = INTEGER (Returned)
*        Upper mapping region bounds.
*     LMTR( ARY__MXDIM ) = INTEGER (Returned)
*        Lower bounds of mapping transfer region.
*     UMTR( ARY__MXDIM ) = INTEGER (Returned)
*        Upper bounds of mapping transfer region.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise and determine whether a mapping transfer region can
*     possibly exist.
*     -  Loop to process each dimension in turn.
*     -  Obtain the bounds of the mapping region and convert them into
*     the reference frame pixel index system.
*     -  Calculate the bounds of the mapping transfer region in the
*     reference frame pixel index system.
*     -  Convert the results obtained so far into the data object pixel
*     index system.
*     -  Determine whether the mapping transfer region exists, whether
*     it fills the mapping region and whether the mapping region
*     comprises the whole data object, using the bounds information for
*     the current dimension.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-JUN-1989 (RFWS):
*        Original version.
*     7-SEP-1989 (RFWS):
*        Substantial re-write to use all (ARY__MXDIM) dimensions when
*        calculating the mapping region bounds information.
*     11-SEP-1989 (RFWS):
*        Fixed bug in calculation of upper mapping transfer region
*        bounds.
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
*        DCB_SFT( ARY_MXDIM, ARY__MXDCB ) = INTEGER (Read)
*           Accumulated pixel shifts for data object.
*        DCB_UBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read)
*           Upper bounds of data object.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_DTWEX( ARY__MXACB ) = LOGICAL (Read)
*           Whether a data transfer window exists.
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_LBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Lower bounds of access region.
*        ACB_LDTW( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Lower bounds of data transfer window.
*        ACB_SFT( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Accumulated pixel shifts since access was obtained.
*        ACB_UBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Upper bounds of access region.
*        ACB_UDTW( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Upper bounds of data transfer window.

*  Arguments Given:
      INTEGER IACB

*  Arguments Returned:
      LOGICAL MTREX
      LOGICAL MRFULL
      LOGICAL WHOLE
      INTEGER LMRB( ARY__MXDIM )
      INTEGER UMRB( ARY__MXDIM )
      INTEGER LMTR( ARY__MXDIM )
      INTEGER UMTR( ARY__MXDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IDCB               ! Index to an entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an index to the data object entry in the DCB.
      IDCB = ACB_IDCB( IACB )

*  Initialise. The mapping transfer region can only exist if a data
*  transfer window exists.
      MTREX = ACB_DTWEX( IACB )
      MRFULL = MTREX
      WHOLE = MTREX

*  Loop to consider each dimension.
      DO 1 I = 1, ARY__MXDIM

*  Obtain the bounds of the mapping region, removing the ACB
*  accumulated pixel index shift to convert them into the reference
*  frame pixel index system.
         LMRB( I ) = ACB_LBND( I, IACB ) - ACB_SFT( I, IACB )
         UMRB( I ) = ACB_UBND( I, IACB ) - ACB_SFT( I, IACB )

*  Obtain the bounds of the mapping transfer region in the reference
*  frame pixel index system. This is formed by the intersection of the
*  mapping region, the data transfer window and the actual data object
*  bounds. Note that the accumulated DCB pixel index shifts have to be
*  removed from the latter to convert them into the reference frame
*  system.
         LMTR( I ) = MAX( LMRB( I ), ACB_LDTW( I, IACB ),
     :                    DCB_LBND( I, IDCB ) - DCB_SFT( I, IDCB ) )
         UMTR( I ) = MIN( UMRB( I ), ACB_UDTW( I, IACB ),
     :                    DCB_UBND( I, IDCB ) - DCB_SFT( I, IDCB ) )

*  Convert the above results into the data object pixel index system by
*  adding the accumulated DCB pixel index shifts (note the above
*  calculations have to be carried out in the reference frame pixel
*  index system to avoid overflow problems due to the "infinite" extent
*  of the data transfer window associated with base arrays).
         LMRB( I ) = LMRB( I ) + DCB_SFT( I, IDCB )
         UMRB( I ) = UMRB( I ) + DCB_SFT( I, IDCB )
         LMTR( I ) = LMTR( I ) + DCB_SFT( I, IDCB )
         UMTR( I ) = UMTR( I ) + DCB_SFT( I, IDCB )

*  Note whether the mapping transfer region exists.
         MTREX = MTREX .AND. ( LMTR( I ) .LE. UMTR( I ) )

*  Note whether the mapping transfer region completely fills the mapping
*  region.
         MRFULL = MTREX .AND. MRFULL .AND.
     :            ( LMTR( I ) .EQ. LMRB( I ) ) .AND.
     :            ( UMTR( I ) .EQ. UMRB( I ) )

*  Note whether the mapping region (and mapping transfer region)
*  comprise the whole data object.
         WHOLE = MRFULL .AND. WHOLE .AND.
     :           ( LMTR( I ) .EQ. DCB_LBND( I, IDCB ) ) .AND.
     :           ( UMTR( I ) .EQ. DCB_UBND( I, IDCB ) )
1     CONTINUE
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_GMRB', STATUS )

      END
