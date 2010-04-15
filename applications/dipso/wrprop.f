      SUBROUTINE WRPROP( NDFNAM, INDFM, STATUS )
*+
* Name:
*     WRPROP

*  Purpose:
*     Copy the internal FLUX values to an output NDF, basing the NDF on
*     a supplied model NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WRPROP( NDFNAM, INDFM, STATUS )

*  Description:
*     An output NDF is created by copying the supplied model NDF. The
*     values in the internal FLUX array are then stored in the output
*     DATA array. The position at which each flux value is stored in
*     the output DATA array is determined by the values in the internal
*     WAVE array and the values in the AXIS CENTRE array for axis 1 of
*     the NDF.  FLUX values are stored at the pixel for which the AXIS
*     CENTRE value matches the internal WAVE value. An error is
*     reported if there is no overlap between the AXIS CENTRE values
*     and the WAVE values. An error is also reported if the NDF is not
*     1-dimensional.
*
*     Only the DATA component of the output NDF is set by this routine.
*     The shape of the NDF and all other components (AXIS, TITLE, all
*     extensions, etc) are inherited from the model NDF.

*  Arguments:
*     NDFNAM = CHARACTER * ( * ) (Given)
*        The name of the NDF structure.
*     INDFM = INTEGER (Given)
*        An identifier for the model NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-SEP-1994 (DSB):
*        Original version.
*     29-SEP-2004 (DSB):
*        Use CNF_PVAL
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) NDFNAM
      INTEGER INDFM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :     EL,                   ! No. of elements mapped
     :     INDF1,                ! Identifier for output NDF
     :     IPAXIS,               ! Pointer to mapped AXIS CENTRE array
     :     IPDATA,               ! Pointer to mapped DATA array
     :     LBND,                 ! Lower pixel bound of model NDF
     :     NDIM,                 ! No. of dimensions in model NDF
     :     NGOOD,                ! No. of good values in o/p DATA array
     :     PLACE,                ! Place holder for new NDF
     :     UBND                  ! Upper pixel bound of model NDF
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the bounds of the model NDF. An error will be reported if it is
*  not 1 dimensional.
      CALL NDF_BOUND( INDFM, 1, LBND, UBND, NDIM, STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Get a place holder for a new NDF of the required name.
      CALL NDF_PLACE( DAT__ROOT, NDFNAM, PLACE, STATUS )

*  Create the output NDF by copying the model NDF.
      CALL NDF_COPY( INDFM, PLACE, INDF1, STATUS )

*  Map the DATA component of the output NDF.
      CALL NDF_MAP( INDF1, 'DATA', '_REAL', 'WRITE', IPDATA, EL,
     :              STATUS )

*  Map the AXIS CENTRE component for axis 1. The NDF is 1-dimensional
*  and so the mapped AXIS CENTRE array will be the same size as the
*  mapped DATA array.
      CALL NDF_AMAP( INDF1, 'CENTRE', 1, '_REAL', 'READ', IPAXIS, EL,
     :               STATUS )

*  Fill the DATA array with values taken from the internal FLUX array. A
*  FLUX value is only assigned to a DATA element if the corresponding
*  WAVE and AXIS CENTRE values are the same. DATA values which do not
*  have a corresponding element in the FLUX array are set bad.
      CALL AXMAT( EL, %VAL( CNF_PVAL( IPAXIS ) ),
     :            %VAL( CNF_PVAL( IPDATA ) ), NGOOD, STATUS )

*  Report an error if there was no overlap between the AXIS CENTRE
*  values and the WAVE values.
      IF( NGOOD .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'WRPROP_ERR2', 'None of the pixels in the ' //
     :        'model NDF have the same axis position as any of the '//
     :        'stored flux values.', STATUS )
      END IF

*  Delete the output NDF if an error has occured.
      IF( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF1, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

      END
