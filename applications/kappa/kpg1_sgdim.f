      SUBROUTINE KPG1_SGDIM( NDF, NDIM, DIMV, STATUS )
*+
*  Name:
*     KPG1_SGDIM

*  Purpose:
*     Determines the number of significant dimensions in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SGDIM( NDF, NDIM, DIMV, STATUS )

*  Description:
*     This routine finds the number of significant dimensions, i.e.
*     those with greater than one element, in an NDF.  If the number
*     found is not equal to a specified number a bad status, SAI_ERROR,
*     is returned.  Likewise should there be no significant dimensions.
*     The significant dimensions are recorded and returned.

*  Arguments:
*     NDF = INTEGER (Given)
*        The NDF identifier.
*     NDIM = INTEGER (Given)
*        The desired number of dimensions.
*     DIMV( NDF__MXDIM ) = INTEGER (Returned)
*        The significant dimensions i.e. the ones that are greater than
*        one.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  The NDF must exist.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 11 (MJC):
*        Original version.
*     1992 April 16 (MJC):
*        Added NDF token to the error reports.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER
     :  NDF,
     :  NDIM

*  Arguments Returned:
      INTEGER
     :  DIMV( NDF__MXDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  ACTDIM,                  ! Actual number of dimensions in the
                                 ! NDF
     :  DIM( NDF__MXDIM ),       ! The NDF dimensions
     :  I,                       ! Loop counter
     :  SIGDIM                   ! Number of signifcant dimensions

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find the number of dimensions.

      CALL NDF_DIM( NDF, NDF__MXDIM, DIM, ACTDIM, STATUS )

*    Initialise counter.

      SIGDIM = 0

*    Initialise the significant dimensions.

      DO I = 1, ACTDIM
         DIMV( I ) = 0
      END DO

*    Loop for each dimension.

      DO I = 1, ACTDIM

*       Is the dimension significant?

         IF ( DIM( I ) .GT. 1 ) THEN

*          Yes, so add it to the total.

            SIGDIM = SIGDIM + 1

*          Record the dimension.

            DIMV( SIGDIM ) = I
         END IF
      END DO

*    Look for error conditions.
*    ==========================

*    Must have at least one significant dimension.

      IF ( SIGDIM .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP( 'KPG1_SGDIM_NOSIG',
     :     'All dimensions are one in the NDF ^NDF.', STATUS )

*    The effective dimensionality of the NDF must equal the prescribed
*    number.

      ELSE IF ( SIGDIM .NE. NDIM ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', NDF )
         CALL MSG_SETI( 'NDIM', NDIM )
         CALL ERR_REP( 'KPG1_SGDIM_NODIM',
     :     'The NDF ^NDF is not ^NDIM-dimensional.', STATUS )
      END IF

      END
