      SUBROUTINE IRM_CHKOB( LOC, NAME, TYPE, NDIMS, DIMS, STATUS )
*+
*  Name:
*     IRM_CHKOB

*  Purpose:
*     Check a component of an object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_CHKOB( LOC, NAME, TYPE, NDIMS, DIMS, STATUS )

*  Description:
*     This routine checks that a component exists within the enclosing
*     object, with the specified name, HDS type and shape. If no such
*     component exists, and error status is returned.

*  Arguments:
*     LOC = CHARACTER (Given)
*        Locator to the enclosing HDS object.
*     NAME = CHARACTER (Given)
*        The name of the component to be checked.
*     TYPE = CHARACTER (Given)
*        The expected HDS type for the component.
*     NDIMS = INTEGER (Given)
*        The expected number of dimensions for the component. This
*        should be set to zero for a scalar object, in which case the
*        DIMS argument is ignored.
*     DIMS( * ) = INTEGER (Given)
*        The expected size of each dimension of the component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-DEC-1990 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Arguments Given:
      CHARACTER LOC*(*)
      CHARACTER NAME*(*)
      CHARACTER TYPE*(*)
      INTEGER   NDIMS
      INTEGER   DIMS( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER   CHR_LEN             ! Function giving used length of a 
                                    ! string.
      CHARACTER CMPLOC*(DAT__SZLOC) ! Locator to the required component.
      INTEGER   IDIM                ! Dimension counter.
      INTEGER   OBDIM( DAT__MXDIM ) ! Dimension sizes found for
                                    ! required component.
      INTEGER   OBNDIM              ! No. of dimensions found for
                                    ! required component.
      CHARACTER OBTYPE*(DAT__SZTYP) ! Type found for required component.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Strip leading blanks from the NAME and TYPE arguements.
      CALL CHR_LDBLK( NAME )
      CALL CHR_LDBLK( TYPE )

*  Get a locator to the required object.
      CALL DAT_FIND( LOC, NAME, CMPLOC, STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'IRM_CHKOB_ERR1',
     :                 'Unable to find object ^NAME',
     :                 STATUS )
         GO TO 999
      END IF

*  Get the objects data type.
      CALL DAT_TYPE( CMPLOC, OBTYPE, STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'IRM_CHKOB_ERR2',
     :                 'Unable to obtain the data type of object ^NAME',
     :                 STATUS )
         GO TO 999
      END IF

*  Check the object type matches the given type.
      IF( OBTYPE( :CHR_LEN(TYPE) ) .NE. TYPE ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL MSG_SETC( 'TYPE', OBTYPE )
         CALL ERR_REP( 'IRM_CHKOB_ERR3',
     :                 'Object ^NAME has an invalid data type of ^TYPE',
     :                 STATUS )
         GO TO 999
      END IF

*  Get the objects shape.
      CALL DAT_SHAPE( CMPLOC, DAT__MXDIM, OBDIM, OBNDIM, STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'IRM_CHKOB_ERR4',
     :                 'Unable to obtain the shape of object ^NAME',
     :                 STATUS )
         GO TO 999
      END IF

*  Check the number of dimensions matches the given value.
      IF( OBNDIM .NE. NDIMS ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL MSG_SETI( 'NDIM', OBNDIM )
         CALL ERR_REP( 'IRM_CHKOB_ERR5',
     :   'Object ^NAME has an invalid no. of dimensions =^NDIM',
     :                 STATUS )
         GO TO 999
      END IF

*  Check the dimension sizes matches the given values.
      DO IDIM = 1, NDIMS
         IF( OBDIM( IDIM ) .NE. DIMS( IDIM ) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', NAME )
            CALL MSG_SETI( 'SZ', OBDIM( IDIM ) )
            CALL MSG_SETI( 'DM', IDIM )
            CALL ERR_REP( 'IRM_CHKOB_ERR6',
     :      'Object ^NAME has an invalid size of ^SZ for dimension ^DM',
     :                    STATUS )
            GO TO 999
         END IF
      END DO
      
*  Annul the locator to the component.
 999  CALL DAT_ANNUL( CMPLOC, STATUS )

      END
