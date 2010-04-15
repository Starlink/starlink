      SUBROUTINE KPG1_CHAXD( INDF, DIM, DATAVL, SCALE, OFFSET, STATUS )
*+
*  Name:
*     KPG1_CHAXD

*  Purpose:
*     Check for usable AXIS structures

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_CHAXD( INDF, DIM, DATAVL, SCALE, OFFSET, STATUS )

*  Description:
*     This routine looks for monotonic AXIS structures within the two
*     specified axes of the supplied NDF. If both axes have such AXIS
*     structures, then a flag (DATAVL) is returned true. If either axis
*     is non-monotonic, a warning message is issued, DATAVL is returned
*     false, and a scale of 1.0 and offset of 0.0 are returned.
*
*     Each axis is then checked for linearity. If the axis is linear,
*     then the coresponding scale and offset of the linear mapping from
*     pixel to data coordinates are returned. Otherwise a warning
*     message is issued and the returned scale and offset refer to a
*     linear approximation to the axis coordinate system.

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the NDF.
*     DIM( 2 ) = INTEGER (Given)
*        The indices of the axes to be checked, in increasing order.
*     DATAVL = LOGICAL (Returned)
*        Returned true if the NDF contains monotonic AXIS structures
*        for both requested axes. Returned false otherwise.
*     SCALE( 2 ) = DOUBLE PRECISION (Returned)
*        The scale factors in the linear relationships between axis
*        coordinates and pixel coordinates. Returned equal to 1.0 if
*        DATVAL is returned false.
*     OFFSET( 2 ) = DOUBLE PRECISION (Returned)
*        The offsets in the linear relationships between axis
*        coordinates and pixel coordinates. Returned equal to 0.0 if
*        DATVAL is returned false.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  All AXIS values are treated as _DOUBLE values.
*     -  The returned values of SCALE and OFFSET are such that:
*
*        AXIS = SCALE( I )*PIXEL + OFFSET( I )
*
*        where PIXEL is a pixel coordinate for the I'th dimension listed
*        in array DIM (i.e. dimension DIM( I ) ), and DATA is the
*        corresponding axis coordinate.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-OCT-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants

*  Arguments Given:
      INTEGER INDF
      INTEGER DIM( 2 )

*  Arguments Returned:
      LOGICAL DATAVL
      DOUBLE PRECISION SCALE( 2 )
      DOUBLE PRECISION OFFSET( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION
     :        DALBND( NDF__MXDIM ),! AXIS lower bounds
     :        DAUBND( NDF__MXDIM ) ! AXIS upper bounds

      INTEGER
     :        AEL,               ! No. of elements in mapped axis array
     :        APNTR,             ! Pointer to mapped axis array
     :        AXIS,              ! Index of current dimension
     :        I,                 ! Dimension counter.
     :        LBND( NDF__MXDIM ),! Lower bounds of the NDF
     :        NDIM,              ! No. of dimensions in NDF
     :        UBND( NDF__MXDIM ) ! Upper bounds of the NDF

      LOGICAL
     :        MONOTO             ! Is the current axis monotonic?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if there is an axis coordinate system defined within the NDF.
      CALL NDF_STATE( INDF, 'Axis', DATAVL, STATUS )

*  Axis coordinate systems can only be used if they are monotonic.
*  Check each requested axis for monotonacity.
      IF( DATAVL ) THEN
         DO  I = 1, 2
            AXIS = DIM( I )

*  Map the axis centre values for this dimension as an array of _DOUBLE
*  values.
            CALL NDF_AMAP( INDF, 'Centre', AXIS, '_DOUBLE', 'READ',
     :                     APNTR, AEL, STATUS )
            IF( STATUS .EQ. SAI__OK ) THEN

*  Start a new error context so that any error reports concerning a
*  non-monotonic axis may be annulled. Instead a warning message will
*  be issued so that the application can continue using world
*  co-ordinates.
               CALL ERR_MARK
               CALL KPG1_MONOD( .TRUE., AEL, %VAL( APNTR ), MONOTO,
     :                          STATUS )

               IF( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL( STATUS )
                  MONOTO = .FALSE.
               END IF

               CALL ERR_RLSE

*  Issue a warning message.
               IF( .NOT. MONOTO ) THEN
                  CALL MSG_SETI( 'AX', AXIS )
                  CALL NDF_MSG( 'NDF', INDF )
                  CALL MSG_OUT( 'KPG1_CHAXD_NOTMONO', 'Axis ^AX of '//
     :                          '^NDF is not monotonic and therefore '//
     :                          'cannot be used for annotation or '//
     :                          'stored in the graphics database.',
     :                          STATUS )

*  Flag that the axis coordinates cannot be used.
                  DATAVL = .FALSE.

               END IF

            END IF

*  Unmap the axis centre array.
            CALL NDF_AUNMP( INDF, 'Centre', AXIS, STATUS )

         END DO

      END IF

*  If we still have usable data coordinates, check that they are
*  linear. The linear transformation giving data coordinates in terms
*  of world coordinates is returned if both axes are linear. Warning
*  messages are issued for each non-linear axis.
      IF( DATAVL ) THEN
         CALL KPG1_DCLID( 2, INDF, DALBND, DAUBND, SCALE, OFFSET,
     :                    DATAVL, STATUS )

*  Force the axes to be considered linear. The returned values of SCALE
*  and offset will describe a linear approximation to the AXIS values.
         IF( .NOT. DATAVL ) THEN
            DATAVL = .TRUE.
            CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

            DO I = 1, 2
               AXIS = DIM( I )
               SCALE( I ) = ( DAUBND( I ) - DALBND( I ) )/
     :                       DBLE( UBND( AXIS ) - LBND( AXIS ) + 1 )
               OFFSET( I ) = DAUBND( I ) - SCALE( I )*
     :                                     DBLE( UBND( AXIS ) )
            END DO

         END IF

      END IF

*  If no usable data coordinates are available, return a unit
*  tranformation.
      IF( .NOT. DATAVL ) THEN

         DO I = 1, 2
            SCALE( I ) = 1.0
            OFFSET( I ) = 0.0
         END DO

      END IF

      END
