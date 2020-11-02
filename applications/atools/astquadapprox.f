      SUBROUTINE ASTQUADAPPROX( STATUS )
*+
*  Name:
*     ASTQUADAPPROX

*  Purpose:
*     Obtain a quadratic approximation to a 2D Mapping.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTQUADAPPROX( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application returns the co-efficients of a quadratic fit to the
*     supplied Mapping over the input area specified by LBND and UBND.
*     The Mapping must have 2 inputs, but may have any number of outputs.
*     The i'th Mapping output is modelled as a quadratic function of the
*     2 inputs (x,y):
*
*     output_i = a_i_0 + a_i_1*x + a_i_2*y + a_i_3*x*y + a_i_4*x*x +
*                a_i_5*y*y
*
*     The FIT array is returned holding the values of the co-efficients
*     a_0_0, a_0_1, etc.

*  Usage:
*     astquadapprox this lbnd ubnd nx ny

*  ADAM Parameters:
*     FIT() = _DOUBLE (Write)
*        An array returning the co-efficients of the quadratic approximation
*        to the specified transformation.  The first 6 elements hold the
*        fit to the first Mapping output. The next 6 elements hold the
*        fit to the second Mapping output, etc. So if the Mapping has 2
*        inputs and 2 outputs the quadratic approximation to the forward
*        transformation is:
*
*        X_out = fit(1) + fit(2)*X_in + fit(3)*Y_in + fit(4)*X_in*Y_in +
*                fit(5)*X_in*X_in + fit(6)*Y_in*Y_in
*
*        Y_out = fit(7) + fit(8)*X_in + fit(9)*Y_in + fit(10)*X_in*Y_in +
*                fit(11)*X_in*X_in + fit(12)*Y_in*Y_in
*
*     THIS = LITERAL (Read)
*        An NDF or text file holding the Mapping. If an NDF is supplied,
*        the Mapping from the base Frame of the WCS FrameSet to the
*        current Frame will be used. The Mapping must have 2 inputs.
*     LBND( 2 ) = _DOUBLE (Read)
*        A two element array containing the lower bound of the input region
*        in each input dimension. If an NDF was supplied for THIS, then a
*        null (!) value can be supplied in which case a
*        default will be used corresponding to the GRID cordinates of the
*        bottom left corner of the bottom left pixel in the NDF (i.e. a
*        value of 0.5 on every grid axis).
*     NX = _INTEGER (Read)
*        The number of points to place along the first Mapping input. The
*        first point is at LBND( 1 ) and the last is at UBND( 1 ). If a
*        value less than three is supplied a value of three will be used.
*        If an NDF was supplied for THIS, then a null
*        (!) value can be supplied in which case a default will be used
*        corresponding to the number of pixels along the axis.
*     NY = _INTEGER (Read)
*        The number of points to place along the second Mapping input. The
*        first point is at LBND( 2 ) and the last is at UBND( 2 ). If a
*        value less than three is supplied a value of three will be used.
*        If an NDF was supplied for THIS, then a null
*        (!) value can be supplied in which case a default will be used
*        corresponding to the number of pixels along the axis.
*     RMS = _DOUBLE (Write)
*        The RMS residual between the mapping and the fit, taken over all
*        outputs.
*     UBND( 2 ) = _DOUBLE (Read)
*        A two element array containing the upper bound of the input region
*        in each input dimension. If an NDF was supplied for THIS,
*        then a null (!) value can be supplied in which case a
*        default will be used corresponding to the GRID cordinates of the
*        top right corner of the top right pixel in the NDF (i.e. a value of
*        (DIM+0.5) on every grid axis, where DIM is the number of pixels
*        along the axis).

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     26-FEB-2010 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PAR_ERR'          ! Parameter system error constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  External References:
      EXTERNAL AST_ISAMAPPING

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER THIS
      INTEGER NIN, NOUT, I, DIMS( NDF__MXDIM ), NX, NY,
     :        NDIM, INDF, INFRM, OUTFRM, IAST
      CHARACTER ATTR*20
      DOUBLE PRECISION LBND( NDF__MXDIM ), UBND( NDF__MXDIM )
      DOUBLE PRECISION FIT( 6*NDF__MXDIM ), RMS

*.

*  Check inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Mapping.
      CALL KPG1_GTOBJ( 'THIS', 'Mapping', AST_ISAMAPPING, THIS,
     :                 STATUS )

*  See if an NDF was used to specify the Mapping, by attempting to access the
*  parameter as an NDF. INDF will be returned set to NDF__NOID (without error)
*  if the Mapping was not supplied va an NDF.
      CALL NDF_EXIST( 'THIS', 'READ', INDF, STATUS )

*  Determine the Nin and Nout attributes of the Mapping
      NIN = AST_GETI( THIS, 'Nin', STATUS)
      NOUT = AST_GETI( THIS, 'Nout', STATUS)

      IF ( NIN .NE. 2 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NI', NIN )
         CALL ERR_REP( 'ASTQUADAPPROX_ERR1', 'The supplied '//
     :                 'Mapping has ^NI input axes, but the only '//
     :                 'allowed value is 2.', STATUS)
      END IF

      IF ( NOUT .GT. NDF__MXDIM .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NO', NOUT )
         CALL MSG_SETI( 'NC', NDF__MXDIM )
         CALL ERR_REP( 'ASTQUADAPPROX_ERR1', 'The supplied '//
     :                 'Mapping has ^NO output'//
     :                 'axes, but the maximum allowed number of '//
     :                 'axes is ^NC', STATUS)
      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the lower input limits. Use a default of (0.5,0.5,...) if a null
*  value is supplied and the Mapping was specified via an NDF (d1,d2,...)
*  are the pixel dimension.
      CALL PAR_EXACD( 'LBND', NIN, LBND, STATUS )
      IF ( STATUS .EQ. PAR__NULL .AND. INDF .NE. NDF__NOID ) THEN
         CALL ERR_ANNUL( STATUS )

         DO I = 1, NIN
            LBND( I ) = 0.5
         END DO

      END IF

*  Get the upper input limits. Use a default of (d1+0.5,d2+0.5,...) if a null
*  value is supplied and the Mapping was specified via an NDF (d1,d2,...)
*  are the pixel dimension.
      CALL PAR_EXACD( 'UBND', NIN, UBND, STATUS )
      IF ( STATUS .EQ. PAR__NULL .AND. INDF .NE. NDF__NOID ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL NDF_DIM( INDF, NIN, DIMS, NDIM, STATUS )
         DO I = 1, NDIM
            UBND( I ) = DIMS( I ) + 0.5
         END DO
      END IF

*  Get the NX and NY parameters.
      CALL PAR_GET0I( 'NX', NX, STATUS )
      CALL PAR_GET0I( 'NY', NY, STATUS )

*  Find the quadratic fit.
      IF ( AST_QUADAPPROX( THIS, LBND, UBND, NX, NY, FIT, RMS,
     :                     STATUS ) ) THEN

*  Display the fit and RMS.
         CALL MSG_BLANK( STATUS )
         DO I = 1, 6 * NOUT
            CALL MSG_SETD( 'FIT', FIT( I ) )
            CALL MSG_SETI( 'IND', I )
            CALL MSG_OUT( ' ', '  FIT(^IND) = ^FIT', STATUS )
         END DO
         CALL MSG_BLANK( STATUS )

         CALL MSG_SETD( 'RMS', RMS )
         CALL MSG_OUT( ' ', '  RMS = ^RMS', STATUS )
         CALL MSG_BLANK( STATUS )

*  Output the fit and rms.
         CALL PAR_PUT1D( 'FIT', 6 * NOUT, FIT, STATUS )
         CALL PAR_PUT0D( 'RMS', RMS, STATUS )

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'The least squares quadratic fit could '//
     :                 'not be determined.', STATUS )
      ENDIF

*  Tidy up.
 999  CONTINUE

* Free the NDF if one was used.
      IF( INDF .NE. NDF__NOID ) CALL NDF_ANNUL( INDF, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTQUADAPPROX_ERR', 'Error finding a'//
     :                 ' quadratic approximation to a mapping',
     :                 STATUS )
      END IF

      END

