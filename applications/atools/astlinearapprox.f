      SUBROUTINE ASTLINEARAPPROX( STATUS )
*+
*  Name:
*     ASTLINEARAPPROX

*  Purpose:
*     Find a linear approximation to a mapping

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTLINEARAPPROX( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application allows you to find a linear approximation
*     to a mapping over a region in the base coordinates. A typical use
*     might be to calculate the orientation and scale of an image after
*     being transformed by a Mapping.

*  Usage:
*     astlinearapprox this lbndin ubndin fit

*  ADAM Parameters:
*     FIT() = _DOUBLE (Write)
*        An array returning the co-efficients of the linear approximation
*        to the specified transformation. The first Nout elements hold
*        the constant offsets for the transformation outputs. The remaining
*        elements hold the gradients. So if the Mapping has 2 inputs and 2
*        outputs the linear approximation to the forward transformation is:
*
*           X_out = fit[0] + fit[2]*X_in + fit[3]*Y_in
*           Y_out = fit[1] + fit[4]*X_in + fit[5]*Y_in
*
*     THIS = LITERAL (Read)
*        An NDF or text file holding the Mapping. If an NDF is supplied,
*        the Mapping from the base Frame of the WCS FrameSet to the
*        current Frame will be used.
*     LBNDIN() = _DOUBLE (Read)
*        An array with one element for each Mapping input coordinate. This
*        should contain the lower bound of the input region in each input
*        dimension. If an NDF was supplied for THIS and FORWARD is true, then
*        a null (!) value can be supplied in which case a default will be used
*        corresponding to the GRID cordinates of the bottom left corner of the
*        bottom left pixel in the NDF (i.e. a value of 0.5 on every grid axis).
*     UBNDIN() = _DOUBLE (Read)
*        An array with one element for each Mapping input coordinate. This
*        should contain the upper bound of the input region in each input
*        dimension. Note that it is permissible for the upper bound to be
*        less than the corresponding lower bound, as the values will simply
*        be swapped before use. If an NDF was supplied for THIS and FORWARD is
*        true, then a null (!) value can be supplied in which case a default
*        will be used corresponding to the GRID cordinates of the top right
*        corner of the top right pixel in the NDF (i.e. a value of (DIM+0.5)
*        on every grid axis, where DIM is the number of pixels along the axis).
*     TOL = _DOUBLE (Read)
*        The maximum permitted deviation from linearity, expressed as a
*        positive Cartesian displacement in the output coordinate space of
*        the Mapping. If a linear fit to the forward transformation of the
*        Mapping deviates from the true transformation by more than this
*        amount at any point which is tested, then no fit coefficients will
*        be returned.

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council
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
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     01-MAY-2009 (PWD):
*        Original version, based on AST_MAPBOX
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
      INTEGER NIN, NOUT, I, DIMS( NDF__MXDIM ),
     :        NDIM, INDF, INFRM, OUTFRM, IAST
      CHARACTER ATTR*20
      DOUBLE PRECISION LBNDIN( NDF__MXDIM ), UBNDIN( NDF__MXDIM )
      DOUBLE PRECISION FIT( ( NDF__MXDIM + 1 ) * NDF__MXDIM )
      DOUBLE PRECISION TOL
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

      IF ( ( NIN .GT. NDF__MXDIM .OR. NOUT .GT. NDF__MXDIM )
     :    .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NI', NIN )
         CALL MSG_SETI( 'NO', NOUT )
         CALL MSG_SETI( 'NC', NDF__MXDIM )
         CALL ERR_REP( 'ASTLINEARAPPROX_ERR1', 'The supplied '//
     :                 'Mapping has ^NI input axes and ^NO output'//
     :                 'axes, but the maximum allowed number of '//
     :                 'axes is ^NC', STATUS)
      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the lower input limits. Use a default of (0.5,0.5,...) if a null
*  value is supplied and the Mapping was specified via an NDF (d1,d2,...)
*  are the pixel dimension.
      CALL PAR_EXACD( 'LBNDIN', NIN, LBNDIN, STATUS )
      IF ( STATUS .EQ. PAR__NULL .AND. INDF .NE. NDF__NOID ) THEN
         CALL ERR_ANNUL( STATUS )

         DO I = 1, NIN
            LBNDIN( I ) = 0.5
         END DO

      END IF

*  Get the upper input limits. Use a default of (d1+0.5,d2+0.5,...) if a null
*  value is supplied and the Mapping was specified via an NDF (d1,d2,...)
*  are the pixel dimension.
      CALL PAR_EXACD( 'UBNDIN', NIN, UBNDIN, STATUS )
      IF ( STATUS .EQ. PAR__NULL .AND. INDF .NE. NDF__NOID ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL NDF_DIM( INDF, NIN, DIMS, NDIM, STATUS )
         DO I = 1, NDIM
            UBNDIN( I ) = DIMS( I ) + 0.5
         END DO
      END IF

*  Get the TOL parameter.
      CALL PAR_GET0D( 'TOL', TOL, STATUS )

*  Find the bounding box.
      IF ( AST_LINEARAPPROX( THIS, LBNDIN, UBNDIN, TOL, FIT, STATUS ) )
     :   THEN

*  Display the fit.
         CALL MSG_BLANK( STATUS )
         DO I = 1, ( NIN + 1 ) * NOUT
            CALL MSG_SETD( 'FIT', FIT( I ) )
            CALL MSG_SETI( 'IND', I - 1 )
            CALL MSG_OUT( ' ', '  FIT(^IND) = ^FIT', STATUS )
         END DO
         CALL MSG_BLANK( STATUS )

*  Output the fit.
         CALL PAR_PUT1D( 'FIT', ( NIN + 1 ) * NOUT, FIT, STATUS )
      ELSE
         IF ( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
         END IF
      ENDIF

*  Tidy up.
 999  CONTINUE

* Free the NDF if one was used.
      IF( INDF .NE. NDF__NOID ) CALL NDF_ANNUL( INDF, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTLINEARAPPROX_ERR', 'Error finding a'//
     :                 ' linear approximation to a mapping',
     :                 STATUS )
      END IF

      END

