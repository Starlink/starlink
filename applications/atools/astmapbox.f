      SUBROUTINE ASTMAPBOX( STATUS )
*+
*  Name:
*     ASTMAPBOX

*  Purpose:
*     Find a bounding box for a Mapping.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTMAPBOX( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application allows you to find the "bounding box" which just
*     encloses another box after it has been transformed by a Mapping
*     (using either its forward or inverse transformation). A typical use
*     might be to calculate the size of an image after being transformed
*     by a Mapping.
*
*     The routine works on one dimension at a time. When supplied with the
*     lower and upper bounds of a rectangular region (box) of input
*     coordinate space, it finds the lowest and highest values taken by a
*     nominated output coordinate within that region. It also returns the
*     input coordinates where these bounding values are attained. It
*     should be used repeatedly to obtain the extent of the bounding box
*     in more than one dimension.

*  Usage:
*     astmapbox this lbndin ubndin forward coordout

*  ADAM Parameters:
*     THIS = LITERAL (Read)
*        An NDF or text file holding the Mapping. If an NDF is supplied,
*        the Mapping from the base Frame of the WCS FrameSet to the
*        current Frame will be used.
*     LBNDIN() = _DOUBLE (Read)
*        An array with one element for each Mapping input coordinate. This
*        should contain the lower bound of the input box in each input
*        dimension. If an NDF was supplied for THIS and FORWARD is true, then
*        a null (!) value can be supplied in which case a default will be used
*        corresponding to the GRID cordinates of the bottom left corner of the
*        bottom left pixel in the NDF (i.e. a value of 0.5 on every grid axis).
*     UBNDIN() = _DOUBLE (Read)
*        An array with one element for each Mapping input coordinate. This
*        should contain the upper bound of the input box in each input
*        dimension. Note that it is permissible for the upper bound to be
*        less than the corresponding lower bound, as the values will simply
*        be swapped before use. If an NDF was supplied for THIS and FORWARD is
*        true, then a null (!) value can be supplied in which case a default
*        will be used corresponding to the GRID cordinates of the top right
*        corner of the top right pixel in the NDF (i.e. a value of (DIM+0.5)
*        on every grid axis, where DIM is the number of pixels along the axis).
*     FORWARD = _LOGICAL (Read)
*        If this value is TRUE, then the Mapping's forward
*        transformation will be used to transform the input
*        box. Otherwise, its inverse transformation will be used.
*
*        (If the inverse transformation is selected, then references
*        to "input" and "output" coordinates in this description
*        should be transposed. For example, the size of the LBNDIN
*        and UBNDIN arrays should match the number of output
*        coordinates, as given by the Mapping's Nout attribute.
*        Similarly, the COORDOUT argument, below, should nominate one
*        of the Mapping's input coordinates.)
*     COORDOUT = _INTEGER (Read)
*        The index of the output coordinate for which the lower and
*        upper bounds are required. This value should be at least one,
*        and no larger than the number of Mapping output coordinates.
*     LBNDOUT = _DOUBLE (Write)
*        The lowest value taken by the nominated output coordinate
*        within the specified region of input coordinate space.
*     UBNDOUT = _DOUBLE (Write)
*        The highest value taken by the nominated output coordinate
*        within the specified region of input coordinate space.
*     XL() = _DOUBLE (Write)
*        An array with one element for each Mapping input
*        coordinate. This will return the coordinates of an input
*        point (although not necessarily a unique one) for which the
*        nominated output coordinate attains the lower bound value
*        returned in LBNDOUT.
*     XU() = _DOUBLE (Write)
*        An array with one element for each Mapping input
*        coordinate. This will return the coordinates of an input
*        point (although not necessarily a unique one) for which the
*        nominated output coordinate attains the upper bound value
*        returned in UBNDOUT.

*  Copyright:
*     Copyright (C) 2002, 2004 Central Laboratory of the Research
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-OCT-2002 (DSB):
*        Original version.
*     30-SEP-2004 (DSB):
*        Modified to use NDF GRID bounds as input bounds if an NDF is
*        supplied. Also show axis label with results (if available).
*     23-MAY-2007 (DSB):
*        Correct dimensionality of displayed XU and XL values.
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
      INTEGER NIN, NOUT, NAXIN, NAXOUT, COORDOUT, I, DIMS( NDF__MXDIM ),
     :        NDIM, INDF, IAT, INFRM, OUTFRM, IAST
      LOGICAL FORWRD
      CHARACTER ATTR*20
      DOUBLE PRECISION LBNDIN( NDF__MXDIM ), UBNDIN( NDF__MXDIM )
      DOUBLE PRECISION XL( NDF__MXDIM ), XU( NDF__MXDIM ), LBNDOUT,
     :                 UBNDOUT
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

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

      IF( ( NIN .GT. NDF__MXDIM .OR. NOUT .GT. NDF__MXDIM )
     :    .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NI', NIN )
         CALL MSG_SETI( 'NO', NOUT )
         CALL MSG_SETI( 'NC', NDF__MXDIM )
         CALL ERR_REP( 'ASTMAPBOX_ERR1', 'The supplied Mapping has '//
     :                 '^NI input axes and ^NO output axes, but the '//
     :                 'maximum allowed number of axes is ^NC', STATUS)
      END IF

*  Get the FORWARD parameter.
      CALL PAR_GET0L( 'FORWARD', FORWRD, STATUS )

*  Store the number of axes in the input positions and the number in the
*  output positions.
      IF( FORWRD ) THEN
         NAXIN = NIN
         NAXOUT = NOUT
      ELSE
         NAXIN = NOUT
         NAXOUT = NIN
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the lower input limits. Use a default of (0.5,0.5,...) if a null
*  value is supplied and the forward transformation is being used and the
*  Mapping was specified via an NDF (d1,d2,...) are the pixel dimension.
      CALL PAR_EXACD( 'LBNDIN', NAXIN, LBNDIN, STATUS )
      IF( STATUS .EQ. PAR__NULL .AND. FORWRD .AND.
     :    INDF .NE. NDF__NOID ) THEN
         CALL ERR_ANNUL( STATUS )

         DO I = 1, NIN
            LBNDIN( I ) = 0.5
         END DO

      END IF

*  Get the upper input limits. Use a default of (d1+0.5,d2+0.5,...) if a null
*  value is supplied and the forward transformation is being used and the
*  Mapping was specified via an NDF (d1,d2,...) are the pixel dimension.
      CALL PAR_EXACD( 'UBNDIN', NAXIN, UBNDIN, STATUS )
      IF( STATUS .EQ. PAR__NULL .AND. FORWRD .AND.
     :    INDF .NE. NDF__NOID ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL NDF_DIM( INDF, NIN, DIMS, NDIM, STATUS )
         DO I = 1, NDIM
            UBNDIN( I ) = DIMS( I ) + 0.5
         END DO
      END IF

*  Get the other parameters.
      CALL PAR_GDR0I( 'COORDOUT', 1, 1, NAXOUT, .FALSE., COORDOUT,
     :                STATUS )

*  Find the bounding box.
      CALL AST_MAPBOX( THIS, LBNDIN, UBNDIN, FORWRD, COORDOUT, LBNDOUT,
     :                 UBNDOUT, XL, XU, STATUS )

*  If an NDF was suplied get its current and base Frames. Swap them if the
*  inverse Mapping was used.
      IF( INDF .NE. NDF__NOID ) THEN
         CALL KPG1_GTWCS( INDF, IAST, STATUS )
         IF( FORWRD ) THEN
            INFRM = AST_GETFRAME( IAST, AST__BASE, STATUS )
            OUTFRM = AST_GETFRAME( IAST, AST__CURRENT, STATUS )
         ELSE
            OUTFRM = AST_GETFRAME( IAST, AST__BASE, STATUS )
            INFRM = AST_GETFRAME( IAST, AST__CURRENT, STATUS )
         END IF

      ELSE
         INFRM = AST__NULL
         OUTFRM = AST__NULL
      END IF

*  Display the results.
      CALL MSG_BLANK( STATUS )
      IF( OUTFRM .NE. AST__NULL ) THEN
         ATTR = 'LABEL('
         IAT = 6
         CALL CHR_PUTI( COORDOUT, ATTR, IAT )
         CALL CHR_APPND( ')', ATTR, IAT )

         CALL MSG_SETC( 'LB', ' (' )
         CALL MSG_SETC( 'LB', AST_GETC( OUTFRM, ATTR( : IAT ),
     :                                  STATUS ) )
         CALL MSG_SETC( 'LB', ')' )
      ELSE
         CALL MSG_SETC( 'LB', ' ' )
      END IF

      CALL MSG_SETI( 'O', COORDOUT )
      CALL MSG_SETD( 'L', LBNDOUT )
      CALL MSG_SETD( 'H', UBNDOUT )
      CALL MSG_OUT( ' ', 'Output axis ^O^LB varies between ^L and ^H'//
     :              ' within the specified region of input space.',
     :              STATUS )
      CALL PAR_PUT0D( 'LBNDOUT', LBNDOUT, STATUS )
      CALL PAR_PUT0D( 'UBNDOUT', UBNDOUT, STATUS )

      CALL MSG_BLANK( STATUS )
      DO I = 1, NAXIN
         CALL MSG_SETD( 'XL', XL( I ) )
         IF( I .NE. NAXIN ) CALL MSG_SETC( 'XL', ',' )
      END DO

      IF( INFRM .NE. AST__NULL ) THEN
         CALL MSG_SETC( 'D', AST_GETC( INFRM, 'DOMAIN', STATUS ) )
         CALL MSG_OUT( ' ', 'The lowest output axis value was '//
     :              'attained at, for example, input ^D position '//
     :              '(^XL).', STATUS )
      ELSE
         CALL MSG_OUT( ' ', 'The lowest output axis value was '//
     :              'attained at, for example, input position '//
     :              '(^XL).', STATUS )
      END IF

      CALL PAR_PUT1D( 'XL', NAXOUT, XL, STATUS )

      CALL MSG_BLANK( STATUS )
      DO I = 1, NAXIN
         CALL MSG_SETD( 'XU', XU( I ) )
         IF( I .NE. NAXIN ) CALL MSG_SETC( 'XU', ',' )
      END DO

      IF( INFRM .NE. AST__NULL ) THEN
         CALL MSG_SETC( 'D', AST_GETC( INFRM, 'DOMAIN', STATUS ) )
         CALL MSG_OUT( ' ', 'The highest output axis value was '//
     :              'attained at, for example, input ^D position '//
     :              '(^XU).', STATUS )
      ELSE
         CALL MSG_OUT( ' ', 'The highest output axis value was '//
     :              'attained at, for example, input position '//
     :              '(^XU).', STATUS )
      END IF

      CALL PAR_PUT1D( 'XU', NAXOUT, XU, STATUS )
      CALL MSG_BLANK( STATUS )

*  Tidy up.
 999  CONTINUE

* Free the NDF if one was used.
      IF( INDF .NE. NDF__NOID ) CALL NDF_ANNUL( INDF, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTMAPBOX_ERR', 'Error finding a bounding '//
     :                 'box for a Mapping.', STATUS )
      END IF

      END
