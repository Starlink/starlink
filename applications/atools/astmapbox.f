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
*        dimension.
*     UBNDIN() = _DOUBLE (Read)
*        An array with one element for each Mapping input coordinate. This 
*        should contain the upper bound of the input box in each input 
*        dimension. Note that it is permissible for the upper bound to be 
*        less than the corresponding lower bound, as the values will simply
*        be swapped before use.
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

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-OCT-2002 (DSB):
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
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  External References:
      EXTERNAL AST_ISAMAPPING

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER THIS
      INTEGER NIN, NOUT, NAXIN, NAXOUT, COORDOUT, I
      LOGICAL FORWRD
      DOUBLE PRECISION LBNDIN( NDF__MXDIM ), UBNDIN( NDF__MXDIM )
      DOUBLE PRECISION XL( NDF__MXDIM ), XU( NDF__MXDIM ), LBNDOUT,
     :                 UBNDOUT

*.

*  Check inherited status.      
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Mapping.
      CALL ATL1_GTOBJ( 'THIS', 'Mapping', AST_ISAMAPPING, THIS,
     :                 STATUS )

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

*  Get the other parameters.
      CALL PAR_EXACD( 'LBNDIN', NAXIN, LBNDIN, STATUS ) 
      CALL PAR_EXACD( 'UBNDIN', NAXIN, UBNDIN, STATUS ) 
      CALL PAR_GDR0I( 'COORDOUT', 1, 1, NAXOUT, .FALSE., COORDOUT, 
     :                STATUS ) 

*  Find the bounding box.
      CALL AST_MAPBOX( THIS, LBNDIN, UBNDIN, FORWRD, COORDOUT, LBNDOUT,
     :                 UBNDOUT, XL, XU, STATUS ) 

*  Display the results.
      CALL MSG_BLANK( STATUS )
      CALL MSG_SETI( 'O', COORDOUT )
      CALL MSG_SETD( 'L', LBNDOUT )
      CALL MSG_SETD( 'H', UBNDOUT )
      CALL MSG_OUT( ' ', 'Output axis ^O varies between ^L and ^H '//
     :              'within the specified region of input space.', 
     :              STATUS )
      CALL PAR_PUT0D( 'LBNDOUT', LBNDOUT, STATUS ) 
      CALL PAR_PUT0D( 'UBNDOUT', UBNDOUT, STATUS ) 

      CALL MSG_BLANK( STATUS )
      DO I = 1, NAXOUT
         CALL MSG_SETD( 'XL', XL( I ) )
         IF( I .NE. NAXOUT ) CALL MSG_SETC( 'XL', ',' )
      END DO
      CALL MSG_OUT( ' ', 'The lowest output axis value was '//
     :              'attained at, for example, input position '//
     :              '(^XL).', STATUS )
      CALL PAR_PUT1D( 'XL', NAXOUT, XL, STATUS ) 

      CALL MSG_BLANK( STATUS )
      DO I = 1, NAXOUT
         CALL MSG_SETD( 'XU', XU( I ) )
         IF( I .NE. NAXOUT ) CALL MSG_SETC( 'XU', ',' )
      END DO
      CALL MSG_OUT( ' ', 'The highest output axis value was '//
     :              'attained at, for example, input position '//
     :              '(^XU).', STATUS )
      CALL PAR_PUT1D( 'XU', NAXOUT, XU, STATUS ) 
      CALL MSG_BLANK( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTMAPBOX_ERR', 'Error finding a bounding '//
     :                 'box for a Mapping.', STATUS )
      END IF

      END
