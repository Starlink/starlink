      SUBROUTINE ASTTRANGRID( STATUS )
*+
*  Name:
*     ASTTRANGRID

*  Purpose:
*     Transform a grid of positions

*  Language:
*     Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTTRANGRID( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application uses a supplied Mapping to transforms the
*     pixel coordinates at the centre of every pixel in a grid with
*     specified pixel index bounds. The resulting coordinate values are
*     stored in an output NDF with the same pixel bounds. This output NDF
*     has an extra trailing pixel axis that enumerates the outputs from
*     the Mapping. This extra axis has pixel index bounds "1:Nout", where
*     "Nout" is the number of axis values produced by the supplied Mapping.

*     Efficiency is improved by first approximating the Mapping with a linear
*     transformation applied over the whole region of the input grid which
*     is being used. If this proves to be insufficiently accurate, the
*     input region is sub-divided into two along its largest dimension and
*     the process is repeated within each of the resulting sub-regions.
*     This process of sub-division continues until a sufficiently good
*     linear approximation is found, or the region to which it is being
*     applied becomes too small (in which case the original Mapping is
*     used directly).

*  Usage:
*     asttrangrid this lbnd ubnd tol maxpix forward result

*  ADAM Parameters:
*     LBND() = _INTEGER (Read)
*        The lower pixel index bounds of the output NDF. The number of
*        values supplied should equal the Nin attribute of the supplied
*        Mapping.
*     FORWARD = _LOGICAL (Read)
*        A TRUE value indicates that the Mapping's forward coordinate
*        transformation is to be applied, while a FALSE value indicates
*        that the inverse transformation should be used. [TRUE]
*     MAXPIX = _INTEGER (Read)
*        A value which specifies an initial scale size (in input grid
*        points) for the adaptive algorithm which approximates non-linear
*        Mappings with piece-wise linear transformations. Normally, this
*        should be a large value (larger than any dimension of the region
*        of the input grid being used). In this case, a first attempt to
*        approximate the Mapping by a linear transformation will be made
*        over the entire input region.
*
*        If a smaller value is used, the input region will first be divided
*        into sub-regions whose size does not exceed "maxpix" grid points
*        in any dimension. Only at this point will attempts at
*        approximation commence.
*
*        This value may occasionally be useful in preventing false
*        convergence of the adaptive algorithm in cases where the Mapping
*        appears approximately linear on large scales, but has
*        irregularities (e.g. holes) on smaller scales. A value of, say, 50
*        to 100 grid points can also be employed as a safeguard in
*        general-purpose software, since the effect on performance is
*        minimal.
*
*        If too small a value is given, it will have the effect of
*        inhibiting linear approximation altogether (equivalent to setting
*        "tol" to zero). Although this may degrade performance, accurate
*        results will still be obtained. [100]
*     RESULT = NDF (Write)
*        The output NDF. This will have "Nin+1" pixel axes, where "Nin"
*        is the number of inputs for the supplied Mapping. The extra
*        pixel axis in the output NDF will have bounds "1:Nout", where
*        "Nout" is the number of outputs for the supplied Mapping. The
*        bounds on the first Nin axes of the output NDF are given by LBND
*        and UBND.
*     THIS = LITERAL (Read)
*        An NDF or text file holding the Mapping. If an NDF is supplied,
*        the Mapping from the base Frame of the WCS FrameSet to the
*        current Frame will be used. The inputs of this Mapping
*        correspond to pixel coordinates within the output NDF.
*     TOL = _DOUBLE (Read)
*        The maximum tolerable geometrical distortion which may be
*        introduced as a result of approximating non-linear Mappings by a
*        set of piece-wise linear transformations. This should be
*        expressed as a displacement within the output coordinate system
*        of the Mapping.
*
*        If piece-wise linear approximation is not required, a value of zero
*        may be given. This will ensure that the Mapping is used without any
*        approximation, but may increase execution time.
*
*        If the value is too high, discontinuities between the linear
*        approximations used in adjacent panel will be higher. If this is a
*        problem, reduce the tolerance value used. [0.0]
*     UBND() = _INTEGER (Read)
*        The upper pixel index bounds of the output NDF. The number of
*        values supplied should equal the Nin attribute of the supplied
*        Mapping.

*  Notes:
*     - The supplied Mapping inputs correspond to pixel coordinates in
*     the grid. Particularly, this means that integer values are located
*     at pixel corners. But astTranGrid assumes an input coordinate system
*     in which integer values are located at pixel centres. Therefore,
*     the supplied Mapping is modified before passing it to astTranGrid.
*     This modification consists of prepending a Shiftmap that shifts each
*     input axis value by -0.5.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: David S. Berry (Starlink, UCLan)
*     {enter_new_authors_here}

*  History:
*     25-FEB-2010 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type definitions
      IMPLICIT NONE             ! No default typing

*  Global includes
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'NDF_PAR'         ! NDF constants
      INCLUDE 'AST_PAR'         ! AST constants and function declarations
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function

*  External references
      EXTERNAL AST_ISAMAPPING

*  Status
      INTEGER STATUS

*  Local variables
      LOGICAL FORWRD
      INTEGER LBND( NDF__MXDIM ), UBND( NDF__MXDIM )
      DOUBLE PRECISION TOL, SHIFT( NDF__MXDIM )
      INTEGER NIN, NOUT, THIS, MAXPIX, INDF, IP, EL, SM, I, MAP
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin AST and NDF contexts
      CALL AST_BEGIN( STATUS )
      CALL NDF_BEGIN

*  Get the Mapping, and its Nout and Nin attributes.
      CALL KPG1_GTOBJ( 'THIS', 'Mapping', AST_ISAMAPPING, THIS,
     :                 STATUS )

*  Get the other scalar parameters.
      CALL PAR_GET0L( 'FORWARD', FORWRD, STATUS )
      CALL PAR_GET0I( 'MAXPIX', MAXPIX, STATUS )
      CALL PAR_GET0D( 'TOL', TOL, STATUS )

*  If required, invert the Mapping.
      IF( .NOT. FORWRD ) CALL AST_INVERT( THIS, STATUS )

*  Get the Mapping, and its Nout and Nin attributes.
      NOUT = AST_GETI( THIS, 'Nout', STATUS )
      NIN = AST_GETI( THIS, 'Nin', STATUS )

*  Check they are usable.
      IF( ( NIN .GE. NDF__MXDIM .OR. NOUT .GE. NDF__MXDIM )
     :    .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NI', NIN )
         CALL MSG_SETI( 'NO', NOUT )
         CALL MSG_SETI( 'NC', NDF__MXDIM - 1 )
         IF( FORWRD ) THEN
            CALL ERR_REP( 'ASTTRANGRID_ERR1', 'The supplied Mapping '//
     :             'has ^NI input axes and ^NO output axes, but '//
     :             'the maximum allowed number of axes is ^NC', STATUS)
         ELSE
            CALL ERR_REP( 'ASTTRANGRID_ERR1', 'The supplied Mapping '//
     :             'has ^NO input axes and ^NI output axes, but '//
     :             'the maximum allowed number of axes is ^NC', STATUS)
         END IF

      END IF

*  Get the lower pixel bounds for the first NIN axes of the output NDF.
      CALL PAR_EXACI( 'LBND', NIN, LBND, STATUS )

*  Get the upper pixel bounds for the first NIN axes of the output NDF.
      CALL PAR_EXACI( 'UBND', NIN, UBND, STATUS )

*  Complete the bounds of the output NDF.
      LBND( NIN + 1 ) = 1
      UBND( NIN + 1 ) = NOUT

*  Create the output NDF.
      CALL NDF_CREAT( 'RESULT', '_DOUBLE', NIN + 1, LBND, UBND, INDF,
     :                STATUS )

*  Map it.
      CALL NDF_MAP( INDF, 'DATA', '_DOUBLE', 'WRITE', IP, EL, STATUS )

*  AST_TRANGRID uses a coordinate system in which integral values are at
*  the centre of the grid elements. Since we have advertised that the
*  inputs to THIS are "pixel coordinates", we need to shift the values by
*  0.5 of a pixel to put the integral value at the pixel centre as
*  required by AST_TRANGRID.
      DO I = 1, NIN
         SHIFT( I ) = -0.5D0
      END DO
      SM = AST_SHIFTMAP( NIN, SHIFT, ' ', STATUS )
      MAP = AST_CMPMAP( SM, THIS, .TRUE., ' ', STATUS )

*  Find the total number of points in the grid.
      EL = 1
      DO I = 1, NIN
         EL = EL*( UBND( I ) - LBND( I ) + 1 )
      END DO

*  Fill it with the required coordinate values.
      CALL AST_TRANGRID( MAP, NIN, LBND, UBND, TOL, MAXPIX, .TRUE.,
     :                   NOUT, EL, %VAL(CNF_PVAL(IP)), STATUS )

* End the AST and NDF contexts
      CALL NDF_END( STATUS )
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTTRANGRID_ERR', 'Error transforming a grid '//
     :                 'of positions.', STATUS )
      END IF

      END
