      SUBROUTINE ASTMATRIXMAP( STATUS )
*+
*  Name:
*     ASTMATRIXMAP

*  Purpose:
*     Create a MatrixMap.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTMATRIXMAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new MatrixMap and optionally initialises
*     its attributes. A MatrixMap is a form of Mapping which performs
*     a general linear transformation.  Each set of input coordinates,
*     regarded as a column-vector, are pre-multiplied by a matrix (whose
*     elements are specified when the MatrixMap is created) to give a new
*     column-vector containing the output coordinates. If appropriate,
*     the inverse transformation may also be performed.

*  Usage:
*     astmatrixmap nin nout form matrix options result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     FORM = INTEGER (Read)
*        An integer which indicates the form in which the matrix
*        elements will be supplied.
*
*        A value of zero indicates that a full NOUT x NIN  matrix
*        of values will be supplied via the MATRIX parameter
*        (below). In this case, the elements should be given in row
*        order (the elements of the first row, followed by the
*        elements of the second row, etc.).
*
*        A value of 1 indicates that only the diagonal elements of the
*        matrix will be supplied, and that all others should be
*        zero. In this case, the elements of MATRIX should contain
*        only the diagonal elements, stored consecutively.
*
*        A value of 2 indicates that a "unit" matrix is required, whose
*        diagonal elements are set to unity (with all other elements zero).
*        In this case, the MATRIX parameter is not used.
*     MATRIX() = _DOUBLE (Read)
*        The array of matrix elements to be used, stored according to
*        the value of FORM.
*     NIN = _INTEGER (Read)
*        The number of input coordinates (i.e. the number of columns in the
*        matrix).
*     NOUT = INTEGER (Read)
*        The number of output coordinates (i.e. the number of rows in the
*        matrix).
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute
*        assignments to be used for initialising the new MatrixMap.
*     RESULT = LITERAL (Read)
*        A text file to receive the new MatrixMap.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-FEB-2001 (DSB):
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

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER NEL
      INTEGER NIN
      INTEGER NOUT
      INTEGER FORM
      DOUBLE PRECISION MATRIX( NDF__MXDIM*NDF__MXDIM )
      INTEGER RESULT
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the number of input and output axes required.
      CALL PAR_GDR0I( 'NIN', 2, 1, NDF__MXDIM, .FALSE., NIN, STATUS )
      CALL PAR_GDR0I( 'NOUT', NIN, 1, NDF__MXDIM, .FALSE., NOUT,
     :                STATUS )

*  Get the matrix form.
      CALL PAR_GDR0I( 'FORM', 0, 0, 2, .FALSE., FORM, STATUS )

*  Determine the number of matrix elements required.
      IF( FORM .EQ. 0 ) THEN
         NEL = NOUT*NIN
      ELSE IF( FORM .EQ. 1 ) THEN
         NEL = MIN( NOUT, NIN )
      ELSE
         NEL = 0
      END IF

*  Get the matrix elements
      IF( NEL .GT. 0 ) THEN
         CALL PAR_EXACD( 'MATRIX', NEL, MATRIX, STATUS )
      END IF

*  Create the required MatrixMap.
      RESULT = AST_MATRIXMAP( NIN, NOUT, FORM, MATRIX, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTMATRIXMAP_ERR', 'Error creating a new '//
     :                 'MatrixMap.', STATUS )
      END IF

      END
