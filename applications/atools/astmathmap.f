      SUBROUTINE ASTMATHMAP( STATUS )
*+
*  Name:
*     ASTMATHMAP

*  Purpose:
*     Create a MathMap.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTMATHMAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new MathMap and optionally initialises
*     its attributes.
*
*     A MathMap is a Mapping which allows you to specify a set of forward
*     and/or inverse transformation functions using arithmetic operations
*     and mathematical functions similar to those available in Fortran. The
*     MathMap interprets these functions at run-time, whenever its forward
*     or inverse transformation is required. Because the functions are not
*     compiled in the normal sense (unlike an IntraMap), they may be used to
*     describe coordinate transformations in a transportable manner. A
*     MathMap therefore provides a flexible way of defining new types of
*     Mapping whose descriptions may be stored as part of a dataset and
*     interpreted by other programs.
*
*     See the reference documentation for the AstMathMap constructor in
*     SUN/210 for a complete description of the syntax of the transformation
*     functions.

*  Usage:
*     astmathmap nin nout fwd inv options result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     FWD = LITERAL (Read)
*        A group expression specifying the expressions defining the forward
*        transformation. The number of forward transformation functions
*        supplied must be at least equal to NOUT, but may be increased to
*        accommodate any additional expressions which define intermediate
*        variables for the forward transformation. The syntax of these
*        expressions is described in SUN/210
*     INV = LITERAL (Read)
*        A group expression specifying the expressions defining the inverse
*        transformation. The number of inverse transformation functions
*        supplied must be at least equal to NIN, but may be increased to
*        accommodate any additional expressions which define intermediate
*        variables for the inverse transformation. The syntax of these
*        expressions is described in SUN/210
*     NIN = _INTEGER (Read)
*        Number of input variables for the MathMap. This determines the
*        value of its Nin attribute.
*     NOUT = _INTEGER (Read)
*        Number of output variables for the MathMap. This determines the
*        value of its Nout attribute.
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute
*        assignments to be used for initialising the new MathMap.
*     RESULT = LITERAL (Read)
*        A text file to receive the new MathMap.

*  Copyright:
*     Copyright (C) 2003 Central Laboratory of the Research Councils.
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
*     17-SEP-2003 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAXEXP             ! Maximum number of expressions for MathMap
      PARAMETER( MAXEXP = 100 )

*  Local Variables:
      CHARACTER FWD( MAXEXP )*(GRP__SZNAM)
      CHARACTER INV( MAXEXP )*(GRP__SZNAM)
      INTEGER IGRP
      INTEGER NFWD
      INTEGER NIN
      INTEGER NINV
      INTEGER NOUT
      INTEGER RESULT
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the number of input coordinates.
      CALL PAR_GDR0I( 'NIN', 2, 1, NDF__MXDIM, .FALSE., NIN, STATUS )

*  Get the number of output coordinates.
      CALL PAR_GDR0I( 'NOUT', 2, 1, NDF__MXDIM, .FALSE., NOUT, STATUS )

*  Get a GRP group holding the algebraic expressions for the forward
*  transformation.
      IGRP = GRP__NOID
      CALL KPG1_GTGRP( 'FWD', IGRP, NFWD, STATUS )
      DO WHILE( NFWD .LT. NOUT .AND. STATUS .EQ. SAI__OK )
         CALL MSG_SETI( 'N', NOUT )
         CALL MSG_OUT( 'ASTMATHMAP_MSG1', 'At least ^N forward '//
     :                 'expressions are required - please enter '//
     :                 'them again.', STATUS )
         CALL PAR_CANCL( 'FWD', STATUS )
         CALL KPG1_GTGRP( 'FWD', IGRP, NFWD, STATUS )
      END DO

*  Report an error if too many expressions were given.
      IF( NFWD .GT. MAXEXP .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'MX', MAXEXP )
         CALL MSG_SETI( 'N', NFWD )
         CALL ERR_REP( 'ASTMATHMAP_ERR1', 'Too many (^N) forward '//
     :                 'expressions given. No more than ^MX '//
     :                 'should be supplied.', STATUS )
      END IF

*  Copy the expressions into a local array.
      CALL GRP_GET( IGRP, 1, NFWD, FWD, STATUS )

*  Likewise, get a GRP group holding the algebraic expressions for the
*  inverse transformation.
      CALL KPG1_GTGRP( 'INV', IGRP, NINV, STATUS )
      DO WHILE( NINV .LT. NIN .AND. STATUS .EQ. SAI__OK )
         CALL MSG_SETI( 'N', NIN )
         CALL MSG_OUT( 'ASTMATHMAP_MSG2', 'At least ^N inverse '//
     :                 'expressions are required - please enter '//
     :                 'them again.', STATUS )
         CALL PAR_CANCL( 'INV', STATUS )
         CALL KPG1_GTGRP( 'INV', IGRP, NINV, STATUS )
      END DO

*  Report an error if too many expressions were given.
      IF( NINV .GT. MAXEXP .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'MX', MAXEXP )
         CALL MSG_SETI( 'N', NINV )
         CALL ERR_REP( 'ASTMATHMAP_ERR2', 'Too many (^N) inverse '//
     :                 'expressions given. No more than ^MX '//
     :                 'should be supplied.', STATUS )
      END IF

*  Copy the expressions into a local array, and delete the group.
      CALL GRP_GET( IGRP, 1, NINV, INV, STATUS )
      CALL GRP_DELET( IGRP, STATUS )

*  Create the required MathMap.
      RESULT = AST_MATHMAP( NIN, NOUT, NFWD, FWD, NINV, INV, ' ',
     :                      STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTMATHMAP_ERR', 'Error creating a new '//
     :                 'MathMap.', STATUS )
      END IF

      END
