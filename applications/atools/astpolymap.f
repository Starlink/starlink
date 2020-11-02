      SUBROUTINE ASTPOLYMAP( STATUS )
*+
*  Name:
*     ASTPOLYMAP

*  Purpose:
*     Create a PolyMap.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTPOLYMAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new PolyMap and optionally initialises
*     its attributes.
*
*     A PolyMap is a form of Mapping which performs a general polynomial
*     transformation.  Each output coordinate is a polynomial function of
*     all the input coordinates. The coefficients are specified separately
*     for each output coordinate. The forward and inverse transformations
*     are defined independantly by separate sets of coefficients.

*  Usage:
*     astpolymap nin nout ncoeff_f coeff_f ncoeff_i coeff_i options result

*  ADAM Parameters:
*     COEFF_F = LITERAL (Read)
*        A group expression specifying the coefficients of the forward
*        transformation polynomials. Each sub-group of "2 + NIN" adjacent
*        elements describe a single coefficient of the forward transformation.
*        Within each such group, the first element is the coefficient value;
*        the next element is the integer index of the PolyMap output which
*        uses the coefficient within its defining polynomial (the first output
*        has index 1); the remaining elements of the group give the integer
*        powers to use with each input coordinate value (powers must not be
*        negative, and floating point values are rounded to the nearest integer).
*
*        For instance, if the PolyMap has 3 inputs and 2 outputs, each group
*        consisting of 5 elements, A groups such as "(1.2, 2.0, 1.0, 3.0, 0.0)"
*        describes a coefficient with value 1.2 which is used within the
*        definition of output 2. The output value is incremented by the
*        product of the coefficient value, the value of input coordinate
*        1 raised to the power 1, and the value of input coordinate 2 raised
*        to the power 3. Input coordinate 3 is not used since its power is
*        specified as zero. As another example, the group "(-1.0, 1.0,
*        0.0, 0.0, 0.0 )" describes adds a constant value -1.0 onto
*        output 1 (it is a constant value since the power for every input
*        axis is given as zero).
*
*        Each final output coordinate value is the sum of all the terms
*        described by the sub-groups within the supplied group. Supplying
*        a null (!) value will result in the forward transformation being
*        undefined.
*     COEFF_I = LITERAL (Read)
*        A group expression specifying the coefficients of the inverse
*        transformation polynomials. Each sub-group of "2 + NOUT" adjacent
*        elements describe a single coefficient of the inverse transformation
*        using the same scheme as "COEFF_F", except that inputs and outputs
*        are transposed. Supplying a null (!) value will result in the
*        inverse transformation being undefined.
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     NIN = _INTEGER (Read)
*        The number of input coordinates.
*     NOUT = INTEGER (Read)
*        The number of output coordinates.
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute
*        assignments to be used for initialising the new PolyMap.
*     RESULT = LITERAL (Read)
*        A text file to receive the new PolyMap.

*  Copyright:
*     Copyright (C) 2003-2004 Central Laboratory of the Research
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     28-SEP-2003(DSB):
*        Original version.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER IGRP
      INTEGER IPCOF
      INTEGER IPCOI
      INTEGER NCOF
      INTEGER NCOI
      INTEGER NEL
      INTEGER NIN
      INTEGER NOUT
      INTEGER RESULT
      INTEGER SIZE
      LOGICAL DONE
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the number of input and output axes required.
      CALL PAR_GDR0I( 'NIN', 2, 1, NDF__MXDIM, .FALSE., NIN, STATUS )
      CALL PAR_GDR0I( 'NOUT', NIN, 1, NDF__MXDIM, .FALSE., NOUT,
     :                STATUS )

*  Loop until we have good forward coefficients.
      DONE = .FALSE.
      DO WHILE( .NOT. DONE .AND. STATUS .EQ. SAI__OK )

*  Get a group holding the coefficients.
         IGRP = GRP__NOID
         CALL KPG1_GTGRP( 'COEFF_F', IGRP, SIZE, STATUS )

*  Report an error if the wrong number have been supplied.
         NCOF = SIZE/( NIN + 2 )
         IF( NCOF*( NIN + 2 ) .NE. SIZE .AND. STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETI( 'SIZE', SIZE )
            CALL MSG_SETI( 'M', NIN + 2 )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ASTPOLYMAP_ERR1', 'No of elements (^SIZE) '//
     :                    'in group supplied for parameter COEFF_F '//
     :                    'is wrong. Should be a multiple of ^M.',
     :                    STATUS )
         END IF

*  Allocate memory to hold the floating point values.
         CALL PSX_CALLOC( SIZE, '_DOUBLE', IPCOF, STATUS )

*  Read the values from the group into the memory.
         CALL ATL1_GTOFL( IGRP, SIZE, 1, %VAL( CNF_PVAL( IPCOF ) ),
     :                    STATUS )

*  If an error occurred reading the group, annull the error, cancel the
*  parameter and go round to get a new group. Otherwise, indicate that we
*  can leave the loop.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            NCOF = 0
            DONE = .TRUE.

         ELSE IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'ASTPOLYMAP_ERR2', 'Please supply a new '//
     :                    'group:', STATUS )
            CALL ERR_FLUSH( STATUS )
            CALL PAR_CANCL( 'COEFF_F', STATUS )
            CALL PSX_FREE( IPCOF, STATUS )

         ELSE
            DONE = .TRUE.
         END IF

         CALL GRP_DELET( IGRP, STATUS )
      END DO

*  Loop until we have good inverse coefficients.
      DONE = .FALSE.
      DO WHILE( .NOT. DONE .AND. STATUS .EQ. SAI__OK )

*  Get a group holding the coefficients.
         IGRP = GRP__NOID
         CALL KPG1_GTGRP( 'COEFF_I', IGRP, SIZE, STATUS )

*  Report an error if the wrong number have been supplied.
         NCOI = SIZE/( NOUT + 2 )
         IF( NCOI*( NOUT + 2 ) .NE. SIZE .AND.
     :       STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETI( 'SIZE', SIZE )
            CALL MSG_SETI( 'M', NOUT + 2 )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ASTPOLYMAP_ERR3', 'No of elements (^SIZE) '//
     :                    'in group supplied for parameter COEFF_I '//
     :                    'is wrong. Should be a multiple of ^M.',
     :                    STATUS )
         END IF

*  Allocate memory to hold the floating point values.
         CALL PSX_CALLOC( SIZE, '_DOUBLE', IPCOI, STATUS )

*  Read the values from the group into the memory.
         CALL ATL1_GTOFL( IGRP, SIZE, 1, %VAL( CNF_PVAL( IPCOI ) ),
     :                    STATUS )

*  If an error occurred reading the group, annull the error, cancel the
*  parameter and go round to get a new group. Otherwise, indicate that we
*  can leave the loop.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            NCOI = 0
            DONE = .TRUE.

         ELSE IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'ASTPOLYMAP_ERR4', 'Please supply a new '//
     :                    'group:', STATUS )
            CALL ERR_FLUSH( STATUS )
            CALL PAR_CANCL( 'COEFF_I', STATUS )
            CALL PSX_FREE( IPCOI, STATUS )

         ELSE
            DONE = .TRUE.
         END IF

         CALL GRP_DELET( IGRP, STATUS )
      END DO

*  Create the required PolyMap.
      RESULT = AST_POLYMAP( NIN, NOUT, NCOF, %VAL( CNF_PVAL( IPCOF ) ),
     :                      NCOI, %VAL( CNF_PVAL( IPCOI ) ),
     :                      ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTPOLYMAP_ERR', 'Error creating a new '//
     :                 'PolyMap.', STATUS )
      END IF

      END
