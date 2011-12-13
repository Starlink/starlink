      SUBROUTINE ARD1_FPLOT( SZEXPR, EXPR, SZOPND, OPND, OK, STATUS )
*+
*  Name:
*     ARD1_FPLOT

*  Purpose:
*     Analyse an ARD description into operators, keywords and statement.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_FPLOT( SZEXPR, EXPR, SZOPND, OPND, OK, STATUS )

*  Description:
*     This routine plots the ARD description described in the expression
*     array EXPR, if and only if the ARD expression consists of a single
*     "Load Keyword" instruction (i.e. a single region). A fast (ish)
*     algorithm is used which does not involve evaluating a pixel mask.

*  Arguments:
*     SZEXPR = INTEGER (Given)
*        The size of the EXPR array.
*     EXPR( SZEXPR ) = INTEGER (Given)
*        The logical expression representing the ARD description is
*        defined by this array. Each value in the array
*        (starting at index 1) represents an instruction which can be
*        executed by the ARD expression evaluator routine (ARD1_EVAL)
*        Such instructions are stored in the returned array in the same
*        order in which they appear in the ARD description. Keyword
*        fields are represented by a "Load Keyword Region" (LKR)
*        instruction which causes the expression evaluator to load a
*        mask defined by the keyword. The parameters which describe
*        such keywords are stored in the OPND array. The index within the
*        OPND array at which the keyword description starts is stored as an
*        argument for the LKR instruction. An "End Expression" instruction
*        is inserted at the end of the ARD description.
*     SZOPND = INTEGER (Given)
*        The size of the OPND array.
*     OPND( SZPND ) = DOUBLE PRECISION (Given)
*        Holds information about all the operands (i.e. keywords)
*        used within the logical expression representing the ARD
*        description. Each operand is defined by a variable length block
*        of values (see ARD1_FPLOT prologue for details). Checks are made on
*        the validity of the keyword argument lists.
*     OK = LOGICAL (Returned)
*        Returned .TRUE if the ARD description was plotted, and .FALSE.
*        otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     useful,but WITHOUT ANY WARRANTY; without even the implied
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
*     1-OCT-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants
      INCLUDE 'AST_PAR'          ! AST_ constants and functions
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Constants:
      INCLUDE 'ARD_COM'          ! ARD common blocks
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
*        CMN_IPPAR = INTEGER (Write)
*           Pointer to na array holding parameter values.
*        CMN_NPARC = INTEGER (Write)
*           The number of parameters in CMN_IPPAR.
*        CMN_FRMC = INTEGER (Write)
*           The user coord Frame.
*        CMN_TYPEC = INTEGER (Write)
*           The region type identifier.

*  Arguments Given:
      INTEGER SZEXPR
      INTEGER EXPR( SZEXPR )
      INTEGER SZOPND
      DOUBLE PRECISION OPND( SZOPND )

*  Arguments Returned:
      LOGICAL OK

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ARD1_INIT         ! Initialise ARD common blocks
      EXTERNAL ARD1_INTRA        ! The dist->current Frame transform routine

*  Local Variables:
      DOUBLE PRECISION DAST      ! DOUBLE equivalence to variable IAST.
      DOUBLE PRECISION DPP       ! Distance per pixel
      DOUBLE PRECISION HL        ! Half the requested box length
      DOUBLE PRECISION HW        ! Half the requested box width
      DOUBLE PRECISION P0( 2 )   ! The box centre
      DOUBLE PRECISION PA0       ! Requested angle as a position angle
      DOUBLE PRECISION PA1       ! The position angle at the end
      DOUBLE PRECISION PA2       ! The position angle at the end
      DOUBLE PRECISION RBPAR( 16 )! New parameters for a ROTBOX
      INTEGER CFRM               ! User coord Frame
      INTEGER FR                 ! Frame pointer
      INTEGER I                  ! Frame index
      INTEGER IAST(2)            ! AST Frame or FrameSet pointer
      INTEGER IBASE              ! Index of original Base Frame
      INTEGER IMAP               ! AST IntraMap identifier
      INTEGER IOPND              ! Next free entry in operands array
      INTEGER IPAR               ! Index of first region parameters
      INTEGER IPPAR              ! Pointer to memory holding parameters
      INTEGER LASTOP             ! Index of last used operand value
      INTEGER NPAR               ! No. of region parameters
      INTEGER OTYPE              ! Original region type
      INTEGER TYPE               ! Region type

*  Make DAST and IAST share the same memory so that we can interpret
*  the real operand value as an integer identifier.
      EQUIVALENCE ( IAST, DAST )

*.

*  Initialise returned values.
      OK = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  To be usable, the EXPR array should have at least 3 elements. The first
*  should be an LKR instruction. the second will then be an index into the OPND
*  array, and the third should be an "End expression" instruction.
      IF( SZEXPR .GE. 3 .AND. EXPR( 1 ) .EQ. ARD__LKR .AND.
     :    EXPR( 3 ) .EQ. ARD__END ) THEN

*  Begin an AST context.
         CALL AST_BEGIN( STATUS )

*  Save the index within OPRNDS of the start of the block of values
*  which describe the required region.
         IOPND = EXPR( 2 )

*  Get the integer code which identifies the type of region required.
         TYPE = NINT( OPND( IOPND ) )

*  Save the number of parameters describing the region
         NPAR = NINT( OPND( IOPND + 2 ) )

*  Save the index of the first region parameter.
         IPAR = IOPND + 3

*  See if the pixel->user mapping is linear. If not, extract the AST
*  FrameSet pointer. At the same time, note the number of region
*  parameters and the index of the first region parameter.
         IF( OPND( IOPND + 3 + NPAR ) .EQ. 0.0 ) THEN
            DAST = OPND( IOPND + 4 + NPAR )
            DPP = OPND( IOPND + 5 + NPAR )
            LASTOP = IOPND + 5 + NPAR

*  If the total FrameSet is not an AST Plot, report an error.
            IF( .NOT. AST_ISAPLOT( IAST( 1 ), STATUS ) .AND.
     :          STATUS .EQ. SAI__OK ) THEN
              STATUS = ARD__INTER
              CALL ERR_REP( 'ARD1_FPLOT_ERR1', 'ARD1_FPLOT: Total '//
     :                      'FrameSet is not a Plot (programming '//
     :                      'error).', STATUS )

*  If it is a Plot, ensure the GRAPHICS Frame is the Base Frame.
            ELSE
               IBASE = AST_GETI( IAST( 1 ), 'BASE', STATUS )
               DO I = 1, AST_GETI( IAST( 1 ), 'NFRAME', STATUS )
                  FR = AST_GETFRAME( IAST( 1 ), I, STATUS )
                  IF( AST_GETC( FR, 'DOMAIN', STATUS ) .EQ.
     :                'GRAPHICS' ) CALL AST_SETI( IAST( I ), 'BASE', I,
     :                                            STATUS )
                  CALL AST_ANNUL( FR, STATUS )
               END DO
            END IF

*  If the mapping is linear, report an error (ARD1_PLOT set the CMN_LINOK
*  flag to false in order to ensure that no Mapping would be considered to
*  be linear.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = ARD__INTER
            CALL MSG_SETI( 'TYPE', TYPE )
            CALL ERR_REP( 'ARD1_FPLOT_ERR2', 'Illegal linear Mapping '//
     :                   'encountered within ARD1_FPLOT (region type '//
     :                   '^TYPE) (programming error).', STATUS )
         END IF

*  Report an error if there are insufficient values in OPND.
         IF( LASTOP .GT. SZOPND .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = ARD__INTER
            CALL MSG_SETI( 'TYPE', TYPE )
            CALL ERR_REP( 'ARD1_FPLOT_ERR3', 'Operand stack exhausted'//
     :                    ' while processing region type ^TYPE in '//
     :                    'ARD1_FPLOT (programming error).', STATUS )

            CALL MSG_SETI( 'NPAR', NPAR )
            CALL ERR_REP( 'ARD1_FPLOT_ERR4', '^NPAR parameters '//
     :                    'required.', STATUS )

            CALL MSG_SETI( 'IOP', IOPND )
            CALL ERR_REP( 'ARD1_FPLOT_ERR5', 'Starting at index ^IOP',
     :                     STATUS )

            CALL MSG_SETI( 'OPSIZ', SZOPND )
            CALL ERR_REP( 'ARD1_FPLOT_ERR6', 'Stack size: ^OPSIZ',
     :                     STATUS )

         END IF

*  Save the user coord Frame.
         CFRM = AST_GETFRAME( IAST( 1 ), AST__CURRENT, STATUS )

*  Save the original region type.
         OTYPE = TYPE

*  Convert RECT regions into BOX regions.
         IF( TYPE .EQ. ARD__REC ) THEN
            DO I = 1, 2
               HW = 0.5*AST_AXDISTANCE( CFRM, I, OPND( IPAR + I - 1 ),
     :                                  OPND( IPAR + I + 1 ), STATUS )
               OPND( IPAR + I - 1 ) = AST_AXOFFSET( CFRM, I,
     :                                             OPND( IPAR + I - 1 ),
     :                                             HW, STATUS )
               OPND( IPAR + I + 1 ) = 2*ABS( HW )
            END DO
            TYPE = ARD__BOX

*  Convert ROTBOX regions into POLYGON regions. A polygon vertex is put at
*  the middle of each side in case we are dealing with spherical coords, in
*  which case a polygon edge greater than 180 arc-degrees could cause
*  problems.
         ELSE IF( TYPE .EQ. ARD__ROT ) THEN

*  Offset away from the centre point along the first edge, at the
*  requested angle, going half the box length. Using AST caters for both
*  Cartesian and spherical user coords. Store points back in the PAR
*  array, in a suitable order to make the points a continuous curve.
            PA0 = ( 90.0 - OPND( IPAR + 4 ) )*ARD__DTOR
            HL = 0.5*OPND( IPAR + 2 )
            HW = 0.5*OPND( IPAR + 3 )
            P0( 1 ) = OPND( IPAR )
            P0( 2 ) = OPND( IPAR + 1 )

            PA1 = AST_OFFSET2( CFRM, P0, PA0, HL, RBPAR, STATUS )

*  Now turn to the left by 90 degrees and offset up by half the box height.
            PA1 = PA1 - ARD__PIBY2
            PA2 = AST_OFFSET2( CFRM, RBPAR, PA1, HW, RBPAR( 3 ),
     :                         STATUS )

*  Now offset down by half the box height.
            PA2 = AST_OFFSET2( CFRM, RBPAR, PA1, -HW, RBPAR( 15 ),
     :                         STATUS )

*  Now offset up and down by half the box height, starting at the box
*  centre.
            PA1 = PA0 - ARD__PIBY2
            PA2 = AST_OFFSET2( CFRM, P0, PA1, HW, RBPAR( 5 ), STATUS )
            PA2 = AST_OFFSET2( CFRM, P0, PA1, -HW, RBPAR( 13 ), STATUS )

*  Offset away from the centre point along the first edge, away from the
*  requested angle, going half the box length.
            PA1 = AST_OFFSET2( CFRM, P0, PA0, -HL, RBPAR( 9 ), STATUS )

*  Now turn to the left by 90 degrees and offset up by half the box height.
            PA1 = PA1 - ARD__PIBY2
            PA2 = AST_OFFSET2( CFRM, RBPAR( 9 ), PA1, HW, RBPAR( 7 ),
     :                         STATUS )

*  Now offset down by half the box height.
            PA2 = AST_OFFSET2( CFRM, RBPAR( 9 ), PA1, -HW, RBPAR( 11 ),
     :                         STATUS )

*  Reset the region type and the number of parameters.
            TYPE = ARD__POL
            NPAR = 16

         END IF

*  We create an AST IntraMap defining the Mapping from distance along a
*  curve to user coordinates. This IntrMap needs to know how all about
*  the region being drawn. Allocate memory for a copy of the supplied
*  region parameters, and copy them.
         CALL PSX_CALLOC( NPAR, '_DOUBLE', IPPAR, STATUS )
         IF( OTYPE .NE. ARD__ROT ) THEN
            CALL ARD1_COPYD( NPAR, OPND( IPAR ),
     :                       %VAL( CNF_PVAL( IPPAR ) ), STATUS )
         ELSE
            CALL ARD1_COPYD( NPAR, RBPAR, %VAL( CNF_PVAL( IPPAR ) ),
     :                       STATUS )
         END IF

*  Store information needed by the IntraMap transformation routine
*  in common .
         CMN_TYPEC = TYPE
         CMN_NPARC = NPAR
         CMN_IPPAR = IPPAR
         CMN_FRMC = CFRM

*  Register the IntraMap transformation routine.
         CALL AST_INTRAREG( 'ARDDRAW', 1, 2, ARD1_INTRA, AST__NOINV,
     :                      ' ', ' ', ' ', STATUS )

*  Create the IntraMap.
         IMAP = AST_INTRAMAP( 'ARDDRAW', 1, 2, ' ', STATUS )

*  Now draw the curve. DO this within a new error reporting context, and
*  annul any error that occurs (e.g. because ARD1_INTRA does not support the
*  type of region being drawn).
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL ERR_MARK

            CALL AST_GENCURVE( IAST( 1 ), IMAP, STATUS )

            IF( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
            ELSE
               OK = .TRUE.
            END IF

            CALL ERR_RLSE
         END IF

*  Reinstate the original Base Frame.
         CALL AST_SETI( IAST( 1 ), 'BASE', IBASE, STATUS )

*  End the AST context.
         CALL AST_END( STATUS )

      END IF


      END
