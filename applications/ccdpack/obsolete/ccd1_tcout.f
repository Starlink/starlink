      SUBROUTINE CCD1_TCOUT( MAP, STATUS )
*+
*  Name:
*     CCD1_TCOUT

*  Purpose:
*     Output transformation coefficients between AST domains.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_TCOUT( MAP, STATUS )

*  Description:
*     This routine prints out the coefficients of the transformation
*     represented by a given AST mapping.
*
*     The only mappings it can properly deal with are linear.  A check
*     is made to see whether the mapping does in fact appear to be 
*     linear; if it does not then a linear approximation to it is 
*     still output, but a warning is given to the effect that the
*     mapping appears to be nonlinear.  The check is fairly rudimentary,
*     so that false negatives and false positives are both possible.
*     They should not be very common however.
*
*     If no mapping can be obtained, or some error occurs in attempting
*     to find one, then a warning to that effect is output.  The STATUS
*     is not set to a non-null value in this case.
*
*     Output is via the CCD1_MSG facility.

*  Arguments:
*     MAP = INTEGER (Given)
*        AST pointer to the mapping to be described.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     23-FEB-1999 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'MSG_PAR'          ! Standard MSG/ERR constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants (machine precision etc)
      
*  Arguments Given:
      INTEGER MAP
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BUFFER * ( MSG__SZMSG ) ! Buffer for output
      DOUBLE PRECISION DIFLX    ! Discrepancy between linear and actual X coords
      DOUBLE PRECISION DIFLY    ! Discrepancy between linear and actual Y coords
      INTEGER IAT               ! Character position in buffer
      LOGICAL NONLIN            ! Does transformation appear to be nonlinear?
      INTEGER NCHAR             ! Number of characters converted
      DOUBLE PRECISION PXI( 4 ) ! Input X coordinates for test transformation
      DOUBLE PRECISION PYI( 4 ) ! Input Y coordinates for test transformation
      DOUBLE PRECISION PXO( 4 ) ! Output X coordinates for test transformation
      DOUBLE PRECISION PYO( 4 ) ! Output Y coordinates for test transformation
      DOUBLE PRECISION TR( 6 )  ! Coefficients of linear transformation
      
*  Local Data:
      DATA PXI / 0D0, 1D0, 0D0, 1D4 /
      DATA PYI / 0D0, 0D0, 1D0, 1D4 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get mapping between frames and transform points.
      CALL AST_TRAN2( MAP, 4, PXI, PYI, .TRUE., PXO, PYO, STATUS )

*  Check that the transformation has been successful.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL CCD1_MSG( ' ', 
     :   '  (An error occurred during the transformation)', STATUS )
      ELSE

*  Get coefficients of linear transformation.
         TR( 1 ) = PXO( 1 )
         TR( 4 ) = PYO( 1 )
         TR( 2 ) = PXO( 2 ) - PXO( 1 )
         TR( 5 ) = PYO( 2 ) - PYO( 1 )
         TR( 3 ) = PXO( 3 ) - PXO( 1 )
         TR( 6 ) = PYO( 3 ) - PYO( 1 )

*  See how nonlinear it looks - if it has picked up more than a 'small'
*  error in the conversion, then we note that the mapping looks nonlinear.
*  For 'small' we use the machine precision in REAL (calculations are in
*  double precision), which is arbitrary, but seems to work.
         DIFLX = TR( 1 ) + TR( 2 ) * PXI( 4 ) + TR( 3 ) * PYI( 4 )
     :           - PXO( 4 )
         DIFLY = TR( 4 ) + TR( 5 ) * PXI( 4 ) + TR( 6 ) * PYI( 4 )
     :           - PYO( 4 )
         NONLIN = MAX( ABS( DIFLX ), ABS( DIFLY ) ) 
     :            .GT. ( VAL__EPSR * 100D0 )

*  Output the coefficients.
         BUFFER = ' '
         IAT = 2
         BUFFER( IAT: ) = '  A ='
         IAT = IAT + 7
         CALL CHR_DTOC( TR( 1 ), BUFFER( IAT: ), NCHAR )
         IAT = 27
         BUFFER( IAT: ) = '  B ='
         IAT = IAT + 7
         CALL CHR_DTOC( TR( 2 ), BUFFER( IAT: ), NCHAR )
         IAT = 52
         BUFFER( IAT: ) = '  C ='
         IAT = IAT + 7
         CALL CHR_DTOC( TR( 3 ), BUFFER( IAT: ), NCHAR )
         CALL CCD1_MSG( ' ', BUFFER, STATUS )
         BUFFER = ' '
         IAT = 2
         BUFFER( IAT: ) = '  D ='
         IAT = IAT + 7
         CALL CHR_DTOC( TR( 4 ), BUFFER( IAT: ), NCHAR )
         IAT = 27
         BUFFER( IAT: ) = '  E ='
         IAT = IAT + 7
         CALL CHR_DTOC( TR( 5 ), BUFFER( IAT: ), NCHAR )
         IAT = 52
         BUFFER( IAT: ) = '  F ='
         IAT = IAT + 7
         CALL CHR_DTOC( TR( 6 ), BUFFER( IAT: ), NCHAR )
         CALL CCD1_MSG( ' ', BUFFER, STATUS )
       
*  Warn if the mapping looks nonlinear.
         IF ( NONLIN ) THEN
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL CCD1_MSG( ' ', '     ' //
     :      '  (Linear approximation to apparently nonlinear mapping)', 
     :                     STATUS )
         END IF

      END IF

      END
* $Id$
