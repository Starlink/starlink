      SUBROUTINE CCD1_PTHMP( FSETS, PATH, NSTEP, DMNLST, MAP, STATUS )
*+
*  Name:
*     CCD1_PTHMP

*  Purpose:
*     Generate an AST mapping from a path through a graph.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_PTHMP( FSETS, PATH, NSTEP, DMNLST, MAP, STATUS )

*  Description:
*     This routine uses a number of steps through a graph of conversions
*     between AST framesets to come up with a mapping from the first
*     frameset in the first edge of the path to the second frameset in 
*     the final edge of the path.  As a special case, if the initial
*     and terminal framesets are the same, a unit mapping is returned.

*  Arguments:
*     FSETS( * ) = INTEGER (Given)
*        Array of pointers to AST framesets into which the (1,*) and 
*        (2,*) elements of the PATH nodes index.
*     PATH( 4, NSTEP ) = INTEGER (Given)
*        A set of edges forming a connected graph between the nodes 
*        representing the framesets in the FSETS array.  The from-
*        and to- node numbers are in positions (1,*) and (2,*).
*        PATH(2,N-1) must be equal to PATH(1,N) for N = 2..NSTEP.
*     NSTEP = INTEGER (Given)
*        The second dimension of PATH.
*     DMNLST = CHARACTER * ( * ) (Given)
*        Domain list argument for passing to AST_CONVERT.
*     MAP = INTEGER (Returned)
*        AST pointer to a mapping between the Current frame of the 
*        the first frameset in the path to the Current frame of the
*        last frameset in the path.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-APR-1999 (MBT):
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
      
*  Arguments Given:
      INTEGER FSETS( * )
      INTEGER NSTEP
      INTEGER PATH( 4, NSTEP )
      CHARACTER * ( * ) DMNLST
      
*  Arguments Returned:
      INTEGER MAP
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FSMAP              ! AST pointer to a conversion frameset
      INTEGER MAP1               ! AST pointer to a mapping
      INTEGER MAP2               ! AST pointer to a mapping
      INTEGER I                  ! Loop variable
      
*.

*  Set default (failed) return value.
      MAP = AST__NULL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin new AST context.
      CALL AST_BEGIN( STATUS )

*  Check that the path is not empty.
      IF ( NSTEP .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_PTHMP_EMPTYPTH', 
     :                 'Empty path for conversion', STATUS )
         GO TO 99
      END IF

*  Treat as a special case the case in which the initial frameset is 
*  identical with the terminal frameset.
      IF ( PATH( 1, 1 ) .EQ. PATH( 2, NSTEP ) ) THEN
         MAP = AST_UNITMAP( 2, ' ', STATUS )
         GO TO 99
      END IF

*  Get the mapping corresponding to the first edge.
      FSMAP = AST_CONVERT( FSETS( PATH( 1, 1 ) ), 
     :                     FSETS( PATH( 2, 1 ) ), DMNLST, STATUS )
      MAP1 = AST_GETMAPPING( FSMAP, AST__BASE, AST__CURRENT, STATUS )

*  Walk through path, accumulating additional mappings.
      DO 1 I = 2, NSTEP
         FSMAP = AST_CONVERT( FSETS( PATH( 1, I ) ), 
     :                        FSETS( PATH( 2, I ) ), DMNLST, STATUS )
         MAP2 = AST_GETMAPPING( FSMAP, AST__BASE, AST__CURRENT, STATUS )
         MAP1 = AST_CMPMAP( MAP1, MAP2, .TRUE., ' ', STATUS )
 1    CONTINUE

*  Construct the final mapping.
      MAP = AST_SIMPLIFY( MAP1, STATUS )

*  Exit label.
 99   CONTINUE

*  Export the mapping from this AST context.
      CALL AST_EXPORT( MAP, STATUS )

*  End AST context.
      CALL AST_END( STATUS )

      END
* $Id$
