      SUBROUTINE IRM_STLMT( LMTX, LMTY, XLMT, YLMT, STATUS )
*+
*  Name:
*     IRM_STLMT

*  Purpose:
*     Set axes limits of a NCAR graph.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_STLMT( LMTX, LMTY, XLMT, YLMT, STATUS )

*  Description:
*     This routine sets the NCAR parameters 'X/MINIMUM.', X/MAXIMUM.',
*     'Y/MINIMUM.' and 'Y/MAXIMUM.' to specify the limits of the
*     coordinates to be considered. If either X or Y axis has its limit
*     set differently from the limits of data values, the parameter 
*     'WINDOW.' is set so that the curve portion falling outside the 
*     grid window will be omitted. The parameters 'X/NICE.' and 'Y/NICE.' 
*     are set as well so that the start and the end of the axes are at 
*     the start and the end pixels rather than at major marks.      

*  Arguments:
*     LMTX = INTEGER (Given)
*        Its value determines how the lower and upper limits of the X
*        axis is set. It can take following values:
*
*          -1 :The values given in XLMT( 1 ) and XLMT( 2 ) will be 
*              used as lower and upper limits of X axis, respectively.
*
*           0 :The graph package will examine the user's x coordinate
*              data and find the minimum and maximum value as the axis
*              lower and upper limits.
*
*           1 :The values given in XLMT( 1 ) will be used as the lower
*              limit of the axis. The graph package uses the maximum 
*              value of user's x coordinate as the axis upper limit.
*
*           2 :The values given in XLMT( 2 ) will be uses as the upper
*              limit of the axis.  The graph package uses the minimum
*              value of user's x coordinate as the axis lower limit.
*          
*     LMTY = INTEGER (Given)
*        Its value determines how the lower and upper limits of the Y
*        axis is set. It can take following values:
*
*          -1 :The values given in YLMT( 1 ) and YLMT( 2 ) will be 
*              used as lower and upper limits of Y axis, respectively.
*
*           0 :The graph package will examine the user's Y coordinate
*              data and find the minimum and maximum value as the axis
*              lower and upper limits.
*
*           1 :The values given in YLMT( 1 ) will be used as the lower
*              limit of the axis. The graph package uses the maximum 
*              value of user's Y coordinate as the axis upper limit.
*
*           2 :The values given in YLMT( 2 ) will be uses as the upper
*              limit of the axis.  The graph package uses the minimum
*              value of user's Y coordinate as the axis lower limit.
*             
*     XLMT( 2 ) = REAL (Given)
*        The user specified lower limit and higher limit of x axis,
*        respectively.
*     YLMT( 2 ) = REAL (Given)
*        The user specified lower limit and highter limit of y axis,
*        respectively.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1991 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER LMTX
      INTEGER LMTY

      REAL XLMT( 2 )
      REAL YLMT( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Locatl Argument:
      REAL NULL1                 ! 'Null 1' of NCAR special value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the NULL/1 of present NCAR setting.
      CALL AGGETF( 'NULL/1.', NULL1 )

*  If both limits of x axis are to be specified by user, set them 
*  according to the values given by XLMT(1) and XLMT(2).
      IF ( LMTX .EQ. -1 ) THEN
         CALL AGSETF( 'X/MINIMUM.', MIN( XLMT( 1 ), XLMT( 2 ) ) )
         CALL AGSETF( 'X/MAXIMUM.', MAX( XLMT( 1 ), XLMT( 2 ) ) )

*  If both limits of x axis are set according to the minimum and 
*  maximum of user's x coordinate, set the parameters as 'NULL 1' 
*  of NCAR to let NCAR find limits itself.
      ELSE IF ( LMTX .EQ. 0 ) THEN
         CALL AGSETF( 'X/MINIMUM.', NULL1 )
         CALL AGSETF( 'X/MAXIMUM.', NULL1 )

*  If lower limit of x axis is specified by user, while the upper
*  limit is set according to the max. of user's x coordinate, set
*  the parameters accordingly.
      ELSE IF ( LMTX .EQ. 1 ) THEN
         CALL AGSETF( 'X/MINIMUM.', MIN( XLMT( 1 ), XLMT( 2 ) ) )
         CALL AGSETF( 'X/MAXIMUM.', NULL1 )

*  If upper limit of x axis is specified by user, while the lower
*  limit is set according to the min. of user's x coordinate, set
*  the parameters accordingly.
      ELSE IF ( LMTX .EQ. 2 ) THEN
         CALL AGSETF( 'X/MINIMUM.', NULL1 )
         CALL AGSETF( 'X/MAXIMUM.', MAX( XLMT( 1 ), XLMT( 2 ) ) )
      END IF

*  If both limits of y axis are to be specified by user, 
*  set the parameters according to the values given by YLMT(1) 
*  and YLMT(2).
      IF ( LMTY .EQ. -1 ) THEN
         CALL AGSETF( 'Y/MINIMUM.', YLMT( 1 ) )
         CALL AGSETF( 'Y/MAXIMUM.', YLMT( 2 ) )


*  If both limits are set according to the max. and min. of user's
*  y coordinate, set parameters as 'NULL 1' of NCAR to let NCAR 
*  find the limits itself.
      ELSE IF ( LMTY .EQ. 0 ) THEN
         CALL AGSETF( 'Y/MINIMUM.', NULL1 )
         CALL AGSETF( 'Y/MAXIMUM.', NULL1 )

*  If lower limit of Y axis is specified by user, while the upper
*  limit is set according to the max. of user's Y coordinate, set
*  the parameters accordingly.
      ELSE IF ( LMTY .EQ. 1 ) THEN
         CALL AGSETF( 'Y/MINIMUM.', YLMT( 1 ) )
         CALL AGSETF( 'Y/MAXIMUM.', NULL1 )

*  If upper limit of Y axis is specified by user, while the lower
*  limit is set according to the min. of user's Y coordinate, set
*  the parameters accordingly.
      ELSE IF ( LMTY .EQ. 2 ) THEN
         CALL AGSETF( 'Y/MINIMUM.', NULL1 )
         CALL AGSETF( 'Y/MAXIMUM.', YLMT( 2 ) )
      END IF

*  Set the parameter 'WINDOW.' so that the curve portions falling 
*  outside the grid window are omitted.
      IF ( ( LMTX .NE. 0 ) .OR. ( LMTY .NE. 0 ) ) THEN
         CALL AGSETF( 'WINDOW.', 0. )
      END IF

*  Set 'nice' values for both axes, so that major tick marks do not 
*  occur necessarily at vertices.
      CALL AGSETF( 'X/NICE.', 0. )
      CALL AGSETF( 'Y/NICE.', 0. )

*  End of the routine.                                                        
      END
