      SUBROUTINE KPG1_ASTTL( IPLOT, IWCS, INDF, STATUS )
*+
*  Name:
*     KPG1_ASTTL

*  Purpose:
*     Set the Title attribute of a Plot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASTTL( IPLOT, IWCS, INDF, STATUS )

*  Description:
*     This routine sets the Title attribute of an AST Plot. The Title
*     string can be obtained from a variety of places, and the following
*     priority order is used:
*
*     The value of the Title attribute supplied by the user via the
*     application's STYLE parameter is given top priority. If no Title
*     was supplied then the Title component of the NDF is used. If this
*     is blank, then the TItle attribute stored in the current Frame of
*     the NDF's WCS FrameSet is used, if it has been set explicitly (i.e. 
*     is not just a default value). Otherwise, the name of the NDF is used.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        An AST pointer for the Plot. The Title attribute of the current
*        Frame will be set on exit.
*     IWCS = INTEGER (Given)
*        An AST pointer for the WCS FrameSet obtained from the NDF.
*     INDF = INTEGER (Given)
*        An NDF identifier for the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-FEB-2006 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER IPLOT
      INTEGER IWCS
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER TTL*80           ! Title string
      INTEGER TTLLEN             ! Used length of TTL

*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First priority is given to values explicitly supplied by the user via
*  the application's STYLE parameter. If such a value was supplied it
*  will currently be assigned to the Title attribute of the Plot. However,
*  simply testing the state of the attribute using AST_TEST will not
*  distinguish between titles supplied via the STYLE parameter a title
*  stored in the  NDF's WCS FrameSet. We assume that if the Title in the
*  Plot is the same as the Title in the WCS FrameSet, then the user has
*  not supplied the Title via the STYLE parameter. If these two titles
*  differ, then return with the Plot unchanged since we will then assume
*  that the Plot's Title was supplied via the STYLE parameter (if the
*  user has explicitly supplied a blank title, then we find a better value).
      TTL = AST_GETC( IPLOT, 'TITLE', STATUS ) 
      IF( TTL .EQ. ' ' .OR. 
     :    TTL .EQ. AST_GETC( IWCS, 'TITLE', STATUS ) .OR.
     :    .NOT. AST_TEST( IPLOT, 'TITLE', STATUS ) ) THEN

*  If the user has not supplied a title via the STYLE parameter, or has
*  supplied a blank title, then the next priority is to use the NDF's Title
*  component, if set.
         TTL = ' '
         CALL NDF_CGET( INDF, 'TITLE', TTL, STATUS ) 

*  If the NDF has no Title component, use the WCS FrameSet Title if it has 
*  been set explicitly (i.e. is not a default value). 
         IF( TTL .EQ. ' ' .AND. AST_TEST( IWCS, 'TITLE', STATUS ) ) THEN
            TTL = AST_GETC( IWCS, 'TITLE', STATUS ) 
         END IF

*  If we still have no title, use the NDF path. Ensure we know the length
*  of the title string.
         IF( TTL .EQ. ' ' ) THEN
            CALL KPG1_NDFNM( INDF, TTL, TTLLEN, STATUS )
         ELSE
            TTLLEN = CHR_LEN( TTL )
         END IF

*  Set the Plot Title.
         CALL AST_SETC( IPLOT, 'TITLE', TTL( : TTLLEN ), STATUS )

      END IF

      END 
