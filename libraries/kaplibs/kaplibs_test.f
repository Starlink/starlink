      SUBROUTINE KAPLIBS_TEST( STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST_ constants and function declarations

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER LIST*80
      DOUBLE PRECISION ATTRS( 20 )! Saved graphics attributes
      DOUBLE PRECISION BOX( 4 )  ! Bounds of used region of (X,Y) axes
      DOUBLE PRECISION X,Y       !
      INTEGER IPICD              ! AGI identifier for the DATA picture
      INTEGER IPICF              ! AGI identifier for the frame picture
      INTEGER IPICK              ! AGI identifier for the KEY picture
      INTEGER IPLOT              ! Pointer to AST Plot for DATA picture
      INTEGER NFRM               ! Frame index increment between IWCS and IPLOT
      LOGICAL ALIGN              ! DATA pic. aligned with a previous picture?
      REAL MARGIN( 4 )           ! Margins round DATA picture
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Check IRA looks OK.
      CALL IRA_IPROJ( LIST, STATUS )
      IF( INDEX( LIST, 'AITOFF' ) .EQ. 0 ) THEN
         CALL MSG_SETC( 'S', LIST )
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'AITOFF not included in string returned '//
     :                 'by IRA_IPROJ (^S).', STATUS )
         GO TO 999
      END IF

*  Check the PDA seed can be set.
      CALL KPG1_PSEED( STATUS )

*  Tell the user what should happen.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', 'A graphics window should appear '//
     :              'containing a red diagonal line, a grid, and '//
     :              'a set of annotated axes.',
     :              STATUS )
      CALL MSG_BLANK( STATUS )

* Create an AST Plot for plotting.
      MARGIN( 1 ) = 0.1
      MARGIN( 2 ) = 0.1
      MARGIN( 3 ) = 0.1
      MARGIN( 4 ) = 0.1
      BOX( 1 ) = 0.0
      BOX( 2 ) = 0.0
      BOX( 3 ) = 100.0
      BOX( 4 ) = 100.0
      CALL KPG1_PLOT( AST__NULL, 'NEW', 'KAPLIBS_TEST', ' ', MARGIN, 0,
     :                ' ', ' ', 0.0, 0.0, ' ', BOX, IPICD, IPICF, IPICK,
     :                IPLOT, NFRM, ALIGN, STATUS )

*  Set the appearance of lines drawn using PGPLOT so that they mimic
*  curves produced using astCurves.
      CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTRS, STATUS )

*  Draw a diagonal line using PGPLOT.
      CALL AST_TRAN2( IPLOT, 1, 0.0D0, 0.0D0, .FALSE., X, Y, STATUS )
      CALL PGMOVE( REAL( X ), REAL( Y ) )

      CALL AST_TRAN2( IPLOT, 1, 1.0D2, 1.0D2, .FALSE., X, Y, STATUS )
      CALL PGDRAW( REAL( X ), REAL( Y ) )

*  Re-instate the previous PGPLOT attributes.
      CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTRS, STATUS )

*  Draw an axes grid.
      CALL KPG1_ASGRD( IPLOT, IPICF, .TRUE., STATUS )

*  Shutdown PGPLOT and the graphics database.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, flush it and report a warning.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         CALL MSG_OUT( ' ', 'KAPLIBS test failed', STATUS )
      ELSE
         CALL MSG_OUT( ' ', 'KAPLIBS test passed', STATUS )
      END IF

      END
