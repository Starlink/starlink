      INTEGER FUNCTION KPG1_CNVRT( FROM, TO, DOMAINLIST, STATUS )
*+
*  Name:
*     KPG1_CNVRT

*  Purpose:
*     A wrapper for AST_CONVERT that allows alignment of CmpFrames with
*     SkyFrames, etc.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPG1_CNVRT( FROM, TO, DOMAINLIST, STATUS )

*  Description:
*     This routine is a drop-in replacement for AST_CONVERT, modified to
*     allow conversions to occur succesfully in some situations where
*     AST_CONVERT would fail to find a conversion.
*
*     Specifically, this function first calls AST_CONVERT directly,
*     returning the FrameSet if a conversion is found succesfully. If 
*     AST_CONVERT fails to find a conversion, and FROM is a SkyFrame, 
*     SpecFrame or TimeFrame (or a FrameSet in which the current Frame is 
*     a SkyFrame, SpecFrame or TimeFrame), and if TO is a CmpFrame (or a 
*     FrameSet in which the current Frame is a CmpFrame), then the CmpFrame
*     is searched for a Frame of the same class as FROM, and if found, a
*     PermMap is used to connect the CmpFrame to the other Frame, using bad
*     values for the unconnected CmpFrame axes. If this fails to produce
*     conversion, the same check is performed with TO and FROM swapped.
*
*     See the description of AST_CONVERT in SUN/210 for further details.

*  Arguments:
*     FROM = INTEGER (Given)
*        Pointer to a Frame which represents the "source" coordinate
*	 system. This is the coordinate system in which you already have
*	 coordinates available.
*     TO = INTEGER (Given)
*        Pointer to a Frame which represents the "destination" coordinate
*	 system. This is the coordinate system into which you wish to
*	 convert your coordinates.
*     DOMAINLIST = CHARACTER * ( * ) (Given)
*        A character string containing a comma-separated list of Frame
*	 domains. This may be used to define a priority order for the
*	 different intermediate coordinate systems that might be used to
*	 perform the conversion.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*       If the requested coordinate conversion is possible, the function
*	returns a pointer to a FrameSet which describes the conversion.
*	Otherwise, a null Object pointer (AST__NULL) is returned without
*	error.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-APR-2006 (DSB):
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
      INTEGER FROM
      INTEGER TO
      CHARACTER DOMAINLIST*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DOM*80           ! Domain of Current Frame in FrameSet
      CHARACTER DOMLST*255       ! Domain search list
      CHARACTER TEXT*30          ! General text string
      INTEGER I                  ! Frame Index
      INTEGER IAT                ! No. of characters in string
      INTEGER IBASE1             ! Index of original Base Frame in IWCS1
      INTEGER IBASE2             ! Index of original Base Frame in IWCS2
      INTEGER ICURR1             ! Index of Current Frame in IWCS1
      INTEGER ICURR2             ! Index of Current Frame in IWCS2
      INTEGER IMAT1              ! Index of alignment Frame in IWCS1 
      INTEGER IMAT2              ! Index of alignment Frame in IWCS2
      INTEGER MAP                ! Simplified mapping between two Frames
      INTEGER NAXC1              ! Number of axies in current Frame of IWCS1
      INTEGER NAXC2              ! Number of axies in current Frame of IWCS2
      INTEGER NFRM1              ! No. of Frames supplied in IWCS1
      INTEGER TEMP               ! AST pointer to a FrameSet
      LOGICAL WARNED             ! Warning of duplicate Frames issued?
*.

*  Initialise
      KPG1_CNVRT = AST__NULL 

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Use AST_CONVERT to attempt to align the FrameSets. 
      KPG1_CNVRT = AST_CONVERT( FROM, TO, DOMAINLIST, STATUS ) 

*  If no conversion was possible... 
      IF( KPG1_CNVRT == AST__NULL ) THEN

*  Begin an AST context
         CALL AST_BEGIN( STATUS )

*  Get pointers to the two supplied Frames, using the current Frame if a
*  FrameSet was supplied.
         IF( AST_ISAFRAMESET( FROM, STATUS ) ) THEN
            FROM_FRAME = AST_GETFRAME( FROM, AST__CURRENT, STATUS )
         ELSE 
            FROM_FRAME = AST_CLONE( FROM, STATUS )
         END IF

         IF( AST_ISAFRAMESET( TO, STATUS ) ) THEN
            TO_FRAME = AST_GETFRAME( TO, AST__CURRENT, STATUS )
         ELSE 
            TO_FRAME = AST_CLONE( TO, STATUS )
         END IF

*  See if FROM is a SkyFrame, SpecFrame or TimeFrame, and TO is a CmpFrame. 
*  If they are, retain pointers to them. Otherwise, perform the same checks
*  in reverse.
         IF( ( AST_ISASKYFRAME( FROM_FRAME, STATUS ) .OR.
     :         AST_ISASPECFRAME( FROM_FRAME, STATUS ) .OR.
     :         AST_ISATIMEFRAME( FROM_FRAME, STATUS ) ) .AND.
     :         AST_ISACMPFRAME( TO_FRAME, STATUS ) ) THEN
            ATOMIC = FROM
            CMP = TO

         ELSE IF( ( AST_ISASKYFRAME( TO_FRAME, STATUS ) .OR.
     :              AST_ISASPECFRAME( TO_FRAME, STATUS ) .OR.
     :              AST_ISATIMEFRAME( TO_FRAME, STATUS ) ) .AND.
     :              AST_ISACMPFRAME( FROM_FRAME, STATUS ) ) THEN
            ATOMIC = TO
            CMP = FROM

         ELSE
            ATOMIC = AST__NULL
            CMP = AST__NULL
         END IF

*  If we have an atomic Frame and a CmpFrame, we search the CmpFrame for a
*  Frame which will align with the atomic Frame. 
         IF( ATOMIC .NE. AST__NULL ) THEN

*  If the atomic Frame has only 1 axis, loop round all axes in the CmpFrame, 
*  getting a pointer to the primary Frame which defines the axis. If this 
*  primary Frame can be aligned with the atomic Frame, thenis of the same class as the atomic Frame noted above, note 
the axis index. 
 
            NAX = AST_GETI( CMP, 'Naxes', STATUS )
            DO IAX = 1,NAX

               TEMP = AST_PICKAXES( CMP, NATX, IAX, MAP, STATUS )

            END DO


         END IF

*  End the AST context
         CALL AST_END( STATUS )

      END IF

      END
