      SUBROUTINE POL1_PKAXS( NDIM, GI, CONST, IWCS, STATUS )
*+
*  Name:
*     POL1_PKAXS

*  Purpose:
*     Pick the required axes to display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_PKAXS( NDIM, GI, CONST, IWCS, STATUS )

*  Description:
*     This routine modifies the supplied FrameSet to select the axes
*     which are to span the display. Other axes are assigned constant
*     values as specified in CONST. If the Base Frame has no Domain, it
*     is given a Domain equal to "<COLX>-<COLY>..." where <COLX>, <COLY>,
*     ... are the names of the catalogue columns included in the Frame.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of catalogue columns supplied.
*     GI( NDIM ) = INTEGER (Given)
*        An array of CAT identifiers for columns within the catalogue.
*        Ignored if NDIM is zero. The Base Frame of the returned FrameSet
*        is spanned by these two columns.
*     CONST( NDIM ) = DOUBLE PRECISION (Given)
*        Selects the base Frame axes in the returned FrameSet. Each value
*        corresponds to one of the NDIM columns given in GI, and is either
*        AST__BAD or a constant value. Axes for which AST__BAD is supplied
*        will be included in the base Frame of the returned FrameSet. Axes
*        for which a constant value is supplied wil not be included, but
*        will be set to the specified constant value.
*     IWCS = INTEGER (Returned)
*        An AST pointer to the returned FrameSet. AST__NULL is returned if
*        an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-FEB-2001 (DSB):
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
      INCLUDE 'CAT_PAR'          ! CAT constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER NDIM
      INTEGER GI( NDIM )
      DOUBLE PRECISION CONST( NDIM )

*  Arguments Given and Returned:
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DOM*255          ! Domain for Base Frame
      CHARACTER NAME*(CAT__SZCMP)! Axis name
      DOUBLE PRECISION CON( NDF__MXDIM )! Good constants
      DOUBLE PRECISION TEST( 1, NDF__MXDIM )! A test point
      INTEGER BFRM               ! Pointer to base Frame
      INTEGER CFRM               ! Pointer to current Frame
      INTEGER IAT                ! No. of characters in string
      INTEGER IBASE              ! Original Base Frame index
      INTEGER ICURR              ! Original Current Frame index
      INTEGER INEW               ! Index of new Frame
      INTEGER INPRM( NDF__MXDIM )! Input axis permutation
      INTEGER J                  ! Axis index
      INTEGER MAP                ! Pointer to a Mapping
      INTEGER NCON               ! Number fo good constants supplied
      INTEGER NFRM               ! Pointer to a Frame
      INTEGER NREQ               ! Number of required axes
      INTEGER OUTPRM( NDF__MXDIM )! Output axis permutation
      INTEGER PMAP               ! Pointer to PermMap
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get a pointer to the Base Frame fo the FrameSet.
      BFRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )

*  Set up the information required to create a PermMap which goes from
*  the NDIM-dimensional Base Frame found above, to a Frame containing only the
*  required axes.
      NREQ = 0
      NCON = 0
      DO J = 1, NDIM
         IF( CONST( J ) .EQ. AST__BAD ) THEN
            NREQ = NREQ + 1
            OUTPRM( NREQ ) = J
            INPRM( J ) = NREQ
         ELSE
            NCON = NCON + 1
            INPRM( J ) = -NCON
            CON( NCON ) = CONST( J )
         END IF
      END DO

*  If it would not be a unit mapping, create the PermMap.
      IF( NCON .GT. 0 ) THEN
         PMAP = AST_PERMMAP( NDIM, INPRM, NREQ, OUTPRM, CON, ' ',
     :                       STATUS )

*  Create a new Base Frame by picking the required axes from the
*  original Base Frame.
         NFRM = AST_PICKAXES( BFRM, NREQ, OUTPRM, MAP, STATUS )

*  Add this new Frame into the FrameSet, remembering the index of the
*  Current and Base Frames.
         ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )
         IBASE = AST_GETI( IWCS, 'BASE', STATUS )
         CALL AST_ADDFRAME( IWCS, AST__BASE, PMAP, NFRM, STATUS )

*  Get the index of the newly added Frame.
         INEW = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Re-instate the original Current Frame.
         CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )

*  Make the new Frame the Base Frame.
         CALL AST_SETI( IWCS, 'BASE', INEW, STATUS )

*  Set the domain of the original Base Frame to POLNDBASE.
         CALL AST_SETC( BFRM, 'DOMAIN', 'POLNDBASE', STATUS )

      END IF

*  If the Base Frame has no Domain value, give it a default Domain based
*  on the names of the catalogue columns.
      BFRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )
      IF( .NOT. AST_TEST( BFRM, 'DOMAIN', STATUS ) ) THEN
         DOM = ' '
         IAT = 0

         DO J = 1, NDIM
            IF( CONST( J ) .EQ. AST__BAD ) THEN
               IF( IAT .GT. 0 ) CALL CHR_APPND( '-', DOM, IAT )
               CALL CAT_TIQAC( GI( J ), 'NAME', NAME, STATUS )
               CALL CHR_APPND( NAME, DOM, IAT )
            END IF
         END DO

         CALL AST_SETC( BFRM, 'DOMAIN', DOM( : IAT ), STATUS )
      END IF

*  Now modify the Current Frame so that it also has 2 axes.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
      IF( AST_GETI( CFRM, 'NAXES', STATUS ) .EQ. NDIM ) THEN

*  Transform a point from the 2D Base Frame to the nD Current Frame.
         TEST( 1, 1 ) = 0.0D0
         TEST( 1, 2 ) = 0.0D0
         CALL AST_TRANN( IWCS, 1, 2, 1, TEST, .TRUE., NDIM, 1, TEST,
     :                   STATUS )

*  Set up the information required to create a PermMap which goes from
*  the NDIM-dimensional current Frame found above, to a new Frame
*  containing only the axes with indices equal to the indices of the chosen
*  base Frame axes.
         NREQ = 0
         NCON = 0
         DO J = 1, NDIM
            IF( CONST( J ) .EQ. AST__BAD ) THEN
               NREQ = NREQ + 1
               OUTPRM( NREQ ) = J
               INPRM( J ) = NREQ
            ELSE
               NCON = NCON + 1
               INPRM( J ) = -NCON
               CON( NCON ) = TEST( 1, J )
            END IF
         END DO

*  Create the PermMap.
         PMAP = AST_PERMMAP( NDIM, INPRM, NREQ, OUTPRM, CON, ' ',
     :                       STATUS )

*  Create a new Current Frame by picking the required axes from the
*  original Current Frame.
         NFRM = AST_PICKAXES( CFRM, NREQ, OUTPRM, MAP, STATUS )

*  Set the domain of the original Base Frame to POLNDCURRENT.
         CALL AST_SETC( CFRM, 'DOMAIN', 'POLNDCURRENT', STATUS )

*  Add this new Frame into the FrameSet.
         CALL AST_ADDFRAME( IWCS, AST__CURRENT, PMAP, NFRM, STATUS )

      END IF

*  Export the pointer from the current AST context.
      CALL AST_EXPORT( IWCS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
