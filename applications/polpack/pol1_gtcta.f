      SUBROUTINE POL1_GTCTA( PARAM, EPARAM, CI, NDIM, GI, CONST,
     :                       IWCS, STATUS )
*+
*  Name:
*     POL1_GTCTA

*  Purpose:
*     Attempt to read an AST FrameSet from a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_GTCTA( PARAM, EPARAM, CI, NDIM, GI, IWCS, STATUS )

*  Description:
*     This routine attempts to read an AST FrameSet from the textual 
*     information stored with the supplied catalogue (class COMMENT). To
*     be usable the Base Frame of the FrameSet must contain axes with
*     symbols equal to the names of the supplied catalogue columns (GI).
*     If not succesful, a default FrameSet is created containing a single 
*     Frame. If the catalogue columns are known to be RA/DEC values then 
*     a SkyFrame is used. Otherwise a simple Frame (with no Domain) is used. 
*     If the Base Frame obtained in this way (default or not) has no Domain, 
*     it is given a Domain equal to "<COLX>-<COLY>..." where <COLX>, <COLY>, 
*     ... are the names of the catalogue columns supplied in GI. Finally, 
*     the user is allowed to change the Current Frame in the returned 
*     FrameSet using the parameter specified by PARAM/EPARAM.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use when setting the Current Frame
*        in the returned FrameSet. 
*     EPARAM = CHARACTER * ( * ) (Given)
*        The name of the epoch parameter to use when setting the Current Frame
*        in the returned FrameSet. 
*     CI = INTEGER (Given)
*        A CAT identifier (see SUN/181) for the supplied catalogue.
*     NDIM = INTEGER (Given)
*        The number of catalogue columns supplied.
*     GI( NDIM ) = INTEGER (Given)
*        An array of CAT identifiers for columns within the catalogue.
*        Ignored if NDIM is zero. The Base Frame of the returned FrameSet
*        is spanned by these two columns.
*     IWCS = INTEGER (Returned)
*        An AST pointer to the returned FrameSet. AST__NULL is returned if 
*        an error occurs. 
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1998 (DSB):
*        Original version.
*     6-AUG-1998 (DSB):
*        Changed so that the Base Frame is left unchanged in the returned
*        FrameSet.
*     10-NOV-1998 (DSB):
*        Added argument EPARAM. Change name from KPG1_ to POL1_. Call
*        KPG1_ASFRM instead of KPG1_GTFRM to set current Frame.
*     16-SEP-1999 (DSB):
*        Added TOKEN argument to KPG1_ASFRM calls.
*     2-FEB-2001 (DSB):
*        Changed to allow Base Frame to be 3D.
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
      CHARACTER PARAM*(*)
      CHARACTER EPARAM*(*)
      INTEGER CI
      INTEGER NDIM
      INTEGER GI( NDIM )
      DOUBLE PRECISION CONST( NDIM )

*  Arguments Returned:
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR

*  Local Variables:
      CHARACTER ATTR*20          ! Attribute name
      CHARACTER CVAL*255         ! Textual attribute value
      CHARACTER DOM*255          ! Domain for Base Frame
      CHARACTER NAME( NDF__MXDIM )*(CAT__SZCMP)! Axis name
      CHARACTER SYM*20           ! Symbol attribute value
      CHARACTER SYS*3            ! Ref. frame for RA/DEC values
      CHARACTER TEXT*30          ! Attribute value
      DOUBLE PRECISION CON( NDF__MXDIM )! Good constants
      DOUBLE PRECISION EP        ! Epoch of RA/DEC values
      DOUBLE PRECISION EQ        ! Equinox of RA/DEC values
      INTEGER AXES( NDF__MXDIM ) ! Indices of axes to use
      INTEGER BFRM               ! Pointer to a Frame
      INTEGER DECAX              ! Index of catalogue DEC axis
      INTEGER FRM                ! Pointer to a Frame
      INTEGER FRM1               ! Pointer to a Frame
      INTEGER FRM2               ! Pointer to a Frame
      INTEGER GC                 ! CAT identifier for a catalogue parameter
      INTEGER I                  ! Axis index
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
      INTEGER NUSED              ! Number or axes allocated so far
      INTEGER OUTPRM( NDF__MXDIM )! Output axis permutation
      INTEGER PERM( NDF__MXDIM ) ! Axis permutation array
      INTEGER PMAP               ! Pointer to PermMap
      INTEGER RAAX               ! Index of catalogue RA axis
      LOGICAL DONE               ! Have we read enough AST Objects?
*.

*  Initialise.
      IWCS = AST__NULL

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  We require columns with names given by GI.
      DO I = 1, NDIM      
         CALL CAT_TIQAC( GI( I ), 'NAME', NAME( I ), STATUS )
      END DO

*  Reset the pointer for the next item of textual information to be read
*  from the catalogue.
      CALL CAT_RSTXT( CI, STATUS )

*  Read Objects from the catalogue until a FrameSet is obtained in which
*  the Base Frame has axes corresponding to columns supplied in GI.
      DONE = .FALSE.
      DO WHILE( .NOT. DONE .AND. STATUS .EQ. SAI__OK ) 
         CALL KPG1_RCATW( CI, IWCS, STATUS )

         IF( IWCS .NE. AST__NULL ) THEN
            IF( AST_ISAFRAMESET( IWCS, STATUS ) .AND. 
     :          AST_GETI( IWCS, 'NIN', STATUS ) .EQ. NDIM ) THEN

*  Find the indices of the Base Frame axes with symbols equal to the
*  names of the columns supplied in GI.
               BFRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )
               DO J = 1, NDIM
                  AXES( J ) = 0
               END DO

               DO I = 1, AST_GETI( BFRM, 'NAXES', STATUS )

                  ATTR = 'Symbol('
                  IAT = 7
                  CALL CHR_PUTI( I, ATTR, IAT )
                  CALL CHR_APPND( ')', ATTR, IAT )
                  SYM = AST_GETC( BFRM, ATTR( : IAT ), STATUS )

                  DO J = 1, NDIM
                     IF( SYM .EQ. NAME( J ) ) AXES( J ) = I
                  END DO

               END DO

               CALL AST_ANNUL( BFRM, STATUS )

               DONE = .TRUE.
               DO J = 1, NDIM
                  IF( AXES( J ) .EQ. 0 ) DONE = .FALSE.
               END DO

               IF( .NOT. DONE ) CALL AST_ANNUL( IWCS, STATUS )

            ELSE
               CALL AST_ANNUL( IWCS, STATUS )
            END IF

         ELSE
            DONE = .TRUE.
         END IF         

      END DO

*  If we have not read a FrameSet succesfully from the catalogue, create
*  a Frame to form the default FrameSet.
      IF ( IWCS .EQ. AST__NULL ) THEN

*  See if the supplied column names include RA and DEC columns.
         RAAX = 0
         DECAX = 0
         DO J = 1, NDIM
            IF( CHR_SIMLR( NAME( I ), 'RA' ) ) THEN
               RAAX = I
            ELSE IF( CHR_SIMLR( NAME( I ), 'DEC' ) ) THEN
               DECAX = I
            END IF
         END DO

*  If so, we create a SkyFrame (i.e. a CURSA "Target List" - see SUN/190).
         FRM1 = AST__NULL 
         IF( RAAX .NE. 0 .AND. DECAX .NE. 0 ) THEN
            FRM1 = AST_SKYFRAME( ' ', STATUS )

*  Look for the EPOCH and EQUINOX catalogue parameters. If supplied
*  set the corresponding attributes in the SkyFrame, and then retrieve
*  them as floating point values.
            IF( STATUS .EQ. SAI__OK ) THEN 
               CALL CAT_TIDNT( CI, 'EQUINOX', GC, STATUS )
               IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
            END IF

            IF( GC .NE. CAT__NOID ) THEN
               CALL CAT_TIQAC( GC, 'VALUE', CVAL, STATUS )
               CALL AST_SETC( FRM1, 'EQUINOX', CVAL, STATUS )
               EQ = AST_GETD( FRM1, 'EQUINOX', STATUS )
            ELSE
               EQ = -1.0D0
            END IF

            CALL CAT_TIDNT( CI, 'EPOCH', GC, STATUS )
            IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

            IF( GC .NE. CAT__NOID ) THEN
               CALL CAT_TIQAC( GC, 'VALUE', CVAL, STATUS )
               CALL AST_SETC( FRM1, 'EPOCH', CVAL, STATUS )
               EP = AST_GETD( FRM1, 'EPOCH', STATUS )
            ELSE
               EP = -1.0D0
            END IF

*  Choose the reference frame (FK4 or FK5). 
            IF( EQ .NE. -1.0D0 ) THEN
               IF( EQ .LT. 1984.0D0 ) THEN
                  SYS = 'FK4'
               ELSE
                  SYS = 'FK5'
               END IF

            ELSE IF( EP .NE. -1.0D0 ) THEN
               IF( EP .LT. 1984.0D0 ) THEN
                  SYS = 'FK4'
               ELSE
                  SYS = 'FK5'
               END IF

            ELSE
               SYS = 'FK5'
            END IF

            CALL AST_SETC( FRM1, 'SYSTEM', SYS, STATUS )

*  Tell the user what is being assumed about the RA/DEC values.
            EP = AST_GETD( FRM1, 'EPOCH', STATUS )
            IF( EP .LT. 1984.0D0 ) THEN
               CALL MSG_SETC( 'EP', 'B' )
            ELSE
               CALL MSG_SETC( 'EP', 'J' )
            END IF
            CALL MSG_SETR( 'EP', REAL( EP ) )

            EQ = AST_GETD( FRM1, 'EQUINOX', STATUS )
            IF( EQ .LT. 1984.0D0 ) THEN
               CALL MSG_SETC( 'EQ', 'B' )
            ELSE
               CALL MSG_SETC( 'EQ', 'J' )
            END IF
            CALL MSG_SETR( 'EQ', REAL( EQ ) )

            CALL MSG_SETC( 'REF', SYS )

            CALL MSG_BLANK( STATUS )
            CALL MSG_OUT( 'POL1_GTCTA_1', '  Assuming catalogue'//
     :                    ' RA/DEC values are ^REF, Equinox=^EQ'//
     :                    ', Epoch=^EP', STATUS )
            CALL MSG_BLANK( STATUS )

*  Indicate how many axes have been assigned to a Frame.
            NUSED = 2
         ELSE
            NUSED = 0
         END IF

*  If there are no non-celestial axes, clone the above SkyFrame.
         IF( NUSED .EQ. NDIM ) THEN
            FRM = AST_CLONE( FRM1, STATUS )

*  If necessary, create a simple Frame to hold any extra axes.
         ELSE
            FRM2 = AST_FRAME( NDIM - NUSED, ' ', STATUS )

*  If a SkyFrame was produced above, produce a compound Frame holding all
*  the axes.
            IF( FRM1 .NE. AST__NULL ) THEN
               FRM = AST_CMPFRAME( FRM1, FRM2, ' ', STATUS )

*  Otherwise, clone the simple Frame.
            ELSE
               FRM = AST_CLONE( FRM2, STATUS )
            END IF

         END IF

*  Permute the axes of the total Frame to ensure that the indices of the 
*  RA and DEC axes are the same as the indicies of the RA and DEC columns 
*  in GI.
         IF( FRM1 .NE. AST__NULL ) THEN
             J = 3
             DO I = 1, NDIM
                IF( RAAX .EQ. I ) THEN
                   PERM( I ) = 1
                ELSE IF( DECAX .EQ. I ) THEN
                   PERM( I ) = 2
                ELSE
                   PERM( I ) = J 
                   J = J + 1
                END IF
             END DO

             CALL AST_PERMAXES( FRM, PERM, STATUS ) 

         END IF

*  Ensure that all the axes have the correct symbols, label and units
*  attributes (these will already be OK if the axes belong to a SkyFrame).
         DO I = 1, NDIM
            IF( I .NE. RAAX .AND. I .NE. DECAX ) THEN 
               ATTR = 'Symbol('
               IAT = 7
               CALL CHR_PUTI( I, ATTR, IAT )
               CALL CHR_APPND( ')', ATTR, IAT )
               CALL AST_SETC( FRM, ATTR( : IAT ), NAME( I ), STATUS )

               TEXT = ' '
               CALL POL1_TIQAC( GI( I ), 'COMMENTS', TEXT, STATUS )
               IF( TEXT .EQ. ' ' ) THEN
                  IAT = 0
                  CALL CHR_APPND( NAME( I ), TEXT, IAT )
                  CALL CHR_APPND( ' axis', TEXT, IAT )
               END IF

               ATTR = 'Label('
               IAT = 6
               CALL CHR_PUTI( I, ATTR, IAT )
               CALL CHR_APPND( ')', ATTR, IAT )
               CALL AST_SETC( FRM, ATTR( : IAT ), TEXT( : IAT ), 
     :                        STATUS )

               TEXT = ' '
               CALL POL1_TIQAC( GI( I ), 'UNITS', TEXT, STATUS )
               IF( TEXT .NE. ' ' ) THEN
                  ATTR = 'Units('
                  IAT = 6
                  CALL CHR_PUTI( I, ATTR, IAT )
                  CALL CHR_APPND( ')', ATTR, IAT )
                  CALL AST_SETC( FRM, ATTR( : IAT ), TEXT, STATUS )
               END IF

            END IF
         END DO

*  Create the default FrameSet holding the above Frame.
         IWCS = AST_FRAMESET( FRM, ' ', STATUS )

*  Add in a POLANAL Frame defining the reference direction.
         CALL POL1_PTANG( 0.0, IWCS, STATUS )

*  If a FrameSet was read, check to see if it was created by V1.0 of
*  POLPACK which left the GRID Frame as the Base Frame. These catalogues
*  are marked by having Frames with Domain OLDGRID in the WCS FrameSet. 
*  Such FrameSets are changed by making the PIXEL Frame the Base Frame.
      ELSE 
         ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )

         IF( AST_FINDFRAME( IWCS, AST_FRAME( 2, ' ', STATUS ),
     :                      'OLDGRID', STATUS ) .NE. AST__NULL .AND.
     :       AST_GETC( AST_GETFRAME( IWCS, AST__BASE, STATUS ), 
     :                 'DOMAIN', STATUS ) .EQ. 'GRID' ) THEN

            IF( AST_FINDFRAME( IWCS, AST_FRAME( 2, ' ', STATUS ),
     :                         'PIXEL', STATUS ) .NE. AST__NULL ) THEN
               CALL AST_SETI( IWCS, 'BASE', 
     :                        AST_GETI( IWCS, 'CURRENT', STATUS ),
     :                        STATUS )
            END IF

         END IF                  

         CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )

      END IF

*  Permute the Base Frame axes so that they are in the same order as the GI
*  array.
      BFRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )
      CALL AST_PERMAXES( BFRM, AXES, STATUS ) 

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
               CALL CHR_APPND( NAME( J ), DOM, IAT )
            END IF
         END DO

         CALL AST_SETC( BFRM, 'DOMAIN', DOM( : IAT ), STATUS )
      END IF

*  Give the user the chance to change the Current Frame. 
      CALL KPG1_ASFRM( PARAM, EPARAM, IWCS, 'PIXEL', 'AXIS', .TRUE.,
     :                 'catalogue', STATUS )

*  If an error has occurred, annul the returned FrameSet pointer. Otherwise
*  export the pointer (if it is not null) from the current AST context.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL AST_ANNUL( IWCS, STATUS )

      ELSE IF( IWCS .NE. AST__NULL ) THEN
         CALL AST_EXPORT( IWCS, STATUS )

      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
