      SUBROUTINE POL1_GTCTA( PARAM, EPARAM, CI, NDIM, GI, IWCS, STATUS )
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
*     This routine attempts to read an AST FrameSet with NDIM axes in the
*     Base Frame from the textual information stored with the supplied 
*     catalogue (class COMMENT). If not succesful, a default FrameSet is
*     created containing a single Frame. If the catalogue columns are
*     known to be RA/DEC values then a SkyFrame is used. Otherwise a simple 
*     Frame (with no Domain) is used. If the Base Frame obtained in this 
*     way (default or not) has no Domain, it is given a Domain equal to 
*     "<COLX>-<COLY>..." where <COLX>, <COLY>, ... are the names 
*     of the catalogue columns supplied in GI. Finally, the user is
*     allowed to change the Current Frame in the returned FrameSet using
*     the parameter specified by PARAM/EPARAM.

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
*        Ignored if NDIM is zero.
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

*  Arguments Given:
      CHARACTER PARAM*(*)
      CHARACTER EPARAM*(*)
      INTEGER CI
      INTEGER NDIM
      INTEGER GI( NDIM )

*  Arguments Returned:
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN          
      LOGICAL CHR_SIMLR

*  Local Variables:
      CHARACTER CVAL*255         ! Textual attribute value
      CHARACTER DOM*255          ! Domain for Base Frame
      CHARACTER NAME*(CAT__SZCMP)! Axis catalogue name
      CHARACTER NAME1*(CAT__SZCMP)! Axis 1 catalogue name
      CHARACTER NAME2*(CAT__SZCMP)! Axis 2 catalogue name
      CHARACTER SYS*3            ! Ref. frame for RA/DEC values
      DOUBLE PRECISION EP        ! Epoch of RA/DEC values
      DOUBLE PRECISION EQ        ! Equinox of RA/DEC values
      INTEGER DECAX              ! Index of catalogue DEC axis
      INTEGER FRM                ! Pointer to a Frame
      INTEGER GC                 ! CAT identifier for a catalogue parameter
      INTEGER I                  ! Axis index
      INTEGER IAT                ! No. of characters in string
      INTEGER ICURR              ! Original Current Frame index
      INTEGER PERM(2)            ! Axis permutation array
      INTEGER RAAX               ! Index of catalogue RA axis
      LOGICAL DONE               ! Have we read enough AST Objects?
*.

*  Initialise.
      IWCS = AST__NULL

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Reset the pointer for the next item of textual information to be read
*  from the catalogue.
      CALL CAT_RSTXT( CI, STATUS )

*  Read Objects from the catalogue until a FrameSet is obtained in which
*  the Base Frame has NDIM axes, or no more Objects are left.
      DONE = .FALSE.
      DO WHILE( .NOT. DONE .AND. STATUS .EQ. SAI__OK ) 
         CALL KPG1_RCATW( CI, IWCS, STATUS )

         IF( IWCS .NE. AST__NULL ) THEN
            IF( AST_ISAFRAMESET( IWCS, STATUS ) ) THEN
               IF( AST_GETI( IWCS, 'NIN', STATUS ) .EQ. NDIM ) THEN
                  DONE = .TRUE.
               ELSE
                  CALL AST_ANNUL( IWCS, STATUS )
               END IF
            ELSE
               CALL AST_ANNUL( IWCS, STATUS )
            END IF

         ELSE
            DONE = .TRUE.
         END IF         

      END DO

*  If we have not read a FrameSet succesfully from the catalogue, choose
*  a Frame to form the default FrameSet.
      IF ( IWCS .EQ. AST__NULL ) THEN
         FRM = AST__NULL 

*  If the required Base Frame is 2D, see if the catalogue columns specify
*  a sky coordinate Frame (i.e. a CURSA "Target List" - see SUN/190).
         IF( NDIM .EQ. 2 ) THEN

*  We require columns with names "RA" and "DEC".
            CALL CAT_TIQAC( GI( 1 ), 'NAME', NAME1, STATUS )
            CALL CAT_TIQAC( GI( 2 ), 'NAME', NAME2, STATUS )

            RAAX = 0
            IF( CHR_SIMLR( NAME1, 'RA' ) ) RAAX = 1
            IF( CHR_SIMLR( NAME2, 'RA' ) ) RAAX = 2

            DECAX = 0
            IF( CHR_SIMLR( NAME1, 'DEC' ) ) DECAX = 1
            IF( CHR_SIMLR( NAME2, 'DEC' ) ) DECAX = 2

*  If RA and DEC columns have been supplied, create a SkyFrame.
            IF( RAAX .NE. 0 .AND. DECAX .NE. 0 ) THEN
               FRM = AST_SKYFRAME( 'DOMAIN=SKY', STATUS )

*  Swap the SkyFrames axes if required to ensure that the axes are in the 
*  same order as the catalogue columns.
               IF( RAAX .EQ. 2 ) THEN
                  PERM( 1 ) = 2
                  PERM( 2 ) = 1
                  CALL AST_PERMAXES( FRM, PERM, STATUS )
               END IF

*  Look for the EPOCH and EQUINOX catalogue parameters. If supplied
*  set the corresponding attributes in the SkyFrame, and then retrieve
*  them as floating point values.
               IF( STATUS .EQ. SAI__OK ) THEN 

                  CALL CAT_TIDNT( CI, 'EQUINOX', GC, STATUS )
                  IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
   
                  IF( GC .NE. CAT__NOID ) THEN
                     CALL CAT_TIQAC( GC, 'VALUE', CVAL, STATUS )
                     CALL AST_SETC( FRM, 'EQUINOX', CVAL, STATUS )
                     EQ = AST_GETD( FRM, 'EQUINOX', STATUS )
                  ELSE
                     EQ = -1.0D0
                  END IF
   
                  CALL CAT_TIDNT( CI, 'EPOCH', GC, STATUS )
                  IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
   
                  IF( GC .NE. CAT__NOID ) THEN
                     CALL CAT_TIQAC( GC, 'VALUE', CVAL, STATUS )
                     CALL AST_SETC( FRM, 'EPOCH', CVAL, STATUS )
                     EP = AST_GETD( FRM, 'EPOCH', STATUS )
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

                  CALL AST_SETC( FRM, 'SYSTEM', SYS, STATUS )

*  Tell the user what is being assumed about the RA/DEC values.
                  EP = AST_GETD( FRM, 'EPOCH', STATUS )
                  IF( EP .LT. 1984.0D0 ) THEN
                     CALL MSG_SETC( 'EP', 'B' )
                  ELSE
                     CALL MSG_SETC( 'EP', 'J' )
                  END IF
                  CALL MSG_SETR( 'EP', REAL( EP ) )

                  EQ = AST_GETD( FRM, 'EQUINOX', STATUS )
                  IF( EQ .LT. 1984.0D0 ) THEN
                     CALL MSG_SETC( 'EQ', 'B' )
                  ELSE
                     CALL MSG_SETC( 'EQ', 'J' )
                  END IF
                  CALL MSG_SETR( 'EQ', REAL( EQ ) )

                  CALL MSG_SETC( 'REF', SYS )

                  CALL MSG_BLANK( STATUS )
                  CALL MSG_OUT( 'POL1_GTCTA_1', '  Assuming catalogue'//
     :                          ' RA/DEC values are ^REF, Equinox=^EQ'//
     :                          ', Epoch=^EP', STATUS )
                  CALL MSG_BLANK( STATUS )

               END IF

            END IF

         END IF

*  If we could not produce a SkyFrame, create a simple Frame.
         IF( FRM .EQ. AST__NULL ) FRM = AST_FRAME( NDIM, ' ', STATUS )

*  Create the default FrameSet holding the Frame (or SkyFrame) created
*  above, and then annul the Frame.
         IWCS = AST_FRAMESET( FRM, ' ', STATUS )   
         CALL AST_ANNUL( FRM, STATUS )

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

*  If the Base Frame has no Domain value, give it a default Domain based
*  on the names of the catalogue columns.
      FRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )
      IF( .NOT. AST_TEST( FRM, 'DOMAIN', STATUS ) ) THEN 

         CALL CAT_TIQAC( GI( 1 ), 'NAME', DOM, STATUS )
         IAT = CHR_LEN( DOM )

         DO I = 2, NDIM
            CALL CAT_TIQAC( GI( I ), 'NAME', NAME, STATUS )
            CALL CHR_APPND( '-', DOM, IAT )  
            CALL CHR_APPND( NAME, DOM, IAT )  
         END DO

         CALL AST_SETC( FRM, 'DOMAIN', DOM, STATUS )
      END IF

*  Give the user the chance to change the Current Frame. 
      CALL KPG1_ASFRM( PARAM, EPARAM, IWCS, 'PIXEL', 'AXIS', .TRUE.,
     :                 STATUS )

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
