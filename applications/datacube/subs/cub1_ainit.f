      SUBROUTINE CUB1_AINIT( INDF, IWCS, FRCUR, PFRAME, CFRAME,
     :                       MAPC, NVIN, NVOUT, NFRM, STATUS )
*+
*  Name:
*     CUB1_AINIT

*  Purpose:
*     Gets commonly used variables from the NDF AST extension

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CUB1_AINIT( INDF, FRCUR, PFRAME, CFRAME, MAPC, 
*    :                 NVIN, NVOUT, NFRM, STATUS )

*  Arguments:
*     INDF = _INTEGER (Given)
*        Pointer to the input NDF
*     IWCS = _INTEGER (Given)
*        Pointer to the WCS extension of the NDF
*     FRCUR = _INTEGER (Returned)
*        Pointer to the current Frame
*     PFRAME = _INTEGER (Returned)
*        Index of the PIXEL frame
*     CFRAME = _INTEGER (Returned)
*        Index of the Current frame
*     MAPC = _INTEGER (Returned)
*        Current mapping in use
*     NVIN = _INTEGER (Returned)
*        Number of input variables 
*     NVOUT = _INTEGER (Returned)
*        Number of output variables 
*     NFRM = _INTEGER (Returned)
*        Number of Frames in the FrameSet
*     STATUS = _INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Queries the NDF WCS extension and returns information about the
*     AST Frameset contained in it.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

*  Authors:
*     AALLAN: Alasdair Allan (STARLINK, Keele University)

*  History:
*      05-SEP-2000 (AALLAN):
*        Original version.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         
      INCLUDE 'NDF_PAR'        
      INCLUDE 'AST_PAR'
      INCLUDE 'CUB1_PAR'        ! Package constants

*  Arguments Given:
      INTEGER INDF              ! Pointer to the input NDF

*  Arguments Returned:
      INTEGER IWCS              ! Pointer to the WCS extension of the NDF
      INTEGER FRCUR             ! Pointer to the current Frame
      INTEGER PFRAME            ! Index of the PIXEL frame
      INTEGER CFRAME            ! Index of the Current frame
      INTEGER MAPC              ! Current mapping in use
      INTEGER NVIN              ! Number of input variables 
      INTEGER NVOUT             ! Number of output variables 
      INTEGER NFRM              ! Number of frames in frameset
      
*  Status:
      INTEGER STATUS            ! Global status
      
*  External References:        

*  Internal References:
      
*  Local Variables:
      INTEGER IFRM              ! Loop counter over AST Frames
      INTEGER FRM               ! Pointer to AST Frame under consideration
      LOGICAL SWCS              ! WCS present?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      
*  Check for WCS extension
      CALL NDF_STATE(INDF, 'WCS', SWCS, STATUS)
      IF( .NOT. SWCS ) THEN
         STATUS = SAI__ERROR 
         CALL NDF_MSG( 'NDFNAME', INDF )
         CALL ERR_REP( 'CUB1_AINIT_ERR',
     :                 'CUB1_AINIT: NDF ^NDFNAME '//
     :'does not have a WCS extension.',
     :                  STATUS )
         GOTO 999
      ENDIF

*  Get a pointer to the WCS extension
      CALL NDF_GTWCS(INDF, IWCS, STATUS)

*  Validate the transformation
         IF ( IWCS .EQ. AST__NULL ) THEN
             STATUS = SAI__ERROR
             CALL NDF_MSG( 'NDFNAME', INDF )
             CALL ERR_REP( 'CUB1_AINIT_NOAST', '  NDF ^NDFNAME '//
     :'does not have a valid WCS extension.',STATUS ) 
             GO TO 940 
         ELSE IF ( AST_GETC( IWCS, 'Class', STATUS ) .NE. 'FrameSet' ) 
     :      THEN
             STATUS = SAI__ERROR
             CALL NDF_MSG( 'NDFNAME', INDF )
             CALL ERR_REP( 'CUB1_AINIT_FRAME', '  NDF ^NDFNAME '//
     :'does not have a WCS extension with class FrameSet.', STATUS)
             GO TO 940             
         ENDIF
          
*  We have a valid WCS component, get pointer to the current FrameSet
         FRCUR = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Lets find out which frame contains the PIXEL domain (its going to be
*  frame 2, but we may as well do it properly) in the output WCS frameset.
         NFRM = AST_GETI( IWCS, 'Nframe', STATUS )
         DO IFRM = 1, NFRM
            FRM = AST_GETFRAME( IWCS, IFRM, STATUS )
            IF( AST_GETC( FRM, 'Domain', STATUS ) 
     :          .EQ. 'PIXEL' ) PFRAME = IFRM
         END DO
      
*  Get mapping between PIXEL and Current frames.
         MAPC = AST_GETMAPPING( IWCS, PFRAME, AST__CURRENT, STATUS )
         MAPC = AST_SIMPLIFY( MAPC, STATUS )

*  Get the index of the current frame for future use 
         CFRAME = AST_GETI( IWCS, 'Current', STATUS )

*  Obtain the number of input and output co-ordinates for the Mapping
         NVIN = AST_GETI( MAPC, 'Nin', STATUS )
         NVOUT = AST_GETI( MAPC, 'Nout', STATUS ) 

*  Clean up - Arrive here if an error occurs
*  ========================================= 

940   CONTINUE

960   CONTINUE

999   CONTINUE

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CUB1_AINIT_ERR',
     :                 'CUB1_AINIT: Error accessing the WCS component.',
     :                  STATUS )
      END IF

*  Time at the bar please...
      END







