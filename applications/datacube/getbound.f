      SUBROUTINE GETBOUND(STATUS)
*+
*  Name:
*     GETBOUND

*  Purpose:
*     Reports the minimum and maximum bounds of a N-dimensional NDF in the
*     current WCS Frame.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GETBOUND( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Simple application designed to be run from inside a user written
*     csh script or from the command line, it will report the extent of the
*     axes of the datacube in the current AST frameset. The result will be
*     output to the screen and stored in the parameter system.

*  Usage:
*     getbound in=file

*  ADAM Parameters:
*     IN = NDF (Read)
*        A IFU 3D NDF datacube.
*     NDIM = _INTEGER (Write)
*        The number of dimensions of the NDF.
*     LBOUND( ) = _INTEGER (Write)
*        The lower bounds of the NDF.
*     UBOUND( ) = _INTEGER (Write)
*        The upper bounds of the NDF.

*  Examples:
*     getbound in=smirfsp0
*        Returns the minimum and maximum bounds in the current WCS
*        frame set of the file smirfsp0 on all axes via the parameter 
*        system and to the console.

*  Authors:
*     AALLAN: Alasdair Allan (STARLINK, Keele University)

*  History:
*      05-SEP-2000 (AALLAN):
*        Original version.
*     20-NOV-2000 (AALLAN):
*       Incorporated changes made to source at ADASS

*  Bugs:
*     {note_any_bugs_here}

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

*-
      IMPLICIT NONE
      
      INCLUDE 'SAE_PAR'
      INCLUDE 'NDF_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'PAR_PAR'
      
*  General charater variables
      CHARACTER*(120) TEXT      ! Temporary string for message output

*  Loop counters            
      INTEGER IDIM              ! Loop counter over NDF dimensions
     
*  Pointers
      INTEGER INDF              ! Pointer to the input NDF
      INTEGER IWCS              ! Pointer to the WCS extension of the NDF
      INTEGER FRCUR             ! Pointer to the current Frame

*  Input NDF
      INTEGER IDIMS(NDF__MXDIM) ! Dimensions of the input NDF
      INTEGER ILBND(NDF__MXDIM) ! Input NDF lower bounds
      INTEGER IUBND(NDF__MXDIM) ! Input NDF upper bounds
      INTEGER NDIMI             ! Number of dimensions in input NDF

*  AST related
      INTEGER NFRM              ! Number of frames in frameset
      INTEGER CFRAME            ! Index of the Current frame
      INTEGER PFRAME            ! Index of the PIXEL frame
      INTEGER MAPC              ! Current mapping in use
      INTEGER NVIN              ! Number of input variables 
      INTEGER NVOUT             ! Number of output variables 
      
*  Input co-ordinates
      DOUBLE PRECISION ASTART( NDF__MXDIM ) ! Start co-ord of each axis
      DOUBLE PRECISION AEND( NDF__MXDIM )   ! End co-ord of each axis
      
*  Mapped co-ordinates
      DOUBLE PRECISION DDLBND(NDF__MXDIM)   ! Co-ord lower bound of input NDF 
      DOUBLE PRECISION DDUBND(NDF__MXDIM)   ! Co-ord upper bounds of output NDF
      DOUBLE PRECISION DDXL( NDF__MXDIM )   ! Co-ord of input pt gives lower bnd
      DOUBLE PRECISION DDXU( NDF__MXDIM )   ! Co-ord of input pt gives upper bnd      
*  Logical flags

*  Status:
      INTEGER STATUS  

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN    
          
*  Start of code block.
      CALL NDF_BEGIN
      CALL AST_BEGIN(STATUS)
            
*  Associate the input NDF
      CALL NDF_ASSOC('IN','READ',INDF,STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 999
      
*  Grab information about the AST FrameSet
      CALL CUB1_AINIT( INDF, IWCS, FRCUR, PFRAME, CFRAME,
     :                 MAPC, NVIN, NVOUT, NFRM, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN      
          CALL ERR_REP( 'GETWAVE_ERR',
     :                  'GETWAVE: Problems with accessing AST FrameSet', 
     :                  STATUS ) 
          GOTO 940
      END IF 
      
*  Dimensions.
      CALL NDF_DIM(INDF, NDF__MXDIM, IDIMS, NDIMI, STATUS)

*  Bounds.
      CALL NDF_BOUND(INDF, NDF__MXDIM, ILBND, IUBND, NDIMI, STATUS) 

*  Convert to double precision
      DO IDIM = 1, NVIN
         ASTART( IDIM ) = DBLE( ILBND(IDIM) ) - 0.5
         AEND( IDIM )   = DBLE( IUBND(IDIM) ) - 0.5
      END DO        
        
*  Find the bounding box of the transformed co-ordinates
      DO IDIM = 1, NVIN
         CALL AST_MAPBOX( MAPC, ASTART, AEND, .TRUE., IDIM,
     :                    DDLBND( IDIM ), DDUBND( IDIM ), DDXL, DDXU,
     :                    STATUS )
      END DO        

*  Report input and output bounding values
      CALL MSG_OUT(' ', ' ', STATUS)
      
      WRITE(TEXT, '(A)') '    Pixel Bounding Values:'
      CALL MSG_OUT(' ', TEXT, STATUS)
      WRITE(TEXT, '(A)') '    ---------------------'
      CALL MSG_OUT(' ', TEXT, STATUS)
      WRITE(TEXT, '(A, I1)') '    Dimensions: ', NDIMI
      CALL MSG_OUT(' ', TEXT, STATUS)
      DO IDIM = 1, NDIMI
            IF ( IDIM .NE. 1 ) CALL MSG_SETC( 'BOUNDS', ',' )
            CALL MSG_SETI( 'BOUNDS', ILBND( IDIM ) )
            CALL MSG_SETC( 'BOUNDS', ':' )
            CALL MSG_SETI( 'BOUNDS', IUBND( IDIM ) )
      ENDDO
      WRITE(TEXT, '(A)')     '    Pixel Bounds: ^BOUNDS'
      CALL MSG_OUT(' ', TEXT, STATUS)
      CALL MSG_OUT(' ', ' ', STATUS)
      
      WRITE(TEXT, '(A)') '    WCS Bounding Values:'
      CALL MSG_OUT(' ', TEXT, STATUS)
      WRITE(TEXT, '(A)') '    -------------------'
      CALL MSG_OUT(' ', TEXT, STATUS)
      WRITE(TEXT, '(A, I1)') '    Dimensions: ', NDIMI
      CALL MSG_OUT(' ', TEXT, STATUS)
      DO IDIM = 1, NDIMI
            CALL MSG_SETD( 'BOUNDS', DDLBND( IDIM ) )
            CALL MSG_SETC( 'BOUNDS', ':' )
            CALL MSG_SETD( 'BOUNDS', DDUBND( IDIM ) )
            CALL MSG_SETI( 'IDIM', IDIM )
            WRITE(TEXT, '(A)')     '    WCS Axis ^IDIM Bounds: ^BOUNDS'
            CALL MSG_OUT(' ', TEXT, STATUS)
      ENDDO
      CALL MSG_OUT(' ', ' ', STATUS)

*  Output boundary values to parameters

      CALL PAR_PUT1I( 'DIMS', NVOUT, IDIMS, STATUS )
      CALL PAR_PUT1D( 'LBOUND', NVOUT, DDLBND, STATUS )
      CALL PAR_PUT1D( 'UBOUND', NVOUT, DDUBND, STATUS )
 
*  Close things down.

940   CONTINUE
*  Tidy up any remaining AST FrameSets
      CALL AST_ANNUL( MAPC, STATUS )      
      CALL AST_ANNUL( FRCUR, STATUS )
      
*  Tidy up AST and NDF states
      CALL AST_ANNUL( IWCS, STATUS)
      CALL AST_END(STATUS)
      CALL NDF_END(STATUS)
      
999   CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'GETWAVE_ERR',
     :                  'GETWAVE: Fatal Error', STATUS ) 
      END IF

*  Everything must end sometime
      END
      
      
      
