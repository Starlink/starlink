      SUBROUTINE PUTAXIS(STATUS)
*+
*  Name:
*     PUTAXIS

*  Purpose:
*     Builds an AXIS extension in an NDF from the information present in
*     a WCS extension so that the file can be used inside FIGARO and other
*     non-WCS aware applications.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PUTAXIS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Simple application designed to be run from inside a user written
*     csh script or from the command line, it will build an AXIS extension
*     for a specified NDF from the WCS information already present in the 
*     NDF. It can cope with N-dimensional NDFs.

*  Usage:
*     putaxis in=file spectral=num

*  ADAM Parameters:
*     IN = _NDF (Read)
*        A IFU 3D NDF datacube.
*     SPECTRAL = _INTEGER (Read)
*        The axis number of the spectral dispersion axis. [3]

*  Examples:
*     putaxis in=smirfsp0 spectral=3
*        Builds an NDF AXIS extension in the file smirfsp0 from the existing
*        WCS extension. The 3rd axis is specified as the LAMBDA axis and
*        will be labelled accordingly

*  Authors:
*     AALLAN: Alasdair Allan (STARLINK, Keele University)

*  History:
*     04-SEP-2000 (AALLAN):
*        Original version.
*     06-SEP-2000 (AALLAN):
*        AST routines wrapped using GENERIC.
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_PAR'
      INCLUDE 'CUB1_PAR'                    ! Package constants
      
*  General charater variables
      CHARACTER*(120) TEXT                  ! Temporary str for message output

*  Loop counters            
      INTEGER IDIM                          ! Loop counter over NDF dimensions
      INTEGER IPIX                          ! Loop counter over pixels in axis
     
*  Pointers
      INTEGER INDF                          ! Pointer to the input NDF
      INTEGER IWCS                          ! Pointer to the WCS ext of the NDF
      INTEGER FRCUR                         ! Pointer to the current Frame
      INTEGER AXPNT                         ! Pointer to the AXIS centre arrays

*  Input NDF
      INTEGER IDIMS(NDF__MXDIM)             ! Dimensions of the input NDF
      INTEGER ILBND(NDF__MXDIM)             ! Input NDF lower bounds
      INTEGER IUBND(NDF__MXDIM)             ! Input NDF upper bounds
      INTEGER NDIMI                         ! Number of dimensions in input NDF
      INTEGER NPIX                          ! Number of pixels in the input NDF
      INTEGER ISIZ                          ! Input NDF dimension size

*  AXIS information
      CHARACTER*( DAT__SZTYP ) ATYPE        ! NDF array implementation type
      INTEGER AEL                           ! Number of AXIS elements mapped 
      INTEGER SPECTRAL                      ! Spectral dispersion axis

*  AST related
      INTEGER NFRM                          ! Number of frames in frameset
      INTEGER CFRAME                        ! Index of the Current frame
      INTEGER PFRAME                        ! Index of the PIXEL frame
      INTEGER MAPC                          ! Current mapping in use
      INTEGER NVIN                          ! Number of input variables 
      INTEGER NVOUT                         ! Number of output variables 
      
*  Input co-ordinates
      DOUBLE PRECISION ASTART( NDF__MXDIM ) ! Start co-ord of each axis
      DOUBLE PRECISION AEND( NDF__MXDIM )   ! End co-ord of each axis
      
*  Mapped co-ordinates
      DOUBLE PRECISION DDLBND(NDF__MXDIM)   ! Co-ord lower bound of input NDF 
      DOUBLE PRECISION DDUBND(NDF__MXDIM)   ! Co-ord upper bounds of output NDF
      DOUBLE PRECISION DDXL( NDF__MXDIM )   ! Co-ord of input pt gives lower bnd
      DOUBLE PRECISION DDXU( NDF__MXDIM )   ! Co-ord of input pt gives upper bnd      
*  Logical flags
      LOGICAL AXES                          ! Do we have an AXIS extension?

*  Status:
      INTEGER STATUS  

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN    
            
*  Start of code block.

      CALL NDF_BEGIN
      CALL AST_BEGIN(STATUS)
            
*  Associate the input NDF
      CALL NDF_ASSOC('IN','Update',INDF,STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 999

*  Check to see whether we have an AXIS array
      CALL NDF_STATE( INDF, 'Axis', AXES, STATUS) 

      IF ( AXES ) THEN      
          STATUS = SAI__ERROR 
          CALL NDF_MSG( 'NDFNAME', INDF )
          CALL ERR_REP( 'PUTAXIS_ERR',
     :                  'PUTAXIS: NDF ^NDFNAME '//
     :'already has an AXIS co-ordinate system defined. ', 
     :                  STATUS ) 
          GOTO 960
      END IF 
      
*  Grab information about the AST FrameSet
      CALL CUB1_AINIT( INDF, IWCS, FRCUR, PFRAME, CFRAME,
     :                 MAPC, NVIN, NVOUT, NFRM, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN      
          CALL ERR_REP( 'PUTAXIS_ERR',
     :                  'PUTAXIS: Problems with accessing AST FrameSet', 
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


*  Define a default AXIS co-ordinate system
      CALL NDF_ACRE( INDF, STATUS )
      WRITE(TEXT, '(A)') '   Attaching default AXIS co-ordinates'
      CALL MSG_OUT(' ', TEXT, STATUS)

*  Label the axes
      CALL PAR_GET0I( 'SPECTRAL', SPECTRAL, STATUS)
      DO IDIM = 1, NVIN
         IF( IDIM .EQ. SPECTRAL ) THEN
            CALL NDF_ACPUT('LAMBDA', INDF, 'Lab', SPECTRAL, STATUS)
         ELSE   
            CALL NDF_ACPUT('LINEAR', INDF, 'Lab', IDIM, STATUS)
         ENDIF
      END DO        

*  Get number of pixels for the current input image
      NPIX = 1
      DO IDIM = 1, NVIN
         ISIZ = IUBND( IDIM ) - ILBND( IDIM ) + 1
         NPIX = NPIX * ISIZ
      END DO       

*  Get the lowest precision type to which all the NDF axis arrays may be
*  modified with, since we've just defined them using NDF_ACRE it's going
*  to be _REAL anyway, but lets avoid suprises.

      CALL NDF_ATYPE( INDF, 'Centre', 0, ATYPE, STATUS)

*  Do the AST transform
*  --------------------

      DO IDIM = 1, NVIN
      
         WRITE(TEXT, '(A,I1,A)') 
     :'   Mapping AXIS ', IDIM, ' using AST WCS extension' 
         CALL MSG_OUT(' ', TEXT, STATUS)
         
*  Map the axis arrays     
         CALL NDF_AMAP( INDF, 'Centre', IDIM, ATYPE, 'Update',
     :                  AXPNT, AEL, STATUS)
          
*  Fill array to pass to CUG1_DOAST<T> then call it   

*  byte array.
         IF ( ATYPE .EQ. '_BYTE' ) THEN
            CALL CUG1_FILLAB( MAPC, IDIM, AEL, NVIN, NVOUT, 
     :                       %VAL(AXPNT), STATUS)
*  double-precision array.
         ELSE IF ( ATYPE .EQ. '_DOUBLE' ) THEN
            CALL CUG1_FILLAD( MAPC, IDIM, AEL, NVIN, NVOUT, 
     :                       %VAL(AXPNT), STATUS)               
*  integer array.
         ELSE IF ( ATYPE .EQ. '_INTEGER' ) THEN
            CALL CUG1_FILLAI( MAPC, IDIM, AEL, NVIN, NVOUT, 
     :                       %VAL(AXPNT), STATUS)                  
*  single-precision array.
         ELSE IF ( ATYPE .EQ. '_REAL' ) THEN
            CALL CUG1_FILLAR( MAPC, IDIM, AEL, NVIN, NVOUT, 
     :                       %VAL(AXPNT), STATUS)    
*  unsigned-byte array.
         ELSE IF ( ATYPE .EQ. '_UBYTE' ) THEN
            CALL CUG1_FILLAUB( MAPC, IDIM, AEL, NVIN, NVOUT, 
     :                       %VAL(AXPNT), STATUS)   
*  unsigned-word array.
         ELSE IF ( ATYPE .EQ. '_UWORD' ) THEN
            CALL CUG1_FILLAUW( MAPC, IDIM, AEL, NVIN, NVOUT, 
     :                       %VAL(AXPNT), STATUS) 
*  word array.
         ELSE IF ( ATYPE .EQ. '_WORD' ) THEN
            CALL CUG1_FILLAW( MAPC, IDIM, AEL, NVIN, NVOUT, 
     :                       %VAL(AXPNT), STATUS)    
         END IF  
         
         CALL NDF_AUNMP(INDF, 'Centre', IDIM, STATUS)            
      END DO
      
*  Close things down.

940   CONTINUE
*  Tidy up any remaining AST FrameSets
      CALL AST_ANNUL( MAPC, STATUS )      
      CALL AST_ANNUL( FRCUR, STATUS )

960   CONTINUE      
*  Tidy up AST and NDF states
      CALL AST_ANNUL( IWCS, STATUS)
      CALL AST_END(STATUS)
      CALL NDF_END(STATUS)
      
999   CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'PUTAXIS_ERR',
     :                  'PUTAXIS: Fatal Error', STATUS ) 
      END IF

*  Everything must end sometime
      END
      
      
      
