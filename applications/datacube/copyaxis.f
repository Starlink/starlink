      SUBROUTINE COPYAXIS(STATUS)
*+
*  Name:
*     COPYAXIS

*  Purpose:
*     Copies an AXIS extension into an NDF from the information present in
*     another NDF

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL COPYAXIS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Simple application designed to be run from inside a user written
*     csh script or from the command line, it will build an AXIS extension
*     for a specified NDF from the information present in an NDF of identical
*     dimensionality and bounds.

*  Usage:
*     copyaxis in=file like=template

*  ADAM Parameters:
*     IN = _NDF (Read & Write)
*        The NDF to be modified
*     LIKE = _NDF (Read)
*        The template NDF.

*  Examples:
*     copyaxis in=smirfp0 like=smirfsp1
*        Copies the AXIS structure from the file smirfsp1 to smirfsp0

*  Authors:
*     AALLAN: Alasdair Allan (STARLINK, Keele University)

*  History:
*      19-SEP-2000 (AALLAN):
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
      INTEGER IAXP                          ! Pointer to the input AXIS arrays
      
      INTEGER LNDF                          ! Pointer to the like NDF
      INTEGER LAXP                          ! Pointer to the like AXIS arrays

*  Input NDF
      INTEGER IDIMS(NDF__MXDIM)             ! Dimensions of the input NDF
      INTEGER ILBND(NDF__MXDIM)             ! Input NDF lower bounds
      INTEGER IUBND(NDF__MXDIM)             ! Input NDF upper bounds
      INTEGER NDIMI                         ! Number of dimensions in input NDF
      INTEGER NPIXI                         ! Number of pixels in the input NDF
      INTEGER ISIZ                          ! Input NDF dimension size

*  Like NDF      
      INTEGER LDIMS(NDF__MXDIM)             ! Dimensions of the like NDF
      INTEGER LLBND(NDF__MXDIM)             ! Like NDF lower bounds
      INTEGER LUBND(NDF__MXDIM)             ! Like NDF upper bounds
      INTEGER NDIML                         ! Number of dimensions in like NDF
      INTEGER NPIXL                         ! Number of pixels in the like NDF
      INTEGER LSIZ                          ! Like NDF dimension size  
          
*  AXIS information
      CHARACTER*( DAT__SZTYP ) ATYPE        ! NDF array implementation type
      INTEGER IAEL                          ! Number of input elements mapped 
      INTEGER LAEL                          ! Number of like elements mapped 
          
*  Logical flags
      LOGICAL AXES                          ! Do we have an AXIS extension?

*  Status:
      INTEGER STATUS  

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN    
            
*  Start of code block.

      CALL NDF_BEGIN
            
*  Associate the input NDF
      CALL NDF_ASSOC('IN','Update',INDF,STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 999

*  Check to see whether we have an AXIS array
      CALL NDF_STATE( INDF, 'Axis', AXES, STATUS) 

      IF ( AXES ) THEN      
          STATUS = SAI__ERROR 
          CALL NDF_MSG( 'NDFNAME', INDF )
          CALL ERR_REP( 'COPYAXIS_ERR',
     :                  'COPYAXIS: NDF ^NDFNAME '//
     :'already has an AXIS co-ordinate system defined. ', 
     :                  STATUS ) 
          GOTO 960
      END IF 
      
      CALL NDF_ASSOC('LIKE','Read',LNDF,STATUS)
             
*  Dimensions.
      CALL NDF_DIM(LNDF, NDF__MXDIM, LDIMS, NDIML, STATUS)
      CALL NDF_DIM(INDF, NDF__MXDIM, IDIMS, NDIMI, STATUS)

*  Confirm the two NDFs have the same dimensions
      IF ( NDIMI .NE. NDIML ) THEN      
          STATUS = SAI__ERROR 
          CALL NDF_MSG( 'NDFNAME', INDF )
          CALL NDF_MSG( 'LIKNAME', LNDF )
          CALL ERR_REP( 'COPYAXIS_ERR',
     :                  'COPYAXIS: NDF ^NDFNAME and ^LIKNAME '//
     :'are not of the same dimensionality. ', 
     :                  STATUS ) 
          GOTO 960
      END IF 

*  Bounds.
      CALL NDF_BOUND(LNDF, NDF__MXDIM, LLBND, LUBND, NDIML, STATUS) 
      CALL NDF_BOUND(INDF, NDF__MXDIM, ILBND, IUBND, NDIMI, STATUS) 

*  Confirm the two NDFs are the same size
      NPIXI = 1
      DO IDIM = 1, NDIMI
         ISIZ = IUBND( IDIM ) - ILBND( IDIM ) + 1
         LSIZ = LUBND( IDIM ) - LLBND( IDIM ) + 1
         IF ( ISIZ .NE. LSIZ ) THEN      
            STATUS = SAI__ERROR 
            CALL NDF_MSG( 'NDFNAME', INDF )
            CALL NDF_MSG( 'LIKNAME', LNDF )
            CALL ERR_REP( 'COPYAXIS_ERR',
     :                  'COPYAXIS: The axes of NDF ^NDFNAME '//
     :'and NDF ^LIKNAME are not all the same length.', 
     :                  STATUS ) 
            GOTO 960
         END IF             
         NPIXI = NPIXI * ISIZ
         NPIXL = NPIXL * LSIZ
      END DO 

*  Get the lowest precision type for the NDF axis arrays
      CALL NDF_ATYPE( LNDF, 'Centre', 0, ATYPE, STATUS)

*  Define a default AXIS co-ordinate system in the input NDF
      CALL NDF_ACRE( INDF, STATUS )
      CALL NDF_MSG( 'NDFNAME', INDF )
      WRITE(TEXT, '(A)') 
     :'   Attaching default AXIS co-ordinates to ^NDFNAME.'
      CALL MSG_OUT(' ', TEXT, STATUS)

*  Label the axes
      DO IDIM = 1, NDIMI
         CALL NDF_ACPUT('LINEAR', INDF, 'Lab', IDIM, STATUS)
      END DO        

*  Do the axis copy
*  ----------------
      DO IDIM = 1, NDIML
      
         CALL NDF_MSG( 'LIKNAME', LNDF )
         WRITE(TEXT, '(A,I1,A)') 
     :'   Copying AXIS ', IDIM, ' from ^LIKNAME' 
         CALL MSG_OUT(' ', TEXT, STATUS)
         
*  Map the like AXIS array
         CALL NDF_AMAP( LNDF, 'Centre', IDIM, ATYPE, 'Read',
     :                  LAXP, LAEL, STATUS)

*  Map the like AXIS array
         CALL NDF_AMAP( INDF, 'Centre', IDIM, ATYPE, 'update',
     :                  IAXP, IAEL, STATUS)
         IF ( IAEL .NE. LAEL ) THEN      
            STATUS = SAI__ERROR 
            CALL NDF_MSG( 'NDFNAME', INDF )
            CALL NDF_MSG( 'LIKNAME', LNDF )
            CALL NDF_MSG( 'AXNUM', IDIM )
            CALL ERR_REP( 'COPYAXIS_ERR',
     :                  'COPYAXIS: Axis ^AXNUM of NDF ^NDFNAME'//
     :'and NDF ^LIKENAME do not have the same lengths. This error'//
     :'should not occur under any circumstances. Very Odd!', 
     :                  STATUS ) 
            GOTO 960
         END IF 
                     
*  byte array.
         IF ( ATYPE .EQ. '_BYTE' ) THEN
            CALL CUG1_CPYAB( STATUS)
*  double-precision array.
         ELSE IF ( ATYPE .EQ. '_DOUBLE' ) THEN
            CALL CUG1_CPYAD( %VAL(LAXP), %VAL(IAXP), IAEL, STATUS)   
*  integer array.
         ELSE IF ( ATYPE .EQ. '_INTEGER' ) THEN
            CALL CUG1_CPYAI( %VAL(LAXP), %VAL(IAXP), IAEL, STATUS)      
*  single-precision array.
         ELSE IF ( ATYPE .EQ. '_REAL' ) THEN
            CALL CUG1_CPYAR( %VAL(LAXP), %VAL(IAXP), IAEL, STATUS)    
*  unsigned-byte array.
         ELSE IF ( ATYPE .EQ. '_UBYTE' ) THEN
            CALL CUG1_CPYAUB( %VAL(LAXP), %VAL(IAXP), IAEL, STATUS)   
*  unsigned-word array.
         ELSE IF ( ATYPE .EQ. '_UWORD' ) THEN
            CALL CUG1_CPYAUW( %VAL(LAXP), %VAL(IAXP), IAEL, STATUS) 
*  word array.
         ELSE IF ( ATYPE .EQ. '_WORD' ) THEN
            CALL CUG1_CPYAW( %VAL(LAXP), %VAL(IAXP), IAEL, STATUS)    
         END IF  

*  Unmap the arrays
         CALL NDF_AUNMP(LNDF, 'Centre', IDIM, STATUS)            
         CALL NDF_AUNMP(INDF, 'Centre', IDIM, STATUS)            
      END DO
      
      
*  Close things down.

940   CONTINUE

960   CONTINUE      
*  Tidy up AST and NDF states
      CALL NDF_END(STATUS)
      
999   CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'COPYAXIS_ERR',
     :                  'COPYAXIS: Fatal Error', STATUS ) 
      END IF

*  Everything must end sometime
      END
      
      
      
