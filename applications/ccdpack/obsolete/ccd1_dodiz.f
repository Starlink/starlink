      SUBROUTINE CCD1_DODIZ( NDFIN, WEIGHT, NPXIN, ITYPE, ODAT, 
     :                       OWHT, OCNT, FRCUR, CMAP, IX, IY, 
     :                       NDIMI, OX, OY,  NDIMO, ILBND, OLBND, 
     :                       PIXFRAC, GETV, GETS, GETZ, GETM, 
     :                       SCALE, ZERO, VARFAC, STATUS )
     
*+
*  Name:
*     CCD1_DODIZ

*  Purpose:
*     Runs though most of the drizziling algorithim, but calls CCD1_ODIZx
*     to assign the actual input data values IDAT to the ouptu data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_DODIZ( NDFIN, WEIGHT, NPXIN, ITYPE, 
*    :                 ODAT, OWHT, OCNT, FRCUR, CMAP, 
*    :                 IX, IY, NDIMI, OX, OY, NDIMO, ILBND, 
*    :                 OLBND, PIXFRAC, GETV, GETS, GETZ, GETM,
*    :                 SCALE, ZERO, VARFAC, STATUS )

*  Description:
*    Setup for the drizziling algorithim, works out co-ordinates of the
*    input pixel on teh output grid, calls CCD1_BOX to work out overlap
*    and then CCG1_ODIZx to drop the counts into the output pixels

*  Arguments:
*     NDFIN = DOUBLE PRECISION (Given)
*        Pointer to the current input NDF
*     WEIGHT = DOUBLE PRECISION (Given)
*        Weight for the current image
*     NPXIN = INTEGER (Given)
*        Number of pixels in the input NDF
*     ITYPE = CHAR * ( DAT__SZTYP ) (Given)
*        Data type of the input NDF's
*     ODAT( OX, OY ) = REAL (Given)
*        Data component of the output NDF
*     OWHT( OX, OY ) = DOUBLE PRECISION (Given)
*        Weight component of the output NDF
*     OCNT( OX, OY ) = INTEGER (Given)
*        Count component of the output NDF
*     FRCUR = INTEGER (Given)
*        Pointer to the Current AST Frame
*     CMAP = INTEGER (Given)
*        Pointer to the Current AST mapping
*     IX = INTEGER (Given)
*        Size of the input NDF X-dimension
*     IY = INTEGER (Given)
*        Size of the intput NDF Y-dimension
*     NDIMI = INTEGER (Given)
*        Number of dimensions in the input NDF
*     OX = INTEGER (Given)
*        Size of the output NDF X-dimension
*     OY = INTEGER (Given)
*        Size of the output NDF Y-dimension
*     NDIMO = INTEGER (Given)
*        Number of dimensions in the output NDF
*     ILBND( NDF__MXDIM ) = INTEGER (Given)
*        Lower bounds for input NDF
*     OLBND( NDF__MXDIM ) = INTEGER (Given)
*        Lower bounds for output NDF
*     PIXFRAC = DOUBLE PRECISION (Given)
*        Fractional size of pixel "drop"
*     GETV = LOGICAL (Given)
*        Are we using image variances as weights?
*     GETS = LOGICAL (Given)
*        Are we scaling the images?
*     GETZ = LOGICAL (Given)
*        Do we have a zero point for the images?
*     GETM = LOGICAL (Given)
*        Are we using variance maps?
*     SCALE( CCD1__MXNDF + 1 ) = DOUBLE PRECISION (Given)
*        Scale corrections factors
*     ZERO( CCD1__MXNDF + 1 )  = DOUBLE PRECISION (Given)
*        Zero point corrections (will be ADDED to images)
*     VARFAC = DOUBLE PRECISION (Given)
*        Variance scaling factor (used when GETM is TRUE)
*     STATUS = INTEGER (Given and Returned)
*        Global status value

*  Authors:
*     AA: Alasdair Allan (STARLINK, Keele University)
*     {enter_new_authors_here}

*  History:
*     10-APR-1999 (AA):
*        Original version
*     07-SEP-1999 (AA):
*        Shipping version
*     {enter_changes_here}

*  Bugs:
*     {note_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE           ! no implicit typing allowed

*  Global Constants:
      INCLUDE 'DAT_PAR'        ! HDS constants
      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'PRM_PAR'        ! Magic-value and extreme constants
      INCLUDE 'NDF_PAR'        ! NDF constants
      INCLUDE 'AST_PAR'

*  Arguments Given:
      CHARACTER * ( DAT__SZTYP ) ITYPE    ! NDF array implementation type
      
      DOUBLE PRECISION PIXFRAC            ! Fractional size of pixel "drop"

      INTEGER CMAP                        ! Pointer to the current AST mapping
      INTEGER FRCUR                       ! Pointer to the current AST frame
      INTEGER NPXIN                       ! Number of pixels in input NDF
      INTEGER NDFIN                       ! Pointer to the current input NDF
      INTEGER NDIMI                       ! Number of dimensions in input NDF
      INTEGER NDIMO                       ! Number of dimensions in output NDF
      INTEGER IX, IY                      ! Size of each input NDF dimension
      INTEGER OX, OY                      ! Size of each output NDF dimension
      INTEGER ILBND( NDF__MXDIM )         ! Lower bounds of input NDF
      INTEGER OLBND( NDF__MXDIM )         ! Lower bounds of output NDF

      INTEGER OCNT( OX, OY )              ! Output NDF Counter
      
      REAL ODAT( OX, OY )                 ! Output NDF Data 
      REAL OWHT( OX, OY )                 ! Output NDF Weights

      INTEGER IDAT                        ! Pointer to input NDF data
      INTEGER IVAR                        ! Pointer to input NDF variances
      
      DOUBLE PRECISION WEIGHT             ! Weight for current input NDF
      DOUBLE PRECISION SCALE              ! Scale factor correction
      DOUBLE PRECISION ZERO               ! zero point correction
      DOUBLE PRECISION VARFAC             ! Variance scaling factor
      
      LOGICAL GETV                        ! Are we using variances as weights?
      LOGICAL GETS                        ! Are we using scaling?
      LOGICAL GETZ                        ! Do we have a zero point correction?
      LOGICAL GETM                        ! Using variance maps
      
*  Arguments Returned:

*  Local Variables:
      INTEGER X, Y                        ! Loop counter for PIXELS
      INTEGER U, V                        ! Loop counter with BND offsets
      
      INTEGER I, J                        ! Loop counters for affected pixels
      INTEGER L, M                        ! Loop counters with BND offsets

      INTEGER IWCS                        ! Pointer to the WCS in input NDF
      INTEGER JREG                        ! Index of the CCD_REG AST frame 
      
      DOUBLE PRECISION XIN( 4 ), YIN( 4 )    ! Corners of the input pixel
      DOUBLE PRECISION XOUT( 4 ), YOUT( 4 )  ! Corners of the output pixel
      DOUBLE PRECISION JACOB              ! Area of input pixel in output grid
      DOUBLE PRECISION OVER               ! Overlap between input & output pixel
      
*  Status:
      INTEGER STATUS                      ! Global status

*.

       
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
           
*  Get pointers to the WCS component, do it here instead of passing it down
*  from above, we're using enough memory as it it... 
      CALL CCD1_GTWCS(NDFIN, IWCS, STATUS)         

*  Make sure the user has actually run REGISTER, if not warn them
      CALL CCD1_FRDM( IWCS, 'CCD_REG', JREG, STATUS )
              
*  There is no CCD_REG Frame, warn the user (bad things may happen!)
      IF( JREG .EQ. 0 ) THEN         
         CALL MSG_SETC( 'CURR_DOM', 
     :                  AST_GETC(FRCUR, 'Domain', STATUS))
         CALL CCD1_MSG( ' ', '    Domain of current AST Frame: '
     :                  //' ^CURR_DOM (WARNING)', STATUS )
      ENDIF                 

*  Check that the image is 2 dimensional, if not we're in real trouble
*  since the pixel mapping alogrithims (ie the bit that does the actual
*  drizziling) only works for 2 (not N) dimensions. This shouldn't be
*  a problem right? An N-dimensional mosaic is a pretty wierd thing to
*  do, or am I just being provincial here...?
      IF ( NDIMI .NE. 2 ) THEN
      
         CALL MSG_SETI( 'IN_DIM', NDIMI )
         CALL CCD1_MSG( ' ', '    Input Dimensions: '
     :                  //' ^IN_DIM (ERROR)', STATUS )
     
         STATUS = SAI__ERROR
         CALL ERR_REP( 'DRIZZLE_ERR',
     :                 'DRIZZLE: Not a two-dimensional input'
     :                 //' image? Can not proceed with drizziling.',
     :                 STATUS )
         GOTO 999
         
      ELSE IF ( NDIMO .NE. 2 ) THEN
      
         CALL MSG_SETI( 'OUT_DIM', NDIMO )
         CALL CCD1_MSG( ' ', '    Output Dimensions: '
     :                  //' ^OUT_DIM (ERROR)', STATUS )  
         
         STATUS = SAI__ERROR
         CALL ERR_REP( 'DRIZZLE_ERR',
     :                 'DRIZZLE: Not a two-dimensional output'
     :                 //' image? Can not proceed with drizziling.',
     :                 STATUS )
         GOTO 999 
         
      ELSE

         CALL MSG_SETI( 'IN_DIM', NDIMI )
         CALL MSG_SETI( 'OUT_DIM', NDIMO )
         CALL CCD1_MSG( ' ', '    Dimensions: Input (^IN_DIM)'
     :                  //' = Ouput (^OUT_DIM)', STATUS )        

      ENDIF

*  Report the weights for each image
      IF ( GETM ) THEN
         CALL CCD1_MSG( ' ', '    Weighting with inverse variance map: '
     :                     //' TRUE', STATUS )
      ELSE IF ( GETV .AND. ( .NOT. GETM ) )  THEN
         CALL MSG_SETD( 'IN_WHT', WEIGHT )
         CALL CCD1_MSG( ' ', '    Image weighting: '
     :                     //' ^IN_WHT', STATUS )
      ENDIF   

*  Report Scaling factor for each image
      IF ( GETS )  THEN
         CALL MSG_SETD( 'IN_SCALE', SCALE )
         CALL CCD1_MSG( ' ', '    Image scaling factor: '
     :                     //' ^IN_SCALE', STATUS )
      ENDIF 
        
*  Report zero point for each image               
      IF ( GETZ )  THEN
         CALL MSG_SETD( 'IN_ZERO', ZERO )
         CALL CCD1_MSG( ' ', '    Zero point correction: '
     :                     //' ^IN_ZERO', STATUS )
      ENDIF  

*  Assign the input NDF Data array to IDAT
*  ---------------------------------------
*
*  Moved here from the main DRIZZLE.F code so I can be a bit more
*  stringent on how the input data type is converted by calling the
*  main alogrithim in a subroutine.
      CALL NDF_MAP( NDFIN, 'Data', ITYPE, 'READ', IDAT, NPXIN, 
     :              STATUS ) 
     
*  Assign the input NDF Variance array to IVAR
*  ------------------------------------------- 
*
*  If we're using inverse variance maps to weight the data then
*  map it here to pass down to the subroutine
      IF( GETM ) THEN
         CALL NDF_MAP( NDFIN, 'Variance', ITYPE, 'READ', IVAR, NPXIN, 
     :                 STATUS )       
      ENDIF   
            
*  Start of Main Loop
*  ==================
*
*  Loop through each pixel of the input image and work out its position 
*  in the output image. This may seem inefficent, unfortunately I haven't
*  be able to thing of a more efficent way to do the tranforms!
      DO 1 X = 1, IX
         DO 2 Y = 1, IY

*  Do the offset thing to the working co-ordinates
               U = X + ILBND( 1 ) - 1
               V = Y + ILBND( 2 ) - 1 
                                    
*  Define the input co-ordinate list to transform, taking account of the
*  drop size definaed by the PIXFRAC parameter. Pixels corners are taken
*  in clockwise direction, important for overlap area calculation coming
*  up later in the loop
* 
*                XIN(1), YIN(1) x-----x XIN(2), YIN(2)
*                               |     |
*                               |     |                 
*                               |     |                  
*                XIN(4), YIN(4) x-----x XIN(3), YIN(3)
*
               
            XIN(1) =   ( DBLE( X + ILBND( 1 ) - 1 ) - 1.0D0 )
     :               + ( 1.0D0 - DSQRT(PIXFRAC) )
            YIN(1) =     DBLE( Y + ILBND( 2 ) - 1 ) 
     :               - ( 1.0D0 - DSQRT(PIXFRAC) )

            XIN(2) =     DBLE( X + ILBND( 1 ) - 1 ) 
     :               - ( 1.0D0 - DSQRT(PIXFRAC) )
            YIN(2) =     DBLE( Y + ILBND( 2 )- 1  ) 
     :               - ( 1.0D0 - DSQRT(PIXFRAC) ) 
                      
            XIN(3) =     DBLE( X + ILBND( 1 ) - 1 )  
     :               - ( 1.0D0 - DSQRT(PIXFRAC) )
            YIN(3) =   ( DBLE( Y + ILBND( 2 ) - 1 ) - 1.0D0 ) 
     :                  + ( 1.0D0 - DSQRT(PIXFRAC) ) 

            XIN(4) =   ( DBLE( X + ILBND( 1 ) - 1 ) - 1.0D0 ) 
     :               + ( 1.0D0 - DSQRT(PIXFRAC) )
            YIN(4) =   ( DBLE( Y + ILBND( 2 ) - 1 ) - 1.0D0 ) 
     :               + ( 1.0D0 - DSQRT(PIXFRAC) ) 

* Do the transform and work out the corresponding output co-ordinates
            CALL AST_TRAN2( CMAP, 4, XIN, YIN, .TRUE., XOUT,
     :                      YOUT, STATUS ) 
           
* Work out the area of the input pixel on the output pixel grid
            JACOB = 0.5D0*( (XOUT(2)-XOUT(4))*(YOUT(1)-YOUT(3)) -
     :                       (XOUT(1)-XOUT(3))*(YOUT(2)-YOUT(4)) )
              
* Loop over output pixels which could be affected by this pixel
            DO J = NINT( MIN(YOUT(1),YOUT(2),YOUT(3),YOUT(4)) ),
     :             NINT( MAX(YOUT(1),YOUT(2),YOUT(3),YOUT(4)) )

            DO I = NINT( MIN(XOUT(1),XOUT(2),XOUT(3),XOUT(4)) ),
     :             NINT( MAX(XOUT(1),XOUT(2),XOUT(3),XOUT(4)) )         

*  Do the offset thing to the working co-ordinates
               L = I - OLBND(1) + 1
               M = J - OLBND(2) + 1  

               CALL CCD1_BOX( I , J , XOUT, YOUT, OVER, STATUS )
               IF( STATUS .EQ. SAI__ERROR ) THEN
                     STATUS = SAI__OK
                  CALL MSG_SETI( 'IS', I )
                  CALL MSG_SETI( 'JS', J )
                  CALL CCD1_MSG( ' ', 
     :                           '    Warning: Overlap '
     :                         //'calculation problem at '
     :                         //' (^IS,^JS)', STATUS ) 
               ENDIF
                 
*  If we have a positive overlap then
               IF( OVER .GT. 0.0D0 ) THEN
                   
                  OVER = OVER/JACOB

*  Call the drizziling algorithim
*  ==============================
*
*  The actual drizziling algorithim is now farmed out to CCD1_ODIZx 
*  so that I can type the incoming data from IDAT properly and
*  shove it into the _REAL data arrays of the output mosaic.

*  Byte array
                  IF ( ITYPE .EQ. '_BYTE' ) THEN
                     CALL CCG1_ODIZB( %VAL(IDAT), ODAT, OWHT, OCNT, 
     :                                IX, IY, OX, OY, X, Y, I, J, L,
     :                                M, SCALE, ZERO, OVER, WEIGHT, 
     :                                GETV, GETS, GETZ, GETM,
     :                                %VAL(IVAR), VARFAC, STATUS )
*  Double-precision array
                  ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                     CALL CCG1_ODIZD( %VAL(IDAT), ODAT, OWHT, OCNT, 
     :                                IX, IY, OX, OY, X, Y, I, J, L,
     :                                M, SCALE, ZERO, OVER, WEIGHT, 
     :                                GETV, GETS, GETZ, GETM, 
     :                                %VAL(IVAR), VARFAC, STATUS )   
*  Integer array
                  ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                     CALL CCG1_ODIZI( %VAL(IDAT), ODAT, OWHT, OCNT, 
     :                                IX, IY, OX, OY, X, Y, I, J, L,
     :                                M, SCALE, ZERO, OVER, WEIGHT, 
     :                                GETV, GETS, GETZ, GETM,  
     :                                %VAL(IVAR), VARFAC, STATUS )
*  Single-precision array
                  ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
                     CALL CCG1_ODIZR( %VAL(IDAT), ODAT, OWHT, OCNT, 
     :                                IX, IY, OX, OY, X, Y, I, J, L,
     :                                M, SCALE, ZERO, OVER, WEIGHT, 
     :                                GETV, GETS, GETZ, GETM,  
     :                                %VAL(IVAR), VARFAC, STATUS )
*  Unsigned-byte array
                  ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
                     CALL CCG1_ODIZUB( %VAL(IDAT), ODAT, OWHT, OCNT, 
     :                                IX, IY, OX, OY, X, Y, I, J, L,
     :                                M, SCALE, ZERO, OVER, WEIGHT, 
     :                                GETV, GETS, GETZ, GETM, 
     :                                %VAL(IVAR), VARFAC, STATUS )
*  Unsigned-word array
                  ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
                     CALL CCG1_ODIZUW( %VAL(IDAT), ODAT, OWHT, OCNT, 
     :                                IX, IY, OX, OY, X, Y, I, J, L,
     :                                M, SCALE, ZERO, OVER, WEIGHT, 
     :                                GETV, GETS, GETZ, GETM, 
     :                                %VAL(IVAR), VARFAC, STATUS )
*  Word array
                  ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                     CALL CCG1_ODIZW( %VAL(IDAT), ODAT, OWHT, OCNT, 
     :                                IX, IY, OX, OY, X, Y, I, J, L,
     :                                M, SCALE, ZERO, OVER, WEIGHT, 
     :                                GETV, GETS, GETZ, GETM, 
     :                                %VAL(IVAR), VARFAC, STATUS ) 
                  END IF                        
                   
               ENDIF

            END DO
            END DO            

*  End of Main Loop
*  ================ 
2        CONTINUE 
1     CONTINUE


*  Clean up - Arrive here if an error occurs
*  ========================================= 
999   CONTINUE
       
*  Tidy up any local AST stuff
      CALL AST_ANNUL( IWCS, STATUS )

*  Time at the bar please...
      END









