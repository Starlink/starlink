      SUBROUTINE ESP_ARD_DRIVER(NDIM,ELEMS,LBND,UBND,
     :                          POINT1,POINT4,STATUS)
*+
*  Name:
*     ESP_ARD_DRIVER
 
*  Purpose:
*     Looks after reading the ARD file and masking the image.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*      CALL ESP_ARD_DRIVER(NDIM,ELEMS,LBND,UBND,POINT1,POINT4,STATUS)
 
*  Description:
*     Determines the name of the ARD file to be used. Then creates an
*     integer mask containing information describing where the bad pixels
*     are. This information (and the image) are passed to ARD_DRIVE
*     which sets to bad the appropriate pixels on the output image.
 
*  Authors:
*     GJP: Grant Privett (STARLINK)
 
*  History:
*     12-JUN-1994 (GJP)
*     (Original version)
*     24-FEB-1997 (GJP)
*     Modified use of pointers slightly.
 
*  Bugs:
*     None known.
 
*-

  
*   Type Definitions:                   ! No implicit typing
      IMPLICIT NONE
 
*   Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
      INCLUDE 'GRP_PAR'                 ! GRP constants
      INCLUDE 'PRM_PAR'                 ! Primdat constants
      INCLUDE 'NDF_PAR'                 ! NDF constants
      
*   Arguments Given:
      INTEGER ELEMS                     ! Number of pixels in the image
      INTEGER LBND(NDF__MXDIM)          ! Lower bound of the image
      INTEGER NDIM                      ! NUmber of dimensions
      INTEGER POINT1(1)                 ! Pointer to the mapped image
      INTEGER POINT4(1)                 ! Pointer to the mask image
      INTEGER UBND(NDF__MXDIM)          ! Upper bound of the image
 
*   Arguments Returned:
 
*   Status:
      INTEGER STATUS                    ! Global status
 
*   Local variables:
      CHARACTER *(256) ARDN             ! Temporary storage of ARDFIL
      LOGICAL ARDINP                    ! Was a sensible ARD file name returned
      INTEGER IGRP                      ! GRP identifier
      INTEGER INDEX                     ! Non zero mask value
      INTEGER LBNDI(NDF__MXDIM)
      INTEGER UBNDI(NDF__MXDIM)
      INTEGER LBNDE(NDF__MXDIM)
      INTEGER UBNDE(NDF__MXDIM)
      REAL TRCOEF(1)                    ! Transformation values
*.
 
 
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Identify the ARD file name.
      ARDINP=.TRUE.
      CALL ERR_MARK
      CALL PAR_CANCL('ARDFIL',STATUS)
      CALL PAR_GET0C('ARDFIL',ARDN,STATUS)
      CALL ARD_GROUP('ARDFIL',GRP__NOID,IGRP,STATUS)
      IF (STATUS.NE.SAI__OK) THEN
     
*      Set the 'no ARD file' flag.
         CALL GRP_DELET(IGRP,STATUS)
         ARDINP=.FALSE.
*         CALL ERR_ANNUL(STATUS)
         CALL ERR_FLUSH(STATUS)
         STATUS=SAI__OK
         CALL MSG_OUT(' ','WARNING! - ARD file not used.',STATUS)

      END IF  
      CALL ERR_RLSE
    
*   Only process an ARD file if the name was sensible.
      IF (ARDINP) THEN

*      Create the mask.
         TRCOEF(1)=VAL__BADR
         INDEX=2
         ARDINP=.TRUE.
         CALL ERR_MARK
         CALL ARD_WORK(IGRP,NDIM,LBND,UBND,TRCOEF,.FALSE.,
     :                 INDEX,%VAL(POINT4(1)),LBNDI,UBNDI,LBNDE,
     :                 UBNDE,STATUS)
         IF (STATUS.NE.SAI__OK) THEN

*         Set the 'no ARD file' flag.
            CALL GRP_DELET(IGRP,STATUS)
            ARDINP=.FALSE.
            CALL ERR_ANNUL(STATUS)
            STATUS=SAI__OK
            CALL MSG_OUT(' ','WARNING! - ARD file not used.',STATUS)

         END IF
         CALL ERR_RLSE
       
*      Correct the output image to show bad pixels where indicated on the mask.
         IF (ARDINP) THEN
          
*         Set the required pixels to bad.
            CALL ESP_ARD_DRIVE2(ELEMS,%VAL(POINT1(1)),
     :                          %VAL(POINT4(1)),STATUS)

*          Close down the group used to hold the pixel mask.
            CALL GRP_DELET(IGRP,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999 

         END IF 
 
      END IF

 9999 CONTINUE
 
      END
 
 
 
      SUBROUTINE ESP_ARD_DRIVE2(ELEMS,ARRAY,MASK,STATUS)
 
*+
*  Name:
*     ESP_ARD_DRIVE2
 
*  Purpose:
*     Sets to bad all the pixels in the image that were selected by the
*     ARD file contents.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL ESP_ARD_DRIVE2(ELEMS,ARRAY,MASK,STATUS)
 
*  Description:
*     Looks through the mask array. Whenever a non-zero value is found
*     the associated pixel in the output image is set to VAL__BADR.
 
*  Authors:
*     GJP: Grant Privett (STARLINK)
 
*  History:
*     12-JUN-1994 (GJP)
*     (Original version)
 
*  Bugs:
*     None known.
 
*-
*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
 
*  Global Constants:
      INCLUDE 'PRM_PAR'              ! PRIMDAT constants
      INCLUDE 'SAE_PAR'              ! Standard SAE parameters
 
*  Status:
      INTEGER STATUS                 ! Global status report
 
*  Arguments Given:
      INTEGER ELEMS                  ! Size of the image
      INTEGER MASK(ELEMS)            ! The masking image
 
*  Arguments Given and Returned:
      REAL ARRAY(ELEMS)              ! The output image
 
*  Local variables:
       INTEGER I                      ! Loop variable
*.

*   Check the global status.
      IF (STATUS.NE.SAI__OK) RETURN
 
*   Look to see what is in the mask for each pixel. If it is greater than
*   zero then replace the value in the output image with the bad value.
      DO 10 I=1,ELEMS
         IF (MASK(I).GT.0) ARRAY(I)=VAL__BADR
 10   CONTINUE
 
 9999 CONTINUE
 
      END
 
 

