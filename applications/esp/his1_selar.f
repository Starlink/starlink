      SUBROUTINE HIS1_SELAR(POINT1,PRANGE,USE,LBND,UBND,
     :                      OARRAY,ELEMS,ARRAY,STATUS)
*+
*  Name:
*     HIS1_SELAR

*  Purpose:
*     Sets up image array to include only those pixels that have
*     been selected.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_SELAR(POINT1,PRANGE,USE,LBND,UBND,OARRAY,
*                     ELEMS,ARRAY,STATUS)

*  Description:
*     Allows the user to define which parts of the image are
*     to be used by opting for the whole image or with parts selected
*     using an ARD file missing.

*  Arguments:
*     POINT1( 10 ) = INTEGER (Given)
*        Memory pointer to the modified image array.
*     PRANGE ( 2 ) = INTEGER (Given)
*        The length of each image axis in pixels.
*     USE = CHARACTER * ( * ) (Given)
*        Specifies how the useful parts of the image are defined.
*        'A' employs and ARD subset file, 'W' uses the whole image.
*     LBND ( 2 ) = INTEGER (Given)
*        Lower limits of indices of the image axes.
*     UBND ( 2 ) = INTEGER (Given)
*        Upper limits of the indices of the image axes.
*     OARRAY( ELEMS ) = REAL (Given)
*        The source NDF data component.
*     ELEMS = INTEGER (Given)
*        The number of pixel elements in the source NDF image.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     ARRAY( ELEMS ) = REAL (Returned)
*        A copy of the data in the data component of the source NDF
*        with all the unwanted pixels replaced by the bad value.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     06-NOV-1992 (GJP):
*        Original version.
*     24-FEB-1997 (GJP)
*        Modified use of pointers.
*     {enter_further_changes_here}

*  Bugs:
*     None known.

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'HIS_PAR'          ! HISTPEAK system variables
      INCLUDE 'NDF_PAR'          ! NDF public constants

*  Arguments Given:
      CHARACTER * ( * ) USE      ! User input indicating if the whole
                                 ! image or parts defined by an ARD file
                                 ! are to be used
      INTEGER ELEMS              ! Number of pixels in the source image
      INTEGER LBND(NDF__MXDIM)   ! Low bounds of image axes
                                 ! May not be less than 1
      INTEGER POINT1( 10 )       ! Pointer to the modified image array
      INTEGER PRANGE ( 2 )       ! The length of the image axes in pixels
      INTEGER UBND(NDF__MXDIM)   ! High bounds of the image axes
                                 ! may not be greater than the image size
      REAL OARRAY( ELEMS )       ! The array containing the source NDF
                                 ! data component

*  Arguments Returned.
      REAL ARRAY( ELEMS )        ! An array containing a modified
                                 ! version of the source array Only
                                 ! those points to be used are not
                                 ! assigned a bad value

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER POINT4             ! Pointer to the logical mask
      INTEGER P1(1)              ! Pointer
      INTEGER P4(1)              ! Pointer
      INTEGER I                  ! Temporary loop variable
      INTEGER NDIM               ! Number of dimensions
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Copy the whole image array.
      DO 10 I=1,ELEMS
         ARRAY(I)=OARRAY(I)
 10   CONTINUE

*   Handle extra stages needed if USE = 'A' i.e. an ARD file is in use.
      IF (USE.EQ.'A') THEN

*      Display the message saying the ARD file is being looked at.
         CALL MSG_OUT(' ','Examining the ARD file.',STATUS)

*      Allocate the memory needed for the logical mask array.
         CALL PSX_CALLOC(ELEMS,'_INTEGER',POINT4,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Transfer to the ARD driver control routine.
         P1(1)=POINT1(1)
         P4(1)=POINT4
         NDIM=2
         CALL ESP1_ARD_DRIVER(NDIM,ELEMS,LBND,UBND,P1,P4,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Free the dynamic array space of the logical mask.
         CALL PSX_FREE(POINT4,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

      END IF

 9999 CONTINUE

      END
