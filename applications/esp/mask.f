      SUBROUTINE MASK( STATUS )
*+
*  Name:
*     MASK

*  Purpose:
*     Uses an ARD file to set some pixels of a given image to bad.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MASK( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Allows the user to input the name of an NDF image file and
*     and ARD file. The ARD file is used to specify which parts of the
*     image will NOT be used. An output NDF is then created which is
*     the same as the input file except that all pixels specified by
*     the ARD file have been assigned the value Bad.

*  Usage:
*     MASK IN ARDFIL OUT

*  ADAM Parameters:
*     ARDFIL = _CHAR (Read)
*        The name of the ARD file containing a description of
*        the parts of the image to be masked out i.e. set to bad.
*     IN = _NDF (Read)
*        The name of the source NDF.
*     OUT = _NDF (Write)
*        The name of the output NDF.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     08-Jun-1994 (GJP)
*     (Original version)
*     11-NOV-1999 (MBT)
*     Modified for use with WCS components.

*  Examples:
*
*     mask in=ic3374 ardfil=^ardfile.txt out=ic3374a
*
*        This example uses as the source image IC3374 and sets
*        the pixels specified by the ARD description contained in
*        ARDFILE.TXT to the bad value. The resultant image is output
*        as IC3374A.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      INTEGER ELEMS                   ! Total number of pixels in the image
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER NDF1                    ! Identifier for the source NDF
      INTEGER NDF2                    ! Identifier for the output NDF
      INTEGER NDIM                    ! Number of dimensions in the
                                      ! image
      INTEGER POINT1(1)               ! Pointer to the data component of
                                      ! for the output NDF
      INTEGER POINT2(1)               ! Pointer to the ARD logical mask
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Show that the application is running.
      CALL MSG_BLANK(STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL MSG_OUT(' ','ESP MASK running.',STATUS)

*   Begin an NDF context.
      CALL NDF_BEGIN
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Obtain an identifier for the NDF structure to be examined.
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Propogate the bits of the source NDF required.
      CALL NDF_PROP(NDF1,'DATA,WCS','OUT',NDF2,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Set the output NDF data type to real.
      CALL NDF_STYPE('_REAL',NDF2,'DATA',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Map the output NDF data array as _REAL values for updating.
      CALL NDF_MAP(NDF2,'DATA','_REAL','UPDATE',POINT1(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Change the propogated title.
      CALL NDF_CPUT('ESP - Masked',NDF2,'TITLE',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the image bounds and also the size of the axes in pixels.
      CALL NDF_BOUND(NDF2,2,LBND,UBND,NDIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      PRANGE(1)=UBND(1)-LBND(1)+1
      PRANGE(2)=UBND(2)-LBND(2)+1

*   Allocate the memory needed for the logical mask array.
      CALL PSX_CALLOC(ELEMS,'_INTEGER',POINT2(1),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Transfer to the ARD driver control routine.
      CALL ESP1_ARD_DRIVER(NDIM,ELEMS,LBND,UBND,POINT1,POINT2,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Free the dynamic array space of the logical mask.
      CALL PSX_FREE(POINT2(1),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   End the NDF context.
      CALL NDF_END(STATUS)

      END


