
*+  KFH_TRANSFER2 - Copies a section of image data.
      SUBROUTINE KFH_TRANSFER2(IMAGE,XDIM,YDIM,X1,Y1,X2,Y2,
     : REGION,RXDIM,RYDIM,STATUS)
*    Description :
*     This routine copies the 2-D data of a specified
*     region of an image.
*    Invocation :
*     CALL KFH_TRANSFER2(IMAGE,XDIM,YDIM,X1,Y1,X2,Y2,REGION,RXDIM,RYDIM,
*                        STATUS)
*    Parameters :
*     IMAGE(XDIM,YDIM) = REAL
*           The array containing the raw image data.
*     XDIM = INTEGER
*           The X-dimension of the image.
*     YDIM = INTEGER
*           The Y-dimension of the image.
*     X1 = INTEGER
*           The X-coordinate of the first point from
*           which the region is determined.
*     Y1 = INTEGER
*           The Y-coordinate of the first point.
*     X2 = INTEGER
*           The X-coordinate of the second point.
*     Y2 = INTEGER
*           The Y-coordinate of the second point.
*     REGION(RXDIM,RYDIM) = REAL
*           The array into which the region is copied.
*     RXDIM = INTEGER
*           The X-dimension of the region.
*     RYDIM = INTEGER
*           The Y-dimension of the region.
*     STATUS = INTEGER
*           The status value on entry to this routine.
*    Method :
*     The region is copied by a direct assignment of the
*     data elements concerned to the elements of the
*     new array.
*    Authors :
*     S.Chan (RGVAD::KFH)
*    History :
*     28 October 1983:
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER RXDIM                      ! The X-dimension of the region.
      INTEGER RYDIM                      ! The Y-dimension of the region.
      INTEGER XDIM                       ! The X-dimension of the image.
      INTEGER YDIM                       ! The Y-dimension of the image.
      INTEGER I                          ! General variable.
      REAL IMAGE(XDIM,YDIM)              ! The array containing the image
*                                        ! data.
      INTEGER J                          ! General variable.
      REAL REGION(RXDIM,RYDIM)           ! The array containing the region.
      INTEGER TEMPX                      ! Temporary location.
      INTEGER TEMPY                      ! Temporary location.
      INTEGER X1                         ! X-coordinate of the first point.
      INTEGER X2                         ! X-coordinate of the second
*                                        ! point.
      INTEGER Y1                         ! Y-coordinate of the first point.
      INTEGER Y2                         ! Y-coordinate of the second
*                                        ! point.
*-

*
*    If status value is bad, then return to the main program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Re-arrange coordinates , if necessary.
*

         IF (X1.GT.X2) THEN

            TEMPX = X1
            X1 = X2
            X2 = TEMPX

         ENDIF

         IF (Y1.GT.Y2) THEN

            TEMPY = Y1
            Y1 = Y2
            Y2 = TEMPY

         ENDIF

*
*       Copy the elements of the region.
*

         DO I = Y1,Y2,1

            DO J = X1,X2,1

               REGION(J-X1+1,I-Y1+1) = IMAGE(J,I)

            END DO

         END DO

      ENDIF

      END
