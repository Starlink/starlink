*+  KFH_REPVAL - Reports the value of a specified point.
      SUBROUTINE KFH_REPVAL(PICT,NX,NY,X,Y,STATUS)
*    Description :
*     This routine accepts the coordinates of a point
*     on an image and determines the value of the image
*     at that point. The result is displayed to the user.
*    Invocation :
*     CALL KFH_REPVAL(PICT,NX,NY,X,Y,STATUS)
*    Parameters :
*     PICT(0:NX-1,0:NY-1) = REAL
*           The array containing the image data.
*     NX = INTEGER
*           The X-dimension of the image.
*     NY = INTEGER
*           The Y-dimension of the image.
*     X = INTEGER
*           The X-coordinate of the point whose value
*           is to be determined.
*     Y = INTEGER
*           The Y-coordinate of the point whose value
*           is to be determined.
*     STATUS = INTEGER
*           The status value on entry to this subroutine.
*    Method :
*     The value is found by indexing the image array
*     using the coordinates given.
*    Authors :
*     S.Chan (RGVAD::KFH)
*    History :
*     10 September 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER NX                         ! X-dimension of the image.
      INTEGER NY                         ! Y-dimension of the image.
      REAL PICT(0:NX-1,0:NY-1)           ! Array containing the image.
      REAL VALUE                         ! Value of the image at the
*                                        ! specified point.
      INTEGER X                          ! X-coordinate of the point.
      INTEGER Y                          ! Y-coordinate of the point.
*-

*
*    If the status is bad, then return to the main program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Reference the image array using the point
*       coordinates.
*

         VALUE = PICT(X,Y)

*
*       Output the result.
*

         CALL MSG_OUT('LINE',' ',STATUS)
         CALL MSG_SETR('VAL',VALUE)
         CALL MSG_SETI('X',X)
         CALL MSG_SETI('Y',Y)
         CALL MSG_OUT('MSG1','   VALUE of image at ^X,^Y'/
     :    /' is ^VAL',STATUS)
         CALL MSG_OUT('LINE',' ',STATUS)

      ENDIF

      END
