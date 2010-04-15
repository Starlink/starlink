
*+  KFH_NLIST - Produces a listing of an image.
      SUBROUTINE KFH_NLIST(A,NX,NY,X1,Y1,X2,Y2,FNAME,FACTOR)
*    Description :
*     This routine generates a formatted listing of a
*     1-D or a 2-D image. The resulting file may be
*     typed or printed in the usual manner.
*    Invocation :
*     CALL KFH_NLIST(A,NX,NY,X1,Y1,X2,Y2,FNAME,FACTOR)
*    Parameters :
*     A(NX,NY) = REAL
*           This array contains the image to be listed.
*     NX = INTEGER
*           The X-dimension of the image.
*     NY = INTEGER
*           The Y-dimension of the image.
*     X1 = INTEGER
*           The lower X bound of the region of the image.
*     Y1 = INTEGER
*           The lower Y bound of the region of the image.
*     X2 = INTEGER
*           The upper X bound of the region of the image.
*     Y2 = INTEGER
*           The upper Y bound of the region of the image.
*     FNAME = CHARACTER*(*)
*           The name of the file that is to hold
*           the listing.
*     FACTOR = REAL
*           The factor by which the pixel values
*           are to be multiplied.
*    Method :
*     A new file is created with the filename that
*     the user has given in KFH_NWLIST. The image
*     is divided into vertical strips of 30 pixels
*     wide , so that the first strip runs from
*     pixel 0 to 29. Each horizontal line of data
*     within the range of the strip is taken and
*     put into a buffer called LINE and written to
*     the file. If a FACTOR is specified , then the
*     data is multiplied by that figure before
*     writing it to file.
*    Authors :
*     P.T.Wallace
*     K.F.Hartley
*     S.Chan
*    History :
*     7 January 1982
*     26 september 1983
*    Type Definitions :
      IMPLICIT NONE
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER NX                         ! The X-dimension of the image.
      INTEGER NY                         ! The Y-dimension of the image.
      REAL A(NX,NY)                      ! The array holding the image.
      REAL FACTOR                        ! Figure by which the pixel
*                                        ! values are multiplied.
      CHARACTER*(*) FNAME                ! Variable which is to contain
*                                        ! the file name of the listing.
      INTEGER IX                         ! General variable.
      INTEGER IXE                        ! The last pixel of the strip.
      INTEGER IXS                        ! The first pixel of the strip.
      INTEGER IY                         ! General variable.
      INTEGER IZ                         ! The integer value of Z.
      INTEGER K                          ! Position pointer of the buffer.
      CHARACTER*128 LINE                 ! Line buffer which takes the
*                                        ! image data and transfers it
*                                        ! to the file.
      INTEGER NSTRIP                     ! Strip count.
      INTEGER TEMPX                      ! Temporary location.
      INTEGER TEMPY                      ! Temporary location.
      INTEGER X1                         ! The lower X bound of the region.
      INTEGER Y1                         ! The lower Y bound of the region.
      INTEGER X2                         ! The upper X bound of the region.
      INTEGER Y2                         ! The upper Y bound of the region.
      REAL Z                             ! Variable to hold the result
*                                        ! of FACTOR * pixel value.
*-

*
*    Re-arrange coordinates , if necessary.
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
*    Open report.
*

      OPEN (UNIT=1,FILE=FNAME,STATUS='NEW')

*
*    Initialise strip count.
*

      NSTRIP = 0

*
*    List in vertical strips each of 30 pixels wide.
*

      DO IXS = X1,X2,30

*
*       Initialise strip count.
*

         NSTRIP = NSTRIP + 1

*
*       To save paper a new page is thrown after each
*       strip if there are more than 8 strips.
*       This is really done to handle the 1-D case!
*

         IF (Y2.GT.8) THEN

            WRITE (1,'(''1STRIP'',I3,30X,
     :       ''DIVIDE LISTED VALUES BY'',G12.4/)') NSTRIP,FACTOR

         ELSE

            WRITE(1,'(''0STRIP'',I3,30X,
     :       ''DIVIDE LISTED VALUES BY'',G12.4/)') NSTRIP,FACTOR

         END IF

*
*       Report first and last X.
*

         IXE = MIN(IXS+29,X2)
         LINE = ' '
         WRITE (LINE(:12),'(I12)') IXS-1
         K = 4 + 4 * (IXE-IXS)
         WRITE (LINE(K+1:K+8),'(I8)') IXE-1
         WRITE (1,'(A/)') LINE

*
*       Line by line.
*

         DO IY = Y2,Y1,-1

*
*          Reset line buffer.
*

            LINE = ' '

*
*          Format line coordinate.
*

            WRITE (LINE(:5),'(I5)') IY-1

*
*          Pixel by pixel.
*

            DO IX = IXS,IXE

*
*             Buffer pointer.
*

               K = 9 + 4 * (IX-IXS)

*
*             Format pixel.
*

               Z = FACTOR*A(IX,IY)

               IF (ABS(Z).GT.1000.0) THEN

                  Z = SIGN(Z,1000.0)

               ENDIF

               IZ = NINT(Z)

               IF (IZ.LT.-99) THEN

                  LINE(K:K+3) = '----'

               ELSE IF (IZ.GT.999) THEN

                       LINE(K:K+3) = '++++'

               ELSE

                  WRITE (LINE(K:K+3),'(I4)') IZ

               END IF

*
*          Next pixel.
*

            END DO

*
*          Output the line.
*

            WRITE (1,'(A)') LINE

*
*       Next line.
*

         END DO

*
*    Next strip.
*

      END DO

*
*    Close report.
*

      WRITE (1,'(''1'')')
      CLOSE (UNIT=1)

*
*    Exit.
*

      END
