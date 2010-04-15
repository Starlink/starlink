*+  KFH_HISTST - Computes the histogram of an image.
      SUBROUTINE KFH_HISTST(IMAGE,XDIM,YDIM,X1,Y1,X2,Y2,HGRAM,
     : NUMBIN,HMIN,HMAX,HIST,RNUMB,STATUS)
*    Description :
*     This routine determines the histogram of an image
*    Invocation :
*     CALL KFH_HISTST(IMAGE,XDIM,YDIM,X1,Y1,X2,Y2,HGRAM,NUMBIN,
*      HMIN,HMAX,HIST,RNUMB,STATUS)
*    Parameters :
*     IMAGE(XDIM,YDIM) = REAL
*           This array contains the image data.
*     XDIM = INTEGER
*           This is the X dimension of the image.
*     YDIM = INTEGER
*           This is the Y dimension of the image.
*     X1 = INTEGER
*           This is the lower X bound of the region
*           of the image.
*     Y1 = INTEGER
*           This is the lower Y bound of the region
*           of the image.
*     X2 = INTEGER
*           This is the upper X bound of the region
*           of the image.
*     Y2 = INTEGER
*           This is the upper Y bound of the region
*           of the image.
*     HGRAM(5000) = INTEGER
*           The array which returns the histogram to
*           the calling program.
*     NUMBIN = INTEGER
*           The number of bins that the histogram
*           will contain.
*     HMIN = REAL
*           The lower bound of the histogram.
*     HMAX = REAL
*           The upper bound of the histogram.
*     HIST = LOGICAL
*           The flag which indicates whether a prompt
*           for the number of bins is needed.
*     RNUMB = INTEGER
*           The previous number of bins of the histogram
*           of the region.
*     STATUS = INTEGER
*           The status value on entry to this subroutine.
*    Method :
*     First the minimum and maximum values of the
*     image are found along with the number of bins.
*     The user is prompted for the number of bins for
*     the histogram. If the histogram for this particular
*     region has not been previously determined , then
*     the default value is set to the range of pixel
*     values. Otherwise , it is set to the previously
*     chosen number of bins for the histogram of the
*     region. If the number of bins (calculated
*     or prompted) exceeds 5000 , then the value for
*     the number of bins is set to 5000. The histogram
*     for the region is then determined.
*    Authors :
*     S.Chan (RGVAD::KFH)
*    History :
*     15 September 1983:
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER XDIM                       ! The X-dimension of the image.
      INTEGER YDIM                       ! The Y-dimension of the image.
      INTEGER NELEMENTS                  ! number of elements mapped by NDF_MAP
      INTEGER HGRAM(5000)                ! Returned histogram.
      LOGICAL HIST                       ! Flag indicating whether a
*                                        ! prompt for the number of bins
*                                        ! is required.
      INTEGER PLACE                      ! temporary place holder
      INTEGER HSTLOC                     ! Locator to the histogram.
      INTEGER HSTPTR                     ! Pointer to the histogram.
      REAL HMIN                          ! The minimum value of the image.
      REAL HMAX                          ! The maximum value of the image.
      INTEGER I                          ! General variable.
      REAL IMAGE(XDIM,YDIM)              ! The array holding the image
*                                        ! data.
      INTEGER J                          ! General variable.
      INTEGER NUMBIN                     ! The number of bins in the
*                                        ! histogram.
      REAL RANGE                         ! The range of pixel values.
      INTEGER RNUMB                      ! Previous number of bins of
*                                        ! the histogram.
      INTEGER TEMPX                      ! Temporary location.
      INTEGER TEMPY                      ! Temporary location.
      INTEGER X1                         ! The lower X bound of the
*                                        ! region of the image.
      INTEGER Y1                         ! The lower Y bound of the
*                                        ! region of the image.
      INTEGER X2                         ! The upper X bound of the
*                                        ! region of the image.
      INTEGER Y2                         ! The upper Y bound of the
*                                        ! region of the image.
*-

*
*    If the status is bad, then return to the main program.
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
*       Calculate the minimum and maximum values
*       in the image data.
*

         HMIN = IMAGE(X1,Y1)
         HMAX = IMAGE(X1,Y1)

         DO I = Y1,Y2,1

            DO J = X1,X2,1

               IF (IMAGE(J,I).GT.HMAX) THEN

                  HMAX = IMAGE(J,I)

               ELSEIF (IMAGE(J,I).LT.HMIN) THEN

                  HMIN = IMAGE(J,I)

               ENDIF

            END DO

         END DO

*
*       If the histogram of this particular region has
*       not been calculated before , then set the default
*       number of bins to be the range of pixel values.
*

         IF (.NOT.HIST) THEN

*
*          Calculate the number of bins for
*          the histogram.
*

            RANGE = HMAX-HMIN+1
            NUMBIN = IFIX(RANGE)

*
*       Otherwise , set the default value to the number of
*       bins previously chosen for this histogram.
*

         ELSE

            NUMBIN = RNUMB

         ENDIF

*
*       Prompt the user for the number of bins.
*

         CALL PAR_GET0I('NUMBIN',NUMBIN,STATUS)

*
*       If the input is not legal , then set the number
*       of bins to the calculated value.
*

         IF (STATUS.NE.SAI__OK) THEN

            CALL ERR_ANNUL(STATUS)
            CALL PAR_CANCL('NUMBIN',STATUS)

         ENDIF

         CALL PAR_CANCL('NUMBIN',STATUS)

*
*       If the number of bins exceeds 5000 , then set the
*       number of bins to 5000.
*

         IF (NUMBIN.GT.5000) THEN

            NUMBIN = 5000

         ENDIF

*
*       Create space for the histogram.
*
         CALL NDF_TEMP( PLACE, STATUS )
         CALL NDF_NEW('_INTEGER', 1, 1, NUMBIN, PLACE, HSTLOC,
     :                STATUS)
         CALL NDF_MAP( HSTLOC, 'DATA', '_INTEGER', 'WRITE',
     :                  HSTPTR, NELEMENTS, STATUS )

*
*       Calculate the histogram.
*

         CALL KFH_HISTOGRAM(IMAGE,XDIM,YDIM,%VAL(HSTPTR),X1,Y1,
     :    X2,Y2,NUMBIN,HMIN,HMAX)
         CALL KFH_TRANSFER(%VAL(HSTPTR),HGRAM,NUMBIN,STATUS)

      ENDIF

      END
