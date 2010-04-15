      SUBROUTINE PHOTGREY( STATUS )
*+
*  Name:
*     PHOTGREY

*  Purpose:
*     Displays a grey scale image.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Plots an image as a greyscale on a suitable device.

*  Usage:
*     PHOTGREY

*  ADAM Parameters:
*     IN = NDF (Read)
*        Name of the image.
*     XSTART = _REAL (Read)
*        The first X pixel of the image to be displayed.
*     XEND = _REAL (Read)
*        The last X pixel of the image to be displayed.
*     YSTART = _REAL (Read)
*        The first Y pixel of the image to be displayed.
*     YEND = _REAL (Read)
*        The last Y pixel of the image to be displayed.
*     LOW = _REAL (Read)
*        The data value corresponding to black on the display.
*     HIGH = _REAL (Read)
*        The data value corresponding to white on the display
*     DEVICE = DEVICE (Read)
*           The image display device.

*  Authors:
*     NE: Nick Eaton (STARLINK - Durham University)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25 May 1988
*     22 Nov 1988  Updated with released version of AGI.
*     26 Nov 1990  Changed to NDF and new version of AGI.
*     12 Mar 1992  Unix version, GKS 7.4
*        Original version.
*     6-NOV-1996 (PDRAPER):
*        Converted prologue to new standard.
*     28-SEP-2004 (TIMJ):
*        Use CNF_PVAL
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER XSTART, XEND, YSTART, YEND
      REAL VMAX, VMIN
      LOGICAL DISPOK
      CHARACTER * ( DAT__SZLOC ) WLOC
      INTEGER BASID, BZONE, DATPIN, DX, DY, IDIMS( 2 ), INDF, ITEMP,
     :        IZONE, LBND( 2 ), MAXPOS( 2 ), MINPOS( 2 ), NDIM, NEL,
     :        NINVAL, NPENS, NPIX, PICID1, PICID2, PICID3, UBND( 2 ),
     :        WDIMS( 2 ), WORPIN
      REAL X1, X2, Y1, Y2
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


* Get the picture to plot. Abort if there are any problems.
      call NDF_BEGIN
      call NDF_ASSOC( 'IN', 'READ', indf, status )
      if ( status .ne. SAI__OK ) goto 99

* Check that a data is 2-dimensional
      call NDF_BOUND( indf, 2, lbnd, ubnd, ndim, status )
      if ( status .ne. SAI__OK ) goto 99
      if ( ndim .ne. 2 ) then
         status = sai__error
         call ERR_REP( 'PHOTGREY_BOUNDS',
     :                 'Data array not 2-dimensional', status )
         goto 99
      endif

* Calculate the pixel dimensions and output them
      idims( 1 ) = ubnd( 1 ) - lbnd( 1 ) + 1
      idims( 2 ) = ubnd( 2 ) - lbnd( 2 ) + 1
      call MSG_SETI( 'NX', idims( 1 ) )
      call MSG_SETI( 'NY', idims( 2 ) )
      call MSG_OUT( ' ', 'Array dimensions are ^NX, ^NY', status )

* Map the data. If an error occurs then abort
      call NDF_MAP( indf, 'DATA', '_REAL', 'READ', datpin, nel, status )
      if ( status .ne. SAI__OK ) goto 99

* Find out the dimensions of the array to plot
      call PAR_DEF0I( 'XSTART', 1, status )
      call PAR_GET0I( 'XSTART', xstart, status )
      call PAR_DEF0I( 'XEND', idims( 1 ), status )
      call PAR_GET0I( 'XEND', xend, status )
      call PAR_DEF0I( 'YSTART', 1, status )
      call PAR_GET0I( 'YSTART', ystart, status )
      call PAR_DEF0I( 'YEND', idims( 2 ), status )
      call PAR_GET0I( 'YEND', yend, status )

* Check that these have sensible values. If not set to default values
      if ( xstart .gt. xend ) then
         itemp = xstart
         xstart = xend
         xend = itemp
      endif
      if ( ystart .gt. yend ) then
         itemp = ystart
         ystart = yend
         yend = itemp
      endif
      if ( xstart .lt. 1 ) xstart = 1
      if ( xend .gt. idims( 1 ) ) xend = idims( 1 )
      if ( ystart .lt. 1 ) ystart = 1
      if ( yend .gt. idims( 2 ) ) yend = idims( 2 )

* Get some workspace for the temporary array. Abort on error
      wdims( 1 ) = xend - xstart + 1
      wdims( 2 ) = yend - ystart + 1
      dx = wdims( 1 )
      dy = wdims( 2 )
      call AIF_TEMP( '_INTEGER', 2, wdims, wloc, status )
      call DAT_MAPI( wloc, 'WRITE', 2, wdims, worpin, status )
      if ( status .ne. SAI__OK ) goto 99

* Find the maximum and minimum data values in the subarray
* Use the KAPGEN routine MAXMIN
      call MAXMIN( idims( 1 ), idims( 2 ), %val( CNF_PVAL(datpin) ),
     :             xstart, ystart, xend, yend, npix, ninval, vmax, vmin,
     :             maxpos, minpos, status )

* Get the black and white parameters, using max and min as defaults
      call PAR_DEF0R( 'LOW', vmin, status )
      call PAR_GET0R( 'LOW', vmin, status )
      call PAR_DEF0R( 'HIGH', vmax, status )
      call PAR_GET0R( 'HIGH', vmax, status )

* Abort if the levels are the same
      if ( ( abs( vmax - vmin ) .lt. 1.001 / abs( VAL__BADR ) ) .or.
     :     ( status .ne. SAI__OK ) ) then
         call ERR_REP( 'PHOTGREY_VALUES',
     :                 'The values must be different', status )
         goto 99
      endif

* Open the database. Clear the workstation entry.
      call AGI_ASSOC( 'DEVICE', 'WRITE', picid1, status )
      call AGI_BEGIN
      call AGI_IBASE( basid, status )
      call AGI_SELP( basid, status )
      call AGI_PDEL( status )

* Start up SGS on that device.
      call AGS_ACTIV( status )
      call AGS_NZONE( bzone, status )

* Save the base zone in the database
      call AGS_SZONE( 'FRAME', 'PHOTGREY', picid2, status )

* Check that the status is OK
      if ( status .ne. SAI__OK ) goto 99

* Check the device and set up the colour table
      call SETCOL( dispok, npens, status )
      if ( .not. dispok ) then
         call ERR_REP( 'PHOTGREY_DEVICE', 'Device not suitable',
     :                 status )
         goto 99
      endif

* Scale the data array into the workspace array
      call MSG_OUT( ' ', 'Scaling image', status )
      call GREY( idims( 1 ), idims( 2 ), %val( CNF_PVAL( datpin ) ),
     :           xstart, xend, ystart, yend, dx, dy,
     :           %val( CNF_PVAL( worpin ) ),
     :           npens, vmax, vmin )

* Create an SGS zone of the correct shape, best filling the display
      call SGS_ZSHAP( real( dx ) / real( dy ), 'CC', izone, status )

* Set up the world coordinates to reflect the pixel coordinates
      x1 = real( xstart ) - 1.0
      x2 = real( xend )
      y1 = real( ystart ) - 1.0
      y2 = real( yend )
      call SGS_SW( x1, x2, y1, y2, status )

* Plot out the image
      call MSG_OUT( ' ', 'Plotting image', status )
      call GCA( x1, y2, x2, y1, dx, dy, 1, 1, dx, dy,
     :             %val( CNF_PVAL( worpin ) ) )

* Make a database entry
      call AGS_SZONE( 'DATA', 'PHOTGREY', picid3, status )

* Make sure the frame is the current picture on closing AGI
      call AGI_END( picid2, status )

* Close down SGS and AGI
      call AGS_DEACT( status )
      call AGI_CANCL( 'DEVICE', status )

  99  continue

* Close down
      call AIF_ANTMP( wloc, status )
      call NDF_END( status )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'PHOTGREY_ERR',
     :        'PHOTGREY: Error displaying image.', STATUS )
      END IF

      END

*-----------------------------------------------------------------------
*+  MAXMIN - returns the max and min values found in a defined 2-d
*            sub-array

      SUBROUTINE MAXMIN ( DIM1, DIM2, ARRAY, XSTART, YSTART, XFINSH,
     :                    YFINSH, NUMPIX, NINVAL, MAXMUM, MINMUM,
     :                    MAXPOS, MINPOS, STATUS )
*
*    Description :
*
*     This routine returns the maximum and minimum values found in
*     a specified subsection of an input 2-d array, where it found
*     the maxima and minima, and the number of good and bad pixels in
*     the sub-array.
*
*    Invocation :
*
*     CALL MAXMIN( DIM1, DIM2, ARRAY, XSTART, YSTART, XFINSH, YFINSH,
*    :             NUMPIX, NINVAL, MAXMUM, MINMUM, MAXPOS, MINPOS,
*    :             STATUS )
*
*    Arguments :
*
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     ARRAY( DIM1, DIM2 )  =  REAL( READ )
*         Input array of data
*     XSTART  =  INTEGER( READ )
*         X start co-ordinate of sub-array
*     YSTART  =  INTEGER( READ )
*         Y start co-ordinate of sub-array
*     XFINSH  =  INTEGER( READ )
*         X finish co-ordinate of sub-array
*     YFINSH  =  INTEGER( READ )
*         Y finish co-ordinate of sub-array
*     NUMPIX  =  INTEGER ( WRITE )
*         Number of pixels in sub-array
*     NINVAL  =  INTEGER ( WRITE )
*         Number of bad pixels in sub-array
*     MAXMUM  =  REAL ( WRITE )
*         Maximum value found in sub-array
*     MINMUM  =  REAL ( WRITE )
*         Minimum value found in sub-array
*     MAXPOS( 2 )  =  INTEGER ( WRITE )
*         X,Y co-ordinates of the pixel where the maximum value is
*           (first) found.
*     MINPOS( 2 )  =  INTEGER ( WRITE )
*         X,Y co-ordinates of the pixel where the minimum value is
*           (first) found.
*     STATUS = INTEGER ( READ )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     If sub-array co-ordinates are invalid then
*        Report error and set bad status
*        Set all returned values to zero or undefined
*        Return
*     Endif
*     If sub-array co-ordinates are transposed then
*        Swap them
*     Endif
*     Work out number of pixels in sub-array
*     Initialise max and min to be equal to value of the
*       bottom left pixel of the specified subarray
*     For all lines of the sub-array
*        For all valid pixels in the current line
*           If pixel not bad then
*              Compare current max and min with current pixel value,
*                swap if necessary, and keep positions updated
*           Else
*              Count bad pixel
*           Endif
*        Endfor
*     Endfor
*     If all pixels in sub-array were invalid then
*        Report error and set bad status
*     Endif
*     Return
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm J. Currie  STARLINK (RAL::CUR)
*
*    History :
*
*     14-04-1986 : First implementation (REVA::MJM)
*     1986 Aug 12: Completed prologue and nearly conformed to
*                  Starlink standards (RAL::CUR).
*     1986 Sep  2: Renamed parameters -> arguments section in prologue,
*                  added bad-pixel handling and tidied (RAL::CUR).
*     1988 Jul 13: Added error reporting and check for all undefined
*                  pixels (RAL::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RAL::CUR).
*     1990 Feb 18: Added argument for the number of invalid pixels
*                  (RAL::CUR).
*     1990 Mar  8: Made position of the minimum and maximum undefined
*                  when the sub-array definition is in error (RAL::CUR).
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'PRM_PAR'          ! Magic-value definitions

*    Import :

      INTEGER
     :  DIM1, DIM2,
     :  XSTART,
     :  YSTART,
     :  XFINSH,
     :  YFINSH

      REAL
     :  ARRAY( DIM1, DIM2 )

*    Export :

      INTEGER
     :  NINVAL,
     :  NUMPIX,
     :  MAXPOS( 2 ),
     :  MINPOS( 2 )

      REAL
     :  MAXMUM,
     :  MINMUM

*    Status :

      INTEGER  STATUS

*    Local variables :

      INTEGER
     :  DUMMY,                    ! Used in transposing sub-array coords
     :  I, J                      ! Counters

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    check the sub-array co-ordinates for validity

      IF ( XSTART .LT. 1 .OR. XSTART .GT. DIM1 .OR.
     :     YSTART .LT. 1 .OR. YSTART .GT. DIM2 .OR.
     :     XFINSH .LT. 1 .OR. XFINSH .GT. DIM1 .OR.
     :     YFINSH .LT. 1 .OR. YFINSH .GT. DIM2 ) THEN

*       report error

         CALL ERR_REP( 'ERR_MAXMIN_ARLIM',
     :     'MAXMIN: Sub-array limits outside array dimensions', STATUS )

         STATUS = SAI__ERROR

*       set returned values to sensible numbers and return

         NUMPIX       =  0
         NINVAL       =  0
         MAXMUM       =  VAL__BADR
         MINMUM       =  VAL__BADR
         MAXPOS( 1 )  =  VAL__BADI
         MAXPOS( 2 )  =  VAL__BADI
         MINPOS( 1 )  =  VAL__BADI
         MINPOS( 2 )  =  VAL__BADI

         GOTO 999
      END IF

*    check sub-array x co-ordinates for order

      IF ( XSTART .GT. XFINSH ) THEN
         DUMMY   =  XSTART
         XSTART  =  XFINSH
         XFINSH  =  DUMMY
      END IF

*    check sub-array y co-ordinates for order

      IF ( YSTART .GT. YFINSH ) THEN
         DUMMY   =  YSTART
         YSTART  =  YFINSH
         YFINSH  =  DUMMY
      END IF

*    work out number of pixels in sub-array

      NUMPIX  =  ( XFINSH - XSTART + 1 ) * ( YFINSH - YSTART + 1 )

*    initialise max and min variables to be equal to the value of
*    the lower left corner of the specified subarray, and the
*    positions of each to be there

      MINMUM      =  ABS( VAL__BADR )
      MAXMUM      =  -MINMUM
      MAXPOS( 1 ) =  XSTART
      MAXPOS( 2 ) =  YSTART
      MINPOS( 1 ) =  XSTART
      MINPOS( 2 ) =  YSTART
      NINVAL      =  0

*    loop round all the lines of the specified subarray

      DO  J  =  YSTART, YFINSH

*       loop round all the pixels in the current line

         DO  I  =  XSTART, XFINSH

*          test for valid pixel

            IF ( ARRAY( I, J ) .NE. VAL__BADR ) THEN

*             check current maximum against current pixel value

               IF ( ARRAY( I, J ) .GT. MAXMUM ) THEN
                  MAXMUM  =  ARRAY( I, J )
                  MAXPOS( 1 )  =  XSTART + I - 1
                  MAXPOS( 2 )  =  YSTART + J - 1
               END IF

*             check current minimum against current pixel value

               IF ( ARRAY( I, J ) .LT. MINMUM ) THEN
                  MINMUM  =  ARRAY( I, J )
                  MINPOS( 1 )  =  XSTART + I - 1
                  MINPOS( 2 )  =  YSTART + J - 1
               END IF
            ELSE

*             One more bad pixel to the count.

               NINVAL = NINVAL + 1
            END IF

*        end of loop round pixels in current line

         END DO

*    end of loop round specified lines

      END DO

*    check to see if all pixels are undefined

      IF ( MINMUM .EQ. ABS( VAL__BADR ) .AND. MAXMUM .EQ. -MINMUM ) THEN
         CALL ERR_REP( 'ERR_MAXMIN_ARINV',
     :     'MAXMIN: All pixels in the sub-array are invalid', STATUS )
         STATUS = SAI__ERROR
      END IF

 999  CONTINUE

*    return and end

      END
*-----------------------------------------------------------------------
*+  SETCOL - set the colour table for the device
      subroutine setcol( dispok, npens, status )
*
*    Description :
*     This checks that the image display is suitable, and then sets the
*     colours to be grey levels with equal spacing between the pens.
*     Pens 0 and 1 are not set.
*
*    Invocation :
*     call setcol( dispok, npens, status )
*
*    Arguments
*     dispok=logical(returned)
*      is the display device suitable
*     npens=integer(returned)
*      number of pens on device
*     status=integer(given and returned)
*
*    Method :
*     Check status on entry.
*     Get workstation id and type for the current workstation.
*     Check it is a raster device, and enquire the number of pens supported.
*     Set the colour of each pen, not including pens 0 and 1, to be
*     a grey level, with equal intensity spacings between pens.
*
*    Bugs :
*     None known.
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     26 May 1988
*    endhistory
*
*    Type Definitions :
      implicit none

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'GKS_PAR'

*    Export :
      logical dispok
      integer npens

*    Status :
      integer status

*    Local variables :
      integer conid, j, n1, n2, n3, n4, n5, wkclas, wkid, wtype
      real colour, scale
*-

* Check status on entry
      if ( status .ne. SAI__OK ) goto 99

* Get workstation id and type
      call SGS_ICURW( wkid )
      call GQWKC( wkid, status, conid, wtype )

* Check that it is a raster display and enquire the number of pens
      call GQWKCL( wtype, status, wkclas )
      call GQLWK( wtype, status, n1, n2, n3, n4, n5, npens )

* Does the display live up to expectations, that is
* a raster display with at least 8 pens.
      if ( ( wkclas .eq. GRASTR ) .and. ( npens .ge. 8 ) .and.
     :     ( status .eq. SAI__OK ) ) then
         dispok = .true.
      else
         dispok = .false.
         goto 99
      endif

* Set up the colour table as grey levels between 0 and 1
      scale = real( npens - 3 )
      do j = 2, npens - 1
         colour = real( j - 2 ) / scale
         call GSCR( wkid, j, colour, colour, colour )
      enddo

  99  continue

      end

*-----------------------------------------------------------------------
*+  GREY - scale data array to represent colour pen indices
      subroutine grey( nx, ny, data, xstart, xend, ystart, yend,
     :                 ix, iy, image, npens, vmax, vmin )
*
*    Description :
*     Scale the subarray of the data so that the image array contains
*     colour pen indices, corresponding to the grey level to be plotted.
*
*    Invocation :
*     call grey( nx, ny, data, xstart, xend, ystart, yend, ix, iy,
*    :           image, npens, vmax, vmin )
*
*    Arguments :
*     nx=integer(given)
*           first dimension of data array
*     ny=integer(given)
*           second dimension of data array
*     data=real(nx,ny)(given)
*           data array
*     xstart=integer(given)
*           starting index of subarray
*     xend=integer(given)
*           finishing index of subarray
*     ystart=integer(given)
*           starting index of subarray
*     yend=integer(given)
*           finishing index of subarray
*     ix=integer(given)
*           first dimension of image array
*     iy=integer(given)
*           second dimension of image array
*     image=integer(dx,dy)(given and returned)
*           image array
*     npens=integer(given)
*           number of pens available on diplay
*     vmax=real(given)
*           data value corresponding to highest plotted value
*     vmin=real(given)
*           data value corresponding to lowest plotted value
*
*    Method :
*     Calculate the scaling factor corresponding to the data interval
*     between each pen.
*     For each column
*        For each row
*           Convert each data point to a rough pen number using the
*           scaling factor.
*           Convert this into an integer pen number, ensuring that it
*           will be within the range of the number of pens set.
*           Convert this into the true pen number and store in image array.
*        Endfor
*     Endfor
*
*    Bugs :
*     None known.
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     	26 May 1988
*       19 Aug 1991  Added bad pixel handling
*    endhistory
*
*    Type Definitions :
      implicit none

*    Global constants :
      INCLUDE 'PRM_PAR'

*    Import :
      integer ix, iy, npens, nx, ny, xend, xstart, yend, ystart
      real data( nx, ny ), vmax, vmin

*    Import-Export :
      integer image( ix, iy )

*    Local variables :
      integer idata, ii, j, jj, jx, jy
      real factor, rdata
*-

* Calculate the scaling factor
      factor = real( npens - 3 ) / ( vmax - vmin )

* Calculate the pen indices corresponding to the data values
* Note, have to reverse the y axis for normal orientation
      do j = 1, iy
         jj = iy - j + 1
         jy = j + ystart - 1
         do ii = 1, ix
            jx = ii + xstart - 1
            if ( data( jx, jy ) .ne. val__badr ) then
               rdata = ( data( jx, jy ) - vmin ) * factor
               idata = max( 0, min( int( rdata ), npens - 3 ) )
            else
               idata = -2
            endif
            image( ii, jj ) = idata + 2
         enddo
      enddo

      end

* $Id$
