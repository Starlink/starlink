        subroutine get_centre(x,y,iwstart,iwend,centre,sigma)
*+
* Name:
*    GET_CENTRE

* Invocation:
*    CALL GET_CENTRE(X,Y,IWSTART,IWEND,CENTRE,SIGMA)

* Purpose:
*   To calculate the centroid and standard deviation of data within a
*   range.

* Description:
*   The centroid and standard deviation are calculated, after subtracting the
*   minimum value in the Y array

* Arguments:
*    X(NI) = REAL ARRAY (Given)
*       X axis array
*    Y(NI) = REAL ARRAY (Given)
*       Y data array
*    IWSTART = INTEGER (Given)
*       Start pixel index
*    IWEND = INTEGER (Given)
*       End pixel index
*    CENTRE = DOUBLE PRECISION (Returned)
*       Centre
*    SIGMA = DOUBLE PRECISION (Returned)
*       Standard deviation

* History:
*  Variance in same loop as centroid, TNW 21/3/91
*  Use IWEND as dimension of X,Y, TNW 27/10/93
*-
      implicit none
      integer iwstart
      integer iwend
      real x(iwend)
      real y(iwend)
      double precision sigma
      double precision centre
*
* local
*
      integer j
      double precision subtotd
      double precision subtotdx
      double precision var
      real tmp
      real tmpcen
      real minval,yval
*
*  Centroid and variance
*
      subtotd = 0.0d0
      subtotdx = 0.0d0
      var     = 0.0d0

*  Working centre

      tmpcen = x((iwstart+iwend)/2)

* Get minimum value of array

      minval = y(iwstart)
      do  j=iwstart+1,iwend
        minval = min(minval,y(j))
      end do
      do  j=iwstart,iwend
        tmp = x(j) - tmpcen
        yval = y(j) - minval
        subtotd  = subtotd + dble(yval)
        subtotdx = subtotdx + dble(yval * tmp)
        var = var + dble(y(j) * tmp * tmp)
      end do

      if(subtotd.gt.0.0) then

*     First calculate centre and variance, working in offset coords
*     (relative to tmpcen), then convert centre to x coords

        centre = subtotdx/subtotd
        var = var/subtotd - centre*centre
        sigma = sqrt(var)
        centre = centre + dble(tmpcen)
      else
        centre = 0.0d0
        sigma = 0.0d0
      end if
      end
