      subroutine gr_conv(zone1,zone2,x,y)
*+
* Name:
*   GR_CONV

*  Purpose:
*    Convert coordinates between zones

*  Invocation:
*    CALL GR_CONV(ZONE1,ZONE2,X,Y)

*  Description:
*     The coordinates are converted

*  Arguments:
*    ZONE1 = INTEGER ARRAY (Given)
*      DIAGRAM zones for plot, convert from
*    ZONE2 = INTEGER ARRAY (Given)
*      DIAGRAM zones for plot, convert to
*    X = REAL (Given)
*      X coordinate
*    Y = REAL (Given)
*      Y coordinate

* Authors:
*   TNW: T.N.Wilkins, Durham

* History:
*   10-MAR-1994 TNW:
*     Original version
*-
      implicit none
      include 'gr_inc2'
      integer zone1,zone2
      real x,y

* Convert to NDC

      x = (x - diastate(5,zone1)) / (diastate(6,zone1) - diastate(5
     :     ,zone1))
      y = (y - diastate(7,zone1)) / (diastate(8,zone1) - diastate(7
     :     ,zone1))

      x = x * (diastate(2,zone1) - diastate(1,zone1)) + diastate(1,zone1
     :     )
      y = y * (diastate(4,zone1) - diastate(3,zone1)) + diastate(3,zone1
     :     )

* Convert to new zone

      x = (x - diastate(1,zone2)) / (diastate(2,zone2) - diastate(1
     :     ,zone2))
      y = (y - diastate(3,zone2)) / (diastate(4,zone2) - diastate(3
     :     ,zone2))

      x = x * (diastate(6,zone2) - diastate(5,zone2)) + diastate(5,zone2
     :     )
      y = y * (diastate(8,zone2) - diastate(7,zone2)) + diastate(7,zone2
     :     )
      end
