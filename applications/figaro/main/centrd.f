      subroutine centrd(chans,ncent,value,count,level,nok)
*+
* Name:
*    CENTRD

* Invocation:
*    CALL CENTRD(CHANS,NCENT,VALUE,COUNT,LEVEL,NOK)

* Purpose:
*    Find centroid

* Description:
*     Service routine for ARC functions
*     Performs a centroiding of the data contained in the array
*     ICHANS over NCENT points.  The centroided channel number
*     returned is expressed in terms of the first element of
*     ICHAN (i.e. 1).  The calling program should therefore
*     take this into account.
*
* Arguments:
*    CHANS(NCENT) = REAL ARRAY (Given)
*        is an array containing NCENT contiguous
*                        channels of data from an arc spectrum.
*    NCENT = INTEGER (Given)
*
*    LEVEL = REAL (Given)
*        is the discrimination level in counts.
*    VALUE = REAL (Returned)
*        is the centroided channel number returned.
*    COUNT = REAL (Returned)
*        is the count level of the channel in which the
*                         centroid occurs.
*    NOK = INTEGER (Returned)
*        is returned as non-zero if a line is detected,
*                        else zero.
*-
      implicit none
      integer ncent,nok,i,lcent
      integer i1,l
      real chans(ncent)
      real count,levcen,level,div,sum1,sum2,value
      real c1,c2
*
      nok=0
      sum1=0.
      sum2=0.
*
*   LCENT is the central channel of the block of data in
*         the data array CHANS.
*   LEVCEN is the count level in that channel
*
      lcent=(ncent+1)/2
      levcen=chans(lcent)
*
*   See if LEVCEN is above the local discrimination level
*
      if (levcen.ge.level) then
*
*   Look at channels <=LCENT for ascending with channel
*   number
*
        do i=2,lcent
          c1=chans(i)
          c2=chans(i-1)
          if (c1.lt.c2) go to 200
          sum1=sum1+c2
        end do
*
*   Look at channels >= LCENT for descending with channel
*   number
*
        l=lcent+1
        do i=l,ncent
          c1=chans(i)
          c2=chans(i-1)
          if (c2.lt.c1) go to 200
          sum2=sum2+c1
        end do
*
*   All O.K.  Evaluate centroided channel number
*
        div=2.*levcen
        if (div.ne.0.) then
          value=(sum2-sum1)/div
          i1=int(value+real(lcent)+.5)
          count=chans(i1)
          go to 210
        end if
      end if
*
*   No line found
*
  200 nok=1
  210 continue
      end
