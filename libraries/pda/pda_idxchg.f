

      FUNCTION PDA_IDXCHG(x,y,i1,i2,i3,i4,istat)

c this function determines whether or not the exchange of two
c triangles is necessary on the basis of max-min-angle criterion
c by c. l. lawson.

c the input parameters are
c     x,y = arrays containing the coordinates of the data
c           points,
c     i1,i2,i3,i4 = point numbers of four points p1, p2,
c           p3, and p4 that form a quadrilateral with p3
c           and p4 connected diagonally.
c this function returns an integer value 1 (one) when an ex-
c change is necessary, and 0 (zero) otherwise.

c declaration statements
      dimension x(100), y(100)
      equivalence (c2sq, c1sq), (a3sq, b2sq), (b3sq, a1sq), (a4sq, b1sq)
     :             , (b4sq, a2sq), (c4sq, c3sq)

      integer istat

c preliminary processing
      x1 = x(i1)
      y1 = y(i1)
      x2 = x(i2)
      y2 = y(i2)
      x3 = x(i3)
      y3 = y(i3)
      x4 = x(i4)
      y4 = y(i4)

c calculation
      idx = 0
      u3 = (y2-y3)*(x1-x3) - (x2-x3)*(y1-y3)
      u4 = (y1-y4)*(x2-x4) - (x1-x4)*(y2-y4)
      if ( u3*u4.gt.0.0 ) then
         u1 = (y3-y1)*(x4-x1) - (x3-x1)*(y4-y1)
         u2 = (y4-y2)*(x3-x2) - (x4-x2)*(y3-y2)
         a1sq = (x1-x3)**2 + (y1-y3)**2
         b1sq = (x4-x1)**2 + (y4-y1)**2
         c1sq = (x3-x4)**2 + (y3-y4)**2
         a2sq = (x2-x4)**2 + (y2-y4)**2
         b2sq = (x3-x2)**2 + (y3-y2)**2
         c3sq = (x2-x1)**2 + (y2-y1)**2

         s1sq = u1*u1/(c1sq*amax1(a1sq,b1sq))
         s2sq = u2*u2/(c2sq*amax1(a2sq,b2sq))
         s3sq = u3*u3/(c3sq*amax1(a3sq,b3sq))
         s4sq = u4*u4/(c4sq*amax1(a4sq,b4sq))

         if ( amin1(s1sq,s2sq).lt.amin1(s3sq,s4sq) ) idx = 1
      end if

      PDA_IDXCHG = idx
      return
      end
