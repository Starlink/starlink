      REAL FUNCTION ESP1_MEDIAN(V,GOOD,N,WS,STATUS)
*+
*   Name:
*     ESP1_MEDIAN
*
*   Purpose:
*     Calculate the median of the good pixels of the array V(N).
*     Non-destructive - ie, it does not reorder V.
*
*   Language:
*     Fortran 77
*
*   Invocation:
*     MEDIAN = ESP1_MEDIAN (V, GOOD, N, %VAL(WS), STATUS)
*
*   Description:
*     Uses a destructive selection algorithm to find the median element
*     in V after copying it to a workspace WS, which the caller
*     guarantees is at least N elements in size. The algorithm is based
*     on that described in Numerical Recipes (in C), without using any
*     actual code from there.
*
*   Errors:
*     Sets STATUS to be sai__error and returns 0.0 if there are no
*     good pixels, or if either of the sanity-check assertions at the
*     end fails.
*
*   Arguments:
*     V(N) = REAL (Given)
*       The array we're to find the median of
*     GOOD(N) = INTEGER (Given)
*       GOOD(i) is non-zero if we are to use the element V(i).
*     N = INTEGER (Given)
*       The size of the arrays V and GOOD
*     WS(N) = REAL (Given)
*       A workspace, into which we may temporarily copy V.  The actual
*       workspace will be at least N in length.
*     STATUS = INTEGER (Given and returned)
*       Inherited status
*
*   Authors:
*     NG: Norman Gray (Starlink, Glasgow)
*
*   History:
*     18 Aug 1998
*       (Original version)
*
*   Bugs:
*     None known at present.
*-

*   No implicit typing
      implicit none

*   Arguments:
      integer n                 ! Size of the arrays
      real v(n)                 ! Array we want the median of
      integer good(n)           ! Only examine v(i) when good(i)>0
      real ws(n)                ! Workspace
      integer status            ! Inherited status

*   Local variables
      integer i,j               ! loop counter
      integer l                 ! Number of `good' elements
      integer medind            ! middle index - location of median in
                                ! order
      integer hi,lo,pei
      real pe                   ! Value of partitioning element
      real temp

*   Global symbols
      include 'SAE_PAR'


      if (status .ne. sai__ok) then
         esp1_median = 0.0
         goto 9999
      endif

*   First, copy the good elements of the array to the workspace
      l = 0
      do i=1,n
         if (good(i) .gt. 0) then
            l = l+1
            ws(l) = v(i)
         endif
      enddo
      if (l .eq. 0) then
*      We don't have _any_ good pixels?!
         call err_rep (' ','esp1_median: no good pixels', status)
         status = sai__error
         esp1_median = 0.0
         goto 9999
      endif

*   Get middle index - median should be (v(l/2)+v(l/2+1))/2 if l is odd,
*   but this makes little difference as long as l >~ 100 or so.
      medind = (l+1)/2

      lo = 1
      hi = l

      do while (hi-lo .gt. 1)

         pei = (lo+hi)/2        ! Index of pe in middle (more efficient
                                ! if array already partly ordered?)
         pe = ws(pei)

*      `Remove ws(pei) from the array' by overwriting it with ws(lo).
*      Now deal only with array ws(lo+1..hi), which has pe missing.
         ws(pei) = ws(lo)

*      Make sure ws(lo+1) <= pe <= ws(hi), to ensure `i+1' and `j-1'
*      loops below must terminate.
         if (ws(lo+1) .gt. ws(hi)) then
            temp = ws(lo+1)
            ws(lo+1) = ws(hi)
            ws(hi) = temp
         endif
         if (pe .gt. ws(hi)) then
            temp = pe
            pe = ws(hi)
            ws(hi) = temp
         endif
         if (ws(lo+1) .gt. pe) then
            temp = pe
            pe = ws(lo+1)
            ws(lo+1) = temp
         endif

         i = lo+1
         j = hi
*      Inner loop
         do while (.true.)
*         Find a ws(i) >= pe
            i = i+1
            do while (ws(i) .lt. pe)
               i = i+1
            enddo
*         Find a ws(j) <= pe
            j = j-1
            do while (ws(j) .gt. pe)
               j = j-1
            enddo
*Assert: ws(i)>=pe, ws(j)<=pe, i>lo+1, j<hi
*         As long as i<j we have found a pair of elements on the wrong
*         side of the partitioning element, which should be swapped before
*         trying again (i==j can only happen if there's an even number
*         in the subarray, and ws(j)==pe).
            if (i .ge. j) goto 100 ! LEAP OUT
*         Swap them
            temp = ws(i)
            ws(i) = ws(j)
            ws(j) = temp
         enddo
 100     continue

*      The partitioning element is notionally located _between_ ws(j)
*      and ws(j+1).  Reinsert it by pushing ws(j) to ws(lo) and putting
*      ws(j)=pe
         ws(lo) = ws(j)
         ws(j) = pe
*Assert: ws(lo..j-1) <= ws(j) <= ws(j+1..hi)
*Assert: ...which implies ws(1..j-1) <= ws(j) <= ws(j+1..l)

*      Now concentrate on the partition which contains medind
         if (medind .le. j) hi = j
         if (j .le. medind) lo = j

      enddo

*   Either hi=lo(=medind) (and we're finished), or else hi=lo+1,
*   and the result is the median of this two element array after
*   `sorting'.
      if (hi .eq. lo) then
         if (hi .ne. medind) then
            call err_rep (' ',
     :           'esp1_median: assertion failed: hi.ne.medind',
     :           status)
            status = sai__error
            esp1_median = 0.0
            goto 9999
         endif
         esp1_median = ws(lo)
      elseif (hi .eq. lo+1) then
         if (ws(lo) .le. ws(hi)) then
            esp1_median = ws(lo)
         else
            esp1_median = ws(hi)
         endif
      else
         call err_rep (' ',
     :        'esp1_median: assertion failed: hi.ne.lo+1',
     :        status)
         status = sai__error
         esp1_median = 0.0
         goto 9999
      endif

 9999 continue

      end
