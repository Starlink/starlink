      subroutine split( y, N, CHANGE )
*+
* Name:
*    SPLIT

* Invocation:
*    CALL SPLIT( Y, N, CHANGE )

* Purpose:
*  Find 2 flats in y() and apply splitting algorithm
* Description:
*  Find 2 flats in y() and apply splitting algorithm
      implicit none
      integer n
      real y(n)
*-
      logical CHANGE
      logical LOOP

      integer six
      parameter ( SIX = 6 )
      real w(SIX) , y1
      integer I1, i , nm2

* w() is a window 6 points wide which is slid along y()
      Nm2 = n - 2
      do i = 1 , 4
        w(i+ 2) = y(i)
      end do

      w(2) = y(3)
      i1 = 1
      do while ( LOOP)

* if y(1) = y(2)  .NE. y(3)  treat first 2 like a 2-flat with
* the end pt rule.

* w(3) and w(4) form a 2-flat ?

        if ( w(3) .EQ. w(4) ) then


          if ( (w(3) - w(2)) * (w(5) - w(4)) .LT. 0.0 ) then

* apply the right end pt rule at i1

            if (i1 .ge. 3 ) then
              y1 = 3.0 * w(2) - 2.0 * w(1)
              call medof3(y1, w(3), w(2) , y(i1) , CHANGE)
            end if

* apply the left end pt rule at i1 = 1

            if (i1 .lt. nm2 ) then
              y1 = 3.0 * w(5) - 2.0 * w(6)
              call medof3(y1, w(4), w(5) , y(i1 + 1) , CHANGE)
            end if
          end if
        end if
* slide the window

        do i = 1, 5
          w(i) = w(i + 1)
        end do
        i1 = i1 + 1

        if ( i1 .LT. nm2) then

          w(6) = y(i1 + 3)

* apply the rule to thelast 2 points if needed

        else
          w(6) = w(3)
        end if

        if ( i1 .GE. n) then

          LOOP = .FALSE.
        end if
      end do


      end
