      logical function anytrue(reject,nrej)
*+
* Name:
*    ANYTRUE

* Invocation:
*   (LOGICAL) = ANYTRUE(REJECT,NREJ)
* Purpose:
*   Checks if any element of the logical array reject is .true.
*       - returns as .true. if any is.
*
* Description:
*   Checks if any element of the logical array reject is .true.
*       - returns as .true. if any is.
*
* Arguments:
*    REJECT(NREJ) = LOGICAL ARRAY (Given)
*        Rejection flags
*    NREJ = INTEGER (Given)
*        Dimension of above
*    ANYTRUE = LOGICAL (Returned)
*        True if any of REJECT true
*-
      implicit none
      integer nrej
      logical reject(nrej)
      integer i

      i = 1
      anytrue=.false.
      do while((.not.anytrue).and.(i.le.nrej))
        anytrue = reject(i)
        i = i + 1
      end do
      end
