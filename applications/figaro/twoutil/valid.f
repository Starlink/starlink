      logical function valid(lower,upper,value,name)
*+
* Name:
*    VALID

* Invocation:
*   (LOGICAL) = VALID(LOWER,UPPER,VALUE,NAME)
*
* Description:
*   Check if value in range, and tell user if not!
* Purpose:
*   Check if value in range, and tell user if not!
* Arguments:
*     LOWER = INTEGER (Given)
*        Lower limit
*     UPPER = INTEGER (Given)
*        upper limit
*     NAME (c* = INTEGER (Given)
*        Name of variable
*     VALUE = INTEGER (Given)
*        Value to test
*     VALID = LOGICAL (Returned)
*        True if in range
* Author:
*    T.N.Wilkins, Cambridge, 29th April 1992
*        "           "       9/6/92 Bug fix
*-
      implicit none
      integer lower,upper,value
      character*(*) name
      character*72 chars
      integer len1,pstat

      valid = (value.ge.lower).and.(value.le.upper)
      if(.not.valid) then
        len1 = 0
        call chr_appnd(name,chars,len1)
        call chr_putc(' must be in range ',chars,len1)
        call chr_puti(lower,chars,len1)
        call chr_putc(' to ',chars,len1)
        call chr_puti(upper,chars,len1)
        call par_wruser(chars(:len1),pstat)
      endif
      end
