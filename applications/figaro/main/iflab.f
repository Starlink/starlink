      logical function iflab()
*+
* Name:
*    IFLAB

* Invocation:
*   (LOGICAL) = IFLAB()

* Purpose:
*  To find out if labels required for output plots.

* Description:
*  To find out if labels required for output plots.

* Arguments:
*     NONE

* Returned value:
*    IFLAB = LOGICAL (Given)
*        If labels required for plots)

* History:
*     T.N.Wilkins Manchester 7/6/88
*     T.N.Wilkins Cambridge 1/8/89 Bug fix
*-
      implicit none
      logical value,par_given,first
      save first,value
      data value,first/.true.,.true./

      if(first) then
        if(par_given('LABEL')) then
          call par_rdkey('LABEL',.true.,value)
        end if
        first = .false.
      end if
      iflab = value
      end
