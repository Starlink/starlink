      subroutine gr_annul(dia,status)
*+
* Name:
*    GR_ANNUL

* Invocation:
*    CALL GR_ANNUL(DIA,STATUS)

* Purpose:
*   Annul diagram slot.

* Description:
*   This has the effect of freeing the workspace for future use.
*
* Arguments:
*   DIA = INTEGER (Given)
*      Pointer to memory (common block storage).
*   STATUS = INTEGER (Returned)
*      Error status, 0=ok

* History:
*  T.N.Wilkins Manchester
*      "       Cambridge, renamed 22/6/90
*-
      implicit none
      include 'SAE_PAR'
      integer dia
      integer status
      include 'gr_inc2'

      if((dia.le.0).or.(dia.gt.max_dia).or.(gr_slot(dia).eq.0)) then
        status = SAI__ERROR
      else
        gr_slot(dia) = 0
        if(old_dia.eq.dia) if_old = .false.
      end if
      end
