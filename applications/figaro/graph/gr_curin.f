      subroutine gr_curin(pen,status)
*+
* Name:
*    GR_CURIN

* Invocation:
*    CALL GR_CURIN(PEN,STATUS)

* Purpose:
*  To initialise the cursor. This is for pick_regions etc.
*
* Description:
*  To initialise the cursor. This is for pick_regions etc.
*
* Arguments:
*   PEN = INTEGER (Given)
*     Graphics pen to select
*   STATUS = INTEGER (Given and returned)
*     Error status, 0=ok

* Authors:
*   T.N.Wilkins, Cambridge, 13-NOV-1989
*-
      implicit none
      include 'SAE_PAR'
      integer pen
      integer status
      integer pstat,len1
      character*3 string

*

      if(status.ne.SAI__OK) return
      call pgqinf('cursor',string,len1)
      if(string.eq.'YES') then
*
* Set up dotted lines
*
        call gr_spen(pen)
      else
        call par_wruser('Cursor not available',pstat)
        status = SAI__ERROR
      end if
      end
