      subroutine gr_open(dia,status)
*+
* Name:
*    GR_OPEN

* Invocation:
*    CALL GR_OPEN(DIA,STATUS)

* Purpose:
*   To open a zone, that is a set of stored parameters.

* Description:
*   To open a zone, that is a set of stored parameters.

* Arguments:
*    DIA = INTEGER (Returned)
*        Pointer to store space
*    STATUS = INTEGER (Returned)
*        Error status 0=ok

* History:
*    T.N.Wilkins Manchester
* Change to use a slot system for diagrams, TNW 25/8/88
* TNW 29/11/88 Changed to use getwork
*
      implicit none
      include 'gr_inc'
      include 'gr_inc2'
      include 'SAE_PAR'
*-
      integer dia
      integer status
      integer i

* Find free slot

      do i=1,max_dia
        if(gr_slot(i).eq.0) then
          dia = i
          goto 1
        end if
      end do
      status = SAI__ERROR
      return
   1  continue

      call pgqvp(0,diastate(1,dia),diastate(2,dia),diastate(3,dia),
     :              diastate(4,dia))

* Dummy values

      gr_slot(dia) = 1
      diastate(5,dia) = 0.0
      diastate(6,dia) = 1.0
      diastate(7,dia) = 0.0
      diastate(8,dia) = 1.0
      old_dia = dia
      if_old = .true.
      end
