      subroutine gr_seld(dia,status)
*+
* Name:
*    GR_SELD

* Invocation:
*    CALL GR_SELD(DIA,STATUS)

* Purpose:
*  To select a "zone" in PGPLOT.

* Description:
*  To select a "zone" in PGPLOT. This was designed to replace the
* DIAGRAM routine dia_seld.
*
* Arguments:
*    DIA = INTEGER (Given)
*        zone to select
*    STATUS = INTEGER (Returned)
*
*
*    T.N.Wilkins Manchester
*    Name changed TNW 8/7/88
*    PGPLOT version, TNW 3/90 Cambridge
*
      implicit none
      include 'gr_inc2'
      include 'SAE_PAR'
*-
      integer dia
      integer status

* Check diagram exists

      if((dia.le.0).or.(dia.gt.max_dia).or.(gr_slot(dia).eq.0)) then
        status = SAI__ERROR
      else

* Save current state of PGPLOT for future use

        if(if_old) then
          call pgqvp(0,diastate(1,old_dia),diastate(2,old_dia),
     :              diastate(3,old_dia),diastate(4,old_dia))
          call pgqwin(diastate(5,old_dia),diastate(6,old_dia),
     :              diastate(7,old_dia),diastate(8,old_dia))
        end if

* restore diagram as selected

        call pgvport(diastate(1,dia),diastate(2,dia),diastate(3,dia),
     :              diastate(4,dia))
        call pgwindow(diastate(5,dia),diastate(6,dia),diastate(7,dia),
     :              diastate(8,dia))
        old_dia = dia
        if_old = .true.
      end if
      end
