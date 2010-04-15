      subroutine unmap_res(status)
*+
* Name:
*    UNMAP_RES

* Invocation:
*    CALL UNMAP_RES(STATUS)

* Purpose:
*   To unmap the .RES structure arrays.

* Description:
*   To unmap the .RES structure arrays.
*
* Arguments:
*    STATUS = INTEGER (Given and returned)
*        Error status
* Global variables:
*    ITERATION = INTEGER*2 (Given)
*        Value of iteration-written back to file (in arc_dims)
*    TOLERANCE(MAXTOL) = REAL ARRAY (Given)
*        Tolerances (in arc_dims)
*
* History:
*   Altered TNW 18/10/88 to use ACCRES
*   Minor changes TNW 27/1/89
*   Changes for new results structure, also write iteration, and
*   tolerances TNW 7/91
*   Remove local unused variables: ACD, 28/9/00
*-
      implicit none
      include 'arc_dims'
      integer status

      call accres(' ','data_array','um',0,0,' ',status)
      call accres(' ','more.twodspec.itmask','um',0,0,' ',status)
      call accres(' ','more.twodspec.control','um',0,0,' ',status)
      call accres(' ','more.rest_wave','um',0,0,' ',status)

*      call accres(' ','more.ids','um',0,0,' ',status)

* 16/6 changed work ' ' to 0

      call accres(' ','more.ids','wc',10*nyp,0,idstring(:10*nyp),status)


      call accres(' ','more.traml','um',0,0,' ',status)
      call accres(' ','more.tramr','um',0,0,' ',status)
      call accres(' ','more.twodspec.select','um',0,0,' ',status)


* Not mapped anymore
*      call accres(' ','more.params','um',0,0,' ',status)

*changed work ' ' to 0
      call accres(' ','more.params','wc',mxpars*10,0,parval(:mxpars*10),
     :            status)



* Write iteration and tolerances to file

      call accres(' ','more.twodspec.iteration','ws',1,iteration,' ',
     :            status)
      call accres(' ','more.twodspec.tols','WF',maxtol,tolerance,' '
     :            ,status)

      call accres(' ','more.twodspec.guess_tols','WF',NGTOL,gestol
     :     ,' ',status)
      end


