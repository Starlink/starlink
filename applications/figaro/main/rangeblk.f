      subroutine rangeblk(start2,end2,block2,nbls2,start3,end3,block3,
     :              nbls3,status)
*+
* Name:
*    RANGEBLK

* Invocation:
*    CALL RANGEBLK(START2,END2,BLOCK2,NBLS2,START3,END3,BLOCK3,
*                   NBLS3,STATUS)

* Purpose:
*  To return the ranges and blocking for fitting.

* Description:
*  To return the ranges and blocking for fitting.

* Arguments:
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*      START2 = INTEGER (Returned)
*        Start in 2nd dimension
*      END2 = INTEGER (Returned)
*        End in 2nd dimension
*      BLOCK2 = INTEGER (Returned)
*        Blocking in 2nd dimension
*      NBLS2 = INTEGER (Returned)
*        Number of such blocks
*      START3 = INTEGER (Returned)
*        Start in 3rd dimension
*      END3 = INTEGER (Returned)
*        End in 3rd dimension
*      BLOCK3 = INTEGER (Returned)
*        Blocking in 3rd dimension
*      NBLS3 = INTEGER (Returned)
*        Number of such blocks
* Global variables:
*      SPDIM1 = INTEGER (Given)
*        First spatial dimension of data
*      SPDIM2 = INTEGER (Given)
*        2nd spatial dimension of data
* Subroutines/functions referenced:
*      DSA_AXIS_RANGE   : Get limits in axis direction
*      PAR_RDVAL        : Get parameter from user
* History:
*   T.N.Wilkins, Cambridge, 23-JAN-1990
*        "           "      25-JUN-1991 Cancel blocking parameters
*-
      implicit none
      integer start2
      integer end2
      integer block2
      integer nbls2
      integer start3
      integer end3
      integer block3
      integer nbls3
      integer status
      include 'arc_dims'

*


* just used as workspace/for type conversion

      real value1,value2

      if(spdim1.eq.1) then
        start2 = 1
        end2 = 1
        block2 = 1
      else
        call dsa_axis_range('data',2,' ',.false.,value1,value2,start2,
     :           end2,status)
        call canaxlim(2)
        value2 = real(end2 - start2 + 1)
        call par_rdval('yblock',1.0,value2,1.0,'Array indices',value1)
        block2 = nint(value1)
        call par_cnpar('yblock')
      end if

      if(spdim2.eq.1) then
        start3 = 1
        end3 = 1
        block3 = 1
      else
        call dsa_axis_range('data',3,' ',.false.,value1,value2,start3,
     :           end3,status)
        call canaxlim(3)
        value2 = real(end3 - start3 + 1)
        call par_rdval('tblock',1.0,value2,1.0,'Array indices',value1)
        block3 = nint(value1)
        call par_cnpar('tblock')
      end if

      nbls2 = (end2 - start2)/block2 + 1
      nbls3 = (end3 - start3)/block3 + 1
      end
