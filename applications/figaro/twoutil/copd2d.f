      subroutine copd2d(nelm,in,out)
*+
* Name:
*    COPD2D

* Invocation:
*    CALL COPD2D(NELM,IN,OUT)

* Description:
*  To copy one double precision array to another, this is to be used to
*  call gen_move.
*
* Purpose:
*  To copy one double precision array to another, this is to be used to
*  call gen_move.
*
* Arguments:
*   NELM = INTEGER (Given)
*     Number of elements
*   IN(NELM) = DOUBLE PRECISION ARRAY (Given)
*     Source array
*   OUT(NELM) = DOUBLE PRECISION ARRAY (Returned)
*     Destination array
* Subroutines/functions called:
*      GEN_MOVE    : Fast copy of array
*
* History:
*     TNW 6/12/88
*-
      implicit none
      integer nelm
      double precision in(nelm),out(nelm)
      integer bytes
      include 'PRM_PAR'

      bytes = val__nbd * nelm
      call gen_move(bytes,in,out)
      end
