      subroutine copr2r(nelm,in,out)
*+
* Name:
*    COPR2R

* Invocation:
*    CALL COPR2R(NELM,IN,OUT)

* Purpose:
*  To copy one real array to another

* Description:
*  To copy one real array to another, this is to be used to
*  call gen_move.
*
* Arguments:
*    NELM = INTEGER (Given)
*      Number of elements
*    IN(NELM) = REAL ARRAY (Given)
*      Source array
*    OUT(NELM) = REAL ARRAY (Returned)
*      Destination array
* Subroutines/functions called:
*    GEN_MOVE    : Fast copy of array
* History:
*     TNW 6/12/88
*-
      implicit none
      integer nelm
      real in(nelm),out(nelm)
      integer bytes
      include 'PRM_PAR'

      bytes = val__nbr * nelm
      call gen_move(bytes,in,out)
      end
