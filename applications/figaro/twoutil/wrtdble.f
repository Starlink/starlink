      subroutine wrtdble(n,a,format,unit)
*+
* Name:
*    WRTDBLE

* Invocation:
*    CALL WRTDBLE(N,A,FORMAT,UNIT)
* Purpose:
*    To write a double precision array to a file.
* Description:
*   The element number (integer) is written before each element.
* Arguments:
*     N = INTEGER (Given)
*        Number of elements
*     A(N) = DOUBLE PRECISION ARRAY (Given)
*        Array
*     FORMAT = CHARACTER*(*) (Given)
*        Text
*     UNIT = INTEGER (Given)
*        Unit number of file
* History:
*  TNW 23/11/88
*-
      implicit none
      integer n,unit,i
      double precision a(n)
      character*(*) format
      write(unit,format) (i,a(i),i=1,n)
      end
