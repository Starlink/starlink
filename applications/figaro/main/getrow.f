      subroutine getrow(array2d,dim1,dim2,array1d,row)
*+
* Name:
*    GETROW

* Invocation:
*    CALL GETROW(ARRAY2D,DIM1,DIM2,ARRAY1D,ROW)

* Purpose:
*    To extract a row from an real array into a double precision array.

* Description:
*    To extract a row from an real array into a double precision array.

* Arguments:
*    ARRAY2D(DIM1,DIM2) = REAL ARRAY (Given)
*        2-d array to copy from
*    DIM1 = INTEGER (Given)
*        1st dimension of ARRAY2D
*    DIM2 = INTEGER (Given)
*        2nd dimension of ARRAY2D
*    ROW                (i) Row to copy
*    ARRAY1D = DOUBLE PRECISION (Returned)
*        Returned array.

* History:
*  TNW 4/11/88, Removal of setting LPOS array.
*  TNW/CAVAD 26/4/91 Renamed from find_vig2 to getrow.
*-
      implicit none
      integer dim1,dim2
      real array2d(dim1,dim2)
      integer row,nbad,ignore,cnv_fmtcnv
      double precision array1d(dim1)
*
*  Find vig in chan direction
*
      ignore = cnv_fmtcnv('float','double',array2d(1,row),array1d,dim1,
     :            nbad)
      end
