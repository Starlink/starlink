      real function bimodf_xmean(istart,iend,l,v,status)
*+
* Name:
*    BIMODF_XMEAN

* Invocation:
*   (REAL) = BIMODF_XMEAN(ISTART,IEND,L,V,STATUS)
* Purpose:
*    Evaluate mean and variance of data
*
* Description:
*    Evaluate mean and variance of data
*
*    For grouped data in array L
*    Returns STATUS as zero if ok, otherwise:
*                  -2 : Error calculating variance
*                       -too little data
*                  -3 : Error calculating variance
*                       -too low value data
*
* Arguments:
*    ISTART = INTEGER (Given)
*        Start of range to consider (array index)
*    IEND = INTEGER (Given)
*        End   "   "    "    "         "     "
*    L(*) = REAL ARRAY (Given)
*        Array to work on
*    V = REAL (Returned)
*        Variance
*    STATUS = INTEGER (Returned)
*        Status, 0=ok
*  Function value:
*    BIMODF_XMEAN = REAL (Returned)
*        Mean of data
*
*  IMIN added to and NN removed from argument list-IMIN replaced OK
*                      TNW/CAVAD 20-21/3/91
*  IMIN renamed STATUS, TNW/CAVAD 17-JUN-91
*-
      implicit none
      include 'sae_par'
      integer istart
      integer iend
      real l(*)
      real v
      integer status

      real nn
      real sum
      integer j
      real temp
      real sub

      sum=0.0
      nn=0.0
      status = SAI__OK
      if(istart.ge.iend) then
        status = -3
        go to 90
      end if

*  Compute NN and SIGMA

      do j=istart,iend
        nn=nn+l(j)
        sum=sum+l(j)*real(j)
      end do

*  Changed to allow for Flux-callibrated data with small values for
*  data. TNW 29/6/89

      if(nn.le.1.0e-30) then
        status = -4
        go to 90
      else if(nn.le.5.0) then
        sub = 0.0
      else
        sub = 1.0
      end if

*  Compute mean

      bimodf_xmean=sum/nn

*  Compute variance

      sum=0.0
      do j=istart,iend
        temp = real(j)-bimodf_xmean
        sum = sum+temp*temp*l(j)
      end do
      v=sum/(nn-sub)
      status = SAI__OK
      return

*  Error or insufficient data

 90   continue
      bimodf_xmean=0.0
      v=0.0
      end
