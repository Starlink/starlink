      subroutine robust_smooth(y, n, smooth, rough, versn, err)
*+
* Name:
*    ROBUST_SMOOTH

* Invocation:
*    CALL ROBUST_SMOOTH(Y, N, SMOOTH, ROUGH, VERSN, ERR)
*
* Description:
*   Main routine for Nonlinear Smoothers.
* Purpose:
*   Main routine for Nonlinear Smoothers.
* Arguments:
*     Y(N) = REAL ARRAY (Given)
*        Data to be smoothed
*     N = INTEGER (Given)
*        Number of elements in data
*     VERSN = INTEGER (Given)
*        Specifies the smoother to be used:
*                                1 sepecifies 3RSSH, twice
*                                2 sepecifies 4253H, twice
*     SMOOTH(N) = REAL ARRAY (Returned)
*        Smoothed data
*     ROUGH(N) = REAL ARRAY (Returned)
*        Roughness of data
*  Note tha Y(i) = Smooth(i) + Rough (i) for I = 1,N
*-
      implicit none
      integer n, versn, err,status
      real y(n), smooth(n), rough(n)
* local
      integer i, OK
      Parameter (OK = 0)

      if( n .le. 6) then
        err= 61
        call opt_wruser('Error on RSM',status)
        go to 500
      end if

      do i = 1,n
        smooth(i) = y(i)
      end do

      if (versn .eq. 1) then
        call s3rssh(smooth,n,err)
      else if (versn .eq. 2) then
        call s4253h(smooth,n,err)
      end if

* compute rough form first smoothing

      if (err .eq. OK ) then

        do i = 1, n
          rough(i) = y(i) - smooth(i)
        end do

* rerough smoothers ("twicing")

        if (versn .eq. 1) then

          call s3rssh(rough, n, err)

        else if (versn .eq. 2) then

          call s4253h(rough, n, err)

        end if

        if (err .eq. OK ) then
          do i = 1, n
            smooth(i) = smooth(i) + rough(i)
            rough(i) = y(i) - smooth(i)
          end do
        end if
      end if
  500 continue
      end
