      subroutine get_median(resid,sort_bin,nsort,c1)
*+
* Name:
*    GET_MEDIAN

* Invocation:
*    CALL GET_MEDIAN(RESID,SORT_BIN,NSORT,C1)

* Purpose:
*  To get the median value of an array, except that it seems to
*  prefer to get the mode.

* Description:
*  To get the median value of an array, except that it seems to
*  prefer to get the mode.
*
* Arguments:
*    RESID(NSORT) = REAL ARRAY (Given)
*        Array in question
*    NSORT = INTEGER (Given)
*        Dimension of array
*    C1 = REAL (Returned)
*        Median or mode
*    SORT_BIN(NSORT*2) = REAL ARRAY (Workspace)
*        Elements 1-m SORT, Elements m+1-2m BIN
* History:
*  Tidied a bit TNW/MCR 7/11/88
*  SORT and BIN made into 1 array to simplify call, TNW 19/9/90 CAVAD
*  Minor changes, TNW 19,26/3/91
*  Made a subroutine since not used as function in TWODSPEC, TNW 5/6/91
*-
      implicit none
      integer nsort
      real mode,median,mean,c1
      real resid(nsort),sort_bin(nsort*2)
*
      real maxbin,minbin,rejup,rejlow,sigma2
      integer j,k,npixel,nout1,nout
*
*    Background subtraction using iterative mode
*    mean or median
*
* The array sort contains the data in ascending order
* the array "bin" is the working array for sigma clipping
* (updated at each iteration)
*
      call copr2r(nsort,resid,sort_bin)
*
*   Sort array into ascending order...
*
      call gen_sortf(sort_bin,nsort)

*  Make another copy of the data, and get the range

      call copr2r(nsort,sort_bin,sort_bin(nsort+1))

      minbin = sort_bin(1)
      maxbin = sort_bin(nsort)
*
*  npixel contains the number of points within 2 sigma
*  on current iteration.
*
*  nout1  contains number of points in on the previous iteration
*
*  nout   contains number rejected on last iteration.
*
      npixel = nsort
      nout1 = nsort

* set up now ready for iterative mode calculation
*
      do k=1,60
        call submode(sort_bin(nsort+1),npixel,mode,median,sigma2,mean)
        rejup=mode+sigma2
        rejlow=mode-sigma2

* REJECT ALL THOSE PIXELS GREATER THAN 2 SIGMA FROM MODE
* AND UPDATE THE MATRIX BIN FROM MATRIX SORT WITH ONLY
* those points which satify that condition.
* note. points that where excluded during the previous
* iterations may now be inside.

        npixel=0
        do j=1,nsort
          if((sort_bin(j).ge.rejlow).and.(sort_bin(j).le.rejup)) then
            npixel=npixel+1
            sort_bin(nsort+npixel)=sort_bin(j)
          end if
        end do

        nout = nout1-npixel
        if((nout.eq.0).or.(npixel.eq.0)) then

*    mode successful

          c1=mode
          if(mode.lt.minbin) then
            c1=median
            open(23,file='get_median.err',status='unknown')
            write(23,*)'Array put into routine=',resid
            close(23)
          else if(mode.gt.maxbin) then
            c1=median
          end if
          go to 1
        end if
        nout1=npixel
      end do

* we have exited from mode claculation without
* convergence as the number of pixels is still varying
* after 60 iterations.
* give up and use either mean or median to calculate the
* the background.

      c1=median
    1 continue
      end
