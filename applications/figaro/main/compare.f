      subroutine compare(data1,data2,flag,left,right,x)
*+
* Name:
*    COMPARE

* Invocation:
*    CALL COMPARE(DATA1,DATA2,FLAG,LEFT,RIGHT,X)

* Purpose:
*  To compare the 2 data files.
*
* Description:
*  To compare the 2 data files.
*
* Arguments:
*      DATA1(WAVDIM,SPDIM1) = REAL ARRAY (Given)
*        Data 1
*      DATA2(WAVDIM,SPDIM1) = REAL ARRAY (Given)
*        Data 2
*      LEFT(LINE_COUNT) = REAL ARRAY (Given)
*        Left tram
*      RIGHT(LINE_COUNT) = REAL ARRAY (Given)
*        Right tram
*      X(WAVDIM) = REAL ARRAY (Given)
*        X axis array
*      FLAG(SPDIM1,LINE_COUNT) = LOGICAL ARRAY (Workspace)
*    Subroutines/functions referenced:

* History:
*   T.N.Wilkins, Cambridge, 18-MAY-1990
*-
      implicit none
      include 'SAE_PAR'
      integer status
      include 'arc_dims'
      real data1(wavdim,spdim1)
      real data2(wavdim,spdim1)
      logical flag(spdim1,line_count)
      character*80 chars
      real left(line_count),right(line_count),x(wavdim)
      integer i,j,k,lu
      real diff,test,mean

* Set flag array to false

      do j = 1, line_count
        do i = 1, spdim1
          flag(i,j) = .false.
        end do
      end do

      do j = 1, spdim1
        do i = 1, wavdim
          diff = data1(i,j) - data2(i,j)
          diff = abs(diff)
          if(diff.gt.1.0) then
            mean = (data1(i,j) + data2(i,j))*0.5
            test = abs(mean*0.01)
            if(diff.gt.test) then

*       Difference is significant, so is it within a line boundary?

              test = x(i)
              do k = 1, line_count
                if((test.gt.left(k)).and.(test.lt.right(k))) then
                  flag(j,k) = .true.
                end if
              end do
            end if
          end if
        end do
      end do

      status = SAI__OK
      call dsa_open_text_file(datafile,'.diff','new',.true.,lu,chars,
     :      status)
      write(lu,'('' Line   Xsect'')')
      do i = 1, spdim1
        do j = 1, line_count
          if(flag(i,j)) then
            write(lu,'(2i6)')j,i
          end if
        end do
      end do
      end
