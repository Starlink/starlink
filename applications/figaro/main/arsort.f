      subroutine arsort(chans,waves,nlid)
*+
* Name:
*    ARSORT

* Invocation:
*    CALL ARSORT(CHANS,WAVES,NLID)
C Purpose:
C     Sorts the identified line tables into ascending order of
C     channel number.
* Description:
*    A simple slow sort is used!
C
C     Parameters - (">" input, "!" modified)
C
C     (!) CHANS    (Real array CHANS(NLID)) The channel numbers
C                  of the identified arc lines
C     (!) WAVES    (Real array WAVES(NLID)) The wavelengths of the
C                  identified arc lines
C     (>) NLID     (Integer) The number of identified lines
C
C                                      KS / CIT 9th June 1983
C-
      implicit none
C
      integer nlid
      real chans(nlid),waves(nlid)
C
C     Local variables
C
      integer i,j
      real temp
C
C     Sort arrays on CHANS values
C
      if (nlid.gt.1) then
         do i=1,nlid-1
            do j=i+1,nlid
               if (chans(i).gt.chans(j)) then
                  temp=chans(j)
                  chans(j)=chans(i)
                  chans(i)=temp
                  temp=waves(j)
                  waves(j)=waves(i)
                  waves(i)=temp
               end if
            end do
         end do
      end if
      end
