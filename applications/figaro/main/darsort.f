      subroutine darsort(chans,waves,nlid)
*+
* Name:
*    DARSORT

* Invocation:
*    CALL DARSORT(CHANS,WAVES,NLID)

* Purpose:
*     Sorts the identified line tables into ascending order of
*     channel number.
*
* Arguments:
*     CHANS(NLID) = DOUBLE PRECISION ARRAY (Given and returned)
*        The channel numbers  of the identified arc lines
*     WAVES(NLID) = DOUBLE PRECISION ARRAY (Given and returned)
*          The wavelengths of the  identified arc lines
*     NLID = INTEGER The number of identified lines
*
* Description:
*     Sorts the identified line tables into ascending order of
*     channel number.
* History:
*              KS / CIT 9th June 1983
*   Double precision version T.N.W. Manchester 10/2/87
*-
      implicit none
*
      integer nlid
      double precision chans(nlid),waves(nlid)
*
*     Local variables
*
      integer i,j
      double precision temp
*
*     Sort arrays on CHANS values
*
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
