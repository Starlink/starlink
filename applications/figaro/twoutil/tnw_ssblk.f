      subroutine tnw_ssblk(chars)
*+
* Name:
*    TNW_SSBLK

* Invocation:
*    CALL TNW_SSBLK(CHARS)
*
* Purpose:
*    To change any multiple blanks in the string CHARS to single blanks.
*    Any leading blanks are also removed.
*
* Description:
*    To change any multiple blanks in the string CHARS to single blanks.
*    Any leading blanks are also removed.
*
* Arguments:
*    Given:
*    Altered:
*      CHARS     (c*(*)): Character string to process
*    Returned:
*    Workspace:
*    Subroutines/functions referenced:

* Author:
*   TNW: T.N.Wilkins, Cambridge

* History:
*   TNW: 15-JUN-1990 Original version
*-
      implicit none
      character*(*) chars
      integer i,j,ilen
      logical lstblk

* Must not start with blank

      lstblk = .true.
      i = 1
      ilen = len(chars)

* Continue until we reach the end of the string (excluding any trailing
* blanks we have added in the process).

      do while(i.lt.ilen)
        if(chars(i:i).eq.' ') then

*     If the last character was blank, then this shouldn't be. If it is
*     shift characters from right, and make sure we check from the same
*     character next time (since the new character could be blankas well).

          if(lstblk) then
            do j = i, ilen - 1
              chars(j:j) = chars(j+1:j+1)
            end do
            chars(ilen:ilen) = ' '
            ilen = ilen - 1
            i = i - 1
          end if
          lstblk = .true.
        else
          lstblk = .false.
        end if
        i = i + 1
      end do
      end
