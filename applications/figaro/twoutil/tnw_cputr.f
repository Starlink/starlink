      integer function tnw_cputr(fmt,rnum,string,len1)
*+
* Name:
*    TNW_CPUTR

* Invocation:
*   (INTEGER) = TNW_CPUTR(FMT,RNUM,STRING,LEN1)

* Purpose:
*   Write number to string using specified format

* Description:
*     To format a real number into a string, in such a way as to be
*   compatable with CHRLIB, but allowing the format to be chosen.
*   This is called, for example, as follows:-
*
*     status=tnw_cputr('f7.2',rnum,string,len1)
*
*   External reference - CHR_LEN, (CHR package).

* Authors:
*  TNW: T.N.Wilkins,Manchester, Durham

* History:
*  TNW: 11/12/86 Original version
*  TNW: 27/7/93 Use CHR_LEN rather than ICH_LEN
*
      implicit none

* Input

      character*(*) fmt
      real rnum

* Modify

      character*(*) string
      integer len1

* Output
*
*    tnw_cputr  -  fortran write status
*-

      integer chr_len
      integer status
      character*30 fmt1

* Prepare format

      write(fmt1,'(''('',a,'')'')')fmt(:chr_len(fmt))

* Write to string using FORTRAN write.

      write(string(len1+1:len(string)),fmt1,iostat=status) rnum

* Get length of string

      len1 = chr_len(string)
      tnw_cputr = status
      end
