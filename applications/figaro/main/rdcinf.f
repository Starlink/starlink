      subroutine rdcinf(filnam,name,var,status)
*+
* Name:
*    RDCINF

* Invocation:
*    CALL RDCINF(FILNAM,NAME,VAR,STATUS)

* Purpose:
*   To read a character variable from the .FITS or .OBS structure.

* Description:
*   To read a character variable from the .FITS or .OBS structure.

* Arguments-
*  FILNAM = CHARACTER*(*) (Given)
*        Logical name for file (eg. 'image'), no trailing blanks.
*  NAME = CHARACTER*8 (Given)
*        Name of data
*  VAR = CHARACTER*(*) (Returned)
*        Variable
*  STATUS = INTEGER (Returned)
*        status

* History:
*   T.N.Wilkins Manchester 12/5/87
*   FILNAM argument introduced, TNW 7/7/88
*   Use of DSA TNW (IOA) 13/7/89
*   TNW: 8-FEB-1994, Take number of elements to read (dta_rdvarc) from
*        length of string
*   AJH: 4-Dec-1997 Set size of name to 8 chars.
*   AJH: 16-Mar-1998 Replace dta_rdvarc call with HDS version
*-
      implicit none
      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'DAT_ERR'
      include 'CMP_ERR'
      CHARACTER*(DAT__SZLOC) NLOC
      character*(*) var,filnam,string*80
      character*8 name
      integer status,len1
      logical exist,iffits
      integer strlen,size
      character*1 access

      status = SAI__OK

      call dsa_seek_fits(filnam,name,exist,access,size,strlen,status)

      iffits = exist.and.(status.eq.0).and.(access.eq.'C')

* Try to get from .FITS, if fail to do so, then try .OBS

      if(iffits) then
         call dsa_get_fits_c(filnam,name,1,var,string,status)
      end if
      if((.not.iffits).or.(status.ne.SAI__OK)) then
        status = SAI__OK
        len1 = 0
        call chr_appnd(filnam,string,len1)
        call chr_putc('.OBS',string,len1)
        call chr_appnd(name,string,len1)

*        call dta_rdvarc(string(:len1),len(var),var,status)

        call dta_loc(string(:len1),nloc,status)
        call dat_get0c(nloc,var,status)
        call dta_annul(nloc,status)

*        CALL CMP_GET0C(NLOC, string(:len1),
*     :           var, STATUS)


      end if
      end





