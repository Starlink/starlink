      subroutine rdids(lu,wavelength,name,filend,nwork)
*+
* Name:
*    RDIDS

* Invocation:
*    CALL RDIDS(LU,WAVELENGTH,NAME,FILEND,NWORK)

* Purpose:
*    Read wavelength and line name from file

* Description:
*    To read a record from a line list file. This returns the wavelength
*    and name of the line. If the first character is a "*" then the line
*    is output to the terminal, and the next record read. Blank lines
*    are ignored-again this skips to the next record. If an error is
*    encountered, this is taken as the end of the file.
*
* Arguments:
*      LU = INTEGER (Given)
*        Logical unit of IDS file
*      NWORK = INTEGER (Given and returned)
*        Current index in tables
*      WAVELENGTH(*) = REAL ARRAY (Returned)
*        Wavelength of line in list
*      NAME(*) = CHARACTER*10 ARRAY (Returned)
*        Name of line
*      FILEND = LOGICAL (Returned)
*        If read failed due to end of file

* Authors:
*    TNW: T.N.Wilkins, Cambridge until 9/92, then Durham

* History:
*    TNW: 7-FEB-1991 Original version
*    TNW: 28/7/93 Change so passed arrays for wavelength and name.
*    TNW: 29/7/93 Use CHR routines, not ICH.
*-
      implicit none
      include 'SAE_PAR'
      integer lu,nwork
      real wavelength(*)
      character*10 name(*)
      logical filend
      integer chr_len,ilen,pstat,status,next
      character*80 string
      logical go

*

      nwork = nwork + 1
      go =.true.
      filend = .false.
      do while(go)

*  Read next record

        read(lu,'(a)',iostat=status)string
        if(status.ne.SAI__OK) then
          filend = .true.
          go = .false.
        else

*    Record successfully read

          ilen = chr_len(string)
          if(string(1:1).eq.'*') then

*     Comment-output to terminal

            call par_wruser(string(2:ilen),pstat)
          else if(ilen.gt.1) then

*     Remove multiple and leading blanks

             call tnw_ssblk(string)

*       Data record, attempt to decode

             next = index(string,' ')
             if(next.eq.0) next = ilen
             call chr_ctor(string(:next),wavelength(nwork),status)

*       Status call ne -1, 0 or 1, but -1 indicates a null string
*       which we've already trapped.

            if(status.eq.0) then
              go = .false.
              if(next.ge.70) then
                name(nwork) =  ' '
              else
                name(nwork) = string(next+1:next+10)
              end if
            else if(status.eq.1) then
              filend = .true.
              go = .false.
            end if
          end if
        end if
      end do
      end
