      subroutine get_grey(devnam,status)
*+
* Name:
*    GET_GREY

* Invocation:
*    CALL GET_GREY(DEVNAM,STATUS)

* Description:
*  To select a grey-scale device. The user in prompted for this,
* Purpose:
*  To select a grey-scale device. The user in prompted for this,
* in contrast to other devices, since it may well require changing
* during one run of a program (e.g. get high/low levels in softcopy,
* and then change to hardcopy output). A check is made as to whether
* the device to be opened is the same as the current soft or hard
* device, in which case that is opened instead.
*
*  Arguments:
*    DEVNAM = CHARACTER*(*) (Given)
*        Name of paramter giving device name
*    STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*
*  Subroutines/functions referenced:
*   PAR_RDCHAR = INTEGER (Given and returned)
*        Get character parameter
*   PAR_WRUSER = INTEGER (Given and returned)
*        Write character string to user

* History:
*    T.N.Wilkins Manchester  15/6/88
*    Use of GNS_IDNG, T.N.Wilkins Cambridge 10/8/89
*    Altered to reflect change of names for hardcopy/softcopy devices
*    (we have to know if the greyscale device is the same as one of
*    these). T.N.Wilkins, Cambridge 2/2/90
*    PGPLOT version, TNW Cambridge 3/90
*    Made a subroutine,"    "      25/1/91
*-
      implicit none
      include 'SAE_PAR'
      character*(*) devnam
      character*40 device
      integer status,len1,pstat
      integer pgbegin,gincr
      character*60 prfile,chars*3
      include 'gr_inc'
      save gincr

      if(status.ne.SAI__OK) return


      if((.not.greyscale).or.(ngplot.ge.max_gplot)) then
        call par_rdchar(devnam,' ',device)
        call clgrap
        ngplot = 0
        status = pgbegin(0,device,1,1) - 1
        if(status.ne.SAI__OK) then
          call par_wruser('Error opening device',pstat)
        else
          greyscale = .true.

*   Check if workstation is hardcopy

          call pgqinf('hardcopy',chars,len1)

*   Get name of file created if hardcopy workstation

          if(chars.eq.'YES') then
            gincr = 1
            call pgqinf('file',prfile,len1)
            inquire(file=prfile,name=prname)

*     Select "roman" font and wide lines

            call pgscf(2)
            call pgslw(3)
          else
            gincr = 0
          end if
        end if
      else
        call pgpage
      end if
      ngplot = ngplot + gincr
      call pgask(.false.)
      end
