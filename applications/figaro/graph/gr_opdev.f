      subroutine gr_opdev(batch,ifsoft,prname,status)
*+
* Name:
*    GR_OPDEV

* Invocation:
*    CALL GR_OPDEV(BATCH,IFSOFT,PRNAME,STATUS)

* Purpose:
*   Open a graphics device

* Description:
*   Open a graphics device

* Arguments:
*     BATCH = LOGICAL (Given)
*        If running in batch
*     IFSOFT = LOGICAL (Given)
*        If to open softcopy device
*     STATUS = INTEGER (Returned)
*        Status
*     PRNAME = CHARACTER*(*) (Returned)
*        Name of plot file (if hardcopy)
*
* History:
*   Largely re-written T.N.Wilkins Manchester.
*   TNW 10/11/88, altered to use PAR_RDARY to prompt for size
*   TNW 9/12/88, Changed to reference user variable "HARD"
*   TNW, IOA 12/7/89 Changed to use GNS_IDNG
*   TNW, IOA 3/90 PGPLOT version
*   TNW, Renamed 22/6/90
*   TNW, GR_OPSFT and GR_OPHRD combined into GR_OPDEV, 11/9/90
*   TNW, Error message if VAR_SETCHR gives non-zero status, status set to
*      0, 8/3/91
*   TNW, 25-SEP-1991 Listing of available plot devices moved to GR_LIST
*   AJH, Nov 1997 Removed lib$ from lib$
*- --------------------------------------------------------------------
      implicit none
      include 'SAE_PAR'
      integer status
      logical batch
      logical ifsoft
      integer pstat
      logical get_symbol
      integer pgbegin
      character*64 chars
      character*(*) prname
      integer len1
      character*4 dev

      if(ifsoft) then
        dev = 'SOFT'
      else
        dev = 'HARD'
      end if
*
* Read graphics device name
*
      call var_getchr(dev,0,0,chars,status)
*
*    Open workstation
*
      call err_mark
      status = pgbegin(0,chars,1,1) - 1
      if(status.ne.0) then
         call err_flush(status)
         call err_annul(status)

*    We need status to be kept bad for the moment!

         status = SAI__ERROR
      endif
      call err_rlse

      if((status.ne.SAI__OK).and.batch.and.(.not.ifsoft)) then
        status = SAI__OK
        if(.not.get_symbol('def_hardcopy',chars)) then
          call par_wruser('Could not get default hardcopy device',pstat)
        else
          status = pgbegin(0,chars,1,1) - 1
        end if
      else

*   Get valid name

        do while(status.ne.SAI__OK)
          status = SAI__OK
          call gr_list
          call par_qstr('Re-enter '//dev//' device',' ',.true.,.false.
     :        ,chars)
*
*    Open workstation
*
          call err_mark
          status = pgbegin(0,chars,1,1) - 1
          if(status.ne.0) then
             call err_flush(status)
             call err_annul(status)

*    We need status to be kept bad for the moment!

             status = SAI__ERROR
          endif
          call err_rlse
        end do
      end if
      if (status.ne.0) then
        call par_wruser('Error opening graphics device',pstat)
      else

*   Set device variable to current device name

        call var_setchr(dev,0,0,chars,status)
        if(status.ne.SAI__OK) then
          call par_wruser('Error storing user variable',pstat)
          status = SAI__OK
        end if

*   Is this a hardcopy device (this will catch the case of using a
*   hardcopy device for "SOFT")?

        call pgqinf('hardcopy',chars,len1)
        if(chars(:3).eq.'YES') then

*   Get output file name

          call pgqinf('file',chars,len1)

*   Get name of file created.

          inquire(file=chars,name=prname)

*   For hardcopy output use "roman" font, otherwise "normal". Also
*   use wider lines for hardcopy

          call pgscf(2)
          call pgslw(3)
        else
          call pgscf(1)
          call pgslw(1)
        end if
      end if

*   Don't prompt to advance to new page

      call pgask(.false.)

      end
