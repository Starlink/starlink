      subroutine look(results,iffibre,fitsta,resvar)
*+
* Name:
*    LOOK

* Invocation:
*    CALL LOOK(RESULTS,IFFIBRE,FITSTA,RESVAR)
*
* Purpose:
*   To look at value of the results block
*
* Description:
*   To look at value of the results block
*
* Arguments:
*   RESULTS(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given)
*        Results block
*   RESVAR(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given)
*        Results block variance
*   FITSTA(NCNTRL,NYP,NXP,SPDIM2) = INTEGER ARRAY (Given)
*        Fit status
*   IFFIBRE = LOGICAL (Given)
*        If called from FIBDISP
*  Global variables:
*   PARPTR, PAREND = CHARACTER*(*) (Given)
*        Parameter names start and end positions (include file arc_dims)
*
*  Subroutines/functions referenced:
*   PAR_QNUM    : Get number from user
*   PAR_WRUSER  : Write string to user
*
* Author:
*   TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92, then
*        Durham
*   ACD: A C Davenhall, Starlink, Edinburgh
*
* History:
*   TNW: 14/7/88 Original version
*   TNW: 28/11/89 Re-written to use parameter names array.
*   TNW: Adapted for use in LONGSLIT as well
*   TNW: Altered for new results structures, 1-9/7/91
*   TNW: Minor changes 11/2/94
*   ACD: Remove local unused variables 28/9/00
*-
      implicit none
      include 'PRM_PAR'
      integer status
      include 'arc_dims'
      integer ix,iy,line,k,len1,tmpst
C      integer tmpend
      integer start, stop
      real results(mxpars,nyp,nxp,spdim2),value
      real resvar(mxpars,nyp,nxp,spdim2)
      integer fitsta(ncntrl,nyp,nxp,spdim2),chr_len,len2,len3
      logical flag,loop,par_quest,iffibre,qstat,par_qnum,ifer
      include 'status_inc'
      character*79 chars,chars2

      loop = .true.
      ix = 1
      iy = 1
      line = 1
      do while(loop)
         if(iffibre) then
            qstat = par_qnum('Enter X position',1.0,real(spdim1),real(ix
     :           ),.true.,' ',value)
            ix = nint(value)
            qstat = par_qnum('Enter Y position',1.0,real(spdim2),
     :                       real(iy),.true.,' ',value)
            iy = nint(value)
         else
            qstat = par_qnum('Enter xsect number',1.0,real(nxp),
     :                       real(ix),.true.,' ',value)
            ix=nint(value)
         end if
         if(line_count.gt.1) then
            qstat = par_qnum('Enter line number',1.0,real(line_count),
     :           real(line),.true.,' ',value)
            line=nint(value)
         endif

* Loop over lines (at present this will always be 1)

* Output fit status

         call decode_status(ncntrl,fitsta(1,line,ix,iy),deccntr)

* Describe fit-more useful than putting out values of status array

         call describe_fit(deccntr,chars)
         call par_wruser(chars(:chr_len(chars)),status)
         len1 = 0
         ifer = .false.
         flag = .false.
         call chr_fill(' ',chars)
         call chr_fill(' ',chars2)
         len2 = 0
         call chr_putc('Errors:',chars2,len2)

*   Loop over parameters
* AJH Switch from parptr to parval


*         tmpst = parptr
         do k = 1, mxpars

	 start = ((k-1)*10)+1
	 stop = start + 9
            value = results(k,line,ix,iy)
            if(value.ne.VAL__BADR) then
*               tmpend = tmpst + 9
*               call chr_putc(dynamic_chars(tmpst:tmpend),chars,len1)

		call chr_putc(parval(start:stop),chars,len1)
               call chr_putc(' = ',chars,len1)
               len3 = len1
               call chr_putr(value,chars,len1)

*        Do we have a valid error?

               value = resvar(k,line,ix,iy)
               if(value.ne.VAL__BADR) then
                  value = sqrt(value)
                  len2 = len3
                  ifer = .true.
                  call chr_putr(value,chars2,len2)
               end if
               if(k.eq.mxpars) flag = .true.

*        Output data to terminal

               if(flag) then
                  call par_wruser(chars(:len1),status)
                  if(ifer) then
                     call par_wruser(chars2(:len2),status)
                     call chr_fill(' ',chars2)
                     len2 = 0
                     call chr_putc('Errors:',chars2,len2)
                     ifer = .false.
                  end if
                  len1 = 0
                  call chr_fill(' ',chars)
               else
                  len1 = 35
               end if
               flag = .not.flag
            end if
            tmpst = tmpst + 10
         end do
         loop = par_quest('Another position?',.true.)
      end do
      end
