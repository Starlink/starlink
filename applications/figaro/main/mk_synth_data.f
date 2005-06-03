      subroutine mk_synth_data(left,right,status)
*+
* Name:
*    MK_SYNTH_DATA

* Invocation:
*    CALL MK_SYNTH_DATA(LEFT,RIGHT,STATUS)
*
* Description:
*    To create a synthetic spectrum for one emission line (possibly
*    to be changed in future releases!
*
* Purpose:
*    To create a synthetic spectrum for one emission line (possibly
*    to be changed in future releases!
*
* Arguments:
*      LEFT(NYP) = REAL ARRAY (Given)
*        Left trams
*      RIGHT(NYP) = REAL ARRAY (Given)
*        Right trams
*      STATUS = INTEGER (Given and returned)
*        Error status
*  Subroutines/functions referenced:
*
* Author:
*    T.N.Wilkins, Cambridge, 23-SEP-1991
* History:
*    Changed dsa_map 'w' to 'WRITE' AJH
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
      real left(nyp)
      real right(nyp)
      integer status

*

      logical par_qnum,qstat
      real value
      integer ndim,dims(3),m,rx2chn,start,outptr,slot,dyn_element,line
      integer outxptr,slotx,len1
      character*120 string
      include 'DYNAMIC_MEMORY'

* Which line shall we use?

      qstat = par_qnum('Line?',1.0,real(line_count),1.0,.true.,' ',
     :                  value)
      line = nint(value)

* Get name of output file, and create it

      call dsa_output('output','output',' ',0,0,status)

* Work out dimensions

      start = rx2chn(dynamic_mem(d_xptr),wavdim,left(line))
      m = rx2chn(dynamic_mem(d_xptr),wavdim,right(line)) - start + 1
      dims(1) = m
      dims(2) = spdim1
      dims(3) = spdim2
      if(spdim1.eq.1) then
        ndim = 1
      else if(spdim2.eq.1) then
        ndim = 2
      else
        ndim = 3
      end if

* Create data arrays, and map them (also wavelength axis)

      call dsa_simple_output('output','d','float',ndim,dims,status)

* Write label to output file

      len1 = 0
      call chr_putc('Synthetic data based on ',string,len1)
      call chr_appnd(datafile,string,len1)
      call chr_putc(', line ',string,len1)
      call chr_puti(line,string,len1)
      call dsa_set_object('output',string(:len1),status)

      call dsa_map_data('output','WRITE','float',outptr,slot,status)
      call dsa_map_axis_data('output',1,'WRITE','float',outxptr,slotx,
     :        status)
      if(status.ne.SAI__OK) return
      outptr = dyn_element(outptr)
      outxptr = dyn_element(outxptr)

* Zero output data array

      call zero_real(dynamic_mem(outptr),dims(1)*dims(2)*dims(3))

* Fill output file

      call fill_out(dynamic_mem(outptr),dims(1),line,
     :            %VAL( CNF_PVAL(staptr) ),%VAL( CNF_PVAL(d_rptr) ),
     :            %VAL( CNF_PVAL(d_vptr) ),dynamic_mem(outxptr),
     :            left(line),right(line))

* Unmap arrays and close file

      call dsa_unmap(slot,status)
      call dsa_unmap(slotx,status)
      call dsa_close_structure('output',status)
      end
