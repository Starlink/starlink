      subroutine crres(ftype,status)
*+
* Name:
*    CRRES

* Invocation:
*    CALL CRRES(FTYPE,STATUS)

* Purpose:
*  To create the .RES array.

* Description:
*  To create the .RES array.

* Arguments:
*    FTYPE    (c* = INTEGER (Given)
*        Type of array
*    STATUS = INTEGER (Given and returned)
*        0=ok
* Global variables:
*    MXPARS = INTEGER (Given)
*        Parameter direction dimension of results block
*    NYP = INTEGER (Given)
*        Number of lines to allow room for
*    NXP = INTEGER (Given)
*        First spatial dimension of file
*    SPDIM2 = INTEGER (Given)
*        second spatial dimension of file
*    WAVDIM = INTEGER (Given)
*        1st dimension of data array
*    SPDIM1 = INTEGER (Given)
*        2nd dimension of data array
*
* Subroutines called:
*    DSA_ADD_STRUCTURE   : Add structure to file
*    DSA_READ_STRUCT_DEF : Read structure definition from file
*    DSA_SET_STRUCT_VAR  : Set value of structure variable

* History:
*     T.N.Wilkins Cambridge 5/1/90, based on CRFIB
*           "        "      4/6/91, Call to CHKENV added
*-
      implicit none
      character*(*) ftype,ich_ci*10
      integer status
      include 'arc_dims'
      logical ifcomb
      integer ntols

      ifcomb = ftype(1:4).eq.'COMB'
      ntols=maxtol

* Read structure definition file

      call dsa_read_struct_def('res',status)

* Set structure variables

      call dsa_set_struct_var('nxx',ich_ci(spdim2),status)
      call dsa_set_struct_var('nxp',ich_ci(nxp),status)
      call dsa_set_struct_var('nyp',ich_ci(nyp),status)
      call dsa_set_struct_var('mxpars',ich_ci(mxpars),status)

      call dsa_set_struct_var('dimids',ich_ci(10*nyp),status)
      call dsa_set_struct_var('ndec',ich_ci(10*mxpars),status)

      call dsa_set_struct_var('ntols',ich_ci(ntols),status)

      if(ifcomb) then
        call dsa_set_struct_var('nchans',ich_ci(spdim1),status)
        call dsa_set_struct_var('comb','True',status)
      else
        call dsa_set_struct_var('nchans',ich_ci(wavdim),status)
      end if

      if(spdim2.gt.1) then
        call dsa_set_struct_var('data3d','True',status)
        if(ftype(1:3).eq.'HEX') then
          call dsa_set_struct_var('hex','True',status)
        end if
      end if

* Check structure exists for this to be created in (if NDF format files
* being used this means .MORE.FIGARO).

      call chkenv('data','results',status)

* Add structure

      call dsa_add_structure('data','results','results',status)

* Put values into type

      call accres('data','results','fi',0,0,' ',status)

      if(ifcomb) then
        call accres(' ','more.twodspec.variant','wc',4,0,ftype,status)
      end if
      if(spdim2.gt.1) then
        call accres(' ','more.twodspec.variant','wc',10,0,ftype,status)
      end if

* Write tolerances into file

      call accres(' ','more.twodspec.tols','wf',maxtol,tolerance,' '
     :          ,status)

      end
