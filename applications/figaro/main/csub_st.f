      subroutine csub_st(coen,ni,nl,newstruct,status)
*+
* Name:
*    CSUB_ST

* Invocation:
*    CALL CSUB_ST(COEN,NI,NL,NEWSTRUCT,STATUS)

* Purpose:
*     To set the coefficient structure for CSUB.
*
* Description:
*     To set the coefficient structure for CSUB.
*
* Arguments:
*     COEN = CHARACTER*(*) (Given)
*        Name of data file
*     NL,NI = INTEGER (Given)
*        1st and 2nd dimensions of file
*     STATUS = INTEGER (Given and returned)
*        Error status, 0=ok, if not 0 on entry then nothing gets done!
*     NEWSTRUCT = LOGICAL (Returned)
*        If a new structure was created
* Subroutines/functions called:
*     ACCRES        : Access an application-specific structure
*
*     DSA_ADD_STRUCTURE   : Add a structure to a file
*     DSA_READ_STRUCT_DEF : Read a structure definition file
*     DSA_SET_STRUCT_VAR  : Set a dimension of structure to create
*     ICH_CI              : format integer into character string

* History:
*  Re-written T.N.Wilkins Manchester 25/10/88 to use DSA routines.
*  Made to check for existance of structure, TNW/CAVAD 17/8/90
*  TNW/CAVAD 4/6/91, Call to CHKENV added
*
*- ---------------------------------------------------------------------
      implicit none
      include 'SAE_PAR'
      character*(*) coen
      integer ni,nl
      logical newstruct
      integer status
      character*10 ich_ci

      if(status.ne.SAI__OK) return
*
*  Set up coefficient structure, first testing for existance
*
      call accres(coen,'continuum','fi',2,0,' ',status)
      newstruct = status.ne.SAI__OK
      if(newstruct) then
        status = SAI__OK
        call dsa_read_struct_def('continuum',status)
        call dsa_set_struct_var('ni',ich_ci(ni),status)
        call dsa_set_struct_var('nl',ich_ci(nl),status)

*   Add structure, first checking that the environment exists for it

        call chkenv(coen,'continuum',status)
        call dsa_add_structure(coen,'continuum','continuum',status)
      end if
      call accres(coen,'continuum','fi',0,0,' ',status)

* Write values of NL and NI into structure

      if(newstruct) then
        call accres(' ','nl','wi',1,nl,' ',status)
        call accres(' ','ni','wi',1,ni,' ',status)
      end if
      end
