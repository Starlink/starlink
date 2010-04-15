      subroutine chkenv(envir,struct,status)
*+
* Name:
*    CHKENV

* Invocation:
*    CALL CHKENV(ENVIR,STRUCT,STATUS)

* Purpose:
*   Check ready to create results structure

* Description:
*    To check that the environment exists for a structure which is to
*    be created. The method is to get the DTA name of the structure
*    to be created, parse it, and then to add any bits not found.
*
* Arguments:
*      ENVIR = CHARACTER*(*) (Given)
*        DTA name of file to add structure to (e.g. "data")
*      STRUCT = CHARACTER*(*) (Given)
*        DTA name of structure which is to be created (only the last bit,
*        e.g. "res").
*      STATUS = INTEGER (Given and returned)
*        Error status
* Subroutines/functions referenced:
*    TNW_DTAERR, DSA_ELEMENT_NAME, DTA_TYVAR, DTA_CRVAR
* Author:
*    T.N.Wilkins, Cambridge,  4-JUN-1991
*    A.J.Holloway, Manchester 12-MAY-1998
* History:
*    Remove use of dta_tyvar
*-
      implicit none
      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'DAT_ERR'
      include 'CMP_ERR'
      character*(*) envir,struct
      integer status
      character*(DAT__SZLOC) nloc
*
      integer ind(0:4),dta_status,count,len1
      character*20 stype,string*80

* Get full name of structure to be created

      call dsa_element_name(envir,struct,string,status)

      count = 0
      ind(0) = 1
      do while(ind(count).gt.0)
        count = count + 1
        ind(count) = index(string(ind(count-1):),'.')

*   If there is another dot then up to the dot is a structure which has
*   to exist.

        if(ind(count).gt.0) then
          ind(count) = ind(count-1) + ind(count)
          len1 = ind(count)-2

* Old dta library call
*          call dta_tyvar(string(:len1),stype,dta_status)
*          call cmp_type(nloc, string(:len1), stype, dta_status)

             call dta_loc(string(:len1),nloc,dta_status)
             call dat_type(nloc,stype,dta_status)
             call dta_annul(nloc,dta_status)

*      If it doesn't exist we will have to create it

          if(dta_status.ne.SAI__OK) then
            call dta_crvar(string(:len1),'Struct',status)
            if(status.ne.SAI__OK) then
              call tnw_dtaerr(status,'creating',string(:len1))
              return
            end if
          end if
        end if
      end do
      end

