      subroutine set_routines(flag,deccntr,status)
*+
* Name:
*    SET_ROUTINES

* Invocation:
*    CALL SET_ROUTINES(FLAG,DECCNTR,STATUS)
*
* Description:
*   To set the routines to use for optimisation
* Purpose:
*   To set the routines to use for optimisation
* Arguments:
*     FLAG = INTEGER (Given)
*        0 - alter DECCNTR (current model only)
*                 1 - set common for models as selected
*   STATUS = INTEGER (Given)
*        Error status
* Author:
*  T.N.Wilkins, Cambridge, 14-Oct-1991
*      "            "       1-JUL-1992 Bug fix, key not set for .not.loop
*-
      implicit none
      integer flag,status
      include 'sae_par'
      include 'status_inc'
      integer NDICT, OPT_EXIT, NDICT2
      parameter (NDICT = 4, OPT_EXIT = NDICT, NDICT2 = 5)
      integer key,key2
      character*37 dict(NDICT)
      character*34 routines(NDICT2)
      integer dumi
      real dumr
      character dumc
      logical loop
      data dict/
     :     'SINGLES : Set routine for Singles',
     :     'DOUBLES : Set routine for doubles',
     :     'MULTIPLES : Set routine for multiples',
     :     'EXIT : Exit'/
      data routines/
     :     'N1 : Modified Newton (E04GBF)',
     :     'N2 : Modified Newton (E04KDF)',
     :     'L-M : Modified Levenberg-Marquardt',
     :     'S  : Simplex',
     :     'V  : Ve08'/

      loop = .true.
      key = 0
      do while(loop)
        loop = flag.eq.1
        if(loop) then
          call qmenu('Optimisation Menu',dict,NDICT,OPT_EXIT,dumr,dumc
     :         ,key,dumi,status)
        endif
        if((key.eq.OPT_EXIT).or.(status.ne.SAI__OK)) then
          loop = .false.
        else
          call qmenu('Fitting Routines',routines,NDICT2,0,dumr,dumc,
     :         key2,dumi,status)
          if(flag.eq.0) then
            deccntr(FIT_OPT) = key2
          else
            opt_routines(key) = key2
          endif
        endif
      enddo
      end

