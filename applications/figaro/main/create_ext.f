      subroutine create_ext(dims,lincnt,status)
*+
* Name:
*    CREATE_EXT

* Invocation:
*    CALL CREATE_EXT(DIMS,LINCNT,STATUS)

* Purpose:
*   To create the extatic file
* Description:
*   To create the extatic file
* Arguments:-
*  LINCNT = INTEGER (Given)
*        Number of lines identified
*  DIMS(2) = INTEGER ARRAY (Given)
*        Dimensions of output extatic file
*  STATUS = INTEGER (Given and returned)
*        Error status, 0=ok

* History:
* T.N.Wilkins Manchester
*  Altered to make units a 1-d array TNW 4/7/88
*  SAMNAM array made smaller, TNW 6/7/88
*  Version using dsa routines TNW 24/8/88
*  STATUS made argument, TNW 27/1/89
*  Setting of dims moved from create_ext to count TNW/Cambridge
*-
      implicit none
      integer lincnt
      integer status
      integer dims(2),dims1
      character*10 ich_ci

* ORDER OF ELEMENTS IN FILE
* =========================
* Central x-sect,base,c1,w1,h1,...,mask

* Get name of output file and open it

      call dsa_read_struct_def('ext',status)
      call dsa_set_struct_var('nvars',ich_ci(dims(1)),status)
      call dsa_set_struct_var('ncases',ich_ci(dims(2)),status)
      dims1 = lincnt*32
      call dsa_set_struct_var('nsamnam',ich_ci(dims1),status)
      dims1 = dims(2)*32
      call dsa_set_struct_var('nids',ich_ci(dims1),status)
      dims1 = dims(1)*32
      call dsa_set_struct_var('dimvrnm',ich_ci(dims1),status)
      call dsa_create_structure('exstatic','outable','extatic',status)
      end
