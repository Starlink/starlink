      integer function rdmenu(dictionary,entries,menlis,default,name,
     :          status)
*+
* Name:
*    RDMENU

* Invocation:
*   (INTEGER) = RDMENU(DICTIONARY,ENTRIES,MENLIS,DEFAULT,NAME,
*               STATUS)

* Purpose:
*   Search a dictionary of commands and provide a menu
*
* Description:
*   Search through a DICTIONARY for the current command entered from
*   the keyboard. A default is also allowed for if there is no matching
*   command to the string in batch or a null string in entered
*   interactively.
*
* Arguments:
*   ENTRIES = INTEGER (Given)
*        The number of commands in the dictionary
*   DEFAULT = INTEGER (Given)
*        0   No default - the routine wiil not exit until
*                     an acceptable command has been entered (except in
*                     batch)
*                    i  The default is the Ith entry in the dictionary
*   DICTIONARY(ENTRIES) = CHARACTER*(*) ARRAY (Given)
*        The dictionary of commands to be searched
*   NAME      (c* = INTEGER (Given)
*        The name of the variable
*   MENLIS = CHARACTER*(*) (Given)
*        Explanation of menu
*   RDMENU = INTEGER (Returned)
*        The number of the matching entry in the dictionary
*   STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
* History:
*  Call to set default prompt at end removed, T.N.Wilkins 1/7/88
*  Altered to use PRTOUT                         "        9/11/89
* ----------------------------------------------------------------------
*-
      implicit none

* Import

      integer entries,default
      character*(*) dictionary(entries)
      character*(*) name
      character*(*) menlis
      include 'SAE_PAR'

* Local

      character*79 string
      character*33 prompt
      integer menu
      integer status,key
      logical loop
      logical given,par_batch,par_given,par_abort
      character*2 bss,bsn
      data bss/'\\'/
      bsn = bss(1:1)//'n'
*
* initialize the loop
*
      loop = status.eq.SAI__OK
*
* read the command
*
      given=.true.
      if(loop.and.(.not.par_given(name))) then
        call prtout(menlis,entries,dictionary,2,name)
      end if
      do while(loop)
        if(given) then
          given=.false.
        else
          call dsa_wruser(prompt)
          call dsa_wruser(' (')
          call dsa_wruser(name)
          call dsa_wruser(')')
          call dsa_wruser(bsn)
          call par_cnpar(name)
        end if
        if(default.eq.0) then
          call par_rdchar(name,' ',string)
        else
          call par_rdchar(name,dictionary(default),string)
        end if
        key=0
*
* look for string in dictionary
*
        key=menu(dictionary,entries,string,default)
        if(key.gt.0) then
          loop=.false.
        else if(par_batch()) then
          key=default
          loop=.false.
        end if
        prompt='Word not in dictionary -try again'
        if (key.eq.-1) prompt='Ambiguous answer -try again'

* Has the user aborted the program

        if(par_abort()) then
          status = SAI__ERROR
          loop = .false.
        end if
      end do ! loop
      rdmenu=key
      end
