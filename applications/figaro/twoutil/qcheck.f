      subroutine qcheck(title,dict,ndict,nums,cvals,lvals,key,status)
*+
* Name:
*    QCHECK

* Invocation:
*    CALL QCHECK(TITLE,DICT,NDICT,NUMS,CVALS,LVALS,KEY,STATUS)

* Purpose:
*   Allow user to edit values

* Description:
*   Wrap around of qmenu, to provide the facility for the user to modify
*   a number of values.

* Arguments:
*     TITLE = CHARACTER*(*) (Given)
*        Title for menu
*     DICT(NDICT) = CHARACTER*(*) ARRAY (Given)
*        dictionary, including information on arguments,
*                          description of meaning, etc.
*     NDICT = INTEGER (Given)
*        Number of entries in dictionary
*     NUMS(*) = REAL ARRAY (Given and returned)
*        Numeric arguments
*     CVALS(*) = CHARACTER*(*) ARRAY (Given and returned)
*        Character arguments
*     LVALS(*) = LOGICAL ARRAY (Given and returned)
*        Logical arguments
*     KEY = INTEGER (Returned)
*        Menu option selected
*     STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*    Format for input:
*      "F/T/Q/L OPTION : Explanation"
*                        etc.
*                OPTION  - Menu option, must be in upper case
*                f/F     - decimal arguments
*                t/T     - text arguments
*                l/L     - logical arguments
*                q/Q     - Quit this routine
*      Uppercase means required if all arguments to left given, lower
*      case means optional.

* Author:
*   T.N.Wilkins, Durham
* History:

*   TNW: 15-MAR-1994 Original version
*-
      implicit none
      include 'SAE_PAR'
      integer ndict,status,key
      character*(*) dict(ndict),cvals(*),title
      real nums(*)
      logical lvals(*)

* Workspace

      integer MAXDICT
      parameter (MAXDICT = 20)
      character*80 work(MAXDICT),tchar*1,chr_upper*1
      integer i,narg,types(MAXDICT),NUMBER,LOGICAL,STRING,QUIT
      parameter (NUMBER = 1, LOGICAL = 2, STRING = 3, QUIT = 4)
      integer nref,lref,sref,len1,ind,llen,maxind,maxlen,chr_len,j
     :     ,refs(MAXDICT)
      logical loop
      real value
      character*4 opts,cvalue*40
      data opts/'FLSQ'/

      if(ndict.gt.MAXDICT) then
         call par_wruser('Too many items for workspace',status)
         status = SAI__ERROR
      endif
      loop = status.eq.SAI__OK
      maxind = 0
      maxlen = 0

*  Work out where to place colons and equals sign, and get types of
*  items

      do i = 1, ndict
         ind = index(dict(i),':')
         llen = chr_len(dict(i)(ind:))
         tchar = chr_upper(dict(i)(1:1))
         types(i) = index(opts,tchar)
         if((types(i).eq.NUMBER).or.(types(i).eq.STRING)) ind = ind
     :        + 3
         maxind = max(maxind,ind)
         maxlen = max(maxlen,llen)
      enddo
      maxind = maxind - 2
      maxlen = maxind + maxlen + 2
      do while(loop)
         nref = 0
         lref = 0
         sref = 0
         do i = 1, ndict
            len1 = 0
            call chr_fill(' ',work(i))
            ind = index(dict(i),':')
            call chr_appnd(dict(i)(3:ind-1),work(i),len1)
            if((types(i).eq.NUMBER).or.(types(i).eq.STRING)) then
               call chr_putc(' %',work(i),len1)
               call chr_putc(dict(i)(1:1),work(i),len1)
            endif
            len1 = maxind
            call chr_appnd(dict(i)(ind:),work(i),len1)
            if(types(i).ne.QUIT) then
               do j = len1 + 2, maxlen
                  work(i)(j:j) = '.'
               enddo
               len1 = maxlen + 1
               call chr_putc('= ',work(i),len1)

* Format values into strings

               if(types(i).eq.NUMBER) then
                  nref = nref + 1
                  refs(i) = nref
                  call chr_putr(nums(nref),work(i),len1)
               else if(types(i).eq.STRING) then
                  sref = sref + 1
                  refs(i) = sref
                  call chr_putc(cvals(sref),work(i),len1)
               else if(types(i).eq.LOGICAL) then
                  lref = lref + 1
                  refs(i) = lref
                  call chr_putl(lvals(lref),work(i),len1)
               endif
            endif
         enddo

*  Prompt user

         call qmenu(title,work,ndict,0,value,cvalue,key,narg,
     :        status)
         if(status.ne.SAI__OK) then
            loop = .false.
         else if(types(key).eq.NUMBER) then
            nums(refs(key)) = value
         else if(types(key).eq.LOGICAL) then
            lvals(refs(key)) = .not.lvals(refs(key))
         else if(types(key).eq.STRING) then
            cvals(refs(key)) = cvalue
         else if(types(key).eq.QUIT) then
            loop = .false.
         endif
      enddo
      end
