      integer function menu(dict,ndict,string,default)
*+
* Name:
*    MENU

* Invocation:
*   (INTEGER) = MENU(DICT,NDICT,STRING,DEFAULT)

* Purpose:
*    To search a dictionary and to locate a match.

* Description:
*    To search a dictionary and to locate a match. This allows any
*    abreviation provided that a unique match is made. If no match is
*    made menu returns as 0. If more than one match is made the value -1
*    is returned. If the string is blank the default value is returned.
* Arguments:-
*    DICT(NDICT) = CHARACTER*(*) ARRAY (Given)
*        Dictionary to search for match (in upper case)
*    NDICT = INTEGER (Given)
*        Number of entries in dictionary
*    STRING = CHARACTER*(*) (Given)
*        String to try to match with dictionary (case
*                         unimportant)
*    DEFAULT = INTEGER (Given)
*        Default value (if string is blank)
*    MENU = INTEGER (Returned)
*        The match (element of array DICT in which match
*                         was found (or DEFAULT if string blank)
* Authors:
*   T.N. Wilkins. Manchester
* History:
*   T.N. Wilkins. Cambridge 9/2/90 Changed to use ICH_CFOLD
*                 rather  than ICH_FOLD.
*     "    "      Cambridge, 18/10/90 Altered so will take
*                 dictionary entry which matches most of string,
*                 if more than 1 matches. This allow 1 entry to
*                 be a valid abbreviation for another.
*-
      implicit none
      integer ndict,ilen,llen,i,key,nfound,default,ind
      integer ich_cfold,chr_len,lenfound,lendict
      character*(*) dict(ndict)
      character*(*) string

* Get length and convert to upper case. If any blanks earlier in
* line take the length before that as the length instead.

      lenfound = 0
      ilen=ich_cfold(string,'""')
      ind=index(string,' ')
      if(ind.gt.1) ilen=min((ind-1),ilen)
      if(ilen.gt.0) then
        nfound=0
        key=0
        do i=1,ndict
          lendict = chr_len(dict(i))
          llen = max(1,min(ilen,lendict))
          if (dict(i)(:llen).eq.string(:llen)) then

*       There are 3 possibilities for more than 1 match:
*         LENFOUND = LLEN
*          Either error in dictionary, or 1st answer was same as entry
*          in dictionary. In any case we'll use 1st answer.
*         LENFOUND > LLEN
*          Error on user's part, abbreviation not unique
*         LENFOUND < LLEN
*          First supposed match was not intended match-a parameter had
*          been assumed.

            if((nfound.eq.0).or.(lenfound.lt.llen)) then
              key=i
              nfound=1
              lenfound = lendict
            else if(lenfound.gt.llen) then
              key=i
              nfound=nfound+1
              lenfound = lendict
            end if
          end if
        end do
        if (nfound.gt.1) key=-1
      else
        key=default
      end if
      menu=key
      end
