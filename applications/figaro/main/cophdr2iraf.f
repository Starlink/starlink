      subroutine cophdr2iraf(figfile,im)
*+
* Name:
*    COPHDR2IRAF

* Invocation:
*    CALL COPHDR2IRAF(FIGFILE,IM)
* Purpose:
*    Copy the elements of a .FITS structure into an IRAF header
*
* Arguments:
*      FIGFILE   (c* = INTEGER (Given)
*        Figaro file
*      IM = INTEGER (Given)
*        Pointer to IRAF file
* Description:
*    Copy the elements of a .FITS structure into an IRAF header
*-
*
      implicit none
*
*     Functions
*
      character*(*) figfile
      integer   chr_len
*
*     Local variables
*

* Routine to use to access item

      character access*1

* Comment associated with item

      character comment*80

* Used to hold double prec values

      double precision dvalue

* True if Nth item exists

      logical   exist

* Loop index through elements

      integer   i

* Dummy status argument

      integer   ignore

* Used to hold integer values

      integer   ivalue

* Used to hold logical values

      logical   lvalue

* Counter through items

      integer   n

* Name of item

      character name*16

* Number of elements in item

      integer   nelm

* Running status for DSA routines

      integer   status

* String to hold values of items

      character string*80

* Length of character items

      integer   strlen

* Used to hold real values

      real      value

* IRAF image identifier

      integer   im

* IRAF error code

      integer   ier
*
*     Initialise DSA and open input structure
*
      status=0
*
*     Set up loop through all items in FITS substructure.  We loop
*     so long as the last item existed.  Once we reach an item that
*     doesn't exist, we can stop.
*
      n = 1
      exist = .true.
      do while (exist)
*
*        See if the Nth item exists, and get its details.
*
         call dsa_nth_fits_item (figfile,n,exist,name,access,
     :                                          nelm,strlen,status)
         if (exist) then
            if (access.eq.' ') then
               call par_wruser ('Non-standard FITS item: '//
     :                             name(:chr_len(name)),ignore)
            else
               call chr_lcase(name)
*
*              We can handle this item.  For all its elements,
*              either read strings directly into STRING or read
*              numeric values into variables of suitable type and
*              and then format them into STRING.
*
               do i=1,nelm
*
*                 Character strings
*
                  if (access.eq.'C') then
                     call dsa_get_fits_c (figfile,name,i,string,
     :                                              comment,status)
                     call imakwc(im,name,string,comment,ier)
*
*                 Logical values
*
                  else if (access.eq.'L') then
                     call dsa_get_fits_l (figfile,name,i,lvalue,
     :                                              comment,status)
                     call imakwb(im,name,lvalue,comment,ier)
*
*                    Double precision floating point
*
                  else if (access.eq.'D') then
                     call dsa_get_fits_d (figfile,name,i,dvalue,
     :                                              comment,status)
                     call imakwd(im,name,dvalue,comment,ier)
*
*                    Single precision floating point
*
                  else if (access.eq.'F') then
                     call dsa_get_fits_f (figfile,name,i,value,
     :                                              comment,status)
                     call imakwr(im,name,value,comment,ier)
*
*                    Both long and short integers
*
                  else if ((access.eq.'I').or.(access.eq.'S')) then
                     call dsa_get_fits_i (figfile,name,i,ivalue,
     :                                              comment,status)
                     call imakwi(im,name,ivalue,comment,ier)
                  end if
                  if(ier.ne.0) call irafemess(ier)
               end do
            end if
*
*           Increment N ready for the next item in the substructure
*
            n=n+1
         end if
      end do
      call dsa_object_name(figfile,string,status)
      call imakwc(im,'title',string,' ',ier)
      end
