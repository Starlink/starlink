      subroutine cophdr2hds(im,output)
*+
* Name:
*    COPHDR2HDS

* Invocation:
*    CALL COPHDR2HDS(IM,OUTPUT)

* Purpose:
*  To copy the header information from an IRAF file into a FIGARO file
*  as applicable.
*
* Description:
*  To copy the header information from an IRAF file into a FIGARO file
*  as applicable.
*
* Arguments:
*     IM = INTEGER (Given)
*        IRAF file reference number
*     OUTPUT = CHARACTER*(*) (Given)
*        Figaro data file
*
*  Adapted from DSA docs by T.N.Wilkins Cambridge 1989 (via
*  cophdr2irafd).
*-
*
      implicit none
*
*     Functions
*
      character*(*) output
      integer ich_fold
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

      logical exist

* Loop index through elements

      integer i

* Used to hold integer values

      integer ivalue

* Used to hold logical values

      logical lvalue

* Counter through items

      integer n

* Name of item

      character name*16

* Temporary store for name

      character*16 tmp

* Running status for DSA routines

      integer status

* String to hold values of items

      character string*80

* Length of character items

      integer strlen

* Used to hold real values

      real value

* IRAF image identifier

      integer im

* IRAF error code

      integer ier

* keyword list

      integer kwl

* IRAF data type code

      integer dtype

* Convert DTYPE to letter code

      character*1 types(7)
      integer NDICT
      parameter (NDICT = 21)
      character*8 stand(NDICT)
      data types/'L','C','S','I','I','F','D'/
      data stand/'NAXIS1','NAXIS2','NAXIS3','PIXTYPE','DATATYPE',
     :    'DATAMIN','DATAMAX','CTIME','MTIME','LIMTIME','BITPIX',
     :    'IRAF-B/P','IRAFTYPE','IRAF-MAX','IRAF-MIN','CRVAL1',
     :    'CRPIX1','CDELT1','CRVAL2','CRPIX2','CDELT2'/
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
      call imokwl(im,'*',.false.,kwl,ier)
      if(ier.ne.0) call irafemess(ier)
      do while (exist)
*
*        See if the Nth item exists, and get its details.
*
         call imgnkw(kwl,name,ier)
         exist = ier.eq.0

         if (exist) then
            call imtypk(im,name,dtype,comment,ier)
            if(ier.ne.0) call irafemess(ier)
            access = types(dtype)
*
*       Check for standard IRAF keywords - we don't copy these-except
*       title
*
            tmp = name
            strlen = ich_fold(tmp)
            do i = 1, NDICT
              if(tmp(:strlen).eq.stand(i)(:strlen)) access = ' '
            end do
            if (access.ne.' ') then
*
*              We can handle this item.  For all its elements,
*              either read strings directly into STRING or read
*              numeric values into variables of suitable type and
*              and then format them into STRING.
*
*
*                 Character strings
*
               if (access.eq.'C') then
                  call imgkwc(im,name,string,ier)
                  if(tmp(:strlen).eq.'TITLE') then
                    call dsa_set_object(output,string,status)
                  else
                    call dsa_put_fits_c (output,name,string,
     :                                              comment,status)
                  end if
*
*                 Logical values
*
               else if (access.eq.'L') then
                  call imgkwb(im,name,lvalue,ier)
                  call dsa_put_fits_l (output,name,lvalue,
     :                                              comment,status)
*
*                    Double precision floating point
*
               else if (access.eq.'D') then
                  call imgkwd(im,name,dvalue,ier)
                  call dsa_put_fits_d (output,name,dvalue,
     :                                              comment,status)
*
*                    Single precision floating point
*
               else if (access.eq.'F') then
                  call imgkwr(im,name,value,ier)
                  call dsa_put_fits_f (output,name,value,
     :                                              comment,status)
*
*                    Both long and short integers
*
               else if ((access.eq.'I').or.(access.eq.'S')) then
                  call imgkwi(im,name,ivalue,ier)
                  call dsa_put_fits_i (output,name,ivalue,
     :                                              comment,status)
               end if
               if(ier.ne.0) call irafemess(ier)
            end if
*
*           Increment N ready for the next item in the substructure
*
            n=n+1
         end if
      end do
      call imckwl(kwl,ier)
*
*  See if we can contruct any axix
*
c      do n = 1, naxis
c        call imgkwr(im,name,value,ier)
c      end do
*
      end
