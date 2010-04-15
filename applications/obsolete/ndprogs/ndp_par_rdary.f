      subroutine ndp_par_rdary(ref_name,min_val,max_val,order,
     : units,nv,nvmax,vals)
      implicit none
C
C     Parameters
C
      integer nv,nvmax
      real min_val(nvmax),max_val(nvmax),vals(nvmax)
      character ref_name*(*),order*(*),units*(*)
C
C     Functions
C
      integer ich_encode
      integer ich_len
C
C     Local variables
C
      integer i,next,status
      real amin,amax
      logical accept
      character field1*10,field2*10,message*80
C
C     Find the group min and max
C
      amax = max_val(1)
      amin = min_val(1)
      do i = 1,nv
         amax = max(amax,max_val(i))
         amin = min(amin,min_val(i))
      end do
C
C     Now keep prompting until you get a good answer
C
      accept = .false.
      do while (.not. accept)
         call par_rdary(ref_name,amin,amax,order,units,nv,nvmax,vals)
         accept = .true.
         do i = 1,nv
            if ((vals(i) .gt. max_val(i)) .or.
     :      (vals(i) .lt. min_val(i))) then
               status = ich_encode(field1,min_val(i),1,6,next)
               status = ich_encode(field2,max_val(i),1,6,next)
               write(message,100) i,field1(:ich_len(field1)),
     :         field2(:ich_len(field2))
  100          format('Value of parameter',i4,' must be between ',a,
     :         ' and ',a,'\\n')
               call dsa_wruser(message(:ich_len(message)))
               accept = .false.
               call par_cnpar(ref_name)
            end if
         end do
      end do
      end
