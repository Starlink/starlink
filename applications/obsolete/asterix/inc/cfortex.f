C /* cfortex.f   3.9 */          /* anonymous ftp@zebra.desy.de */
C /* Burkhard Burow  burow@desy.de                 1990 - 1997. */

C NAG f90 only
C Uses an exclamation mark, '!', to start comments. Do:
C prompt> mv cfortex.f cf_temp.f &&sed 's/^C/\!/g' cf_temp.f >cfortex.f
C      to convert the comments here into f90 compliant ones.
C NAG f90 only

      subroutine ss1(b)
      implicit none
      character*(*) b
      character*(13) a
      data a/'first'/
      b = a
      return
      end

      subroutine abc(a,b,c)
      implicit none
      character*(*) b,a,c
      character*(13) d
      d = a
      a = b
      b = c
      c = d
      return
      end

      subroutine forstr1(b)
      implicit none
      character*(*) b
      character*(13) a
      character*(13) forstr
      data a/'firs'/
      b = forstr(a)
      return
      end


      subroutine EASY(a,b)
      implicit none
      integer a,b
      a = b
      return
      end

      character*(*) function forstr(a)
      implicit none
      character*(*) a
      forstr = a
      return
      end

      real function rr(i)
      implicit none
      integer i
      rr = i
      return
      end

      character*(*) function forstr2()
      implicit none
C      character*(13) a   VAX/Ultrix complains about these ().
      character*13 a
      data a/'first'/
      forstr2 = a
      return
      end

      character*(*) function ft(v, w, a)
      implicit none
      character *(*) v(4), w(4)
      real a
      print*,'FT:len(v(1 or 2 or 3 or 4))  =',len(v(1))
      print*,'FT:len(w(1 or 2 or 3))    =',len(w(1))
      print*,'FT:a = ',a
      print*,'FT:v(1,2,3,4) =',v(1),',',v(2),',',v(3),',',v(4)
      print*,'FT:w(1,2,3,4) =',w(1),',',w(2),',',w(3),',',w(4)
      ft = v(1)
      return
      end

      character*(*) function fz(v, w, i)
      implicit none
      integer i,j
      character *(*) v(i), w(i)
      print*,'FZ:len(v(1 or 2 or 3 or 4))  =',len(v(1))
      print*,'FZ:len(w(1 or 2 or 3))    =',len(w(1))
      do 100 j = 1,i
        print*,'FZ:v(',j,') =',v(j),'   w(',j,') =',w(j)
100   continue
      fz = v(1)
      return
      end

      subroutine sz(v, w, i)
      implicit none
      integer i,j
      character *(*) v(i), w(i)
      print*,'SZ:len(v(1 or 2 or 3 or 4))  =',len(v(1))
      print*,'SZ:len(w(1 or 2 or 3))    =',len(w(1))
      do 100 j = 1,i
        print*,'SZ:v(',j,') =',v(j),'   w(',j,') =',w(j)
100   continue
      return
      end

      subroutine subt(v, w, a)
      implicit none
      character *(*) v(4), w(4)
      real a
      print*,'SUBT:len(v(1 or 2 or 3 or 4))  =',len(v(1))
      print*,'SUBT:len(w(1 or 2 or 3))    =',len(w(1))
      print*,'SUBT:a = ',a
      print*,'SUBT:v(1,2,3,4) =',v(1),',',v(2),',',v(3),',',v(4)
      print*,'SUBT:w(1,2,3,4) =',w(1),',',w(2),',',w(3),',',w(4)
      return
      end

      subroutine rev(a)
      implicit none
      integer a(2),t
      t    = a(1)
      a(1) = a(2)
      a(2) = t
      return
      end

      integer function frev(a)
      implicit none
      integer a(2)
      frev = a(1)
      a(1) = a(2)
      a(2) = frev
      return
      end

      subroutine ffcb()
      implicit none
      common /fcb/  v,w,x
      character *(13) v, w(4), x(3,2)
      print*,'FFCB:v =',v,'.'
      print*,'FFCB:w(1,2,3,4) =',w(1),',',w(2),',',w(3),',',w(4),'.'
      print*,'FFCB:x([1,2,3],1) =',x(1,1),',',x(2,1),',',x(3,1),'.'
      print*,'FFCB:x([1,2,3],2) =',x(1,2),',',x(2,2),',',x(3,2),'.'
      v      = 'fcb v'
      w(1)   = 'fcb w(1)'
      w(2)   = 'fcb w(2)'
      w(3)   = 'fcb w(3)'
      x(1,1) = 'fcb x(1,1)'
      x(2,1) = 'fcb x(2,1)'
      x(3,1) = 'fcb x(3,1)'
      x(1,2) = 'fcb x(1,2)'
      x(2,2) = 'fcb x(2,2)'
      x(3,2) = 'fcb x(3,2)'
      end

      subroutine feq()
      parameter (kwbank=690)  
C The & in the next line is for f90 line continuation.
C It is in column 74, i.e. part of f77 comments.
      common/gcbank/nzebra,gversn,zversn,ixstor,ixdiv,ixcons,fendq(16)   &
     &             ,lmain,lr1,ws(kwbank)    
      dimension iq(2),q(2),lq(80),iws(2)  
      equivalence (q(1),iq(1),lq(9)),(lq(1),lmain) ,(iws(1),ws(1))
      nzebra     = 1
      gversn     = 2
      zversn     = 3
      ixstor     = 4
      ixcons     = 5
      fendq(16)  = 6
      lmain      = 7
      lr1        = 8
      ws(kwbank) = 9
      lq(9)      = 10
      end

      subroutine fexist()
      implicit none
      print*,'FEXIST: was called'
      call exist()
      return
      end

      subroutine fa(i)
      implicit none
      integer i
      print*,'FA: integer argument =',i
      call cfortranca(i)
      return
      end

      subroutine fb(i)
      implicit none
      integer i
      print*,'FB: integer argument =',i
      i = i*2
      call cfcb(i)
      return
      end

      subroutine fc(b)
      implicit none
      character*(*) b
      print*,'FC: string argument =',b
      call cfcc(b)
      return
      end

      subroutine fd(b)
      implicit none
      character*(*) b
      character*(13) a
      data a/'birthday'/
      b = a
      call cdcfort(b)
      return
      end

      subroutine fe(v)
      implicit none
      character*(*) v(4)
      print*,'FE:len(v(1 or 2 or 3 or 4))  =',len(v(1))
      print*,'FE:v(1,2,3,4) =',v(1),',',v(2),',',v(3),',',v(4)
      call ce(v)
      return
      end

      subroutine ff(v,n)
      implicit none
      integer n
      character*(*) v(4)
      print*,'FF:len(v(1 or 2 or 3 or 4))  =',len(v(1))
      print*,'FF:v(1,2,3,4) =',v(1),',',v(2),',',v(3),',',v(4)
      print*,'FF:n =',n
      call ccff(v,n)
      return
      end

      integer function fg()
      implicit none
      integer ccg
      fg = ccg()
      return
      end

      character*(*) function fh()
      implicit none
      character*200 cch
      fh = cch()
      return
      end

      character*(*) function fi(v)
      implicit none
      character*(*) v(6)
      character*200 ci
      fi = ci(v)
      return
      end

      character*(*) function fj(v)
      implicit none
      integer v
      character*200 cj
      print*,'FJ:v =',v
      fj = cj(v)
      return
      end

      real function fk()
      implicit none
      real ck
      fk = ck()
      return
      end

      double precision function fl()
      implicit none
      double precision cl
      fl = cl()
      return
      end

      real function fm(r)
      implicit none
      external cm
      real cm,r
      fm = cm(r)
      return
      end

      double precision function fn(a,b)
      implicit none
      double precision cn,a,b
      fn = cn(a,b)
      return
      end

      subroutine vv(d,f,i)
      implicit none
      double precision d(2,2)
      real             f(2,2)
      integer          i(2,2)
      call cvv(d,f,i)
      return
      end

      double precision function v7(d)
      implicit none
      external cv7
      double precision d(1,13,11,7,5,3,2), cv7
      integer i,j,k,l,m,n,o
      print *, 'function cv7 returns the value ', cv7(d)
      v7 = 0
      do             1 i=1,  2
        do           1 j=1,  3
          do         1 k=1,  5
            do       1 l=1,  7
              do     1 m=1, 11
                do   1 n=1, 13
                  do 1 o=1,  1
                    v7 = v7 + d(o,n,m,l,k,j,i) 
1     continue
      return
      end

      logical function fand(a,b)
      implicit none
      logical cand,a,b
      fand = cand(a,b)
      return
      end


      logical function forr(a,b)
      implicit none
      logical cor,a,b

      print *, 'FORTRAN thinks you called forr(a=',a,',b=',b,').'
      forr = cor(a,b)
      print *, 'FORTRAN thinks cor(a,b) returned with a=',a,',b=',b,').'

      if (a.eqv..true.)then
        print *,'Double check: a is true:',a
      endif
      if (a.eqv..false.)then
        print *,'Double check: a is false:',a
      endif
      if (.not.((a.eqv..false.) .or. (a.eqv..true.))) then
        print *,'Double check: ERROR: a is neither true nor false:',a
        print *,'  Please contact burow@desy.de'
      endif

      if (b.eqv..true.)then
        print *,'Double check: b is true:',b
      endif
      if (b.eqv..false.)then
        print *,'Double check: b is false:',b
      endif
      if (.not.((b.eqv..false.) .or. (b.eqv..true.))) then
        print *,'Double check: ERROR: b is neither true nor false:',b
        print *,'  Please contact burow@desy.de'
      endif

C      print *, ' '
C      print *, '   Testing non-FORTRAN/77 (b .eq. .true.) which'
C      print *, '    will not compile on NAG f90 or Apollo or IBM RS/6000.'
C      print *, '   Compile cfortest.c with LOGICAL_STRICT defined'
C      print *, '    if you wish this test to work as expected.'
C      print *, '   This test requires a and b to match the internal '
C      print *, '    representation of .TRUE. and .FALSE. exactly.'
C      if (a.eq..true.)then
C        print *,'Representation check: a matches .true.'
C      endif
C      if (a.eq..false.)then
C        print *,'Representation check: a matches .false.'
C      endif
C      if (.not.(a.eq..false. .or. a.eq..true.)) then
C        print *,'Representation check:  '
C        print *,'         a matches neither .true. nor .false.'
C      endif
C      if (b.eq..true.)then
C        print *,'Representation check: b matches .true.'
C      endif
C      if (b.eq..false.)then
C        print *,'Representation check: b matches .false.'
C      endif
C      if (.not.(b.eq..false. .or. b.eq..true.)) then
C        print *,'Representation check:  '
C        print *,'         b matches neither .true. nor .false.'
C      endif
C      print *,' '

      return
      end


      subroutine fstrtok()
      implicit none
      character*70 cstrtok, a

C Setting up NULL as the NULL pointer for cfortran.h using 4 NUL bytes.
      character NULL*4, NUL(4)
      equivalence (NULL,NUL)

C HP-UX Fortran requires DATA statements to not follow executable statements.
      DATA a/'first+second-third+forth-fifth-sixth seventh'/

CSUNBUG Have to use an equivalenced NUL array to fill NULL with 4 NUL bytes
CSUNBUG  since Sun's 'Sep 8 1987 /usr/bin/f77' has a bug which didn't set NULL
CSUNBUG  to 4 NUL bytes in the following.
CSUNBUG      NULL = CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)
      NUL(1) = CHAR(0)
      NUL(2) = CHAR(0)
      NUL(3) = CHAR(0)
      NUL(4) = CHAR(0)

C NUL character in a will force cfortran.h to pass address of a,
C not of a copy as it usually does.
      a(70:) = NULL

C String until the first '-', then until the first '+'.
      print *,cstrtok(a,    '-')
      print *,cstrtok(NULL, '+')

C Flush the rest of the string.
C Recall cfortran.h kills all trailing blanks. i.e. FORTRAN ' ' -> C "".
      print *,cstrtok(NULL, ' ')

C Further calls return nothing.
      print *,cstrtok(NULL, ' ')

      return
      end

      integer function fadd(a,b)
      implicit none
      integer a,b
      fadd = a + b
      return
      end

      integer function funadd(fun,a,b)
      implicit none
      external fun
      integer a,b,fun
C WARNING FOR Alpha/OSF!
C The DEC Fortran and the DEC C compilers of DEC OSF/1 [RT] V1.2 (Rev. 10)
C will crash on this example as it stands.
C See cfortran.doc for a cleaner example of this misbehavior.
C Note that the routine funarg below, whose argument f is also an integer
C function, does not have this problem.
C This example will work if an extra argument is given to function 'fun'.
C i.e. For Alpha/OSF replace the following line with the kludge:
C     funadd = fun(a,b,1)
      funadd = fun(a,b)
      return
      end

      subroutine funarg(f,a,b,c)
      implicit none
      external fadd3,f
      integer a,b,c,f
      c = f(fadd3,a,b)
      return
      end

      subroutine fadd3(a,b,c)
      implicit none
      integer a,b,c
      c = a + b
      return
      end

      subroutine fqsortex(size)
      implicit none
C Because it's convinient here, we let C tell us the size of INTEGER.
      integer size

      integer base(10),cmp,i
      external cmp
      data base /1,10,2,9,3,8,4,7,5,6/
      call fqsort(base,10,size,cmp)
      print '(10I3)', (base(i), i=1,10)
      return
      end

      integer function cmp(a,b)
      implicit none
      integer a,b
      cmp = a-b
      return
      end

      subroutine fstr()
      implicit none
      character*(4) a,b,c
      character*(1) d(5)
      character*(5) dd
      equivalence (d,dd)
      character*(16) n

      call pstru(n)
      print *,n,'<-'

      a      = '1'
      a(2:2) = CHAR(0)
      call ppstr(a)

      b      = '22'
      call pstr(b)
      print *,b,'<-'
      call pstr(b)
      print *,b,'<-'

      c(1:1) = CHAR(0)
      c(2:2) = CHAR(0)
      c(3:3) = CHAR(0)
      c(4:4) = CHAR(0)
      call pnstr(c)
      c      = '333'
      c(4:4) = CHAR(0)
      call pnstr(c)

      call pnstr(b)
      print *,b,'<-'
      call pnstr(b)
      print *,b,'<-'

      c(1:1) = CHAR(0)
      c(2:2) = CHAR(0)
      c(3:3) = CHAR(0)
      c(4:4) = CHAR(0)
      call pnstr(c)
      d(1)   = '1'
      d(2)   = '2'
      d(3)   = '3'
      d(4)   = '4'
      d(5)   = CHAR(0)
C Need to use equivalenced dd because using d causes f90 to complain:
C Error: Inconsistent structure for arg 1 in call to PPSTR at line 533
      call ppstr(dd)
      call pstr(b)
      print *,b,'<-'
      call pstr(b)
      print *,b,'<-'

      return
      end

      subroutine f14(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
      implicit none
      integer        a,b,c,d,e,f,g,h,i,j,k,l,m,n
      call      cf14(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
      return
      end

      subroutine f20(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
      implicit none
      integer        a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t
      call      cf14(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
      o = 15
      p = 16
      q = 17
      r = 18
      s = 19
      t = 20
      return
      end

      subroutine sz1(s, is, z, iz)
      implicit none
      integer is,iz,j
      character *(*) s(is), z(iz)
      print*,'SZ1:len(s(1))  =',len(s(1)), '   len(z(1))  =',len(z(1))
      do 100 j = 1,is
        print*,'SZ1:s(',j,') =',s(j),'.'
100   continue
      do 200 j = 1,iz
        print*,'SZ1:z(',j,') =',z(j),'.'
200   continue
      return
      end

      subroutine pz(s, is, z, iz)
      implicit none
      integer is,iz,j
      character *(*) s(is),z(iz)
      print*,'PZ:len(s(1))  =',len(s(1)), '   len(z(1))  =',len(z(1))
      do 100 j = 1,is
        print*,'PZ:s(',j,') =',s(j),'.'
        s(j) = '12345678'
100   continue
      do 200 j = 1,iz
        print*,'PZ:z(',j,') =',z(j),'.'
        z(j) = '12345678'
200   continue
      return
      end

