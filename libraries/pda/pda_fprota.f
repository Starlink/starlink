      subroutine pda_fprota(cos,sin,a,b)
c  subroutine pda_fprota applies a givens rotation to a and b.
c  ..
c  ..scalar arguments..
      real cos,sin,a,b
c ..local scalars..
      real stor1,stor2
c  ..
      stor1 = a
      stor2 = b
      b = cos*stor2+sin*stor1
      a = cos*stor1-sin*stor2
      return
      end
