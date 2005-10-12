C     ALGORITHM 611, COLLECTED ALGORITHMS FROM ACM.
C     ALGORITHM APPEARED IN ACM-TRANS. MATH. SOFTWARE, VOL.9, NO. 4,
C     DEC., 1983, P. 503-524.
      integer function pda_imdcon(k)
c
      integer k
c
c  ***  return integer machine-dependent constants  ***
c
c     ***  k = 1 means return standard output unit number.   ***
c     ***  k = 2 means return alternate output unit number.  ***
c     ***  k = 3 means return  input unit number.            ***
c          (note -- k = 2, 3 are used only by test programs.)
c
c  +++  port version follows...
c     external i1mach
c     integer i1mach
c     integer mdperm(3)
c     data mdperm(1)/2/, mdperm(2)/4/, mdperm(3)/1/
c     pda_imdcon = i1mach(mdperm(k))
c  +++  end of port version  +++
c
c  +++  non-port version follows...
      integer mdcon(3)
      data mdcon(1)/6/, mdcon(2)/8/, mdcon(3)/5/
      pda_imdcon = mdcon(k)
c  +++  end of non-port version  +++
c
 999  return
c  ***  last card of pda_imdcon follows  ***
      end
