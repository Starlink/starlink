      logical function pda_stopx(idummy)
c     *****parameters...
      integer idummy
c
c     ..................................................................
c
c     *****purpose...
c     this function may serve as the pda_stopx (asynchronous interruption)
c     function for the nl2sol (nonlinear least-squares) package at
c     those installations which do not wish to implement a
c     dynamic pda_stopx.
c
c     *****algorithm notes...
c     at installations where the nl2sol system is used
c     interactively, this dummy pda_stopx should be replaced by a
c     function that returns .true. if and only if the interrupt
c     (break) key has been pressed since the last call on pda_stopx.
c
c     ..................................................................
c
      pda_stopx = .false.
      return
      end
