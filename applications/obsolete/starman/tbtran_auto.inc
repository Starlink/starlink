CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBTRAN_AUTO.INC
C
C   alan penny                   ral         1994 Nov

      integer  ntmax		! Max number of found triangles allowed
      parameter (ntmax=300*300)

      integer  ipin		! Pointer to input table
      integer  ipmas		! Pointer to master table

      integer  tbvxm		! No of cols in master (+5 for name)
      integer  tbym		! No or rows in master
      integer  kw_mas		! Type of mags/heights/none in MAS table
      integer  kw_in		! Type of mags/heights/none in IN table
      integer  kfit		! Transform type (1=xy shift;3=xyshift:rotn:magn)

      real    toler		! distance allowed for posn fit
      real    ftoler		! distance allowed for fit fit
      integer ntmatch		! Min number of matches a triangle must have
      integer ntotmatch		! Min number of triangle matches for answer
      real    siglim		! Std devs of star from fit allowed
      real    xeqn(3)		! X coeffs of transfromation
      real    yeqn(3)		! Y coeffs of transfromation

      logical  interact		!Fit is interactive/automatic (T/F)

      logical  tbtau_dum1, tbtau_dum2, tbtau_dum3

      common / tbtaucoma / siglim,   tbvxm,    tbym,     kw_in,
     +                     kw_mas,    ipin,   ipmas,      xeqn, yeqn,
     +                      toler,  ftoler, ntmatch, ntotmatch,
     +                       kfit

      common / tbtaucomb /  interact, tbtau_dum1, tbtau_dum2, 
     +                      tbtau_dum3
