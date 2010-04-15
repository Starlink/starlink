*+  ADC_XFUNDD - Evaluate a numeric function
      subroutine chi_afundd(fnumber, nargs, arg, value, status)
*    Description :
*     Performs an evaluation of a numeric function to a DP result.
*    Invocation
*     CALL ADC_XFUNDD(FNUMBER, NARGS, ARG, VALUE, STATUS)
*    Parameters :
*     FNUMBER=INTEGER(INPUT)
*           The number of the function
*     NARGS=INTEGER(INPUT)
*           The argument count
*     ARG=DOUBLE(*)(INPUT)
*           The array of argument list values
*     VALUE=DOUBLE(OUTPUT)
*           Value of the expression
*     STATUS=INTEGER(UPDATE)
*           Status variable
*    Method :
*     Go to the function specified
*     If another function value is calculated then
*        Store it.
*     Endif
*     Note : This routine assumes that functions are always
*            evaluated in the same order.  This halves the time
*            for the calculation of both longitude and latitude
*            coordinates.
*    Authors :
*     Jon Fairclough (RAL::IPMAF)
*    History :
*     3-Feb-1985: Original
*    20-Apr-1985: ADIFF and GRCIRC functions added. LOG10/LOG reversed
*    17-May-1985: SUPGAL and GALSUP routines added
*    27-Nov-1986: renamed from ADC_CALC1 distinguishing from ADC_XFUNDI
*    Type Definitions :
      implicit none
*    Global constants :
      include 'SAE_PAR'                 ! SAI Symbolic Constants
      include 'CHI_ERR'
      include 'CHIPAR_ERR'
      include 'CHI_PAR'
      include 'CHIPAR_PAR'
      include 'CHIPAR1_PAR'
*    Import
      integer fnumber
      integer nargs
      double precision arg(*)                ! Values
*    Export
      double precision value
*    Status :
      integer status
*    Global variables
      include 'CHIWRK_CMN'
*    External references
      double precision chi_agrcirc
*    Local
      integer i
      logical error
      double precision dummy
      double precision dummy1
      double precision dummy2
      double precision dummy3
      double precision dummy4
*-
*    Begin
*
      if (status .ne. SAI__OK) then
         return
      endif
*
*
      if (fnumber .lt. 1 .or.
     :    fnumber .gt. CHI__MXFUN) then
          status = CHI__NTSUP
          goto 100
      endif
*
*   Use the stored value if exists
*
      if (ftflag(fnumber)) then
         value = ftsave(fnumber)
         ftflag(fnumber) = .false.
         goto 100
      endif
*
*   Branch to function required
*
      goto (  1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
     :       11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
     :       21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
     :       31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
     :       41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
     :       51, 52), fnumber
*
      status = CHI__NTSUP
      goto 100
*
    1 value = sqrt (arg(1))
      goto 100
*
    2 value = log10(arg(1))
      goto 100
*
    3 value = log  (arg(1))
      goto 100
*
    4 value = exp  (arg(1))
      goto 100
*
    5 value = sin  (arg(1))
      goto 100
*
    6 value = cos  (arg(1))
      goto 100
*
    7 value = tan  (arg(1))
      goto 100
*
    8 value = asin (arg(1))
      goto 100
*
    9 value = acos (arg(1))
      goto 100
*
   10 value = atan (arg(1))
      goto 100
*
   11 value = sinh (arg(1))
      goto 100
*
   12 value = cosh (arg(1))
      goto 100
*
   13 value = tanh (arg(1))
      goto 100
*
   14 value = abs  (arg(1))
      goto 100
*
   15 status = CHI__NTSUP
      goto 100
*
   16 call sla_eg50  (arg(1), arg(2), value, dummy)
      ftsave(17) = dummy
      ftflag(17) = .true.
      goto 100
*
   17 call sla_eg50  (arg(1), arg(2), dummy, value)
      ftsave(16) = dummy
      ftflag(16) = .true.
      goto 100
*
   18 call sla_ge50  (arg(1), arg(2), value, dummy)
      ftsave(19) = dummy
      ftflag(19) = .true.
      goto 100
*
   19 call sla_ge50  (arg(1), arg(2), dummy, value)
      ftsave(18) = dummy
      ftflag(18) = .true.
      goto 100
*
   20 call sla_preces('FK4', arg(1), arg(2), arg(3), arg(4))
      value = arg(3)
      ftsave(21) = arg(4)
      ftflag(21) = .true.
      goto 100
*
   21 call sla_preces('FK4', arg(1), arg(2), arg(3), arg(4))
      value = arg(4)
      ftsave(20) = arg(3)
      ftflag(20) = .true.
      goto 100
*
   22 call sla_preces('FK5', arg(1), arg(2), arg(3), arg(4))
      value = arg(3)
      ftsave(23) = arg(4)
      ftflag(23) = .true.
      goto 100
*
   23 call sla_preces('FK5', arg(1), arg(2), arg(3), arg(4))
      value = arg(4)
      ftsave(22) = arg(3)
      ftflag(22) = .true.
      goto 100
*
   24 call sla_eqgal (arg(1), arg(2), value, dummy)
      ftsave(25) = dummy
      ftflag(25) = .true.
      goto 100
*
   25 call sla_eqgal (arg(1), arg(2), dummy, value)
      ftsave(24) = dummy
      ftflag(24) = .true.
      goto 100
*
   26 call sla_galeq (arg(1), arg(2), value, dummy)
      ftsave(27) = dummy
      ftflag(27) = .true.
      goto 100
*
   27 call sla_galeq (arg(1), arg(2), dummy, value)
      ftsave(26) = dummy
      ftflag(26) = .true.
      goto 100
*
   28 call sla_fk425 (arg(1), arg(2), arg(3), arg(4), arg(5), arg(6),
     :                value , dummy , dummy1, dummy2, dummy3, dummy4)
      ftsave(29) = dummy
      ftflag(29) = .true.
      goto 100
*
   29 call sla_fk425 (arg(1), arg(2), arg(3), arg(4), arg(5), arg(6),
     :                dummy , value , dummy1, dummy2, dummy3, dummy4)
      ftsave(28) = dummy
      ftflag(28) = .true.
      goto 100
*
   30 call sla_fk524 (arg(1), arg(2), arg(3), arg(4), arg(5), arg(6),
     :                value , dummy , dummy1, dummy2, dummy3, dummy4)
      ftsave(31) = dummy
      ftflag(31) = .true.
      goto 100
*
   31 call sla_fk524 (arg(1), arg(2), arg(3), arg(4), arg(5), arg(6),
     :                dummy , value , dummy1, dummy2, dummy3, dummy4)
      ftsave(30) = dummy
      ftflag(30) = .true.
      goto 100
*
   32 call sla_eqecl (arg(1), arg(2), arg(3), value , dummy)
      ftsave(33) = dummy
      ftflag(33) = .true.
      goto 100
*
   33 call sla_eqecl (arg(1), arg(2), arg(3), dummy , value)
      ftsave(32) = dummy
      ftflag(32) = .true.
      goto 100
*
   34 call sla_ecleq (arg(1), arg(2), arg(3), value , dummy)
      ftsave(35) = dummy
      ftflag(35) = .true.
      goto 100
*
   35 call sla_ecleq (arg(1), arg(2), arg(3), dummy , value)
      ftsave(34) = dummy
      ftflag(34) = .true.
      goto 100
*
   36 call sla_pm    (arg(1), arg(2), arg(3), arg(4), arg(5),
     :                arg(6), arg(7), arg(8), value , dummy)
      ftsave(37) = dummy
      ftflag(37) = .true.
      goto 100
*
   37 call sla_pm    (arg(1), arg(2), arg(3), arg(4), arg(5),
     :                arg(6), arg(7), arg(8), dummy , value)
      ftsave(36) = dummy
      ftflag(36) = .true.
      goto 100
*
   38 value = abs(arg(1) - arg(2))
      goto 100
*
   39 value = chi_agrcirc(arg(1), arg(2), arg(3), arg(4))
      goto 100
*
   40 call sla_galsup(arg(1), arg(2), value, dummy)
      ftsave(41) = dummy
      ftflag(41) = .true.
      goto 100
*
   41 call sla_galsup(arg(1), arg(2), dummy, value)
      ftsave(40) = dummy
      ftflag(40) = .true.
      goto 100
*
   42 call sla_supgal(arg(1), arg(2), value, dummy)
      ftsave(43) = dummy
      ftflag(43) = .true.
      goto 100
*
   43 call sla_supgal(arg(1), arg(2), dummy, value)
      ftsave(42) = dummy
      ftflag(42) = .true.
      goto 100
*
   44 status = CHI__NTSUP
      goto 100
*
   45 status = CHI__NTSUP
      goto 100
*
   46 status = CHI__NTSUP
      goto 100
*
   47 status = CHI__NTSUP
      goto 100
*
   48 status = CHI__NTSUP
      goto 100
*
   49 value = min  (arg(1), arg(2))
      do 491 i = 3, nargs
  491 value = min  (value, arg(i))
      goto 100
*
   50 value = max  (arg(1), arg(2))
      do 501 i = 3, nargs
  501 value = max  (value, arg(i))
      goto 100
*
   51 status = CHI__NTSUP
      goto 100
*
   52 status = CHI__NTSUP
      goto 100
*
  100 continue
*
*    Check arithmetic error
*
      end

