      SUBROUTINE PDA_DQED( PDA_DQEDEV, MEQUA, NVARS, MCON, IND, BL, BU,
     :                     X, FJAC, LDFJAC, FNORM, IGO, IOPT, ROPT,
     :                     IWA, WA )
C***  BEGIN PROLOGUE  DQED
C***  DATE WRITTEN   851210   (YYMMDD)
C***REVISION DATE  870204   (YYMMDD)
C***CATEGORY NO. K1b,K1b1a2,K1b2a
C***KEYWORDS  NONLINEAR LEAST SQUARES, SIMPLE BOUNDS,
C             LINEAR CONSTRAINTS
C***AUTHOR  HANSON, R. J., SNLA
C           KROGH, F. T., JPL-CIT
C***PURPOSE  SOLVE NONLINEAR LEAST SQUARES AND NONLINEAR
C            EQUATIONS.  USER PROVIDES SIMPLE BOUNDS, LINEAR
C            CONSTRAINTS AND EVALUATION CODE FOR THE FUNCTIONS.
C***ROUTINES CALLED  DQEDMN, DQEDEV, CHRCNT, XERRWV
C***LONG DESCRIPTION
C        SUBROUTINE DQED (DQEDEV, MEQUA, NVARS, MCON, IND, BL, BU, X,
C       *            FJ, LDFJ, RNORM, IGO, IOPT, ROPT,
C       *            IWORK, WORK)
C
C
C  Table of Sections
C  -----------------
C  1. Introduction
C     ------------
C  2. Calling Sequence Explained
C     ------- -------- ---------
C  3. Remarks on the Usage Examples
C     ------- -- --- ----- --------
C  4. Error Messages for DQED()
C     --------------------------
C  5. References
C     ----------
C
C  1. Introduction
C     ------------
C  This  software  package  is  available in both single and double
C  precision.    The   double  precision  version  (type  REAL*8)  is
C  described  below.   For  the  REAL  version  of the
C  documentation  substitute  'REAL' for 'DOUBLE  PRECISION' in the
C  type  statements.  Change the names of the subprograms: 'DQED()'
C  to 'SQED()', 'DQEDEV()' to 'SQEDEV()', and 'D1MACH' to 'R1MACH.'
C
C  The Fortran subprogram, DQED(), solves the constrained nonlinear
C  least squares problem:
C
C  Minimize  the  sum  of  squares  of  MEQUA (generally nonlinear)
C  equations,
C
C       f (x) = 0, I=1,...,MEQUA                    Eq. (1)
C        I
C  where  x  is  a  (vector)  set  of  NVARS unknowns.  (The vector
C  function  with  these  MEQUA  components  is  called f(x) in the
C  discussion  that  follows.)   The components of x may have upper
C  and  lower  bounds  given  by  the  user.   (In  fact all of the
C  possible  cases, no bounds, bounds at one end only, or upper and
C  lower  bounds  can  be  specified.)   Linear  constraints on the
C  unknowns,  more  general than simple bounds,  can also be given.
C  These constraints can be of the equality or inequality type:
C
C       a  x + ... + a       x      =  y , L = 1,...,MCON,
C        L1 1         L,NVARS NVARS     L
C                                                   Eq. (2)
C
C  with  bounds  specified on the y , again given by the user.  The
C                                  L
C  constraints  can  actually  be slightly nonlinear.  In this case
C  the constraints can be described as:
C
C       g (x) =  y , L = 1,...,MCON,                Eq. (2')
C        L        L
C  where bounds are specified on each y .  The functions g (x) must
C                                      L                  L
C  be  defined for all x in the set described by the simple bounds.
C  Experienced users may wish to turn directly to Examples 1 and 2,
C  listed  below,  before  reading  the  subprogram  documentation.
C  There  is  no  size relation required for the problem dimensions
C  MEQUA,  NVARS,  and  MCON  except  that MEQUA and NVARS are both
C  positive,  and   MCON is nonnegative.
C
C  This code package will do a decent job of solving most nonlinear
C  least squares problems that can be expressed as Eqs. (1) and (2)
C  above,  provided  that  continuous  derivatives of the functions
C  with  respect  to the parameters can be computed.  This can also
C  include  problems  where  the derivatives must be computed using
C  some    form    of    numerical    differentiation.    Numerical
C  differentiation is not provided with this software for solving
C  nonlinear  least squares problems.  Refer to the subprogram
C  JACG for numerical differentiation.  (Note: D. Salane has this
C  submitted to TOMS.  It is not included here.)
C
C  The  authors  also  plan  to develop methods that will do a much
C  better  job  of  coping  with  constraints more general than the
C  essentially linear ones indicated above in Eqs. (2)-(2').  There
C  are  nonlinear  least squares problems with innocent looking but
C  highly  nonlinear  constraints  where  this package will fail to
C  work.   The authors also hope to reduce the overhead required by
C  the software.  This high overhead is due primarily to the method
C  used  to  solve  the  inner-loop  quadratic  model problem.  The
C  authors  recommend  that  users consider using the option number
C  14, described below, to suppress use of the quadratic model. The
C  user  may  find  that  the software works quite well without the
C  quadratic  model.  This  may  be important when the function and
C  derivatives  evaluations  are  not expensive but many individual
C  problems are being solved.
C
C  There  are  two  fundamental  ways to use the subprogram DQED().
C  The  most  staightforward way is to make one Fortran CALL to the
C  subprogram  and  obtain  values  for  the unknowns, x.  The user
C  provides  a subprogram DQEDEV(), described below, that gives the
C  subprogram DQED() values of the functions f(x) and g(x), and the
C  derivative  or  Jacobian  matrices  for  f(x)  and  g(x) at each
C  desired  point x.  This usage is called 'forward communication.'
C  An  alternate  way to use the subprogram is to provide an option
C  that  allows  the  user  to communicate these values by 'reverse
C  communication.'   The  subprogram returns to the calling program
C  unit  and  requests  values  for f(x) and g(x), and the Jacobian
C  matrices  for  f(x)  and  g(x)  for  a  given value of x.  (This
C  framework   is   often   required   in  applications  that  have
C  complicated  algorithmic  requirements  for  evaluation  of  the
C  functions.)   An  example  using  both  'forward'  and 'reverse'
C  communication  is  provided  below  (see  Remarks  on  the Usage
C  Examples) for least squares fitting of two exponential functions
C  to five data points.
C
C  2. Calling Sequence Explained
C     ------- -------- ---------
C  There   are  arrays  used  by  the  subprogram  that  must  have
C  dimensions equivalent to the following declarations.
C
C        INTEGER MEQUA, NVARS, MCON, LDFJ, IGO
C        INTEGER IND(NVARS+MCON), IOPT(LIOPT), IWORK(LIWORK)
C
C        DOUBLE PRECISION BL(NVARS+MCON), BU(NVARS+MCON), X(NVARS), RNORM,
C       *ROPT(LROPT), FJ(LDFJ,NVARS+1), WORK(LWORK)
C
C        EXTERNAL DQEDEV
C  The array dimensions must satisfy the bounds:
C        LIOPT .ge.  Number required for options in use.
C        LROPT .ge. Number required for options in use.
C         LDFJ .ge. MEQUA+MCON,
C  The  array  dimensions  for  the arrays IWORK(*) and WORK(*) can
C  change  if either option 14 or option 15 are in use.  For use in
C  the formulas, define:
C
C       MC=MCON
C       ME=MEQUA
C       NV=NVARS
C       MX=MAX(MEQUA,NVARS)
C  If the user is not using option 15, then
C       NT=5.
C  If the user is using option 15, then
C       NT=new number, must be .ge. 2.
C  If the user is not using option 14, then
C       NA=MC+2*NV+NT.
C  If the user is using option 14, then
C       NA=MC+NV+1.
C
C
C  In terms of these values defined above,
C        LIWORK .ge. 3*MC+9*NV+4*NT+NA+10
C         LWORK .ge. NA*(NA+4)+NV*(NT+33)+(ME+MX+14)*NT+9*MC+26
C
C  The  subprogram  DQEDEV  must  be declared in a Fortran EXTERNAL
C  statement:
C
C        EXTERNAL DQEDEV
C
C  Initialize the values of the parameters:
C        MEQUA, NVARS, MCON, IND(*), BL(*), BU(*), X(*), LDFJ,
C        IOPT(*), IWORK(1), IWORK(2),
C
C        CALL DQED  (DQEDEV, MEQUA, NVARS, MCON, IND, BL, BU, X,
C       *            FJ, LDFJ, RNORM, IGO, IOPT, ROPT,
C       *            IWORK, WORK)
C
C  Subprogram parameters:
C
C  DQEDEV (Input)
C  -----
C  This  is  the  name  of  a subprogram that the user will usually
C  supply  for  evaluation  of  the  values  of the constraints and
C  model,  and  the  derivatives  of these functions. The user must
C  provide  this subprogram unless 'reverse communication' is used.
C  A  model  for  writing the subprogram DQEDEV() is provided under
C  the heading Example 1 Using Forward Communication, listed below.
C  Users  may  find  it  convenient to modify this model subprogram
C  when  writing  a  subprogram  for  their  own  application.   If
C  'reverse communication' is used, the user does not need to write
C  a stub or dummy subroutine named DQEDEV().  All that is required
C  is  to  declare exactly this name in an EXTERNAL statement.  The
C  code  package  has a dummy subroutine DQEDEV() that will be used
C  in   the   linking  or  load  step.   Example  2  Using  Reverse
C  Communication, listed below, illustrates this detail.
C
C  MEQUA, NVARS, MCON (Input)
C  ------------------
C  Respectively  they  are:  The number of least squares equations,
C  the  number  of unknowns or variables, and the number of general
C  constraints  for the solution, not including simple bounds.  The
C  values  of  MEQUA  and NVARS must be positive; the value of MCON
C  must  be  nonnegative.   Other  values  for these parameters are
C  errors.
C
C  IND(*),BL(*),BU(*) (Input)
C  ------------------
C  These  arrays  describe  the  form of the simple bounds that the
C  components of x are to satisfy.  Components numbered 1,...,NVARS
C  are  used  to  describe  the  form of the simple bounds that the
C  unknown     are     to     satisfy.      Components     numbered
C  NVARS+1,...,NVARS+MCON  are  used  to  describe  the form of the
C  general  MCON linear constraints.  The first NVARS components of
C  IND(*)  indicate  the type of simple bounds that the solution is
C  to  satisfy.   The  corresponding entries of BL(*) and BU(*) are
C  the bounding value.  The only values of IND(*) allowed are 1,2,3
C  or 4.  Other values are errors.  Specifically:
C
C  IND(J)=1, if x .ge. BL(J) is required; BU(J) is not used.
C                J
C        =2, if x .le. BU(J) is required; BL(J) is not used.
C                J
C        =3, if x .ge. BL(J) and x .le. BU(J) is required.
C                J                J
C        =4, if no bounds on x  are required;
C                             J
C                BL(*),BU(*) are not used.
C  General  linear constraints of the form shown in Eq. (2) require
C  that bounds be given for the linear functions y .  Specifically:
C                                                 L
C
C  IND(NVARS+L)=1,  if y .ge. BL(NVARS+L) is required; BU(*) is not
C                       L
C                 needed.
C
C              =2, if y .le. BU(NVARS+L) is required; BL(*) is not
C                      L
C                  needed.
C              =3, if y .ge. BL(NVARS+L) and y .le. BU(NVARS+L)
C                      L                      L
C
C              =4, if no bounds on y  are required;
C                                   L
C                  BL(*),BU(*) are not used.
C
C  The  values of the bounds for the unknowns may be changed by the
C  user  during  the  evaluation of the functions f(x) and g(x) and
C  their Jacobian matrices.
C
C  X(*),FJ(*,*),LDFJ (Input and Output, except LDFJ which is Input)
C  -----------------
C  The  array  X(*)  contains  the  NVARS  values,  x,   where  the
C  functions  f(x)  and  g(x)  and  their Jacobian matrices will be
C  evaluated  by  the subprogram DQED().  After the computation has
C  successfully  completed, the array X(*) will contain a solution,
C  namely  the  unknowns of the problem, x.  (Success is determined
C  by  an  appropriate  value for IGO.  This parameter is described
C  below.)  Initially  the array X(*) must contain a starting guess
C  for  the  unknowns of the problem, x.  The initial values do not
C  need  to  satisfy the constraints or the bounds.  If they do not
C  satisfy the bounds, then the point will be simply projected onto
C  the  bounds  as  a  first  step.  The first linear change to the
C  values  of  x must satisfy the general constraints.  (It is here
C  that the assumption of their linearity is utilized.)
C
C  The  Fortran  two-dimensional array FJ(*,*) is used to store the
C  linear  constraints  of  Eq. (2) (or more generally the Jacobian
C  matrix  of  the functions g(x) with respect to the variables x),
C  and  the  Jacobian  matrix  of  the function f(x).  The Jacobian
C  matrix of the (linear) constraints is placed in rows 1,...,MCON.
C  The  Jacobian  matrix  of  f(x)  is  placed in rows MCON+1, ...,
C  MCON+MEQUA.   The parameter LDFJ is the leading or row dimension
C  of  the  array  FJ(*,*).  Normally the array FJ(*,*) is assigned
C  values   by   the   user  when  the  nonlinear  solver  requests
C  evaluations  of  the  constraints  g(x)  and  the  function f(x)
C  together  with  the Jacobian matrices G(x) and J(x).  The values
C  of  the  constraint  functions  g (x)  are  placed  in the array
C                                   L
C  FJ(L,NVARS+1),  L=1,...,MCON.  The values of the model functions
C  f (x)  are  placed  in  the array at entries FJ(MCON+I,NVARS+1),
C   I
C  I=1,...,MEQUA.   Note  that the second dimension of FJ(*,*) must
C  be at least NVARS+1 in value.
C
C  RNORM (Output)
C  -----
C  This is the value of the Euclidean length or square root of sums
C  of  squares  of  components  of  the  function  f(x)  after  the
C  approximate solution, x, has been found.  During the computation
C  it  is  updated  and equals the best value of the length of f(x)
C  that has been found.
C
C  IGO (Output; it can be an Input if interrupting the code)
C  ---
C  This  flag  directs  user  action and informs the user about the
C  type  of  results obtained by the subprogram.  The user may find
C  it  convenient  to  treat  the cases abs(IGO) .le. 1 the same as
C  IGO=1.  This has no effect on the solution process.
C
C  The  user  can  interrupt the computation at any time, obtaining
C  the  best  values  of the vector x up to this point,  by setting
C  IGO  to any value .gt. 1 and then return control to DQED().  For
C  example  if  a  calculation  must be done in a certain length of
C  time,  the  user  can,  as  the  end of the time draws near, set
C  IGO=20  and return control to DQED().  It is important that this
C  method  be  used  to  stop further computing, rather than simply
C  proceeding.  The reason for this is that certain flags in DQED()
C  must  be reset before any further solving on subsequent problems
C  can  take  place.  The value of IGO .gt. 1 used to interrupt the
C  computation  is  arbitrary and is the value of IGO returned.  If
C  values of IGO =2,...,18 are used to flag this interrupt, they do
C  not mean the same thing as indicated below.  For this reason the
C  value  IGO=20 is recommended for signaling interrupts in DQED().
C  Another   situation  that  may  occur  is  the  request  for  an
C  evaluation  of  the functions and derivatives at a point x where
C  these can't be evaluated.  If this occurs, set IGO=99 and return
C  control  to  DQED().   This will have the effect of defining the
C  derivatives  to  be  all  zero  and the functions to be 'large.'
C  Thus  a  reduction  in  the trust region around the current best
C  estimate  will occur.  Assigning the value IGO=99 will not cause
C  DQED() to stop computing.
C
C      =0   Place  the  value  of  f(x)  in FJ(MCON+*,NVARS+1).  If
C  'reverse  communication'  is  being used, CALL DQED() again.  If
C  'forward communication' is being used, do a RETURN.
C
C      =1  or  (-1)   Evaluate the Jacobians for the functions g(x)
C  and  f(x) as well as evaluating g(x) and f(x).  Use the vector x
C  that is now in the array X(*) as the values  where this
C  evaluation  will  be performed.  Place the Jacobian matrix
C  for  g(x) in the first MCON rows of FJ(*,*).  Place the Jacobian
C  matrix for f(x) in rows MCON+1,...,MCON+MEQUA in FJ(*,*).  Place
C  the  value of g(x) in FJ(*,NVARS+1).  Place the value of f(x) in
C  FJ(MCON+*,NVARS+1).
C
C  (Users who have complicated functions whose derivatives cannot be
C  computed analytically may want to use the numerical differentiation
C  subroutine JAGC.  This is available on the SLATEC library.)
C
C  If  'reverse communication' is being used, CALL DQED() again.
C  If 'forward communication' is being used, do a RETURN.
C
C  A  value  IGO=(-1)  flags  that  that the number of terms in the
C  quadratic  model  is  being  restricted by the amount of storage
C  given  for  that  purpose.   It  is  suggested,  but  it  is not
C  required,  that  additional  storage  be given for the quadratic
C  model  parameters.   See the following description of The Option
C  Array,  option  number 15, for the way to designate more storage
C  for this purpose.
C
C      =2   The function f(x) has a length less than TOLF.  This is
C  the  value  for  IGO to be expected when an actual zero value of
C  f(x)  is  anticipated.   See the description of The Option Array
C  for the value.
C
C      =3   The  function  f(x)  has  reached a value that may be a
C  local   minimum.   However,  the  bounds  on  the  trust  region
C  defining  the size of the step are being hit at each step.  Thus
C  the  situation  is  suspect.  (Situations of this type can occur
C  when  the  solution  is at infinity in some of the components of
C  the   unknowns,  x.  See the description of The Option Array for
C  ways to avoid this value of output value of IGO.
C
C      =4   The function f(x) has reached a local minimum.  This is
C  the  value  of IGO that is expected when a nonzero value of f(x)
C  is anticipated.  See the description of The Option Array for the
C  conditions that have been satisfied.
C
C      =5   The  model  problem  solver  has  noted a value for the
C  linear or quadratic model problem residual vector length that is
C  .ge.  the  current  value  of  the  function, i.e. the Euclidean
C  length   of  f(x).   This  situation  probably  means  that  the
C  evaluation  of  f(x)  has  more  uncertainty  or  noise  than is
C  possible  to  account for in the tolerances used to note a local
C  minimum.  The value for x is suspect, but a minimum has probably
C  been found.
C
C      =6  A small change (absolute) was noted for the vector x.  A
C  full  model problem step was taken.  The condition for IGO=4 may
C  also  be  satisfied, so that a minimum has been found.  However,
C  this test is made before the test for IGO=4.
C
C      =7   A  small change (relative to the length of x) was noted
C  for  the  vector  x.   A full model problem step was taken.  The
C  condition for IGO=4 may also be satisfied, so that a minimum has
C  been  found.   However,  this  test  is made before the test for
C  IGO=4.
C
C      =8   More  than  ITMAX  iterations  were taken to obtain the
C  solution.   The  value obtained for x is suspect, although it is
C  the   best   set  of  x  values  that  occurred  in  the  entire
C  computation.   See  the  description  of  The  Option  Array for
C  directions  on  how  to  increase  this  value.   (Note that the
C  nominal  value  for ITMAX, 75, is sufficient to solve all of the
C  nonlinear test problems described in Ref. (2).)
C
C      =9-18     Errors  in the usage of the subprogram were noted.
C  The  exact condition will be noted using an error processor that
C  prints  an  informative  message  unless  this printing has been
C  suppressed.   A  minimum  value  has  not been found for x.  The
C  relation  between  IGO  and  the  error number are IGO=NERR + 8.
C  Here  NERR is the identifying number.  See below, Error Messages
C  for DQED().
C  The Option Array
C  --- ------ -----
C  Glossary of Items Modified by Options.  Those items with Nominal
C  Values listed can be changed.
C
C       Names    Nominal         Definition
C                Values
C       -----    -------         ----------
C       FC                       Current value of length of f(x).
C       FB                       Best value of length of f(x).
C       FL                       Value of length of f(x) at the
C                                previous step.
C       PV                       Predicted value of length of f(x),
C                                after the step is taken, using the
C                                approximating model.
C  The  quantity  'eps',  used  below,  is  the  machine  precision
C  parameter.   Its  value  is obtained by a call to the Bell Labs.
C  Port subprogram D1MACH(4).  It is machine dependent.
C                MIN(1.D-5,
C       TOLF     sqrt(eps))      Tolerance for stopping when
C                                FC .le. TOLF.
C                MIN(1.D-5,
C       TOLD     sqrt(eps))      Tolerance for stopping when
C                                change to x values has length
C                                .le. TOLD.
C                MIN(1.D-5,
C       TOLX     sqrt(eps))      Tolerance for stopping when
C                                change to x values has length
C                                .le. TOLX*length of x values.
C       TOLSNR    1.D-5          Tolerance used in stopping
C                                condition IGO=4.  Explained below.
C       TOLP      1.D-5          Tolerance used in stopping
C                                condition IGO=4.  Explained below.
C
C  (The  conditions  (abs(FB-PV).le.TOLSNR*FB  and  abs(FC-PV) .le.
C  TOLP*FB)   and  (ABS(FC-FL).le.TOLSNR*FB) together with  taking
C  a  full  model  step, must be satisfied before the condition  IGO=4
C  is returned.  Decreasing any of the values for  TOLF,  TOLD,  TOLX,
C  TOLSNR,  or  TOLP  will likely increase the number of iterations
C  required for convergence.)
C
C       COND       30.           Largest condition number to allow
C                                when solving for the quadratic
C                                model coefficients.  Increasing
C                                this value may result in more
C                                terms being used in the quadratic
C                                model.
C       TOLUSE   sqrt(eps)       A tolerance that is used to avoid
C                                values of x in the quadratic
C                                model's interpolation of previous
C                                points.  Decreasing this value may
C                                result in more terms being used in
C                                the quadratic model.
C        ITMAX     75            The number of iterations to take
C                                with the algorithm before giving
C                                up and noting it with the value
C                                IGO=8.
C        IPRINT     0            Control the level of printed
C                                output in the solver.  A value
C                                of IPRINT .gt. 0 will result in
C                                output of information about each
C                                iteration.  The output unit used
C                                is obtained using the Bell Labs.
C                                Port subprogram, i. e. I1MACH(2).
C        LEVEL      1            Error processor error level.  See
C                                the SLATEC library documentation
C                                for XERROR() for an explanation.
C        NTERMS     5            One more than the maximum number
C                                of terms used in the quadratic
C                                model.
C
C  IOPT(*) (Input)
C  -------
C  In  order  to  use the option array technique to change selected
C  data within a subprogram, it is necessary to understand how this
C  array  is  processed  within the software.  Let LP designate the
C  processing pointer that moves to positions of the IOPT(*) array.
C  Initially  LP=1,  and  as  each option is noted and changed, the
C  value  of  LP is updated.  The values of IOPT(LP) determine what
C  options get changed.  The amount that LP changes is known by the
C  software  to  be  equal to the value two except for two options.
C  These exceptional cases are the last option (=99) and the 'leap'
C  option  (=13)  which  advances LP by the value in IOPT(LP+1).  A
C  negative  value for IOPT(LP) means that this option is not to be
C  changed.   This aids the programmer in using options;  often the
C  code  for  using  an  option can be in the calling program but a
C  negative value of the option number avoids rewriting code.
C
C  Option Usage Example
C  ------ ----- -------
C  In  the  Fortran code fragment that follows, an example is given
C  where  we  change  the  value  of  TOLF and decrease the maximum
C  number  of  iterations  allowed  from  75  to 30.
C  In this example the dimensions of IOPT(*) and ROPT(*) must
C  satisfy:
C
C        DOUBLE PRECISION ROPT(01)
C        INTEGER IOPT(005)
C        .
C        .
C        .
C  C     SET THE OPTION TO CHANGE THE VALUE OF TOLF.
C
C        IOPT(01)=4
C
C  C     THE NEXT ENTRY POINTS TO THE PLACE IN ROPT(*) WHERE
C  C     THE NEW VALUE OF TOLF IS LOCATED.
C
C        IOPT(02)=1
C  C     THIS IS THE NEW VALUE OF TOLF.  THE SPECIFIC VALUE
C  C     1.D-9 IS USED HERE ONLY FOR ILLUSTRATION.
C
C        ROPT(01)=1.D-9
C
C  C     CHANGE THE NUMBER OF ITERATIONS.
C
C        IOPT(03)=2
C
C  C     THIS NEXT ENTRY IS THE NEW VALUE FOR THE MAXIMUM NUMBER OF
C  C     ITERATIONS.
C
C        IOPT(04)=30
C
C  C     THIS NEXT OPTION IS A SIGNAL THAT THERE ARE NO MORE
C  C     OPTIONS.
C
C        IOPT(05)=99
C        .
C        .
C        .
C        CALL DQED()
C        .
C        .
C        .
C  Option Values   Explanation
C  ------ ------   -----------
C     =99          There are no more options to change.
C                  Normally this is the first and only
C                  option that a user needs to specify,
C                  and it can be simply IOPT(01)=99.  The
C                  total dimension of IOPT(*) must be at
C                  least 17, however.  This can lead to a
C                  hard-to-find program bug if the dimension
C                  is too small.
C
C     = 1          Change the amount of printed output.
C                  The next value of IOPT(*) is the print
C                  level desired, IPRINT.  Any value of
C                  IPRINT .gt. 0 gives all the available
C                  output.
C
C     = 2          Change the value of ITMAX.  The next value
C                  of IOPT(*) is the value of ITMAX desired.
C
C     = 3          Pass prior determined bounds for the box
C                  containing the initial point.  This box is the
C                  trust region for the first move from the initial
C                  point.  The next entry in IOPT(*) points to
C                  the place in ROPT(*) where the NVARS values for
C                  the edges of the box are found.
C
C     = 4          Change the value of TOLF.  The next entry of
C                  IOPT(*) points to the place in ROPT(*) where the
C                  new value of TOLF is found.
C
C     = 5          Change the value of TOLX.  The next entry of
C                  IOPT(*) points to the place in ROPT(*) where the
C                  new value of TOLX is found.
C
C     = 6          Change the value of TOLD.  The next entry of
C                  IOPT(*) points to the place in ROPT(*) where the
C                  new value of TOLD is found.
C
C     = 7          Change the value of TOLSRN.  The next entry of
C                  IOPT(*) points to the place in ROPT(*) where the
C                  new value of TOLSNR is found.
C
C     = 8          Change the value of TOLP.  The next entry of
C                  IOPT(*) points to the place in ROPT(*) where the
C                  new value of TOLP is found.
C
C     = 9          Change the value of TOLUSE.  The next entry of
C                  IOPT(*) points to the place in ROPT(*) where the
C                  new value of TOLUSE is found.
C
C     =10          Change the value of COND.  The next entry of
C                  IOPT(*) points to the place in ROPT(*) where the
C                  new value of COND is found.
C
C     =11          Change the value of LEVEL.  The next entry of
C                  IOPT(*) is the new value of LEVEL.
C
C     =12          Pass an option array to the subprogram DQEDGN()
C                  used as the inner loop solver for the
C                  model problem.  The next entry of IOPT(*) is the
C                  starting location for the option array for
C                  DQEDGN() within the array IOPT(*).  Thus the
C                  option array for DQEDGN() must be a part of
C                  the array IOPT(*).
C
C     =13          Move (or leap) the processing pointer LP for the
C                  option array by the next value in IOPT(*).
C
C     =14          Change a logical flag that suppresses the
C                  use of the quadratic model in the inner
C                  loop.  Use the next value in IOPT(*) for
C                  this flag.  If this value = 1, then never
C                  use the quadratic model.  (Just use the
C                  linear model).  Otherwise, use the quadratic
C                  model when appropriate.  This option decreases
C                  the amount of scratch storage as well as the
C                  computing overhead required by the code package.
C                  A user may want to determine if the application
C                  really requires the use of the quadratic model.
C                  If it does not, then use this option to save
C                  both storage and computing time.
C
C     =15          Change, NTERMS,  the maximum number of array
C                  columns that can be used for saving quadratic
C                  model data.  (The value of NTERMS is one more
C                  than the maximum number of terms used.)  Each
C                  unit increase for NTERMS increases the required
C                  dimension of the array WORK(*) by 2*MEQUA+NVARS.
C                  Use the value in IOPT(LP+1) for the new value
C                  of NTERMS.  Decreasing this value to 2 (its
C                  minimum) decreases the amount of storage
C                  required by the code package.
C
C     =16          Change a logical flag so that 'reverse
C                  communication' is used instead of 'forward
C                  communication.'  Example EX01, listed below,
C                  uses 'forward communication.'  Example EX02,
C                  also listed below, uses 'reverse communication.'
C                  Use the next value in IOPT(*) for
C                  this flag.  If this value = 1, then
C                  use 'reverse communication.'  Otherwise,
C                  use 'forward communication.'  WARNING:  This
C                  usage may not work unless the operating system
C                  saves variables between subroutine calls to DQED.
C
C     =17          Do not allow the flag IGO to return with the
C                  value IGO=3.  This means that convergence will
C                  not be claimed unless a full model step is taken.
C                  Normal output values will then be IGO = 2,4,6 or 7.
C                  Use the next value in IOPT(*) for this flag.  If
C                  this value = 1, then force a full model step.
C                  Otherwise,  do not force a full model step if small
C                  steps are noted.
C
C  IWORK(*), WORK(*) (Input and Output)
C  ----------------
C  These  are  scratch arrays that the software uses for storage of
C  intermediate  results.   It  is  important  not  to  modify  the
C  contents of this storage during the computation.
C
C  The  array  locations  IWORK(1)  and  IWORK(2)  must contain the
C  actual  lengths  of  the  arrays WORK(*) and IWORK(*) before the
C  call to the subprogram.  These array entries are replaced by the
C  actual amount of storage required for each array.  If the amount
C  of  storage  for either array is too small, an informative error
C  message will be printed, and the value IGO=13 or 14 returned.
C
C  The  user may find it useful to let the subprogram DQED() return
C  the  amounts  of storage required for these arrays.  For example
C  set  IWORK(1)=1,  IWORK(2)=1.   The  subprogram will return with
C  IGO=13,     IWORK(1)=required    length    of    WORK(*),    and
C  IWORK(2)=required   length   of  IWORK(*).   (Appropriate  error
C  messages will normally be printed.)
C
C  3. Remarks on the Usage Examples
C     ------- -- --- ----- --------
C  The  following  complete  program units, EX01 and EX02, show how
C  one  can  use  the  nonlinear  solver  for  fitting  exponential
C  functions  to  given data.  These examples are calculations that
C  match  two  terms  of  an  exponential series to five given data
C  points.   There are some subtle points about exponential fitting
C  that   are  important  to  note.     First,  the  signs  of  the
C  exponential arguments are restricted to be nonpositive.
C  The size of the arguments should not be much larger than the start
C  of the time data (reciprocated).  This is the reason the lower
C  bounds are set a bit less than the reciprocal of the time value.
C  In many applications that require exponential modeling this is a
C  natural assumption.  The nonlinear solver allows these bounds
C  on  the arguments explicitly.  In addition, the coefficients are
C  constrained  to  be  nonnegative.   These  bounds  are harder to
C  justify.  The idea is to avoid the situation where a coefficient
C  is  very  large  and negative, and the corresponding exponential
C  argument is also large and negative.  The resulting contribution
C  to  the  series may be very small, but its presence is spurious.
C  Finally,  the  single  general  linear constraint that keeps the
C  arguments  separated  (by  0.05 in this example) is used for two
C  purposes.   First,  it naturally orders these values so that the
C  first  one  is  algebraically  largest.  Second, this constraint
C  moves the parameters from the local minimum corresponding to the
C  initial  values  used  in  the  examples.   This constraint also
C  retains  the  validity of the model function h(t) = w*exp(x*t) +
C  y*exp(z*t).  Namely, if the arguments are allowed to coalesce to
C  the  same value, then the model itself must change.  The form of
C  the model can become h(t)=(a+b*t)*exp(c*t) or h(t) = d*exp(e*t).
C  Either one could occur, and the choice is problem dependent.
C  Example 1  Using Forward Communication
C  ---------  ----- ------- -------------
C      PROGRAM EX01
C
CC     Illustrate the use of the Hanson-Krogh nonlinear least
CC     squares solver for fitting two exponentials to data.
CC
CC     The problem is to find the four variables x(1),...,x(4)
CC     that are in the model function
CC
CC          h(t) = x(1)*exp(x(2)*t) + x(3)*exp(x(4)*t)
CC     There are values of h(t) given at five values of t,
CC     t=0.05, 0.1, 0.4, 0.5, and 1.0.
CC     We also have problem constraints that x(2), x(4) .le. 0, x(1),
CC     x(3) .ge. 0, and a minimal separation of 0.05 between x(2) and
CC     x(4).  Nothing more about the values of the parameters is known
CC     except that x(2),x(4) are approximately .ge. 1/min t.
CC     Thus we have no further knowledge of their values.
CC     For that reason all of the initial values are set to zero.
CC
CC     Dimension for the nonlinear solver.
C      DOUBLE PRECISION FJ(6,5),BL(5),BU(5),X(4),ROPT(001),WA(640)
CC  EDIT on 950228-1300:
C      DOUBLE PRECISION RNORM
C      INTEGER IND(5),IOPT(24),IWA(084)
C
C      EXTERNAL DQEDEX
C
C      DATA LDFJ,LWA,LIWA/6,640,084/
C
C      MCON = 1
C      MEQUA = 5
C      NVARS = 4
CC     Define the constraints for variables.
C      BL(1) = 0.
C      BL(2) = -25.
C      BU(2) = 0.
C      BL(3) = 0.
C      BL(4) = -25.
C      BU(4) = 0.
CC     Define the constraining value (separation) for the arguments.
C      BL(5) = 0.05
CC     Define all of the constraint indicators.
C      IND(1) = 1
C      IND(2) = 3
C      IND(3) = 1
C      IND(4) = 3
C      IND(5) = 1
CC     Define the initial values of the variables.
CC     We don't know anything more, so all variables are set zero.
C      DO 10 J = 1,NVARS
C         X(J) = 0.D0
C   10 CONTINUE
CC     Tell how much storage we gave the solver.
C      IWA(1) = LWA
C      IWA(2) = LIWA
CC     No additional options are in use.
C      IOPT(01) = 99
C      CALL DQED(DQEDEX,MEQUA,NVARS,MCON,IND,BL,BU,X,FJ,LDFJ,RNORM,IGO,
C     .          IOPT,ROPT,IWA,WA)
C      NOUT = 6
C      WRITE (NOUT,9001) (X(J),J=1,NVARS)
C      WRITE (NOUT,9011) RNORM
C      WRITE (NOUT,9021) IGO
C
C      STOP
C
C 9001 FORMAT (' MODEL IS H(T) = X(1)*EXP(-T*X(2)) + X(3)*EXP(T*X(4))',/,
C     .  ' X(1),X(2),X(3),X(4) = ',/,4F12.6)
C 9011 FORMAT (' RESIDUAL AFTER THE FIT = ',1PD12.4)
C 9021 FORMAT (' OUTPUT FLAG FROM SOLVER =',17X,I6)
C      END
C      SUBROUTINE DQEDEX(X,FJ,LDFJ,IGO,IOPT,ROPT)
CC     This is the subprogram for evaluating the functions
CC     and derivatives for the nonlinear solver, DQED.
CC
CC     The user problem has MCON constraint functions,
CC     MEQUA least squares equations, and involves NVARS
CC     unknown variables.
CC
CC     When this subprogram is entered, the general (near)
CC     linear constraint partial derivatives, the derivatives
CC     for the least squares equations, and the associated
CC     function values are placed into the array FJ(*,*).
CC     All partials and functions are evaluated at the point
CC     in X(*).  Then the subprogram returns to the calling
CC     program unit. Typically one could do the following
CC     steps:
CC
CC     step 1. Place the partials of the i-th constraint
CC             function with respect to variable j in the
CC             array FJ(i,j), i=1,...,MCON, j=1,...,NVARS.
CC     step 2. Place the values of the i-th constraint
CC             equation into FJ(i,NVARS+1).
CC     step 3. Place the partials of the i-th least squares
CC             equation with respect to variable j in the
CC             array FJ(MCON+i,j), i=1,...,MEQUA,
CC             j=1,...,NVARS.
CC     step 4. Place the value of the i-th least squares
CC             equation into FJ(MCON+i,NVARS+1).
CC     step 5. Return to the calling program unit.
C      DOUBLE PRECISION FJ(LDFJ,*),X(*),ROPT(*)
C      DOUBLE PRECISION T(5),F(5)
C      INTEGER IOPT(*)
C
C      DATA T/0.05,0.10,0.40,0.50,1.00/
C      DATA F/2.206D+00,1.994D+00,1.350D+00,1.216D+00,.7358D0/
C
C      DATA MCON,MEQUA,NVARS/1,5,4/
C
CC     Define the derivatives of the constraint with respect to the x(j).
C      FJ(1,1) = 0.D0
C      FJ(1,2) = 1.D0
C      FJ(1,3) = 0.D0
C      FJ(1,4) = -1.D0
CC     Define the value of this constraint.
C      FJ(1,5) = X(2) - X(4)
CC     Define the derivatives and residuals for the data model.
C      DO 10 I = 1,MEQUA
C         E1 = EXP(X(2)*T(I))
C         E2 = EXP(X(4)*T(I))
C         FJ(MCON+I,1) = E1
C         FJ(MCON+I,2) = X(1)*T(I)*E1
C         FJ(MCON+I,3) = E2
C         FJ(MCON+I,4) = X(3)*T(I)*E2
C         FJ(MCON+I,5) = X(1)*E1 + X(3)*E2 - F(I)
C   10 CONTINUE
C      RETURN
C      END
C  Output from Example 1 Program
C  ------ ---- --------- -------
C
C   MODEL IS H(T) = X(1)*EXP(-T*X(2)) + X(3)*EXP(T*X(4))
C  X(1),X(2),X(3),X(4) =
C      1.999475    -.999801     .500057   -9.953988
C   RESIDUAL AFTER THE FIT =   4.2408D-04
C   OUTPUT FLAG FROM SOLVER =                      4
C
C
C  Example 2  Using Reverse Communication
C  ---------  ----- ------- -------------
C      PROGRAM EX02
C
CC     Illustrate the use of the Hanson-Krogh nonlinear least
CC     squares solver for fitting two exponentials to data.
CC
CC     The problem is to find the four variables x(1),...,x(4)
CC     that are in the model function
CC
CC          h(t) = x(1)*exp(x(2)*t) + x(3)*exp(x(4)*t)
CC     There are values of h(t) given at five values of t,
CC     t=0.05, 0.1, 0.4, 0.5, and 1.0.
CC     We also have problem constraints that x(2), x(4) .le. 0, x(1),
CC     x(3) .ge. 0, and a minimal separation of 0.05 between x(2) and
CC     x(4).  Nothing more about the values of the parameters is known
CC     except that x(2),x(4) are approximately .ge. 1/min t.
CC     Thus we have no further knowledge of their values.
CC     For that reason all of the initial values are set to zero.
CC
CC     Dimension for the nonlinear solver.
C      DOUBLE PRECISION FJ(6,5),BL(5),BU(5),X(4),ROPT(001),WA(640)
CC  EDIT on 950228-1300:
C      DOUBLE PRECISION RNORM
C      INTEGER IND(5),IOPT(24),IWA(084)
C      DOUBLE PRECISION T(5),F(5)
C
C      EXTERNAL DQEDEV
C
C      DATA LDFJ,LWA,LIWA/6,640,084/
C
C      DATA T/0.05,0.10,0.40,0.50,1.00/
C      DATA F/2.206D+00,1.994D+00,1.350D+00,1.216D+00,.7358D0/
C
C      MCON = 1
C      MEQUA = 5
C      NVARS = 4
CC     Define the constraints for variables.
C      BL(1) = 0.
C      BL(2) = -25.
C      BU(2) = 0.
C      BL(3) = 0.
C      BL(4) = -25.
C      BU(4) = 0.
CC     Define the constraining value (separation) for the arguments.
C      BL(5) = 0.05
CC     Define all of the constraint indicators.
C      IND(1) = 1
C      IND(2) = 3
C      IND(3) = 1
C      IND(4) = 3
C      IND(5) = 1
CC     Define the initial values of the variables.
CC     We don't know anything at all, so all variables are set zero.
C      DO 10 J = 1,NVARS
C         X(J) = 0.D0
C   10 CONTINUE
CC     Tell how much storage we gave the solver.
C      IWA(1) = LWA
C      IWA(2) = LIWA
C      NITERS = 0
CC     TELL HOW MUCH STORAGE WE GAVE THE SOLVER.
C      IWA(1) = LWA
C      IWA(2) = LIWA
CC     USE REVERSE COMMUMICATION TO EVALUATE THE DERIVATIVES.
C      IOPT(01)=16
C      IOPT(02)=1
CC     NO MORE OPTIONS.
C      IOPT(03) = 99
C   20 CONTINUE
C      CALL DQED(DQEDEV,MEQUA,NVARS,MCON,IND,BL,BU,X,FJ,LDFJ,RNORM,
C     .IGO,IOPT, ROPT,IWA,WA)
C      IF (IGO.GT.1) GO TO 40
CC     COUNT FUNCTION EVALUATIONS.
C      NITERS = NITERS + 1
CC     DEFINE THE DERIVATIVES OF THE CONSTRAINT WITH RESPECT TO THE X(J).
C      FJ(1,1) = 0.D0
C      FJ(1,2) = 1.D0
C      FJ(1,3) = 0.D0
C      FJ(1,4) = -1.D0
CC     DEFINE THE VALUE OF THIS CONSTRAINT.
C      FJ(1,5) = X(2) - X(4)
CC     DEFINE THE DERIVATIVES AND RESIDUALS FOR THE DATA MODEL.
C      DO 30 I = 1,MEQUA
C          E1 = EXP(X(2)*T(I))
C          E2 = EXP(X(4)*T(I))
C          FJ(MCON+I,1) = E1
C          FJ(MCON+I,2) = X(1)*T(I)*E1
C          FJ(MCON+I,3) = E2
C          FJ(MCON+I,4) = X(3)*T(I)*E2
C          FJ(MCON+I,5) = X(1)*E1 + X(3)*E2 - F(I)
C   30 CONTINUE
C      GO TO 20
C
C   40 CONTINUE
C      NOUT = 6
C      WRITE (NOUT,9001) (X(J),J=1,NVARS)
C      WRITE (NOUT,9011) RNORM
C      WRITE (NOUT,9021) NITERS, IGO
C
C 9001 FORMAT (' MODEL IS H(T) = X(1)*EXP(-T*X(2)) + X(3)*EXP(T*X(4))',/,
C     . ' X(1),X(2),X(3),X(4) = ',/,4F12.6)
C 9011 FORMAT (' RESIDUAL AFTER THE FIT = ',1PD12.4)
C 9021 FORMAT (' NUMBER OF EVALUATIONS OF PARAMETER MODEL =',I6,/,
C     .          ' OUTPUT FLAG FROM SOLVER =',17X,I6)
C      STOP
C      END
C  Output from Example 2 Program
C  ------ ---- --------- -------
C
C  MODEL IS H(T) = X(1)*EXP(-T*X(2)) + X(3)*EXP(T*X(4))
C  X(1),X(2),X(3),X(4) =
C      1.999475    -.999801     .500057   -9.953988
C   RESIDUAL AFTER THE FIT =   4.2408D-04
C   NUMBER OF EVALUATIONS OF PARAMETER MODEL =    14
C   OUTPUT FLAG FROM SOLVER =                      4
C
C  4. Error Messages for DQED()
C     --------------------------
C   'DQED. VALUE OF MEQUA=NO. OF EQUAS. MUST .GT.0. NOW = (I1).'
C   NERR = 01
C   IGO=9
C
C  'DQED. VALUE OF NVARS=NO. OF EQUAS. MUST .GT.0. NOW = (I1).'
C   NERR = 02
C   IGO=10
C
C  'DQED. VALUE OF MCON=NO. OF EQUAS. MUST .GE.0. NOW = (I1).'
C   NERR = 03
C   IGO=11
C
C  'DQED. INVALID OPTION PROCESSED. I1=IOPT(*) ENTRY.   I2=IOPT(I1).'
C   NERR = 04
C   IGO=12
C
C  'DQED. WA(*) STORAGE SHORT. I1=AMOUNT NEEDED. I2=AMOUNT GIVEN.'
C   NERR = 05
C   IGO=13
C
C  'DQED. IWA(*) STORAGE SHORT. I1=AMOUNT NEEDED. I2=AMOUNT   GIVEN.'
C   NERR = 06
C   IGO=14
C
C  'DQEDMN. INVALID OPTION PROCESSED. I1=IOPT(*) ENTRY.   I2=IOPT(I1).
C
C   NERR=07
C   IGO=15
C
C  'DQEDIP. INVALID OPTION PROCESSED. I1=IOPT(*) ENTRY.  I2=IOPT(I1).'
C
C   NERR=08
C   IGO=16
C
C  'DQED. THE EVALUATOR PROGRAM DQEDEV MUST BE WRITTEN BY THE  USER.'
C   NERR=09
C   IGO=17
C
C  'DQED. BOUND INDICATORS MUST BE 1-4. NOW I1=J, I2=IND(I1).'
C   NERR=10
C   IGO=18
C
C  5. References
C     ----------
C***REFERENCES
C Dongarra, J. J., Bunch, J. R., Moler, C. B, Stewart, G. W.,
C LINPACK User's Guide, Soc. Indust. and Appl. Math, Phil.,
C  PA, (1979).

C Hanson, R. J., "Least Squares with Bounds and Linear
C Constraints," SIAM J. Sci. Stat. Comput., vol. 7, no. 3, July,
C (1986), p. 826-834.

C Schnabel, R. B., Frank, P. D, "Tensor Methods for Nonlinear
C Equations," SIAM J. Num. Anal., vol. 21, no. 5, Oct., (1984),
C p. 815-843.
C***END PROLOGUE  DQED
C     REVISED 870204-1100
C     REVISED YYMMDD-HHMM
      DOUBLE PRECISION BL(*),BU(*),X(*),FJAC(LDFJAC,*)
      DOUBLE PRECISION ROPT(*),WA(*)
      DOUBLE PRECISION FNORM
      REAL RDUM
      INTEGER IND(*),IOPT(*),IWA(*)
      LOGICAL NOQUAD
      CHARACTER XMESS*128
      EXTERNAL PDA_DQEDEV
C--PROCEDURES--
C -NAME------TYPE--------ARGS------CLASS-----
C
C  DQEDEV                   6      EXTERNAL
C  DQEDMN                  35      SUBROUTINE
C  CHRCNT                   2      SUBROUTINE
C  XERRWV                  10      SUBROUTINE
C  DQED:
C Name      Memory Status  Type     Argument   Uses and comments.
C                                    Status
C ----      -------------  ----     --------   ------------------
C BL         DUMMY-ARG     REAL      ADJ-ARY Lower bounds
C BU         DUMMY-ARG     REAL      ADJ-ARY Upper bounds
C FJAC       DUMMY-ARG     REAL      ADJ-ARY Jacobian array
C FNORM      DUMMY-ARG     REAL              Norm at solution
C I          /S$A$V$E/ SAV INTEGER           Dummy loop variable
C IDUM       /S$A$V$E/ SAV INTEGER           Dummy for error pack
C IFLAG      /S$A$V$E/ SAV INTEGER           Gate for reverse comm
C IGO        DUMMY-ARG     INTEGER           Directs user action
C IGOOK      /S$A$V$E/ SAV INTEGER           Internal gate for errors
C IIWAAV     /S$A$V$E/ SAV INTEGER           Length claimed for IWA
C IND        DUMMY-ARG     INTEGER   ADJ-ARY Bound indicators
C IOPT       DUMMY-ARG     INTEGER   ADJ-ARY Option array
C IWA        DUMMY-ARG     INTEGER   ADJ-ARY Work array
C IWAAV      /S$A$V$E/ SAV INTEGER           Length claime for WA
C J          /S$A$V$E/ SAV INTEGER           Dummy loop variable
C MILAST     /S$A$V$E/ SAV INTEGER           Last integer in IWA
C MIND       /S$A$V$E/ SAV INTEGER           Point to start IND
C MINDB      /S$A$V$E/ SAV INTEGER           Point to start INDB
C MPJ        /S$A$V$E/ SAV INTEGER           Point to start PJ
C MQC        /S$A$V$E/ SAV INTEGER           Point to start QC
C MUT        /S$A$V$E/ SAV INTEGER           Point to start UT
C MWA        /S$A$V$E/ SAV INTEGER           Point to start WA
C MWJ        /S$A$V$E/ SAV INTEGER           Point to start WJ
C MWLAST     /S$A$V$E/ SAV INTEGER           Last value in WA
C MXB        /S$A$V$E/ SAV INTEGER           Point to start XB
C MXP        /S$A$V$E/ SAV INTEGER           Point to start XP
C MZP        /S$A$V$E/ SAV INTEGER           Point to start ZP
C NALL       /S$A$V$E/ SAV INTEGER           Sum of dimensions
C KP         /S$A$V$E/ SAV INTEGER           Dummy option loop pointer
C LDFJAC     DUMMY-ARG     INTEGER           Row dimension of FJAC
C LEVEL      /S$A$V$E/ SAV INTEGER           Error processor status
C LP         /S$A$V$E/ SAV INTEGER           Dummy option loop pointer
C LPDIFF     /S$A$V$E/ SAV INTEGER           Dummy option loop diff
C MB         /S$A$V$E/ SAV INTEGER           Point to start B
C MBB        /S$A$V$E/ SAV INTEGER           Point to start BB
C MBLB       /S$A$V$E/ SAV INTEGER           Point to start BLB
C MBUB       /S$A$V$E/ SAV INTEGER           Point to start BUB
C MCON       DUMMY-ARG     INTEGER           Number of constraints
C MDX        /S$A$V$E/ SAV INTEGER           Point to start DX
C MDXL       /S$A$V$E/ SAV INTEGER           Point to start DXL
C MEQUA      DUMMY-ARG     INTEGER           Numer of least squares eqs
C MGR        /S$A$V$E/ SAV INTEGER           Point to start GR
C NERR       /S$A$V$E/ SAV INTEGER           Error processor number
C NMESS      /S$A$V$E/ SAV INTEGER           Length of error message
C NOQUAD     /S$A$V$E/ SAV LOGICAL           Flag, suppress quad model
C NPMAX      /S$A$V$E/ SAV INTEGER           Max number of quad terms
C NVARS      DUMMY-ARG     INTEGER           Number of unknowns
C RDUM       /S$A$V$E/ SAV REAL              Dummy variable, error proc
C ROPT       DUMMY-ARG     REAL      ADJ-ARY Option data
C WA         DUMMY-ARG     REAL      ADJ-ARY Working array
C X          DUMMY-ARG     REAL      ADJ-ARY Values of the variables
C XMESS      /S$A$V$E/ SAV CHAR*128          Hold error message
C
      DATA IFLAG/0/
C***FIRST EXECUTABLE STATEMENT  DQED
      ASSIGN 40 TO IGOOK
      IF (IFLAG.EQ.0) THEN
          NOQUAD = .FALSE.
          LEVEL = 1
          IF (MEQUA.LE.0) THEN
              XMESS =
     .      'DQED. VALUE OF MEQUA=NO. OF EQUAS. MUST .GT.0. NOW = (I1).'
              CALL PDA_CHRCNT(XMESS,NMESS)
              NERR = 01
              IGO = 9
              CALL PDA_XERRWV(XMESS,NMESS,NERR,LEVEL,1,MEQUA,IDUM,0,
     .                        RDUM, RDUM)
              ASSIGN 50 TO IGOOK
*
          END IF
*
          IF (NVARS.LE.0) THEN
              XMESS =
     .      'DQED. VALUE OF NVARS=NO. OF EQUAS. MUST .GT.0. NOW = (I1).'
              CALL PDA_CHRCNT(XMESS,NMESS)
              NERR = 02
              IGO = 10
              CALL PDA_XERRWV(XMESS,NMESS,NERR,LEVEL,1,NVARS,IDUM,0,
     .                        RDUM, RDUM)
              ASSIGN 50 TO IGOOK
*
          END IF
*
          IF (MCON.LT.0) THEN
              XMESS =
     .       'DQED. VALUE OF MCON=NO. OF EQUAS. MUST .GE.0. NOW = (I1).'
              CALL PDA_CHRCNT(XMESS,NMESS)
              NERR = 03
              IGO = 11
              CALL PDA_XERRWV(XMESS,NMESS,NERR,LEVEL,1,MCON,IDUM,0,RDUM,
     .                    RDUM)
              ASSIGN 50 TO IGOOK
*
          END IF
*
          DO 10 J = 1,NVARS + MCON
             I = IND(J)
             GO TO (10,10,10,10),I
*
             XMESS =
     .      'DQED. BOUND INDICATORS MUST BE 1-4. NOW I1=J, I2= IND(I1).'
             CALL PDA_CHRCNT(XMESS,NMESS)
             NERR = 10
             IGO = 18
             CALL PDA_XERRWV(XMESS,NMESS,NERR,LEVEL,2,J,IND(J),0,RDUM,
     .                       RDUM)
             ASSIGN 50 TO IGOOK
*
   10     CONTINUE
          NPMAX = 05
C     LOOK THROUGH THE OPTION ARRAY FOR A CHANGE TO NPMAX,
C     THE AMOUNT OF ARRAY STORAGE USED FOR THE QUADRATIC PARAMETERS.
          LP = 1
          LPDIFF = 0
   20     CONTINUE
          LP = LP + LPDIFF
          LPDIFF = 2
          KP = IOPT(LP)
          JP = ABS(KP)
          IF (JP.EQ.99) THEN
              IF (KP.GT.0) GO TO 30
          END IF
C     THIS IS THE ONLY OPTION WHERE THE PROCESSING POINTER
C     MUST BE CHANGED FROM THE VALUE 2.
          IF (JP.EQ.13) LPDIFF = IOPT(LP+1)
C     FOUND A CHANGE TO THE ARRAY SIZE FOR THE QUADRATIC MODEL.
          IF (JP.EQ.15) THEN
              IF (KP.GT.0) NPMAX = IOPT(LP+1)
          END IF
C     SEE IF THE QUADRATIC MODEL IS SUPPRESSED.
C     THIS REQUIRES LESS STORAGE IN THE USER PROGRAM.
          IF (JP.EQ.14) THEN
              IF (KP.GT.0) NOQUAD = IOPT(LP+1) .EQ. 1
          END IF
*
          IF (JP.LT.1 .OR. JP.GT.17) THEN
C     SAW AN OPTION (OR GARBAGE) THAT IS NOT ON THE LIST.
              XMESS =
     . 'DQED. INVALID OPTION PROOCESSED. I1=IOPT(*) ENTRY. I2=IOPT(I1).'
              NERR = 04
              IGO = 12
              CALL PDA_CHRCNT(XMESS,NMESS)
              CALL PDA_XERRWV(XMESS,NMESS,NERR,LEVEL,2,LP,IOPT(LP),0,
     .                        RDUM, RDUM)
              ASSIGN 50 TO IGOOK
*
          END IF
*
          GO TO 20
*
   30     CONTINUE
      END IF
*
      IF (NOQUAD) THEN
          NALL = MCON + NVARS + 2
*
      ELSE
          NALL = MCON + 2*NVARS + NPMAX + 1
      END IF
C
C     COMPUTE POINTERS INTO WORK SPACE FOR VARIOUS ARRAYS
C     REQUIRED IN MAIN PROGRAMS.
      MDX = 1
      MXB = MDX + NALL + 2
      IF(MCON.GT.0)MXB=MXB+NALL+2
      MB = MXB + NVARS
      MBB = MB + NVARS
      MBLB = MBB + NVARS
      IF(MCON.GT.0)MBLB=MBLB+NALL
      MBUB = MBLB + NALL
      IF(MCON.GT.0)MBUB=MBUB+NALL
      MZP = MBUB + NALL
      MXP = MZP + MEQUA*NPMAX
      MQC = MXP + NVARS*NPMAX
      MWJ = MQC + MAX(MEQUA,NVARS)*NPMAX
      MPJ = MWJ + (NALL)* (NALL+1)
      MGR = MPJ + NVARS + 1
      MDXL = MGR + NVARS
      MWA = MDXL + NVARS + NVARS
      MWLAST = MWA + 9* (MCON+1) + 13* (2*NVARS+NPMAX+1)
C
      MINDB = 3
      MIND = MINDB + NALL + NVARS
      MILAST = MIND + 3* (MCON+1) + 4* (2*NVARS+NPMAX+1)
C     CHECK LENGTHS OF ARRAYS ONCE PER PROBLEM.
      IF (IFLAG.EQ.0) THEN
          IWAAV = IWA(1)
          IIWAAV = IWA(2)
C     RETURN THE ACTUAL AMOUNTS OF STORAGE REQD. FOR WA(*), IWA(*).
          IWA(1) = MWLAST
          IWA(2) = MILAST
          IF (IWAAV.LT.MWLAST) THEN
              XMESS =
     .   'DQED. WA(*) STORAGE SHORT. I1=AMOUNT NEEDED. I2=AMOUNT GIVEN.'
              NERR = 05
              IGO = 13
              CALL PDA_CHRCNT(XMESS,NMESS)
              CALL PDA_XERRWV(XMESS,NMESS,NERR,LEVEL,2,MWLAST,IWAAV,0,
     .                        RDUM,RDUM)
              ASSIGN 50 TO IGOOK
*
          END IF
*
          IF (IIWAAV.LT.MILAST) THEN
              XMESS =
     .  'DQED. IWA(*) STORAGE SHORT. I1=AMOUNT NEEDED. I2=AMOUNT GIVEN.'
              NERR = 06
              IGO = 14
              CALL PDA_CHRCNT(XMESS,NMESS)
              CALL PDA_XERRWV(XMESS,NMESS,NERR,LEVEL,2,MILAST,IIWAAV,0,
     .                        RDUM,RDUM)
              ASSIGN 50 TO IGOOK
*
          END IF
*
          IFLAG = 1
      END IF
*
      GO TO IGOOK
*
   40 CONTINUE
*
      CALL PDA_DQEDMN(PDA_DQEDEV,MEQUA,NVARS,MCON,IND,BL,BU,X,FJAC,
     .                LDFJAC,FNORM,IGO,IOPT,ROPT,IWA(MIND),WA(MWA),
     .                WA(MDX),WA(MXB),WA(MB),WA(MBB),WA(MBLB),WA(MBUB),
     .                IWA(MINDB),NPMAX,WA(MZP),WA(MXP),WA(MQC),
     .                MAX(MEQUA,NVARS),WA(MPJ), WA(MWJ),NALL,WA(MGR),
     .                WA(MDXL))
   50 CONTINUE
      IF (IGO.GT.1) IFLAG = 0
      RETURN
C     TOTAL WORKING STORAGE (FOR DQEDGN) IN WA(*)=
C          9*NALL + 4*NVARS = 9*MCON + 13*NVARS.
C     TOTAL WORKING STORAGE (FOR DQEDGN) IN IWA(*)=
C          3*NALL + NV      = 3*MCON +4*NVARS.
C     IN THE ABOVE FORMULA, NVARS (FOR DQEDGN) IS .LE. 3*NVARS
C     IN TERMS OF THE USER'S VARIABLES.
      END
