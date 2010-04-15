      program testsumsl

*+
*  Name:
*     Sumsl

*  Purpose:
*     Tests the pda_sumsl minimisation routine.

*  Description:
*     We create a 3D data array using the following model:
*
*     array(i,j,k) = ( i - X1 )*( j/X2 - X3 )*( k/X4 - X5 )
*
*     where the 5 parameters X1 to X5 are set to (1, 2, 3, 4, 5). We then
*     use PDA_SUMSL to fit the same 5 parameter model to the data array,
*     using the objective function:

*     f( X ) = SUM( ( array - model( X ) )**2 ) over all i,j,k

*  Notes:
*     - The C interface to PDA_SUMSL is tested by SumslC.c

*-
      implicit none

*  External function for calculating the objective function
      external calcf

*  External function for calculating the objective function gradient
      external calcg

*  External function for evaluating the model value.
      external model
      double precision model

*  The number of variables in the fit
      integer n
      parameter ( n = 5 )

*  The length of "V"
      integer lv
      parameter ( lv = 71+n*(n+15)/2 )

*  The dimensions of the data array
      integer nx, ny, nz
      parameter ( nx = 20,
     :            ny = 20,
     :            nz = 20 )

*  Local Variables:
      integer i, j, k           ! Axis values
      integer dims(3)           ! Dimensions of data array
      integer iv(60)            ! Work array
      integer ivec              ! 1D vector index
      integer nf                ! Status flag
      double precision array( nx*ny*nz ) ! Data array
      double precision v( lv )  ! Work array
      double precision d( n )   ! Scale factors for the N free variables
      double precision x( n )   ! Fitted parameter values

*  Initial scale factors for the free parameters
      data d / 1.0, 1.0, 1.0, 1.0, 1.0 /

*  Data array dimensions:
      data dims / nx, ny, nz /

*  Set up the data array
      x( 1 ) = 1
      x( 2 ) = 2
      x( 3 ) = 3
      x( 4 ) = 4
      x( 5 ) = 5

      nf = 1
      ivec = 0
      do i = 1, dims( 1 )
         do j = 1, dims( 2 )
            do k = 1, dims( 3 )
               ivec = ivec + 1
               array( ivec ) = model( 0, i, j, k, x, nf )
            end do
         end do
      end do

*  Set an initial guess at the solution
      x( 1 ) = 1.5
      x( 2 ) = 1.5
      x( 3 ) = 2.5
      x( 4 ) = 2.5
      x( 5 ) = 3.5

*  Find the solution which minimises the objective function.
      iv(1)=0
      call pda_sumsl( n, d, x, calcf, calcg, iv, 60, lv, v, dims, array,
     :                model )

*  Test results
      if( abs( x(1) - 1.0 ) .gt. 1.0D-5 .OR.
     :    abs( x(2) - 1.9518948 ) .gt. 1.0D-5 .OR.
     :    abs( x(3) - 3.07393615 ) .gt. 1.0D-5 .OR.
     :    abs( x(4) - 4.09858153 ) .gt. 1.0D-5 .OR.
     :    abs( x(5) - 4.87973701 ) .gt. 1.0D-5 ) then
         write(*,*) 'Sumsl: Test of PDA_SUMSL failed'
      else
         write(*,*) 'Sumsl: Test of PDA_SUMSL passed'
      end if

      end



      subroutine calcf( n, x, nf, f, dims, array, model )
      implicit none
      integer n, nf, dims(*), i, j, k, ivec
      double precision x(n), array(*), f,model, m, r
      external model

      f = 0.0
      ivec = 0
      do i = 1, dims( 1 )
         do j = 1, dims( 2 )
            do k = 1, dims( 3 )
               ivec = ivec + 1
               m = model( 0, i, j, k, x, nf )
               r = array( ivec ) - model( 0, i, j, k, x, nf )
               f = f + r**2
            end do
         end do
      end do


      end

      subroutine calcg( n, x, nf, g, dims, array, model )
      implicit none
      integer n, nf, dims(*), i, j, k, ivec
      double precision x(n), array(*), g(n), res, model
      external model

      g( 1 ) = 0.0
      g( 2 ) = 0.0
      g( 3 ) = 0.0
      g( 4 ) = 0.0
      g( 5 ) = 0.0
      ivec = 0
      do i = 1, dims( 1 )
         do j = 1, dims( 2 )
            do k = 1, dims( 3 )
               ivec = ivec + 1
               res = array( ivec ) - model( 0, i, j, k, x, nf )

               g(1) = g(1) + res*model( 1, i, j, k, x, nf )
               g(2) = g(2) + res*model( 2, i, j, k, x, nf )
               g(3) = g(3) + res*model( 3, i, j, k, x, nf )
               g(4) = g(4) + res*model( 4, i, j, k, x, nf )
               g(5) = g(5) + res*model( 5, i, j, k, x, nf )

            end do
         end do
      end do

      g(1) = -2*g(1)
      g(2) = -2*g(2)
      g(3) = -2*g(3)
      g(4) = -2*g(4)
      g(5) = -2*g(5)

      end


      double precision function model( axis, i, j, k, x, nf )
      implicit none
      integer axis, i, j, k, nf
      double precision x( 5 ), a, b, c


      if( x( 2 ) .eq. 0.0 .or. x( 4 ) .eq. 0.0 ) then
         nf = 0

      else
         a = dble( i ) - x( 1 )
         b = dble( j )/x( 2 ) - x( 3 )
         c = dble( k )/x( 4 ) - x( 5 )

         if( axis .eq. 0 ) then
            model = a*b*c

         else if( axis .eq. 1 ) then
            model = -b*c

         else if( axis .eq. 2 ) then
            model = -a*c*j/( x(2) * x(2) )

         else if( axis .eq. 3 ) then
            model = -a*c

         else if( axis .eq. 4 ) then
            model = -a*b*k/( x(4) * x(4) )

         else if( axis .eq. 5 ) then
            model = -a*b

         else
            nf = 0
         end if

      end if

      if( nf .eq. 0 ) model = 0.0

      end


