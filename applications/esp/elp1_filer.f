      SUBROUTINE ELP1_FILER(FIOID,BACK,RLIM,INDF,NGALS,
     :     XC,YC,BACKS,RLIMS,STATUS)
*+
*  Name:
*     ELP1_FILER
*
*  Purpose:
*     Opens a user specified text file and reads from it a list of co-ordinates
*     indicating the locations where galaxies may exist in the image. Each of
*     these is to be profiled.  The file may also optionally specify
*     a local background and a profiling radius for each galaxy.
*     
*  Language:
*     Starlink Fortran 77
*
*  Invocation:
*      CALL ELP1_FILER(FIOID,BACK,RLIM,INDF,NGALS,XC,YC,BACKS,RLIMS,STATUS)    
*
*  Description:
*     Reads in a file of position information from an open file,
*     ignoring blank lines and lines starting with # or !.
*
*     The file format consists of a sequence of lines with 2, 3, or 4
*     numbers on each.  The first two are coordinates, the third is a
*     local background value, and the fourth is a profiling radius.  If
*     the background value is negative, it is ignored (thus allowing you
*     to specify a profiling radius without specifying a background value).
*
*     The first two are taken as representing x and y co-ordinates on 
*     an image in the Current co-ordinates of the WCS component.
*     esp1_farr converts these using esp1_s2pr, which checks that they
*     lie within the bounds of the image.
*
*  Arguments:               
*     fioid = integer (Given)
*        FIO identifier for the input file.
*     back = real (Given)
*        The image global background value. Units counts.
*     rlim = real (Given)
*        The default rlim value.
*     indf = integer (Given)
*        NDF identifier for the image.
*     ngals = integer (Returned)
*        Number of galaxies to be profiled.
*     xc(elp__ngals) = real (Returned)
*        X co-ordinates (for galaxies) obtained from the text file.
*     yc(elp__ngals) = real (Returned)
*        Y co-ordinates (for galaxies) obtained from the text file.
*     backs(elp__ngals) = real (Returned)
*        The local background value at each of the co-ordinates.
*        Units counts.
*     rlims(elp__ngals) = real (Returned)
*        Fitting limits for galaxies obtained from the text file, or defaulted
*        from `rlim' parameter.
*     status = integer (Given and Returned)
*        The global status.
*
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     NG: Norman Gray (Starlink, Glasgow)
*
*  History:
*     9-JUL-1993 (GJP)
*     (Original version)
*     26-OCT-1999 (MBT):
*     Modified to deal with COSYS=C.
*     8-NOV-1999 (MBT):
*     Removed COSYS altogether (use WCS instead).
*     21-NOV-1999 (NG):
*     Almost completely rewritten, to remove parsing code, and instead use
*     the much more general esp1_farr routine.
*
*  RCS Id:
*     $Id$
*
*  Bugs:
*     None known.
*
*-

*   Type Definitions:
      implicit none             ! No implicit typing
                                                                        
*  Global Constants:
      include 'SAE_PAR'         ! Standard SAE constants
      include 'FIO_ERR'         ! FIO error definitions
      include 'elp_par'         ! ELLPRO constants
      include 'NDF_PAR'         ! NDF public constants

*  Arguments Given:                              
      integer fioid             ! FIO identifier for the input file
      integer indf              ! NDF identifier for image
      real back                 ! Global background count value
      real rlim                 ! Default fitting limit

*  Arguments returned:
      integer ngals             ! The number of galaxies to be profiled
      real backs(elp__ngals)    ! The local background values
      real xc(elp__ngals)       ! X co-ordinates of the galaxy positions
                                ! found from the file
      real yc(elp__ngals)       ! Y co-ordinates of the galaxy positions
                                ! found from the file
      real rlims(elp__ngals)    ! Fitting limits of the galaxies found
                                ! from the file

*  Status:     
      integer status            ! Global status

*  Local variables:
      integer i                 ! A loop counter
      real pos(elp__ngals,4)    ! Positions read from file
      integer poslen(elp__ngals) ! Number of numbers read
      integer ncolmax, ncolmin  ! Max and min cols read
*.
      
*   Check the inherited global status.
      if (status.ne.sai__ok) return   

*   Call the routine which does all the work      
      call esp1_farr (fioid, indf, elp__ngals, 4, pos, poslen, 
     :     ncolmax, ncolmin, ngals, status)

      if (ncolmin .lt. 2) then
*      At least one line had fewer than two numbers on it.
*      Indicate that the file was badly formatted
         status = sai__error
         call err_rep (' ','Bad file format.',status)
      else

*      Assign the values to the arrays.
         do i=1,ngals
            xc(i) = pos(i,1)
            yc(i) = pos(i,2)
            if (poslen(i) .ge. 3 .and. pos(i,3) .ge. 0.0) then
               backs(i) = pos(i,3)
            else
               call msg_fmtr('xv','f6.1',xc(i))
               call msg_fmtr('yv','f6.1',yc(i))
               call msg_out(' ','Default background used for'//
     :              ' object at ^xv, ^yv ',status) 
               backs(i) = back
            endif
            if (poslen(i) .ge. 4) then
               rlims(i) = pos(i,4)
            else
               rlims(i) = rlim
            endif
         enddo
      endif
                  
*   Display the error message if necessary. Also, tidy up the error system.
      if (status.ne.sai__ok) then
         call msg_out(' ','Errors found when reading the data file.',
     :        status)
      endif
          
      end
