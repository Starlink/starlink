      subroutine elp1_cato (ndf1, validp, zerop, 
     :     result, resnres, resnpoi, 
     :     xco, yco, back, sigma, psize, lbnd, isellpro,
     :     fiod, status)
*+
*   Name:
*     elp1_cato
*
*   Purpose:
*     Output the galaxy fit results to a CAT file.
*
*   Invocation:
*      call elp1_cato (ndf1, validp, zerop, 
*     :     result, resnres, resnpoi, 
*     :     xco, yco, back, sigma, psize, lbnd, isellpro,
*     :     fiod, status)
*
*   Description:
*     Creates an output file using the CAT routines, as documented in
*     SUN/181.  Reads the value of the OUTCAT parameter to fine the name
*     of the output file.
*
*     If the parameter validp is zero -- there are _no_ rows to be
*     output -- then the routine writes a single row of null data.
*
*   Arguments:
*
*     NDF1 = INTEGER (Given)
*        NDF identifier for the image.
*     VALIDP = INTEGER (Given)
*        Number of ellipse radii fitted successfully
*     ZEROP = REAL (Given)
*        The magnitude scale zero point. Units magnitudes.
*     RESULT(RESNRES,RESNPOI) = REAL (Given)
*        Array containing the results.
*     RESNRES = INTEGER (Given)
*        The number of results in the RESULT array.
*     RESNPOI = INTEGER (Given)
*        The maximum number of points in the RESULT array.
*     XCO = REAL (Given)
*        The X index of the origin used. Units pixels.
*     YCO = REAL (Given)
*        The Y index of the origin used. Units pixels.
*     BACK = REAL (Given)
*        Image background value employed. Units counts.
*     SIGMA = REAL (Given)
*        Standard deviation of the background value. Units counts.
*     PSIZE = REAL (Given)
*        The image pixels size. Units arc secs.
*     LBND(10) = INTEGER (Given)
*        Lower limits of the image world co-ordinate system.
*     ISELLPRO = LOGICAL (Given)
*        Is this being called by ellpro rather than ellfou (the output
*        format is minutely different)?
*     FIOD = INTEGER (Given and Returned)
*        Output file FIO descriptor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*   Authors:
*     NG: Norman Gray (Starlink, Glasgow)
*
*   History:
*     16-Dec-1999 (NG)
*       Original version, with logic based on elp1_texto.
*
*   Bugs:
*     None known
*
*   RCS Id:
*     $Id$
*-

*   Type definitions
      implicit none

*   Global constants
      include 'SAE_PAR'
      include 'CAT_PAR'
      include 'elp_par'
      include 'NDF_PAR'

*   Arguments given
      integer lbnd(ndf__mxdim)  ! Lower limits of image world
                                ! co-ordinate system
      integer ndf1              ! NDF indentifier
      integer validp            ! Number of radii fitted successfully
      integer resnres           ! number of results in RESULT array.
      integer resnpoi           ! maximum number of points in RESULT.
      real back                 ! Background count value
      real psize                ! The size of each pixel in
                                ! arc seconds
      real result(resnres,resnpoi) ! Array containing the profiling
                                ! results.
      real sigma                ! Standard deviation of the background
      real xco                  ! X index of the origin
      real yco                  ! Y index of the origin
      real zerop                ! Magnitude scale zero point
      logical isellpro          ! Is this being called by ellpro?

*  Arguments Given and Returned:
      integer fiod              ! Output file FIO descriptor
      character*100 tstr        ! Temporary string
      integer nchar             ! character count

*   Status
      integer status

*   Local variables
      integer ci                ! Catalogue identifier
      integer cols(19)          ! Column identifiers
      integer fnp,              ! Parameters: filename
     :     backp,               ! background
     :     sigmap,              ! sigma
     :     pixelp,              ! pixel size
     :     xbp,                 ! X-coordinate, base
     :     ybp,                 ! Y-coordinate, base
     :     framep,              ! Coordinate frame
     :     xwp,                 ! X-coordinate, world
     :     ywp,                 ! Y-coordinate, world
     :     zeropp               ! Zero point of magnitude
      integer i                 ! Loop counter
      integer iwcs              ! WCS component
      integer tablerows         ! number of rows in output table (from validp)
      logical nullvalue         ! is the table data valid?


*   Let us begin...
      if (status .ne. sai__ok) return
      
      if (validp .le. 0) then
*      Write out a catalogue with a single row of null data
         tablerows = 1
         nullvalue = .true.
      else
         tablerows = validp
         nullvalue = .false.
      endif
      
*   Create the catalogue, from the parameter OUTCAT
      call cat_creat ('OUTCAT', ci, status)

*   If the catalogue couldn't be created, then short-circuit everything
*   and leap to the end.
      if (status .ne. sai__ok) goto 9999

*   Declare the expected number of rows
      call cat_rset (ci, tablerows, status)

*   Create columns, all of type real.  Note that these are in a slightly
*   different (and more rational) order from the corresponding columns
*   in elp1_texto
      call cat_pnew0 (ci, cat__fityp, 'Number', 
     :     cat__typei, cols(1), status)
      call cat_pnew0 (ci, cat__fityp, 'X', 
     :     cat__typer, cols(2), status)
      call cat_pnew0 (ci, cat__fityp, 'Y', 
     :     cat__typer, cols(3), status)
      call cat_pnew0 (ci, cat__fityp, 'SemiMajor', 
     :     cat__typer, cols(4), status)
      call cat_pnew0 (ci, cat__fityp, '1/Ellipt', 
     :     cat__typer, cols(5), status)
      call cat_tattc (cols(5), 'COMM', 'reciprocal of ellipticity', 
     :     status)
      call cat_pnew0 (ci, cat__fityp, 'PA', 
     :     cat__typer, cols(6), status)
      call cat_tattc (cols(6), 'UNITS', 'RADIANS{DEGREES}', status)
      call cat_tattc (cols(6), 'COMM', 'Position angle', status)
      call cat_pnew0 (ci, cat__fityp, 'Count', 
     :     cat__typer, cols(7), status)
      call cat_pnew0 (ci, cat__fityp, 'Dev', 
     :     cat__typer, cols(8), status)
      call cat_pnew0 (ci, cat__fityp, 'Points', 
     :     cat__typei, cols(9), status)
      call cat_pnew0 (ci, cat__fityp, 'PPU', 
     :     cat__typer, cols(10), status)
      call cat_tattc (cols(10), 'COMM', 'Percentage points used',status)
      call cat_pnew0 (ci, cat__fityp, 'Statistic', 
     :     cat__typer, cols(11), status)
      call cat_pnew0 (ci, cat__fityp, '1xSin', 
     :     cat__typer, cols(12), status)
      call cat_pnew0 (ci, cat__fityp, '1xCos', 
     :     cat__typer, cols(13), status)
      call cat_pnew0 (ci, cat__fityp, '2xSin', 
     :     cat__typer, cols(14), status)
      call cat_pnew0 (ci, cat__fityp, '2xCos', 
     :     cat__typer, cols(15), status)
      call cat_pnew0 (ci, cat__fityp, '3xSin', 
     :     cat__typer, cols(16), status)
      call cat_pnew0 (ci, cat__fityp, '3xCos', 
     :     cat__typer, cols(17), status)
      call cat_pnew0 (ci, cat__fityp, '4xSin', 
     :     cat__typer, cols(18), status)
      call cat_pnew0 (ci, cat__fityp, '4xCos', 
     :     cat__typer, cols(19), status)
      
*   Create parameters, and set their values
      call cat_pnew0 (ci, cat__qityp, 'Filename',
     :     cat__typec, fnp, status)
      call ndf_msg ('NAME',ndf1)
      nchar = 0
      call msg_load (' ','^NAME',tstr,nchar,status)
      call cat_tatti (fnp, 'CSIZE', nchar, status)
      call cat_tattc (fnp, 'VALUE', tstr, status)
      
      call cat_pnew0 (ci, cat__qityp, 'Back',
     :     cat__typer, backp, status)
      call cat_tattr (backp, 'VALUE', back, status)

      call cat_pnew0 (ci, cat__qityp, 'Sigma',
     :     cat__typer, sigmap, status)
      call cat_tattr (sigmap, 'VALUE', sigma, status)
      
      call cat_pnew0 (ci, cat__qityp, 'Pixelsize',
     :     cat__typer, pixelp, status)
      call cat_tattr (pixelp, 'VALUE', psize, status)
      
      call cat_pnew0 (ci, cat__qityp, 'Zeropoint',
     :     cat__typer, zeropp, status)
      call cat_tattr (zeropp, 'VALUE', zerop, status)
      
      call cat_pnew0 (ci, cat__qityp, 'XBase',
     :     cat__typer, xbp, status)
      call cat_tattr (xbp, 'VALUE', xco, status)
      
      call cat_pnew0 (ci, cat__qityp, 'YBase',
     :     cat__typer, ybp, status)
      call cat_tattr (ybp, 'VALUE', yco, status)

      call ndf_gtwcs (ndf1, iwcs, status)
      call esp1_xyfmt (iwcs, xco, yco, 'X', 'Y', 'DOM', status)
      nchar = 0
      call msg_load (' ', '^DOM', tstr, nchar, status)
      call cat_pnew0 (ci, cat__qityp, 'Frame',
     :     cat__typec, framep, status)
      call cat_tatti (framep, 'CSIZE', nchar, status)
      call cat_tattc (framep, 'VALUE', tstr, status)
      call esp1_xyfmt (iwcs, xco, yco, 'X', 'Y', 'DOM', status)
      nchar = 0
      call msg_load (' ', '^X', tstr, nchar, status)
      call cat_pnew0 (ci, cat__qityp, 'XWorld',
     :     cat__typec, xwp, status)
      call cat_tatti (xwp, 'CSIZE', nchar, status)
      call cat_tattc (xwp, 'VALUE', tstr, status)
      call esp1_xyfmt (iwcs, xco, yco, 'X', 'Y', 'DOM', status)
      nchar = 0
      call msg_load (' ', '^Y', tstr, nchar, status)
      call cat_pnew0 (ci, cat__qityp, 'YWorld',
     :     cat__typec, ywp, status)
      call cat_tatti (ywp, 'CSIZE', nchar, status)
      call cat_tattc (ywp, 'VALUE', tstr, status)
      
*   Now write the values to the catalogue
      
      do i=1,tablerows
         call cat_put0i (cols(1), i, nullvalue, status)
         call cat_put0r (cols(2), result(1,i), nullvalue, status)
         call cat_put0r (cols(3), result(2,i), nullvalue, status)
         call cat_put0r (cols(4), result(4,i), nullvalue, status)
         call cat_put0r (cols(5), result(3,i), nullvalue, status)
         call cat_put0r (cols(6), result(5,i), nullvalue, status)
         call cat_put0r (cols(7), result(6,i), nullvalue, status)
         call cat_put0r (cols(8), result(7,i), nullvalue, status)
         call cat_put0i (cols(9), int(result(8,i)), nullvalue, status)
         call cat_put0r (cols(10), result(9,i), nullvalue, status)
         if (isellpro) then
            call cat_put0r (cols(11), result(elp__stat,i), nullvalue,
     :           status)
         else
            call cat_put0r (cols(11), 0.0, .true., status)
         endif
         call cat_put0r (cols(12), result(10,i), nullvalue, status)
         call cat_put0r (cols(13), result(11,i), nullvalue, status)
         call cat_put0r (cols(14), result(12,i), nullvalue, status)
         call cat_put0r (cols(15), result(13,i), nullvalue, status)
         call cat_put0r (cols(16), result(14,i), nullvalue, status)
         call cat_put0r (cols(17), result(15,i), nullvalue, status)
         call cat_put0r (cols(18), result(16,i), nullvalue, status)
         call cat_put0r (cols(19), result(17,i), nullvalue, status)
         
         call cat_rapnd (ci, status)
         
      enddo

*   Release the catalogue identifier
      call cat_trlse (ci, status)

*   Report on success
      if (status .eq. sai__ok) then
         call msg_out (' ','Catalogue created OK', status)
      else
         call err_rep (' ','Failed to create catalogue',status)
      endif

 9999 continue
      
      end
