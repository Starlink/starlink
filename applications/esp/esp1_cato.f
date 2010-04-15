      subroutine esp1_cato (phase, ndf1, validp, zerop,
     :     result, resnres, resnpoi,
     :     xco, yco, back, sigma, psize, lbnd, isellpro,
     :     status)
*+
*   Name:
*     esp1_cato
*
*   Purpose:
*     Output the galaxy fit results to a CAT file.
*
*   Invocation:
*      call esp1_cato (phase, ndf1, validp, zerop,
*     :     result, resnres, resnpoi,
*     :     xco, yco, back, sigma, psize, lbnd, isellpro,
*     :     status)
*
*   Description:
*     Creates an output file using the CAT routines, as documented in
*     SUN/181.  Reads the value of the OUTCAT parameter to find the name
*     of the output file.  It's OK for the OUTCAT parameter not to
*     exist: in that case, we simply return with PAR__NULL in the STATUS
*     parameter.
*
*     If the parameter validp is zero -- there are _no_ rows to be
*     output -- then the routine writes a single row of null data.
*
*     The routine runs in three phases: opening the catalogue file,
*     writing successive lines of data, and closing the file.  If PHASE
*     is passed as zero, then the routine does all three phases.
*     Otherwise, you should call it once with PHASE=1, then once for
*     each source with PHASE=2, and finally once with PHASE=3.  For
*     phases 1 and 3, only the phase and status arguments need to
*     have valid values.
*
*     The routine is stateful, so you should not attempt to write two
*     catalogues at once by, for example, mixing calls with different
*     values of the parameter NDF1.
*
*   Arguments:
*
*     PHASE = INTEGER (Given)
*        Which phase of the writing process is this?  See description
*        above for details.
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
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*   Authors:
*     NG: Norman Gray (Starlink, Glasgow)
*     PWD: Peter W. Draper (JAC, Durham University)
*
*   History:
*     16-Dec-1999 (NG)
*       Original version, with logic based on elp1_texto.
*     27-APR-2007 (PWD):
*       Output some (specially X and Y) values with format G14.7
*       not CAT default of E12.3. Need that extra precision for
*       large images.
*
*   Bugs:
*     None known
*-

*   Type definitions
      implicit none

*   Global constants
      include 'SAE_PAR'
      include 'CAT_PAR'
      include 'ELP_PAR'
      include 'NDF_PAR'

*   Arguments given
      integer phase             ! Which phase of writing is this?  See above.
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
      character*100 tstr        ! Temporary string
      integer nchar             ! character count

*   Status
      integer status

*   Local variables
      integer ci                ! Catalogue identifier
      integer cols(20)          ! Column identifiers
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
      integer sourcen           ! source number
      integer iwcs              ! WCS component
      integer tablerows         ! number of rows in output table (from validp)
      logical nullvalue         ! is the table data valid?

*   Save list
      save ci, cols, sourcen

*   Let us begin...
      if (status .ne. sai__ok) return

*   PHASE 1: open catalogue
      if (phase.eq.0 .or. phase.eq.1) then
*      Create the catalogue, from the parameter OUTCAT
         call cat_creat ('OUTCAT', ci, status)

*      If the catalogue couldn't be created, then short-circuit
*      everything and leap to the end.
         if (status .ne. sai__ok) goto 9999

*      Other bits of initialisation
         sourcen = 0
      endif

*   PHASE 2: write data
      if (phase.eq.0 .or. phase.eq.2) then
*      Begin writing out the data.  First define the columns and
*      parameters.  These might be done more naturally in the
*      `phase 1' section above, but that would break the simplifying
*      assumption that the parameters other than phase and status need
*      be valid only in phase 2.

         if (sourcen .eq. 0) then
*         First time through

*         Create columns, all of type real.  Note that these are in a
*         slightly different (and more rational) order from the
*         corresponding columns in elp1_texto
            call cat_pnew0 (ci, cat__fityp, 'SourceN',
     :                      cat__typei, cols(1), status)

            call cat_pnew0 (ci, cat__fityp, 'Number',
     :                      cat__typei, cols(2), status)

            call cat_pnew0 (ci, cat__fityp, 'X',
     :                      cat__typer, cols(3), status)
            call cat_tattc (cols(3), 'EXFMT', 'G14.7', status)

            call cat_pnew0 (ci, cat__fityp, 'Y',
     :                     cat__typer, cols(4), status)
            call cat_tattc (cols(4), 'EXFMT', 'G14.7', status)

            call cat_pnew0 (ci, cat__fityp, 'SemiMajor',
     :                      cat__typer, cols(5), status)
            call cat_tattc (cols(5), 'EXFMT', 'G14.7', status)

            call cat_pnew0 (ci, cat__fityp, '1/Ellipt',
     :                      cat__typer, cols(6), status)
            call cat_tattc (cols(6), 'COMM',
     :                      'reciprocal of ellipticity',status)
            call cat_tattc (cols(6), 'EXFMT', 'G14.7', status)

            call cat_pnew0 (ci, cat__fityp, 'PA', cat__typer,
     :                      cols(7), status)
            call cat_tattc (cols(7), 'UNITS', 'RADIANS{DEGREES}',
     :                      status)
            call cat_tattc (cols(7), 'COMM', 'Position angle', status)

            call cat_pnew0 (ci, cat__fityp, 'Count',
     :                      cat__typer, cols(8), status)

            call cat_pnew0 (ci, cat__fityp, 'Dev',
     :                      cat__typer, cols(9), status)

            call cat_pnew0 (ci, cat__fityp, 'Points',
     :                      cat__typei, cols(10), status)

            call cat_pnew0 (ci, cat__fityp, 'PPU',
     :                      cat__typer, cols(11), status)
            call cat_tattc (cols(11), 'COMM', 'Percentage points used',
     :                      status)

            call cat_pnew0 (ci, cat__fityp, 'Statistic',
     :                      cat__typer, cols(12), status)

            call cat_pnew0 (ci, cat__fityp, '1xSin',
     :                      cat__typer, cols(13), status)
            call cat_tattc (cols(13), 'EXFMT', 'G14.7', status)

            call cat_pnew0 (ci, cat__fityp, '1xCos',
     :                      cat__typer, cols(14), status)
            call cat_tattc (cols(14), 'EXFMT', 'G14.7', status)

            call cat_pnew0 (ci, cat__fityp, '2xSin',
     :                      cat__typer, cols(15), status)
            call cat_tattc (cols(15), 'EXFMT', 'G14.7', status)

            call cat_pnew0 (ci, cat__fityp, '2xCos',
     :                      cat__typer, cols(16), status)
            call cat_tattc (cols(16), 'EXFMT', 'G14.7', status)

            call cat_pnew0 (ci, cat__fityp, '3xSin',
     :                      cat__typer, cols(17), status)
            call cat_tattc (cols(17), 'EXFMT', 'G14.7', status)

            call cat_pnew0 (ci, cat__fityp, '3xCos',
     :                      cat__typer, cols(18), status)
            call cat_tattc (cols(18), 'EXFMT', 'G14.7', status)

            call cat_pnew0 (ci, cat__fityp, '4xSin',
     :                      cat__typer, cols(19), status)
            call cat_tattc (cols(19), 'EXFMT', 'G14.7', status)

            call cat_pnew0 (ci, cat__fityp, '4xCos',
     :                      cat__typer, cols(20), status)
            call cat_tattc (cols(20), 'EXFMT', 'G14.7', status)

*         Create parameters, and set their values
            call cat_pnew0 (ci, cat__qityp, 'Filename',
     :           cat__typec, fnp, status)
            call ndf_msg ('NAME',ndf1)
            nchar = 0
            call msg_load (' ','^NAME',tstr,nchar,status)
            call cat_tatti (fnp, 'CSIZE', nchar, status)
            call cat_tattc (fnp, 'VALUE', tstr, status)

            call cat_pnew0 (ci, cat__qityp, 'Back',
     :           cat__typer, backp, status)
            call cat_tattr (backp, 'VALUE', back, status)

            call cat_pnew0 (ci, cat__qityp, 'Sigma',
     :           cat__typer, sigmap, status)
            call cat_tattr (sigmap, 'VALUE', sigma, status)

            call cat_pnew0 (ci, cat__qityp, 'Pixelsize',
     :           cat__typer, pixelp, status)
            call cat_tattr (pixelp, 'VALUE', psize, status)

            call cat_pnew0 (ci, cat__qityp, 'Zeropoint',
     :           cat__typer, zeropp, status)
            call cat_tattr (zeropp, 'VALUE', zerop, status)

            call cat_pnew0 (ci, cat__qityp, 'XBase',
     :           cat__typer, xbp, status)
            call cat_tattr (xbp, 'VALUE', xco, status)

            call cat_pnew0 (ci, cat__qityp, 'YBase',
     :           cat__typer, ybp, status)
            call cat_tattr (ybp, 'VALUE', yco, status)

            call ndf_gtwcs (ndf1, iwcs, status)
            call esp1_xyfmt (iwcs, xco, yco, 'X', 'Y', 'DOM', status)
            nchar = 0
            call msg_load (' ', '^DOM', tstr, nchar, status)
            call cat_pnew0 (ci, cat__qityp, 'Frame',
     :           cat__typec, framep, status)
            call cat_tatti (framep, 'CSIZE', nchar, status)
            call cat_tattc (framep, 'VALUE', tstr, status)
            call esp1_xyfmt (iwcs, xco, yco, 'X', 'Y', 'DOM', status)
            nchar = 0
            call msg_load (' ', '^X', tstr, nchar, status)
            call cat_pnew0 (ci, cat__qityp, 'XWorld',
     :           cat__typec, xwp, status)
            call cat_tatti (xwp, 'CSIZE', nchar, status)
            call cat_tattc (xwp, 'VALUE', tstr, status)
            call esp1_xyfmt (iwcs, xco, yco, 'X', 'Y', 'DOM', status)
            nchar = 0
            call msg_load (' ', '^Y', tstr, nchar, status)
            call cat_pnew0 (ci, cat__qityp, 'YWorld',
     :           cat__typec, ywp, status)
            call cat_tatti (ywp, 'CSIZE', nchar, status)
            call cat_tattc (ywp, 'VALUE', tstr, status)

*         End of first-time writing of header
         endif

*      Increment the source count
         sourcen = sourcen+1

*      Get the number of rows to be output, also checking that validp is
*      positive
         if (validp .le. 0) then
*         Write out a catalogue with a single row of null data
            tablerows = 1
            nullvalue = .true.
         else
            tablerows = validp
            nullvalue = .false.
         endif

*      Now write the values to the catalogue

         do i=1,tablerows
            call cat_put0i (cols(1), sourcen, nullvalue, status)
            call cat_put0i (cols(2), i, nullvalue, status)
            call cat_put0r (cols(3), result(1,i), nullvalue, status)
            call cat_put0r (cols(4), result(2,i), nullvalue, status)
            call cat_put0r (cols(5), result(4,i), nullvalue, status)
            call cat_put0r (cols(6), result(3,i), nullvalue, status)
            call cat_put0r (cols(7), result(5,i), nullvalue, status)
            call cat_put0r (cols(8), result(6,i), nullvalue, status)
            call cat_put0r (cols(9), result(7,i), nullvalue, status)
            call cat_put0i (cols(10), int(result(8,i)), nullvalue,
     :           status)
            call cat_put0r (cols(11), result(9,i), nullvalue, status)
            if (isellpro) then
               call cat_put0r (cols(12), result(elp__stat,i), nullvalue,
     :              status)
            else
               call cat_put0r (cols(12), 0.0, .true., status)
            endif
            call cat_put0r (cols(13), result(10,i), nullvalue, status)
            call cat_put0r (cols(14), result(11,i), nullvalue, status)
            call cat_put0r (cols(15), result(12,i), nullvalue, status)
            call cat_put0r (cols(16), result(13,i), nullvalue, status)
            call cat_put0r (cols(17), result(14,i), nullvalue, status)
            call cat_put0r (cols(18), result(15,i), nullvalue, status)
            call cat_put0r (cols(19), result(16,i), nullvalue, status)
            call cat_put0r (cols(20), result(17,i), nullvalue, status)

            call cat_rapnd (ci, status)

         enddo

      endif


*   PHASE 3: close catalogue
      if (phase.eq.0 .or. phase.eq.3) then
*      Release the catalogue identifier
         call cat_trlse (ci, status)

*      Report on success
         if (status .eq. sai__ok) then
            call msg_out (' ','Catalogue created OK', status)
         else
            call err_rep (' ','Failed to create catalogue',status)
         endif
      endif

 9999 continue

      end
