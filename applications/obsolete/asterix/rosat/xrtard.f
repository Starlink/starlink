	subroutine xrtard(status)
*+
* Description:
*   An ASTERIX version of WELLARD to create an ARD file for the
*   masking out of ROSAT sources.
*
* Authour:
*   Richard Beard (University of Birmingham)
*
* History:
*   17 Jun 98 V2.2-0 (RB):
*     Original version.
*-

* External constants:
	include 'SAE_PAR'
	include 'DAT_PAR'

* Global status:
	integer status

* Local constants:
	character*30		version
	  parameter		( version = 'XRTARD Version 2.2-0' )

* Local variables
	character*132		rootname
	character*80		ardfile
	character*132		timrange
	character*132		srclist

	character*(dat__szloc)	srcloc
	character*(dat__szloc)	posloc
	character*(dat__szloc)	imgloc
	character*(dat__szloc)	xloc, yloc

	real			energy
	real			fraction

	integer			srcid
	integer			xptr, yptr
*.

* Check initial status
	if (status.ne.sae__ok) return

* Version id
	call msg_prnt(version)

* Initialise ASTERIX
	call ast_init()

* Read in the parameters
	call usi_get0c('ROOTNAME', rootname, status)
	call usi_get0c('ARDFILE', ardfile, status)
	call usi_get0c('TIMRANGE', timrange, status)
	call usi_get0c('SRCLIST', srclist, status)
	call usi_get0r('ENERGY', energy, status)
	call usi_get0r('PFRAC', fraction, status)

* Run XSPOKES
	call xspokes(status)
	call ast_init()

* How many sources
	call usi_assoc('SRCLIST', 'SSDS', 'READ', srcid, status)
	call adi1_getloc(srcid, srcloc, status)
	call dat_find(srcloc, 'POSIT', posloc, status)
	call cmp_get0i(posloc, 'NSRC', nsrc, status)
	call msg_seti('NSRC', nsrc)
	call msg_prnt('Found ^NSRC sources')

* Map some space for the positions
	call dyn_mapr(1, nsrc, xptr, status)
	call dyn_mapr(1, nsrc, yptr, status)

* Extract the radial offsets
	call dat_find(posloc, 'IMG_COORDS', imgloc, status)
	call dat_find(imgloc, 'X', xloc, status)
	call cmp_get1r(xloc, 'DATA_ARRAY', nsrc, %val(xptr), nsrc,
     :	               status)
	call dat_find(imgloc, 'Y', yloc, status)
	call cmp_get1r(yloc, 'DATA_ARRAY', nsrc, %val(yptr), nsrc,
     :	               status)

* Do the REGIONS processing
        call xrtard_regions(nsrc, %val(xptr), %val(yptr),
     :	                    fraction, energy, status)

* Tidy up
	call ast_close()
	call ast_err(status)

	end


	subroutine xrtard_regions(n, xpos, ypos, fraction, energy, status)

* Import/Export:
	integer			n
	real			xpos(n), ypos(n)
	real			fraction, energy
	integer			status

* Local variables:
	character*132		ardfile

	real			rad
	real			psf
	real			list(3)

	integer			i
	integer			ardunit, ardid

* Check initial status
	if (status.ne.sae__ok) return

* Open up the ARDFILE for append
	call usi_get0c('ARDFILE', ardfile, status)
	call fio_open(ardfile, 'APPEND', 'LIST', 0, ardunit, status)
	call arx_open('WRITE', ardid, status)

* Do the main processing
	do i = 1, n

*   Calculate the radial offset in arcmis
	  rad = 60.0 * sqrt(xpos(i)*xpos(i) + ypos(i)*ypos(i))

*   Calculate the enclosed PSF in degrees
          call xrt_getpsf(fraction, energy, rad, psf, status)
	  psf = psf / 60.0

*   Print some info
	  call msg_seti('I', i)
	  call msg_setr('AZ', xpos(i))
	  call msg_setr('EL', ypos(i))
	  call msg_setr('RAD', psf)
	  call msg_prnt('Source ^I at (^AZ,^EL) masked to ^RAD degrees')

*   Fill out the parameter defaults
	  list(1) = xpos(i)
	  list(2) = ypos(i)
	  list(3) = psf

*   Add the region to the ARD description
	  call arx_genput(ardid, 0, .FALSE., 'CIRCLE', 3, list, status)
	end do

* Write out all the ARD infomation to file
	call arx_writef(ardid, ardunit, status)
	call arx_close(ardid, status)
	call fio_close(ardunit, status)

* Report errors
	if (status.ne.sai__ok) then
	  call ast_rexit('XRTARD_REGIONS', status)
	end if

	end
