*-----------------------------------------------------------------------
*+  DAOPLOT - plot out positions of objects from DAOPHOT
      subroutine daoplot( status )
*
*    Description :
*     This plots out the positions of objects from a DAOPHOT results
*     file on top of a grey image created by DAOGREY. The file has to
*     be one of the results files created by DAOPHOT.
*
*    Invocation :
*     call daoplot( status )
*
*    Parameters :
*     pfile=character*(32)(given)
*           name of file containing DAOPHOT positions
*     device=device(given)
*           The image display
*
*    Method :
*     Check status on entry.
*     Open the DAOPHOT results file and read the header.
*     Get the last database entry of style DAOGREY.
*     Create a new zone equal in size and shape to the database entry.
*     For each record in the results file plot a cross on the screen.
*
*    Bugs :
*     None known.
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     26 May 1988
*     22 Nov 1988  Use released version of AGI
*     21 Jan 1991  Use new version of AGI
*     14 Feb 1995  Mend the offset problem when images have non-zero origin (GJP)
*    endhistory
*
*    Type Definitions :
      implicit none

*    Global constants :
      include 'SAE_PAR'

*    Status :
      integer status

*    Local Constants :
      integer lun
      parameter ( lun = 1 )

*    Local variables :
      character pfile * 32
      integer basid, fstat, id, izone, ncol, nl, nrow
      integer picid1, picid2, wkid
      real amag, ap1, frad, hibad, lobad, phpadu, readns, sky, thresh
      real x, y
*-

* Check the input status value
      if ( status .ne. SAI__OK ) goto 99

* Get the name of the file to be plotted
      call PAR_GET0C( 'PFILE', pfile, status )

* Open the DAOPHOT file for reading.
      call INFILE( lun, pfile, fstat )

* Check the status is OK
      if ( fstat .ne. 0 ) then
         status = SAI__ERROR
         call ERR_REP( 'DAOPLOT_INFILE',
     :                 'File could not be opened', status )
         goto 99
      endif

* Read the DAOPHOT file header. Set nl = -1 on entry
      call RDHEAD( lun, nl, ncol, nrow, lobad, hibad, thresh, ap1,
     :             phpadu, readns, frad )

* If the file does not have the required header then abort
      if ( nl .le. 0 ) then
         call CLFILE( lun )
         status = SAI__ERROR
         call ERR_REP( 'DAOPLOT_HEADER',
     :                 'File does not have correct header', status )
         goto 99
      endif

* Open up AGI and recall the last picture of name 'DAOGREY'
      call AGI_BEGIN
      call AGI_ASSOC( 'DEVICE', 'UPDATE', picid1, status )
      call AGI_IBASE( basid, status )
      call AGI_SELP( basid, status )
      call AGI_RCL( 'DAOGREY', picid2, status )

* Create an SGS zone for this picture
      call AGS_ACTIV( status )
      call AGS_NZONE( izone, status )
      if ( status .ne. SAI__OK ) goto 99

* Set up a yukky colour in pen 1
      call SGS_ICURW( wkid )
      call GSCR( wkid, 1, 0.4, 1.0, 0.0 )

* Plot out the positions of the stars from the DAOPHOT file
* Id = - 1 indicates end of file
      id = 0
      do while ( id .ge. 0 )
         call RDSTAR( 1, nl, id, x, y, amag, sky )
         if ( id .gt. 0 ) then
            call SGS_LINE( x-2.0, y, x+2.0, y )
            call SGS_LINE( x, y-2.0, x, y+2.0 )
         endif
      enddo

* Close the data file and SGS
      call CLFILE( lun )
      call AGS_DEACT( status )

* Close down AGI
      call AGI_CANCL( 'DEVICE', status )
      call AGI_END( -1, status )

  99  continue

      end

