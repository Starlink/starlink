*   Test case for ems_fioer.c.in.  Create a IOSTAT error, and check 
*   that we get a reasonable message.  We can't check mechanically that
*   the message is reasonable, but we can check that it does appear, and
*   doesn't appear at the wrong times.
      integer function test_iostat()

      implicit none
      integer ios
      integer status
      integer rval
      character*80 retmsg
      integer retmsglen

      include 'SAE_PAR'
     
*   Should produce an IOSTAT error for `illegal unit number'
      open (unit=-10, file='wibble', status='OLD', iostat=ios)
      
      if ( ios .lt. 0 ) then
         write (*,'("test_iostat: Unexpected EOF")')
         rval = 1
      else if ( ios .eq. 0 ) then
         write (*,'("test_iostat: open succeeded -- BAD !")')
         rval = 1
      else
*      ios .gt. 0 -- what should happen
         call ems_fioer( 'IOSMSG', ios )
         status = SAI__OK
         call ems_expnd( '^IOSMSG', retmsg, retmsglen, status )
         write (*,'("Should be a message indicating ",
     :        "''illegal unit number''...")')
         write (*,'("<",a,">")') retmsg(:retmsglen)
         if ( retmsg(:10) .eq. 'ems_fioer:' ) then
*         The ems_fioer failure messages start with this string
            rval = 1
         else
            rval = 0
         endif
      endif

      test_iostat = rval

      end
