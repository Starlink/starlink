/*
*+
*  Name:
*    sysinfo
*
*  Purpose:
*    Return system information.
*
*  Language:
*    ANSI C
*
*  Notes:
*    Solaris version. The sysconf and processor_info calls are 
*    Solaris-specific.
*
*  Authors:
*    Tim Gledhill, University of Hertfordshire (tmg)
*
*  History:
*    24-MAR-1994: (tmg)
*      Original Version.
*    15-Jan-1996: (tmg)
*      Return processor speed in MHz.
*-
*/

/* Include Files.                                                 */

#include <unistd.h>
#include <sys/types.h>
#include <sys/processor.h>

main()
{


/* Local Variables.                                                */

      int i;
      int proc_speed;

      long  phys_mem;
      long  page_size;
      long  nproc_online;

      float max_mem;

      processor_info_t info;
    

/* Get the installed physical memory in Mb.                        */

      phys_mem = sysconf( _SC_PHYS_PAGES );
      page_size = sysconf( _SC_PAGESIZE );
      max_mem = ( (float) page_size / 1024 *
                  (float) phys_mem  / 1024 ); 

/* Get the number of online processors.                           */

      nproc_online = sysconf( _SC_NPROCESSORS_ONLN );

/* Get the processor speed in MHz for the first processor.       */

      processor_info( 0, &info );
      proc_speed = info.pi_clock;

/* Output the information.                                        */

      printf( "%.2f %2d ", max_mem, nproc_online);
      printf( "%2d ", proc_speed );

/* End of routine.                                                */

}         
