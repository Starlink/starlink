 /*
 * E.S.O. - VLT project/ESO Archive 
 * $Id: Mem_Map.C,v 1.4 1999/03/19 20:10:43 abrighto Exp $
 *
 * Mem_Map.C - method definitions for class Mem_Map
 * Author:     Doug Schmidt - ripped from ACE_wrappers by K. Gillies.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  3 Aug 96  Created, added call to "sys_error()", set status_
 * Peter W. Draper 23 Jan 97 Added cast to MAP_FAILED comparison (OSF/1).
 *                 21 Nov 97 Added fix for OSF/1 problems with statvfs
 *                           include.
 *                 23 Oct 00 Expanded error messages to be a little
 *                           more informative to an end user.
 */
static const char* const rcsId="@(#) $Id: Mem_Map.C,v 1.4 1999/03/19 20:10:43 abrighto Exp $";

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include "error.h"
#include "config.h"
#include "Mem_Map.h"
#include <stdio.h>
#ifdef HAVE_SYS_STATVFS_H
#ifdef __alpha   // Extern "C" & prototypes missing on OSF/1
extern "C" {
#endif
#include <sys/statvfs.h>
int statvfs(const char *, struct statvfs *);
int fstatvfs(int, struct statvfs *);
#ifdef __alpha
}
#endif
#endif

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
void
Mem_Map::dump (void) const
{
}

 
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// This function rounds the request to a multiple of the page size.
size_t
round_to_pagesize (off_t len)
{
  return (len + (MMAP_PAGE_SIZE - 1)) & ~(MMAP_PAGE_SIZE - 1);
}
 
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
int
Mem_Map::close (void)
{
  
    if (this->base_addr_ != (void*)MAP_FAILED) // allan: 30.7.97 added check
      this->unmap();

  if (this->close_handle_) {
    return ::close(this->handle_);
  }

  return 0;
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
Mem_Map::~Mem_Map (void) 
{
  this->close();
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// This function does the dirty work of actually calling mmap
// to map the file into memory.
int 
Mem_Map::map_it(int handle, 
		int len_request, 
		int prot, 
		int share, 
		void *addr, 
		off_t pos) 
{
  this->base_addr_ = addr;
  this->handle_	= handle;

  struct stat sb;
  long file_len = ::fstat(this->handle_, &sb) < 0 ? -1 : sb.st_size;

  if (file_len == -1) {
    sys_error("get file status (fstat) failed for: ", filename_);	// allan: added error report
    return -1;
  }

  // At this point we know <file_len> is not negative...
  this->length_ = size_t(file_len); 

  if (len_request == -1) {
    len_request = 0;
  }

  if ((this->length_ == 0 && len_request > 0)
      || this->length_ < size_t(len_request)) {

    this->length_ = len_request;

#ifdef HAVE_SYS_STATVFS_H
    // allan: make sure there is enough space on the filesystem
    struct statvfs vfs;
    if (fstatvfs(handle, &vfs) != 0) {
      sys_error("get file system information (fstatvfs) failed for: ", filename_);
      return -1;
    }
    if (vfs.f_frsize > 0) {  // NOTE: must calculate in blocks to avoid 32bit overflow
	unsigned long need = (len_request - file_len + vfs.f_frsize)/vfs.f_frsize;
	unsigned long have = vfs.f_bavail;
	if (have < need) {
	    error("DISK FULL: cannot create a sufficiently large map file: ", filename_);
	    return -1;
	}
    }
#endif

    // Extend the backing store.
    if (::lseek (this->handle_, 
		 len_request > 0 ? len_request - 1 : 0, 
		 SEEK_SET) == -1
	|| ::write (this->handle_, "", 1) != 1
	|| ::lseek (this->handle_, 0, SEEK_SET) == -1) {
      sys_error("write or seek failed for: ", filename_);	// allan: added error report
      return -1;
    }
  }

  if (this->length_ <= 0) {
      error("cannot map zero length file: ", filename_);
      return -1;
  }

  this->base_addr_ = ::mmap ((caddr_t)this->base_addr_, 
			     this->length_,
			     prot, 
			     share, 
			     this->handle_, 
			     off_t (round_to_pagesize (pos)));

  if (this->base_addr_ == (void*)MAP_FAILED) {
      // allan: added error report
      sys_error("failed to map file (insufficient VM?): ", filename_);
      return -1;
  }

  return 0; 
}


//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
int
Mem_Map::open(const char file_name[],
		   int flags,
		   int mode)
{
  ::strncpy (this->filename_, file_name, MAXPATHLEN);

  this->handle_ = ::open(file_name, flags, mode);

  if (this->handle_ == MMAP_INVALID_HANDLE) {
    sys_error("open failed for: ", filename_);	// allan: added error report
    return -1;
  } else {
    this->close_handle_ = 1;
    return 0;
  }
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
int
Mem_Map::map(const char file_name[], 
	     int len, 
	     int flags, 
	     int mode, 
	     int prot, 
	     int share, 
	     void *addr, 
	     off_t pos)
{
  if (this->open(file_name, flags, mode) == -1) {
    return -1;
  }
  return this->map_it(this->handle(), len, prot, share, addr, pos);
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
Mem_Map::Mem_Map(void)
  : length_ (0),
    base_addr_ (0), 
    handle_ (MMAP_INVALID_HANDLE),
    close_handle_ (0),
    status_ (0)
{
  ::memset (this->filename_, 0, sizeof this->filename_);
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// Map a file specified by FILE_NAME. 
Mem_Map::Mem_Map (const char file_name[], 
			  int len, 
			  int flags, 
			  int mode, 
			  int prot, 
			  int share, 
			  void *addr, 
			  off_t pos)
  : base_addr_ (0),
    close_handle_ (0),
    status_ (0)
{
  if (this->map (file_name, len, flags, mode, prot, share, addr, pos) < 0) {
    status_ = 1;
  }
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// Map a file from an open file descriptor HANDLE.  This function will
// lookup the length of the file if it is not given.
Mem_Map::Mem_Map (int handle, 
		  int len, 
		  int prot, 
		  int share, 
		  void *addr, 
		  off_t pos) 
    : close_handle_ (0),
      status_ (0)
{
  memset (this->filename_, 0, sizeof this->filename_);

  if (this->map (handle, len, prot, share, addr, pos) < 0)
    status_ = 1;
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// Close down and remove the file from the file system.
int
Mem_Map::remove (void)
{
  ::ftruncate(this->handle_, 0);
  this->close();

  if (this->filename_[0] != '\0') {
    return ::unlink (this->filename_);
  } 
  // Else 
  return 0;
}
