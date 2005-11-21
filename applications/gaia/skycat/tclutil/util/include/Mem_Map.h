// -*-c++-*-
#ifndef MEM_MAP_H
#define MEM_MAP_H
/*
 * E.S.O. - VLT project 
 * $Id: Mem_Map.h,v 1.2 2005/02/02 01:43:01 brighton Exp $
 *
 * Mem_Map.h - utility class wrapper for mmap(2), Author: Doug Schmidt
 *             (ripped from ACE C++ library for use in OCS by K. Gillies, 
 *              "...Doug says it's okay...."
 * 
 * who             when       what 
 * --------------  --------   ----------------------------------------
 * Allan Brighton  02 Aug 96  Created, added status() method, removed TRACE
 *                 06 Aug 96  Changed ssize_t to int, since it is not defined on sunos
 *                            define MAP_FAILED to (void*)-1, not def on HP,sunos
 *                 03 Dec 96  Added filename() method to return filename mapped.
 *                            Reformatted .h file and put comments above declarations
 *                            for readability.
 * Peter W. Draper 27 Sep 05  All lengths are now size_t, which usually means
 *                            an unsigned long, consequently all use of 
 *                            -1 as a special length has been changed to 0.
 */


#include <sys/types.h>
#include <sys/param.h>

// the wrapper is needed on some HPs
extern "C" {
#include <sys/mman.h>
}

#include <fcntl.h>

/* allan: 6.8.96: add defs missing on HP and/or sunos */
#ifndef MAP_FAILED
#define MAP_FAILED NULL
#endif

#ifndef MS_SYNC
#define MS_SYNC 0
#endif

#ifdef NEED_MMAP_PROTO
extern "C" {
extern caddr_t mmap(caddr_t, size_t, int, int, int, off_t);
extern int munmap(caddr_t, size_t);
}
#endif

// MMAP additional flags
#define PROT_RDWR (PROT_READ|PROT_WRITE)

// Default file permissions.
#define MMAP_DEFAULT_PERMS 0666
#define MMAP_INVALID_HANDLE 0
// Default size of mapped page on SunOS, HP and Solaris.
#define MMAP_PAGE_SIZE 4096

// External used to round request.
size_t
round_to_pagesize (off_t len);

// C++ interface to the mmap(2) UNIX system call. 
class Mem_Map {
public:
    // Default constructor.
    Mem_Map (void);

    // Map a file from an open file descriptor <handle>.  This function
    // will lookup the length of the file if it is not given.
    Mem_Map (int handle, 
	     size_t length = 0, 
	     int prot = PROT_READ, 
	     int share = MAP_SHARED, 
	     void *addr = 0, 
	     off_t pos = 0);

    // Map a file specified by <file_name>.
    Mem_Map (const char file_name[], 
	     size_t len = 0, 
	     int flags = O_RDWR,
	     int mode = MMAP_DEFAULT_PERMS, 
	     int prot = PROT_READ, 
	     int share = MAP_SHARED, 
	     void *addr = 0, 
	     off_t pos = 0);

    // Map a file from an open file descriptor <handle>.  This function
    // will lookup the length of the file if it is not given.
    int map (int handle, 
	     size_t length = 0, 
	     int prot = PROT_READ, 
	     int share = MAP_SHARED, 
	     void *addr = 0,
	     off_t pos = 0);

    // Remap the file associated with <handle_>.
    int map (size_t length = 0, 
	     int prot = PROT_READ, 
	     int share = MAP_SHARED, 
	     void *addr = 0, 
	     off_t pos = 0);

    // Map a file specified by <file_name>.
    int map (const char file_name[], 
	     size_t len = 0, 
	     int flags = O_RDWR,
	     int mode = MMAP_DEFAULT_PERMS, 
	     int prot = PROT_READ, 
	     int share = MAP_SHARED, 
	     void *addr = 0, 
	     off_t pos = 0);


    // Destructor.
    ~Mem_Map (void);

    const char* filename() {return filename_;}

    // Open the file without mapping it.
    int open (const char file_name[], 
	      int flags = O_RDWR,
	      int mode = MMAP_DEFAULT_PERMS);

    // Close down the <handle_> if necessary.
    int close (void);

    // This operator passes back the starting address of the mapped file.
    int operator () (void *&addr);

    // Return the base address.
    void *addr (void) const;

    // This function returns the number of bytes currently mapped in the
    // file.
    size_t size (void) const;

    // Unmap the region starting at <base_addr_>.
    int unmap (size_t len = 0);

    // Unmap the region starting at <addr_>.
    int unmap (void *addr, size_t len);

    // Sync <len> bytes of the memory region to the backing store
    // starting at <base_addr_>.  If <len> == 0 then sync the whole
    // region.
    int sync (size_t len = 0, int flags = MS_SYNC);

    // Sync <len> bytes of the memory region to the backing store
    // starting at <addr_>.
    int sync (void *addr, size_t len, int flags = MS_SYNC);

    // Change the protection of the pages of the mapped region to <prot>
    // starting at <base_addr_> up to <len> bytes.  If <len> == 0 then
    // change protection of all pages in the mapped region.
    int protect (size_t len = 0, int prot = PROT_READ);

    // Change the protection of the pages of the mapped region to <prot>
    // starting at <addr> up to <len> bytes.
    int protect (void *addr, size_t len, int prot = PROT_READ);

    // Close down and remove the file from the file system.
    int remove (void);

#if 0
    // Hook into the underlying VM system.
    int advise (int behavior, size_t len = 0);
#endif

    // Return the underlying <handle_>.
    int handle (void) const;

    // Dump the state of an object.
    void dump (void) const;

    // Return the status after the constructor
    int status() {return status_;}

private:
    // Base address of the memory-mapped file.
    void *base_addr_;

    // Name of the file that is mapped.
    char filename_[MAXPATHLEN + 1];

    // Length of the mapping.
    size_t length_;
  
    // HANDLE for the open file.
    int handle_;

    // status after constructor (allan)
    int status_;

    // Keeps track of whether we need to close the handle.  This is set
    // if we opened the file.
    int close_handle_;

    // This method does the dirty work of actually calling ::mmap to map
    // the file into memory.
    int map_it (int handle, 
		size_t len = 0, 
		int prot = PROT_READ, 
		int share = MAP_SHARED, 
		void *addr = 0, 
		off_t pos = 0);

    Mem_Map (const Mem_Map &) {}
    void operator = (const Mem_Map &) {}
};

// inlines

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
inline int
Mem_Map::handle (void) const
{
    return this->handle_;
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
inline int
Mem_Map::map (int handle, 
	      size_t len, 
	      int prot, 
	      int share, 
	      void *addr, 
	      off_t pos)
{
    return this->map_it (handle, len, prot, share, addr, pos);
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// Remap the file associated with <this->handle_>.
inline int
Mem_Map::map (size_t len, 
	      int prot, 
	      int share, 
	      void *addr, 
	      off_t pos)
{
    return this->map_it (this->handle(), len, prot, 
			 share, addr, pos);
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// This operator passes back the starting address of the mapped file.
inline int
Mem_Map::operator () (void *&addr)
{
    if (this->base_addr_ == (void*)MAP_FAILED) { /* allan: not defined on HP */
	return -1;
    } else {
	addr = this->base_addr_;
	return 0;
    }
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// Return the base address.
inline void *
Mem_Map::addr (void) const
{
    return this->base_addr_;
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// This function returns the number of bytes currently mapped in the
// file.
inline size_t
Mem_Map::size (void) const
{
    return this->length_;
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// Unmap the region starting at <this->base_addr_>.
inline int
Mem_Map::unmap (size_t len)
{
    return ::munmap ((caddr_t)this->base_addr_, len == 0 ? this->length_ : len);
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// Unmap the region starting at <addr_>.
inline int
Mem_Map::unmap (void *addr, size_t len)
{
    return ::munmap ((caddr_t)addr, len == 0 ? this->length_ : len);
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// Sync <len> bytes of the memory region to the backing store starting
// at <this->base_addr_>.  If <len> == 0 then sync the whole mapped
// region.
inline int
Mem_Map::sync (size_t len, int flags)
{
    return ::msync ((caddr_t)this->base_addr_, 
		    len == 0 ? this->length_ : len, flags);
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// Sync <len> bytes of the memory region to the backing store starting
// at <addr_>.

inline int
Mem_Map::sync (void *addr, size_t len, int flags)
{
    return ::msync((caddr_t)addr, len, flags);
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// Change the protection of the pages of the mapped region to <prot>
// starting at <this->base_addr_> up to <len> bytes.  If <len> == 0
// then change protection of all pages in the mapped region.
inline int 
Mem_Map::protect (size_t len, int prot)
{
    if (len == 0) {
	len = this->length_;
    }
    return ::mprotect((caddr_t)this->base_addr_, len, prot);
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// Change the protection of the pages of the mapped region to <prot>
// starting at <addr> up to <len> bytes.
inline int 
Mem_Map::protect(void *addr, size_t len, int prot)
{
    return ::mprotect((caddr_t)addr, len, prot);
}

#if 0
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// Hook into the underlying VM system.
inline int
Mem_Map::advise (int behavior, size_t len)
{
    if (len == 0) {
	len = this->length_;
    }
    return ::madvise ((caddr_t)this->base_addr_, len, behavior);
}
#endif

#endif /* ACE_MEM_MAP_H */
