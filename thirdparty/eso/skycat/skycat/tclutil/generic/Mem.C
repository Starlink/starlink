/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id: Mem.C,v 1.4 2005/02/02 01:43:00 brighton Exp $" 
 *
 * Mem.C - method definitions for class Mem, for managing memory
 *         areas with or without shared memory.
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  07/03/96  Created
 * D.Hopkinson     21/01/97  Added constructor to use when multi-buffering shared memory.
 * pbiereic        22/10/99  Attach to shm with SHM_RDONLY when owner=0
 * pbiereic        10/11/99  Use _exit() in signal handler, so that the
 * Peter W. Draper 23/01/00  Added constructor to accept "malloc'd"
 *                           memory. This is may or may not be "owned"
 *                           (I added this to accept memory mapped
 *                           from the NDF library, so as to avoid the
 *                           need for a memory copy).
 *                           message queue of a possible parent process is not closed
 * pbiereic        17/02/03  Added 'using namespace std'.
 * Peter W. Draper 03/09/04  New addr arguments for Mem and Mem_Rep
 *                           constructors. 
 */
static const char* const rcsId="@(#) $Id: Mem.C,v 1.4 2005/02/02 01:43:00 brighton Exp $";

using namespace std;
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include <cstdio>
#include <iostream>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include "error.h"
#include "define.h"
#include "util.h"
#include "Mem.h"
#include "Mem_Map.h"

/*
 * NOTE: on Solaris2.5, the default limit is 8 shared memory areas per process:
 * this can be changed by editing /etc/system and adding the following line:
 *
 *     set shmsys:shminfo_shmseg=200
 */

// Some in-file data
static int const MEM_MMAP_DEFAULT_FLAGS = O_RDONLY;
static int const MEM_MMAP_DEFAULT_PROT = PROT_READ;
static int const MEM_MMAP_DEFAULT_SHARING = MAP_SHARED;


#ifdef NEED_SHM_PROTO
// missing prototypes (on SunOS at least)
extern "C" {
    // should be in sys/shm.h
    void *shmat(int shmid, const void* shmaddr, int shmflg);
    int shmdt(const void* shmaddr);
    int shmget(key_t, size_t, int);
    int shmctl(int shmid, int cmd, shmid_ds *buf);
}
#endif


// The following is used to keep track of shared memory Mem objects.
// On some systems (hp), a shmat will fail if the shared memory
// is alread attached (also: mmap fails if the file is already mapped).
// That is why we need the following. It is also handy for cleaning up when
// a program is killed.
enum {MAX_SHM_ = 255};	   	   // max number of shared memory Mem objects allowed
static MemRep* shmObjs_[MAX_SHM_]; // array of existing shared memory Mem objects
static int shmCount_ = 0;	   // count of objects in above array


/*
 * default constructor
 */

MemRep::MemRep()
    : size(0),
      owner(0),
      refcnt(1),
      ptr(NULL),
      newmem(0),
      shmId(-1),
      shmNum(0),
      semId(-1),
      options(0),
      status(0),
      verbose(0),
      m_map(NULL),
      linkName(NULL)
{
}


/*
 * constructor - attach to existing sysV shared memory with given id
 */
MemRep::MemRep(size_t sz, int own, int id, int verb)
    : size(sz),
      owner(own),
      refcnt(1),
      ptr(NULL),
      newmem(0),
      shmId(id),
      shmNum(0),
      semId(-1),
      options(0),
      status(0),
      verbose(verb),
      m_map(NULL),
      linkName(NULL)
{
    if (shmCount_ >= MAX_SHM_) {
	status = error("too many shared memory segments");
	return;
    }

    // check the size
    shmid_ds shmInfo;
    if (shmctl(shmId, IPC_STAT, &shmInfo) != 0) {
	status = sys_error("bad shared memory Id specified");
	return;
    }
    if (shmInfo.shm_segsz < size) {
	status = error("specified shared memory area is too small");
	return;
    }

    // attach new shared memory segment
    if (owner)
        ptr = shmat(shmId, NULL, 0); 
    else
        ptr = shmat(shmId, NULL, SHM_RDONLY); 

    if (ptr == NULL || ptr == (void *)-1) {
	ptr = NULL;
	shmId  = -1;
	status = sys_error("Invalid shared memory id specified");
	return;
    }
#ifdef DEBUG
    if (verbose)
	log_message("attach to new shared memory with Id: %d", shmId);
#endif
    // enter new object table for later reference
    shmObjs_[shmCount_++] = this;
}


/*
 * constructor - create a new MemRep object with the given size.  If
 * "useShm" is non-zero, shared memory is allocated, otherwise "operator
 * new" is used.
 */
MemRep::MemRep(size_t sz, int useShm, int verb)
    : size(sz),
      owner(1),
      refcnt(1),
      ptr(NULL),
      newmem(0),
      shmId(-1),
      shmNum(0),
      semId(-1),
      options(0),
      status(0),
      verbose(verb),
      m_map(NULL),
      linkName(NULL)
{
    if (size <= 0)
	return;			// null shared mem

    if (! useShm) {
	ptr = new char[size];
        newmem = 1;
	if (!ptr)
	    status = error("out of memory");
	return;
    }

    if (shmCount_ >= MAX_SHM_) {
	status = error("too many shared memory segments");
	return;
    }

    shmId = shmget(IPC_PRIVATE, size, 0666);
    ptr = (void *)shmat(shmId,  NULL, 0);
    if (ptr == NULL || ptr == (void *)-1) {
	ptr = NULL;
	status = sys_error("error creating shared memory");
	return;
    }
#ifdef DEBUG
    if (verbose)
	log_message("Shared Memory area created, Id: %d, size: %d", shmId, size);
#endif

    // enter new object in table for later reference
    shmObjs_[shmCount_++] = this;
}

/*
 * constructor - accept pointer to malloc'd memory.
 *
 * If owner is non-zero the memory is freed when this object is
 * deleted.
 */
MemRep::MemRep(void *inptr, size_t sz, int own)
    : size(sz),
      owner(own),
      refcnt(1),
      ptr(inptr),
      newmem(0),
      shmId(-1),
      shmNum(0),
      semId(-1),
      options(0),
      status(0),
      verbose(0),
      m_map(NULL),
      linkName(NULL)
{
}

/*
 * constructor - mmap the given file using the given options and flags.
 *
 * If owner is non-zero, the file is deleted when this object is deleted.
 */
MemRep::MemRep(const char *filename, int flags, int prot, int share,
               size_t nbytes, int own, int verb, void *addr)
    : size(0),
      owner(own),
      refcnt(1),
      ptr(NULL),
      newmem(0),
      shmId(-1),
      shmNum(0),
      semId(-1),
      options(0),
      status(0),
      verbose(verb),
      m_map(NULL),
      linkName(NULL)
{
    if (! filename) {
	status = error("no file name specified for mmap");
	return;
    }

    if ((flags & O_CREAT) == 0) { // using existing file, not creating a new one?
	// can't map a file that doesn't exist
	if (access(filename, F_OK) != 0) {
	    status = error("file does not exist: ", filename);
	    return;
	}

	// can't map a file without read permission
	if (access(filename, R_OK) != 0) {
	    status = error("file has no read permission: ", filename);
	    return;
	}

	// check the file permissions and the mmap flags, since on solaris at least,
	// mmap(read-write) seems to succede, even if file is read-only
	if ((flags & O_RDWR) && access(filename, W_OK) != 0) {
	    status = error("can't mmap read-only file for writing: ", filename);
	    return;
	}
    }

    // get the name of the real file (if a link)
    //char realname[1024];
    //const char* fname = fileRealname(filename, realname, sizeof(realname));
    //if (fname == realname)
    //linkName = strdup(filename);  // remember the name of the link
    const char* fname = filename;  // allan: 9.11.00: had problems with relative links

    // map the file
    m_map = new Mem_Map(fname,
			nbytes,               // length
			flags,                // Read/Write, etc.
			MMAP_DEFAULT_PERMS,   // mode
			prot,                 // protection,
			share,                // share map on write
                        addr);                // address to map file at

    if (!m_map || m_map->status() != 0) {
	// status = error("mapping of file failed");
	status = 1;
	return;
    }

    size = m_map->size();
    ptr = m_map->addr();

    // enter new object table for later reference
    shmObjs_[shmCount_++] = this;
}


/*
 * internal destructor - Detach and/or delete shared memory, if needed.
 */
MemRep::~MemRep()
{
    if (shmId >= 0 || m_map) {
	// remove this obejct from the table, and shift others left
	// (there won't normally be very many entries in the table...)
	for (int i = 0; i < shmCount_; i++) {
	    if (shmObjs_[i] == this) {
		shmCount_--;
		while (i < shmCount_) {
		    shmObjs_[i] = shmObjs_[i+1];
		    i++;
		}
		shmObjs_[shmCount_] = NULL;
		break;
	    }
	}
    }

    // If there is a semaphore associated with this shared memory, set
    // it to zero .
    if (shmId >= 0) {
	// if we are responsible for deleting this memory, do it now
	if (owner) {
	    struct sembuf semDec[1] = {
		0, 0, IPC_NOWAIT
	    };
	    // Perform the reset
#ifdef HAVE_UNION_SEMUN
	    union semun s; // allan: 11.9.97 - type needed for linux
	    s.val = 0;
#else
	    void* s = NULL;
#endif
	    semDec[0].sem_num = (unsigned short)shmNum;
	    semDec[0].sem_op = (short)(0 - semctl(semId, shmNum, GETVAL, s));
	    semop(semId, &semDec[0], 1);

#ifdef DEBUG
	    if (verbose)
		log_message("removing (IPC_RMID) shared memory area: %d", shmId);
#endif
	    shmctl(shmId, IPC_RMID, NULL);
	}

	// detach and delete, since there are no more references
	if (ptr) {
#ifdef DEBUG
	    if (verbose)
		log_message("detaching shared memory area: %d", shmId);
#endif
	    shmdt((char*)ptr);
	}
    }
    else if (m_map != 0) {	// mmap file
	if (owner && m_map->filename())
	    unlink(m_map->filename()); // if we are the owner, rm the file now
	if(m_map) delete m_map;
    }
    else if (ptr) {		// must be using plain memory
       if (newmem && owner) {
          delete[] (char *)ptr;
       } else if (owner) {
          free( ptr );
       }
    }
    
    ptr = NULL;
    newmem = 0;
    m_map = NULL;
    shmId = -1;
    size = 0;
    status = -1;
    if (linkName) {
	free(linkName);
	linkName = NULL;
    }
}


/*
 * return name of mmap file or NULL if mmap not being used.
 * If flag is 1 and the original file was a link, return the link name,
 * otherwise return the real name.
 */
const char* MemRep::filename(int flag) const
{
    if (m_map) {
	if (flag && linkName)
	    return linkName;
	return m_map->filename();
    }
    return NULL;
}


/*
 * Temporarily unmap the shared memory. This is needed if you want to save lots
 * of these objects without running out of shared memory resources.
 * (Note: only for use with read-only mmapped memory.)
 */
void MemRep::unmap()
{
    if (refcnt > 1)
	return;		// can't unmap, more than one ref

    if (m_map) {
	m_map->close();
	ptr = NULL;
    }

    // not impl for sysV shared mem
}


/*
 * remap the shared memory after a call to unmap()
 * (may be used to unmap and remap with different options, see MemFileOptions)
 * If the optional newsize arg is given and is not 0, it indicates a new
 * file size. This can be used to extend the file size.
 */
int MemRep::remap(int opts, size_t newsize)
{
    if (!m_map || !m_map->filename())
	return error("can't remap memory, not mapped");

    int flags = 0;
    int prot = 0;
    int sharing = 0;

    if (opts == Mem::FILE_DEFAULTS) {
	flags = MEM_MMAP_DEFAULT_FLAGS;
	prot = MEM_MMAP_DEFAULT_PROT;
	sharing = MEM_MMAP_DEFAULT_SHARING;
    }
    else {
	// There are client specified options
	flags |= (opts & Mem::FILE_RDWR ) ? O_RDWR : O_RDONLY;
	prot = (opts & Mem::FILE_RDWR) ? PROT_RDWR : PROT_READ;
	sharing = (opts & Mem::FILE_PRIVATE) ? MAP_PRIVATE: MAP_SHARED;
    }

    // first unmap (very important...)
    m_map->close();

    // now remap
    if (m_map->map(m_map->filename(),
		   newsize,
		   flags,
		   MMAP_DEFAULT_PERMS,
		   prot,
		   sharing,
		   NULL, 0) < 0) {
	return sys_error("mmap failed for file: ", m_map->filename());
    }
    size = m_map->size();
    ptr = m_map->addr();
    options = opts;

    // not impl for sysV shared mem
    return 0;
}


/*
 * search for an existing MemRep object for the given shmId and return it
 * or NULL if not found
 */
static MemRep* findMemRep(int shmId)
{
    if (shmId >= 0) {
	for (int i = 0; i < shmCount_; i++)
	    if (shmObjs_[i]->shmId == shmId)
		return shmObjs_[i];
    }

    return NULL;
}


/*
 * search for an existing MemRep object for the given filename and return it
 * or NULL if not found
 */
static MemRep* findMemRep(const char* filename)
{
    MemRep* rep;
    if (filename) {

	// get the name of the real file
	//char realname[1024];
	//const char* pfile = fileRealname(filename, realname, sizeof(realname));
	const char* pfile = filename;  // allan: 9.11.00: had problems with relative links

	for (int i = 0; i < shmCount_; i++) {
	    if (shmObjs_[i]->m_map && strcmp(shmObjs_[i]->m_map->filename(), pfile) == 0) {
		rep = shmObjs_[i];
		// Note: memory may have been temporarily unmapped (see unmap())
		// If so, remap it here.
		if (rep->ptr == NULL && rep->remap() != 0)
		    return NULL;
		return rep;
	    }
	}
    }

    return NULL;   // not found
}



// --------------------- Below is Mem - Above MemRep -------------------------



/*
 * constructor - attach (if needed) to existing shm area
 */
Mem::Mem(size_t size, int shmId, int owner, int verbose)
    : offset_(0), length_(0)
{
    // see if we have this Id already
    if (rep_ = findMemRep(shmId)) {
	rep_->refcnt++;
	return;
    }

    // make a new one
    rep_ = new MemRep(size, owner, shmId, verbose);
}

/*
 * Constructor to use when multi-buffering shared memory. The fifth argument
 * is the number of the buffer in the sequence of use of shared memory
 * buffers. This is only required to lock the memory when using semaphores.
 * The final argument is the ID of the semaphore used to lock this
 * particular area of shared memory.
 */
Mem::Mem(size_t size, int shmId, int owner, int verbose, int shmNum, int semId)
: offset_(0), length_(0)
{
    // see if we have this Id already
    if (rep_ = findMemRep(shmId)) {
	rep_->refcnt++;
	return;
    }

    // make a new one
    rep_ = new MemRep(size, owner, shmId, verbose);

    rep_->shmNum = shmNum;
    rep_->semId = semId;
}


/*
 * constructor - use Mem_Map to open file and get pointer
 */
Mem::Mem(const char *filename, int verbose)
    : offset_(0), length_(0)
{
    // see if we have mapped this file already
    if (rep_ = findMemRep(filename)) {
	rep_->refcnt++;
	return;
    }

    rep_ = new MemRep(filename,
		    MEM_MMAP_DEFAULT_FLAGS,
		    MEM_MMAP_DEFAULT_PROT,
		    MEM_MMAP_DEFAULT_SHARING, 0, 0, verbose);
}


/*
 * Constructor uses mmap to map a file and adds file options
 */
Mem::Mem(const char *filename, int options, int verbose, void *addr)
    : offset_(0), length_(0)
{
    int flags = 0;
    int prot = 0;
    int sharing = 0;

    if (options == FILE_DEFAULTS) {
	flags = MEM_MMAP_DEFAULT_FLAGS;
	prot = MEM_MMAP_DEFAULT_PROT;
	sharing = MEM_MMAP_DEFAULT_SHARING;
    }
    else {
	// There are client specified options
	flags |= (options & FILE_RDWR ) ? O_RDWR : O_RDONLY;
	prot = (options & FILE_RDWR) ? PROT_RDWR : PROT_READ;
	sharing = (options & FILE_PRIVATE) ? MAP_PRIVATE: MAP_SHARED;
    }

    // see if we have mapped this file already
    if (rep_ = findMemRep(filename)) {
	rep_->refcnt++;
	return;
    }

    rep_ = new MemRep(filename, flags, prot, sharing, 0, 0, verbose, addr);
    rep_->options = options;
}


/*
 * Constructor: create a file of the given size and use mmap to map the
 * file read/write (Note: we have to use O_RDWR for mmap).
 *
 * If owner if non-zero, the file is deleted when there are no more
 * references.
 */
Mem::Mem(size_t size, const char *filename, int owner, int verbose)
    : offset_(0), length_(0)
{
    // see if we have mapped this file already
    if (rep_ = findMemRep(filename)) {
	rep_->refcnt++;
	fmt_error("warning: file %s already exists and is already mmapped!", filename);
	return;
    }

    unlink(filename);		// remove any existing file by this name... (is this needed?)
    rep_ = new MemRep(filename, O_RDWR|O_CREAT, PROT_RDWR, MAP_SHARED,
		      size, owner, verbose);
}

/*
 * Constructor: accept a pointer to memory, do not modify this
 * reference (assume user looks after it).
 */
Mem::Mem(void *ptr, size_t size, int owner)
   : offset_(0), length_(0)
{
   rep_ = new MemRep(ptr, size, owner);
}

/*
 * destructor, detach and/or delete memory if needed
 */
Mem::~Mem()
{
  if (rep_ && --rep_->refcnt <= 0)
    delete rep_;
}


/*
 * assignment operator
 */
Mem& Mem::operator=(const Mem& m)
{
    if (m.rep_)
	m.rep_->refcnt++;		// protect against "shm = shm"
    if (rep_ && --rep_->refcnt <= 0)
	delete rep_;
    offset_ = m.offset_;
    length_ = m.length_;
    rep_ = m.rep_;
    return *this;
}


/*
 * force the memory to be shared (1) or not shared (0)
 */
int Mem::shared(int share)
{
    if (share  == shared())
	return 0;
    Mem m(length(), share, verbose());
    if (m.status() != 0)
	return m.status();
    memcpy(m.ptr(), ptr(), length());
    *this = m;
    return 0;
}


/*
 * remove all "owned" shared memory areas (should be called before exit)
 */
void Mem::cleanup()
{
    for (int i = 0; i < shmCount_; i++) {
	if (shmObjs_[i]->owner && shmObjs_[i]->status == 0) {
	    if (shmObjs_[i]->m_map && shmObjs_[i]->m_map->filename())
		unlink(shmObjs_[i]->m_map->filename());
	    else if (shmObjs_[i]->shmId > -1)
		shmctl(shmObjs_[i]->shmId, IPC_RMID, NULL);
	    shmObjs_[i]->owner = 0;
	}
    }
}

/*
 * external version, for use as a signal handler
 */
void Mem_cleanup(int)
{
    Mem::cleanup();
    _exit(0);
}
