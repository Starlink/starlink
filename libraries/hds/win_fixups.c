/*
 * Purpose:
 *    Home for routines that fixup problems with the MINGW (and
 *    probably CYGWIN) windows ports. Currently these are to do with
 *    supporting inode-like functionality.
 *
 * Authors:
 *    PWD: Peter W. Draper (Starlink - Durham University)
 *
 * History:
 *    16-DEC-2002 (PWD):
 *       Original version.
 */
#include <stdio.h>
#include <sys/types.h>

#if !defined __MINGW32__

/* Not used */
void win_get_inodes( const char *fns, ino_t *st_ino, dev_t *st_rdev ){}

#else

#include <windows.h>
#include <lmerr.h>

/**
 * Get two values that (together with the device number), uniquely
 * identify a file. Taken together these values create the windows
 * equivalent of UNIX inodes. If this fails for any reason a 0 is
 * returned, otherwise a 1 is returned. On failure only the st_rdev
 * value is set to zero, normally the st_ino (if this is the result 
 * of a call to stat(), may have a sensible value).
 */
int win_get_inodes( const char *fns, ino_t *st_ino, dev_t *st_rdev )
{
    HANDLE fh;
    BY_HANDLE_FILE_INFORMATION info;
    DWORD error;
    int result = 1;

    /* Open file, use FILE_SHARE_READ and FILE_SHARE_WRITE as this
     * seems to be needed for win98 (but not for XP) */
    fh = CreateFile( fns, 0, FILE_SHARE_READ | FILE_SHARE_WRITE,
                     NULL, OPEN_EXISTING, 0, NULL );
    if ( fh != INVALID_HANDLE_VALUE ) {

        if ( GetFileInformationByHandle( fh, &info ) ) {
            *st_ino = info.nFileIndexLow;
            *st_rdev = info.nFileIndexHigh;
        }
        else {
            *st_rdev = 0;
            result = 0;
        }
        CloseHandle( fh );
    }
    else {
        *st_rdev = 0;
        result = 0;
    }
    return result;
}
#endif
