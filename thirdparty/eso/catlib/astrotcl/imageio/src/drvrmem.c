/*  This file, drvrmem.c, contains driver routines for memory files.        */

/*  The FITSIO software was written by William Pence at the High Energy    */
/*  Astrophysic Science Archive Research Center (HEASARC) at the NASA      */
/*  Goddard Space Flight Center.  Users shall not, without prior written   */
/*  permission of the U.S. Government,  establish a claim to statutory     */
/*  copyright.  The Government and others acting on its behalf, shall have */
/*  a royalty-free, non-exclusive, irrevocable,  worldwide license for     */
/*  Government purposes to publish, distribute, translate, copy, exhibit,  */
/*  and perform such material.                                             */

#include <stdlib.h>
#include <string.h>
#include <stddef.h>  /* apparently needed to define size_t */
#include "fitsio2.h"

#define RECBUFLEN 1000
static char stdin_outfile[FLEN_FILENAME];

typedef struct    /* structure containing mem file structure */ 
{
    char **memaddrptr;   /* Pointer to memory address pointer; */
                         /* This may or may not point to memaddr. */
    char *memaddr;       /* Pointer to starting memory address; may */
                         /* not always be used, so use *memaddrptr instead */
    size_t *memsizeptr;  /* Pointer to the size of the memory allocation. */
                         /* This may or may not point to memsize. */
    size_t memsize;      /* Size of the memory allocation; this may not */
                         /* always be used, so use *memsizeptr instead. */
    size_t deltasize;    /* Suggested increment for reallocating memory */
    void *(*mem_realloc)(void *p, size_t newsize);  /* realloc function */
    size_t currentpos;   /* current file position, relative to start */
    size_t fitsfilesize; /* size of the FITS file (always <= *memsizeptr) */
} memdriver;

static memdriver memTable[NIOBUF];  /* allocate mem file handle tables */

/*--------------------------------------------------------------------------*/
int mem_init(void)
{
    int ii;

    for (ii = 0; ii < NIOBUF; ii++)  /* initialize all empty slots in table */
    {
       memTable[ii].memaddrptr = 0;
       memTable[ii].memaddr = 0;
    }
    return(0);
}
/*--------------------------------------------------------------------------*/
int mem_setoptions(int options)
{
  /* do something with the options argument, to stop compiler warning */
  options = 0;
  return(options);
}
/*--------------------------------------------------------------------------*/
int mem_getoptions(int *options)
{
  *options = 0;
  return(0);
}
/*--------------------------------------------------------------------------*/
int mem_getversion(int *version)
{
    *version = 10;
    return(0);
}
/*--------------------------------------------------------------------------*/
int mem_shutdown(void)
{
  return(0);
}
/*--------------------------------------------------------------------------*/
int mem_create(char *filename, int *handle)
/*
  Create a new empty memory file for subsequent writes.
  The file name is ignored in this case.
*/
{
    int status;

    /* initially allocate 1 FITS block = 2880 bytes */
    status = mem_createmem(2880L, handle);

    if (status)
    {
        ffpmsg("failed to create empty memory file (mem_create)");
        return(status);
    }

    return(0);
}
/*--------------------------------------------------------------------------*/
int mem_openmem(void **buffptr,   /* I - address of memory pointer          */
                size_t *buffsize, /* I - size of buffer, in bytes           */
                size_t deltasize, /* I - increment for future realloc's     */
                void *(*memrealloc)(void *p, size_t newsize),  /* function  */
                int *handle)
/* 
  lowest level routine to open a pre-existing memory file.
*/
{
    int ii;

    *handle = -1;
    for (ii = 0; ii < NIOBUF; ii++)  /* find empty slot in handle table */
    {
        if (memTable[ii].memaddrptr == 0)
        {
            *handle = ii;
            break;
        }
    }
    if (*handle == -1)
       return(TOO_MANY_FILES);    /* too many files opened */

    memTable[ii].memaddrptr = (char **) buffptr; /* pointer to start addres */
    memTable[ii].memsizeptr = buffsize;     /* allocated size of memory */
    memTable[ii].deltasize = deltasize;     /* suggested realloc increment */
    memTable[ii].fitsfilesize = *buffsize;  /* size of FITS file (upper limit) */
    memTable[ii].currentpos = 0;            /* at beginning of the file */
    memTable[ii].mem_realloc = memrealloc;  /* memory realloc function */
    return(0);
}
/*--------------------------------------------------------------------------*/
int mem_createmem(size_t msize, int *handle)
/* 
  lowest level routine to allocate a memory file.
*/
{
    int ii;

    *handle = -1;
    for (ii = 0; ii < NIOBUF; ii++)  /* find empty slot in handle table */
    {
        if (memTable[ii].memaddrptr == 0)
        {
            *handle = ii;
            break;
        }
    }
    if (*handle == -1)
       return(TOO_MANY_FILES);    /* too many files opened */

    /* use the internally allocated memaddr and memsize variables */
    memTable[ii].memaddrptr = &memTable[ii].memaddr;
    memTable[ii].memsizeptr = &memTable[ii].memsize;

    /* allocate initial block of memory for the file */
    if (msize > 0)
    {
        memTable[ii].memaddr = malloc(msize); 
        if ( !(memTable[ii].memaddr) )
        {
            ffpmsg("malloc of initial memory failed (mem_createmem)");
            return(FILE_NOT_OPENED);
        }
    }

    /* set initial state of the file */
    memTable[ii].memsize = msize;
    memTable[ii].deltasize = 2880;
    memTable[ii].fitsfilesize = 0;
    memTable[ii].currentpos = 0;
    memTable[ii].mem_realloc = realloc;
    return(0);
}
/*--------------------------------------------------------------------------*/
int mem_truncate(int handle, long filesize)
/*
  truncate the file to a new smaller size
*/
{
    char *ptr;

    /* call the memory reallocation function, if defined */
    if ( memTable[handle].mem_realloc )
    {
        ptr = (memTable[handle].mem_realloc)(
                                *(memTable[handle].memaddrptr),
                                 filesize);
        if (!ptr)
        {
            ffpmsg("Failed to reallocate memory (mem_truncate)");
            return(MEMORY_ALLOCATION);
        }

        *(memTable[handle].memaddrptr) = ptr;
        *(memTable[handle].memsizeptr) = filesize;
    }

    memTable[handle].fitsfilesize = filesize;
    return(0);
}
/*--------------------------------------------------------------------------*/
int stdin_checkfile(char *urltype, char *infile, char *outfile)
/*
   do any special case checking when opening a file on the stdin stream
*/
{
    if (strlen(outfile))
    {
        strcpy(stdin_outfile,outfile); /* an output file is specified */
	strcpy(urltype,"stdinfile://");
    }
    else
        *stdin_outfile = '\0';  /* no output file was specified */

    return(0);
}
/*--------------------------------------------------------------------------*/
int stdin_open(char *filename, int rwmode, int *handle)
/*
  open a FITS file from the stdin file stream by copying it into memory
  The file name is ignored in this case.
*/
{
    int status = 0;

    if (*stdin_outfile)
    {
      /* copy the stdin stream to the specified disk file then open the file */

      /* Create the output file */
      status =  file_create(stdin_outfile,handle);

      if (status)
      {
        ffpmsg("Unable to create output file to copy stdin (stdin_open):");
        ffpmsg(stdin_outfile);
        return(status);
      }
 
      /* copy the whole stdin stream to the file */
      status = stdin2file(*handle);
      file_close(*handle);

      if (status)
      {
        ffpmsg("failed to copy stdin to file (stdin_open)");
        ffpmsg(stdin_outfile);
        return(status);
      }

      /* reopen file with proper rwmode attribute */
      status = file_open(stdin_outfile, rwmode, handle);
    }
    else
    {
      /* copy the stdin stream into memory then open file in memory */

      if (rwmode != READONLY)
      {
        ffpmsg("cannot open stdin with WRITE access");
        return(FILE_NOT_OPENED);
      }

      status = mem_createmem(2880L, handle);

      if (status)
      {
        ffpmsg("failed to create empty memory file (stdin_open)");
        return(status);
      }
 
      /* copy the whole stdin stream into memory */
      status = stdin2mem(*handle);

      if (status)
      {
        ffpmsg("failed to copy stdin into memory (stdin_open)");
        free(memTable[*handle].memaddr);
      }
    }

    return(status);
}
/*--------------------------------------------------------------------------*/
int stdin2mem(int hd)  /* handle number */
/*
  Copy the stdin stream into memory.  Fill whatever amount of memory
  has already been allocated, then realloc more memory if necessary.
*/
{
    size_t nread, memsize, filesize, delta;
    char *memptr;
    char simple[] = "SIMPLE";
    int c, ii, jj;

    memptr = *memTable[hd].memaddrptr;
    memsize = *memTable[hd].memsizeptr;
    delta = memTable[hd].deltasize;

    filesize = 0;
    ii = 0;

    for(jj = 0; (c = fgetc(stdin)) != EOF && jj < 2000; jj++)
    {
       /* Skip over any garbage at the beginning of the stdin stream by */
       /* reading 1 char at a time, looking for 'S', 'I', 'M', 'P', 'L', 'E' */
       /* Give up if not found in the first 2000 characters */

       if (c == simple[ii])
       {
           ii++;
           if (ii == 6)   /* found the complete string? */
           {
              memcpy(memptr, simple, 6);  /* copy "SIMPLE" to buffer */
              filesize = 6;
              break;
           }
       }
       else
          ii = 0;  /* reset search to beginning of the string */
    }

   if (filesize == 0)
   {
       ffpmsg("Couldn't find the string 'SIMPLE' in the stdin stream");
       return(FILE_NOT_OPENED);
   }

    /* fill up the remainder of the initial memory allocation */
    nread = fread(memptr + 6, 1, memsize - 6, stdin);
    nread += 6;  /* add in the 6 characters in 'SIMPLE' */

    if (nread < memsize)    /* reached the end? */
    {
       memTable[hd].fitsfilesize = nread;
       return(0);
    }

    filesize = nread;

    while (1)
    {
        /* allocate memory for another FITS block */
        memptr = realloc(memptr, memsize + delta);

        if (!memptr)
        {
            ffpmsg("realloc failed while copying stdin (stdin2mem)");
            return(MEMORY_ALLOCATION);
        }
        memsize += delta;

        /* read another FITS block */
        nread = fread(memptr + filesize, 1, delta, stdin);

        filesize += nread;

        if (nread < delta)    /* reached the end? */
           break;
    }

     memTable[hd].fitsfilesize = filesize;
    *memTable[hd].memaddrptr = memptr;
    *memTable[hd].memsizeptr = memsize;

    return(0);
}
/*--------------------------------------------------------------------------*/
int stdin2file(int handle)  /* handle number */
/*
  Copy the stdin stream to a file.  .
*/
{
    size_t nread = 0;
    char simple[] = "SIMPLE";
    int c, ii, jj, status = 0;
    char recbuf[RECBUFLEN];

    ii = 0;
    for(jj = 0; (c = fgetc(stdin)) != EOF && jj < 2000; jj++)
    {
       /* Skip over any garbage at the beginning of the stdin stream by */
       /* reading 1 char at a time, looking for 'S', 'I', 'M', 'P', 'L', 'E' */
       /* Give up if not found in the first 2000 characters */

       if (c == simple[ii])
       {
           ii++;
           if (ii == 6)   /* found the complete string? */
           {
              memcpy(recbuf, simple, 6);  /* copy "SIMPLE" to buffer */
              break;
           }
       }
       else
          ii = 0;  /* reset search to beginning of the string */
    }

   if (ii != 6)
   {
       ffpmsg("Couldn't find the string 'SIMPLE' in the stdin stream");
       return(FILE_NOT_OPENED);
   }

    /* fill up the remainder of the buffer */
    nread = fread(recbuf + 6, 1, RECBUFLEN - 6, stdin);
    nread += 6;  /* add in the 6 characters in 'SIMPLE' */

    status = file_write(handle, recbuf, nread);
    if (status)
       return(status);

    /* copy the rest of stdin stream */
    while(0 != (nread = fread(recbuf,1,RECBUFLEN, stdin)))
    {
        status = file_write(handle, recbuf, nread);
        if (status)
           return(status);
    }

    return(status);
}
/*--------------------------------------------------------------------------*/
int stdout_close(int handle)
/*
  copy the memory file to stdout, then free the memory
*/
{
    int status = 0;

    /* copy from memory to standard out */
    if(fwrite(memTable[handle].memaddr, 1,
              memTable[handle].fitsfilesize, stdout) !=
              memTable[handle].fitsfilesize )
    {
                ffpmsg("failed to copy memory file to stdout (stdout_close)");
                status = WRITE_ERROR;
    }

    free( memTable[handle].memaddr );   /* free the memory */
    memTable[handle].memaddrptr = 0;
    memTable[handle].memaddr = 0;
    return(status);
}
/*--------------------------------------------------------------------------*/
int mem_compress_open(char *filename, int rwmode, int *hdl)
/*
  This routine opens the compressed diskfile and creates an empty memory
  buffer with an appropriate size, then calls mem_uncompress2mem.
*/
{
    FILE *diskfile;
    int status;
    unsigned char buffer[4];
    size_t finalsize;
    char *ptr;

    /* open the compressed disk file */
    status = file_openfile(filename, READONLY, &diskfile);
    if (status)
    {
        ffpmsg("failed to open compressed disk file (compress_open)");
        ffpmsg(filename);
        return(status);
    }

    if (fread(buffer, 1, 2, diskfile) != 2)  /* read 2 bytes */
    {
        fclose(diskfile);
        return(READ_ERROR);
    }

    if (memcmp(buffer, "\037\213", 2) == 0)  /* GZIP */
    {
        /* the uncompressed file size is give at the end of the file */

        fseek(diskfile, 0, 2);            /* move to end of file */
        fseek(diskfile, -4L, 1);          /* move back 4 bytes */
        fread(buffer, 1, 4L, diskfile);   /* read 4 bytes */

        /* have to worry about integer byte order */
	finalsize  = buffer[0];
	finalsize |= buffer[1] << 8;
	finalsize |= buffer[2] << 16;
	finalsize |= buffer[3] << 24;
    }
    else if (memcmp(buffer, "\120\113", 2) == 0)   /* PKZIP */
    {
        /* the uncompressed file size is give at byte 22 the file */

        fseek(diskfile, 22L, 0);            /* move to byte 22 */
        fread(buffer, 1, 4L, diskfile);   /* read 4 bytes */

        /* have to worry about integer byte order */
	finalsize  = buffer[0];
	finalsize |= buffer[1] << 8;
	finalsize |= buffer[2] << 16;
	finalsize |= buffer[3] << 24;
    }
    else if (memcmp(buffer, "\037\036", 2) == 0)  /* PACK */
        finalsize = 0;  /* for most methods we can't determine final size */
    else if (memcmp(buffer, "\037\235", 2) == 0)  /* LZW */
        finalsize = 0;  /* for most methods we can't determine final size */
    else if (memcmp(buffer, "\037\240", 2) == 0)  /* LZH */
        finalsize = 0;  /* for most methods we can't determine final size */
    else
    {
        /* not a compressed file; this should never happen */
        fclose(diskfile);
        return(1);
    }

    if (finalsize == 0)  /* estimate uncompressed file size */
    {
            fseek(diskfile, 0, 2);   /* move to end of the compressed file */
            finalsize = ftell(diskfile);  /* position = size of file */
            finalsize = finalsize * 3;   /* assume factor of 3 compression */
    }

    fseek(diskfile, 0, 0);   /* move back to beginning of file */

    /* create a memory file big enough (hopefully) for the uncompressed file */
    status = mem_createmem(finalsize, hdl);
    if (status)
    {
        fclose(diskfile);
        ffpmsg("failed to create empty memory file (compress_open)");
        return(status);
    }

    /* uncompress file into memory */
    status = mem_uncompress2mem(filename, diskfile, *hdl);

    fclose(diskfile);

    if (status)
    {
        mem_close_free(*hdl);   /* free up the memory */
        ffpmsg("failed to uncompress file into memory (compress_open)");
        return(status);
    }

    /* if we allocated too much memory initially, then free it */
    if (*(memTable[*hdl].memsizeptr) > (memTable[*hdl].fitsfilesize + 256L) ) 
    {
        ptr = realloc(*(memTable[*hdl].memaddrptr), 
                       memTable[*hdl].fitsfilesize);
        if (!ptr)
        {
            ffpmsg("Failed to reduce size of allocated memory (compress_open)");
            return(MEMORY_ALLOCATION);
        }

        *(memTable[*hdl].memaddrptr) = ptr;
        *(memTable[*hdl].memsizeptr) = memTable[*hdl].fitsfilesize;
    }

    return(0);
}
/*--------------------------------------------------------------------------*/
int mem_iraf_open(char *filename, int rwmode, int *hdl)
/*
  This routine creates an empty memory buffer, then calls iraf2mem to
  open the IRAF disk file and convert it to a FITS file in memeory.
*/
{
    int status;
    size_t filesize = 0;

    /* create a memory file with size = 0 for the FITS converted IRAF file */
    status = mem_createmem(filesize, hdl);
    if (status)
    {
        ffpmsg("failed to create empty memory file (mem_iraf_open)");
        return(status);
    }

    /* convert the iraf file into a FITS file in memory */
    status = iraf2mem(filename, memTable[*hdl].memaddrptr,
                      memTable[*hdl].memsizeptr, &filesize, &status);

    if (status)
    {
        mem_close_free(*hdl);   /* free up the memory */
        ffpmsg("failed to convert IRAF file into memory (mem_iraf_open)");
        return(status);
    }

    memTable[*hdl].currentpos = 0;           /* save starting position */
    memTable[*hdl].fitsfilesize=filesize;   /* and initial file size  */

    return(0);
}
/*--------------------------------------------------------------------------*/
int mem_uncompress2mem(char *filename, FILE *diskfile, int hdl)
{
/*
  lower level routine to uncompress a file into memory.  The file
  has already been opened and the memory buffer has been allocated.
*/

  size_t finalsize;
  int status;
  /* uncompress file into memory */
  status = 0;
  uncompress2mem(filename, diskfile,
		 memTable[hdl].memaddrptr,   /* pointer to memory address */
		 memTable[hdl].memsizeptr,   /* pointer to size of memory */
		 realloc,                     /* reallocation function */
		 &finalsize, &status);        /* returned file size nd status*/
  memTable[hdl].currentpos = 0;           /* save starting position */
  memTable[hdl].fitsfilesize=finalsize;   /* and initial file size  */
  return status;
}
/*--------------------------------------------------------------------------*/
int mem_size(int handle, long *filesize)
/*
  return the size of the file; only called when the file is first opened
*/
{
    *filesize = memTable[handle].fitsfilesize;

    return(0);
}
/*--------------------------------------------------------------------------*/
int mem_close_free(int handle)
/*
  close the file and free the memory.
*/
{
    free( *(memTable[handle].memaddrptr) );

    memTable[handle].memaddrptr = 0;
    memTable[handle].memaddr = 0;
    return(0);
}
/*--------------------------------------------------------------------------*/
int mem_close_keep(int handle)
/*
  close the memory file but do not free the memory.
*/
{
    memTable[handle].memaddrptr = 0;
    memTable[handle].memaddr = 0;
    return(0);
}
/*--------------------------------------------------------------------------*/
int mem_seek(int handle, long offset)
/*
  seek to position relative to start of the file.
*/
{
    if (offset > (long) memTable[handle].fitsfilesize )
        return(END_OF_FILE);

    memTable[handle].currentpos = offset;
    return(0);
}
/*--------------------------------------------------------------------------*/
int mem_read(int hdl, void *buffer, long nbytes)
/*
  read bytes from the current position in the file
*/
{
    if (memTable[hdl].currentpos + nbytes > memTable[hdl].fitsfilesize)
        return(END_OF_FILE);

    memcpy(buffer,
           *(memTable[hdl].memaddrptr) + memTable[hdl].currentpos,
           nbytes);

    memTable[hdl].currentpos += nbytes;
    return(0);
}
/*--------------------------------------------------------------------------*/
int mem_write(int hdl, void *buffer, long nbytes)
/*
  write bytes at the current position in the file
*/
{
    size_t newsize;
    char *ptr;

    if (memTable[hdl].currentpos + nbytes > *(memTable[hdl].memsizeptr))
    {
               
        if (!(memTable[hdl].mem_realloc))
        {
            ffpmsg("realloc function not defined (mem_write)");
            return(WRITE_ERROR);
        }

        /*
          Attempt to reallocate additional memory:
          the memory buffer size is incremented by the larger of:
             1 FITS block (2880 bytes) or
             the defined 'deltasize' parameter
         */

        newsize = maxvalue(
            (((memTable[hdl].currentpos + nbytes - 1) / 2880) + 1) * 2880,
            *(memTable[hdl].memsizeptr) + memTable[hdl].deltasize);

        /* call the realloc function */
        ptr = (memTable[hdl].mem_realloc)(
                                    *(memTable[hdl].memaddrptr),
                                     newsize);
        if (!ptr)
        {
            ffpmsg("Failed to reallocate memory (mem_write)");
            return(MEMORY_ALLOCATION);
        }

        *(memTable[hdl].memaddrptr) = ptr;
        *(memTable[hdl].memsizeptr) = newsize;
    }

    /* now copy the bytes from the buffer into memory */
    memcpy( *(memTable[hdl].memaddrptr) + memTable[hdl].currentpos,
             buffer,
             nbytes);

    memTable[hdl].currentpos += nbytes;
    memTable[hdl].fitsfilesize =
               maxvalue(memTable[hdl].fitsfilesize,
                        memTable[hdl].currentpos);
    return(0);
}
