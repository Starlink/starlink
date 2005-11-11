/*  This file, drvrfile.c contains driver routines for disk files.         */

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
#include "fitsio2.h"

#ifdef HAVE_FTRUNCATE
#include <unistd.h>      /* contains prototype of UNIX file truncate fn  */
#endif

#define IO_SEEK 0        /* last file I/O operation was a seek */
#define IO_READ 1        /* last file I/O operation was a read */
#define IO_WRITE 2       /* last file I/O operation was a write */

static char file_outfile[FLEN_FILENAME];

typedef struct    /* structure containing disk file structure */ 
{
    FILE *fileptr;
    long currentpos;
    int last_io_op;
} diskdriver;

static diskdriver handleTable[NIOBUF];  /* allocate diskfile handle tables */

/*--------------------------------------------------------------------------*/
int file_init(void)
{
    int ii;

    for (ii = 0; ii < NIOBUF; ii++)  /* initialize all empty slots in table */
    {
       handleTable[ii].fileptr = 0;
    }
    return(0);
}
/*--------------------------------------------------------------------------*/
int file_setoptions(int options)
{
  /* do something with the options argument, to stop compiler warning */
  options = 0;
  return(options);
}
/*--------------------------------------------------------------------------*/
int file_getoptions(int *options)
{
  *options = 0;
  return(0);
}
/*--------------------------------------------------------------------------*/
int file_getversion(int *version)
{
    *version = 10;
    return(0);
}
/*--------------------------------------------------------------------------*/
int file_shutdown(void)
{
  return(0);
}
/*--------------------------------------------------------------------------*/
int file_open(char *filename, int rwmode, int *handle)
{
    FILE *diskfile;
    int copyhandle, ii, status;
    char recbuf[2880];
    size_t nread;

    /*
       if an output filename has been specified as part of the input
       file, as in "inputfile.fits(outputfile.fit)" then we have to
       create the output file, copy the input to it, then reopen the
       the new copy.
    */

    if (*file_outfile)
    {
      /* open the original file, with readonly access */
      status = file_openfile(filename, READONLY, &diskfile);
      if (status)
        return(status);
 
      /* create the output file */
      status =  file_create(file_outfile,handle);
      if (status)
      {
        ffpmsg("Unable to create output file for copy of input file:");
        ffpmsg(file_outfile);
        return(status);
      }

      /* copy the file from input to output */
      while(0 != (nread = fread(recbuf,1,2880, diskfile)))
      {
        status = file_write(*handle, recbuf, nread);
        if (status)
           return(status);
      }

      /* close both files */
      fclose(diskfile);
      copyhandle = *handle;
      file_close(*handle);
      *handle = copyhandle;  /* reuse the old file handle */

      /* reopen the new copy, with correct rwmode */
      status = file_openfile(file_outfile, rwmode, &diskfile);

    }
    else
    {
      *handle = -1;
      for (ii = 0; ii < NIOBUF; ii++)  /* find empty slot in table */
      {
        if (handleTable[ii].fileptr == 0)
        {
            *handle = ii;
            break;
        }
      }

      if (*handle == -1)
       return(TOO_MANY_FILES);    /* too many files opened */

      /*open the file */
      status = file_openfile(filename, rwmode, &diskfile);
    }

    handleTable[*handle].fileptr = diskfile;
    handleTable[*handle].currentpos = 0;
    handleTable[*handle].last_io_op = IO_SEEK;

    return(status);
}
/*--------------------------------------------------------------------------*/
int file_openfile(char *filename, int rwmode, FILE **diskfile)
/*
   lowest level routine to physically open a disk file
*/
{
    char mode[4];

    if (rwmode == READWRITE)
    {
          strcpy(mode, "r+b");    /* open existing file with read-write */
    }
    else
    {
          strcpy(mode, "rb");     /* open existing file readonly */
    }

#if MACHINE == ALPHAVMS || MACHINE == VAXVMS
        /* specify VMS record structure: fixed format, 2880 byte records */
        /* but force stream mode access to enable random I/O access      */
    *diskfile = fopen(filename, mode, "rfm=fix", "mrs=2880", "ctx=stm"); 
#else
    *diskfile = fopen(filename, mode); 
#endif

    if (!(*diskfile))           /* couldn't open file */
    {
            return(FILE_NOT_OPENED); 
    }
    return(0);
}
/*--------------------------------------------------------------------------*/
int file_create(char *filename, int *handle)
{
    FILE *diskfile;
    int ii;
    char mode[4];

    *handle = -1;
    for (ii = 0; ii < NIOBUF; ii++)  /* find empty slot in table */
    {
        if (handleTable[ii].fileptr == 0)
        {
            *handle = ii;
            break;
        }
    }
    if (*handle == -1)
       return(TOO_MANY_FILES);    /* too many files opened */

    strcpy(mode, "w+b");    /* open existing file with read-write */

    diskfile = fopen(filename, "r"); /* does file already exist? */

    if (diskfile)
    {
        fclose(diskfile);         /* close file and exit with error */
        return(FILE_NOT_CREATED); 
    }

#if MACHINE == ALPHAVMS || MACHINE == VAXVMS
        /* specify VMS record structure: fixed format, 2880 byte records */
        /* but force stream mode access to enable random I/O access      */
    diskfile = fopen(filename, mode, "rfm=fix", "mrs=2880", "ctx=stm"); 
#else
    diskfile = fopen(filename, mode); 
#endif

    if (!(diskfile))           /* couldn't create file */
    {
            return(FILE_NOT_CREATED); 
    }

    handleTable[ii].fileptr = diskfile;
    handleTable[ii].currentpos = 0;
    handleTable[ii].last_io_op = IO_SEEK;

    return(0);
}
/*--------------------------------------------------------------------------*/
int file_truncate(int handle, long filesize)
/*
  truncate the diskfile to a new smaller size
*/
{

#ifdef HAVE_FTRUNCATE
    int fdesc;

    fdesc = fileno(handleTable[handle].fileptr);
    ftruncate(fdesc, filesize);

    handleTable[handle].currentpos = filesize;
    handleTable[handle].last_io_op = IO_WRITE;

#endif

    return(0);
}
/*--------------------------------------------------------------------------*/
int file_size(int handle, long *filesize)
/*
  return the size of the file in bytes
*/
{
    long position;
    FILE *diskfile;

    diskfile = handleTable[handle].fileptr;

    position = ftell(diskfile);      /* save current postion */
    if (position < 0)
        return(SEEK_ERROR);

    if (fseek(diskfile, 0, 2) != 0)  /* move to end of the existing file */
        return(SEEK_ERROR);

    *filesize = ftell(diskfile);     /* position = size of file */
    if (*filesize < 0)
        return(SEEK_ERROR);

    if (fseek(diskfile, position, 0) != 0) /* move back to orig. position */
        return(SEEK_ERROR);

    return(0);
}
/*--------------------------------------------------------------------------*/
int file_close(int handle)
/*
  close the file
*/
{
    
    if (fclose(handleTable[handle].fileptr) )
        return(FILE_NOT_CLOSED);

    handleTable[handle].fileptr = 0;
    return(0);
}
/*--------------------------------------------------------------------------*/
int file_remove(char *filename)
/*
  delete the file from disk
*/
{
    remove(filename);
    return(0);
}
/*--------------------------------------------------------------------------*/
int file_flush(int handle)
/*
  flush the file
*/
{
    if (fflush(handleTable[handle].fileptr) )
        return(WRITE_ERROR);

    return(0);
}
/*--------------------------------------------------------------------------*/
int file_seek(int handle, long offset)
/*
  seek to position relative to start of the file
*/
{
    if (fseek(handleTable[handle].fileptr, offset, 0 ) )
        return(SEEK_ERROR);

    handleTable[handle].currentpos = offset;
    return(0);
}
/*--------------------------------------------------------------------------*/
int file_read(int hdl, void *buffer, long nbytes)
/*
  read bytes from the current position in the file
*/
{
    if (handleTable[hdl].last_io_op == IO_WRITE)
    {
      if (fseek(handleTable[hdl].fileptr, handleTable[hdl].currentpos, 0 ))
        return(READ_ERROR);
    }
  
    if( (long) fread(buffer, 1, nbytes, handleTable[hdl].fileptr) != nbytes)
        return(READ_ERROR);

    handleTable[hdl].currentpos += nbytes;
    handleTable[hdl].last_io_op = IO_READ;
    return(0);
}
/*--------------------------------------------------------------------------*/
int file_write(int hdl, void *buffer, long nbytes)
/*
  write bytes at the current position in the file
*/
{
    if (handleTable[hdl].last_io_op == IO_READ) 
    {
      if (fseek(handleTable[hdl].fileptr, handleTable[hdl].currentpos, 0 ))
         return(WRITE_ERROR);
    }
  
    if((long) fwrite(buffer, 1, nbytes, handleTable[hdl].fileptr) != nbytes)
        return(WRITE_ERROR);

    handleTable[hdl].currentpos += nbytes;
    handleTable[hdl].last_io_op = IO_WRITE;
    return(0);
}
/*--------------------------------------------------------------------------*/
int file_compress_open(char *filename, int rwmode, int *hdl)
/*
  This routine opens the compressed diskfile by creating a new uncompressed
  file then opening it.  The input file name (the name of the compressed
  file) gets replaced with the name of the uncompressed file, which is
  initially stored in the global file_outfile string.   file_outfile
  then gets set to a null string.
*/
{
    FILE *indiskfile, *outdiskfile;
    int status, clobber = 0;
    char *cptr;

    /* open the compressed disk file */
    status = file_openfile(filename, READONLY, &indiskfile);
    if (status)
    {
        ffpmsg("failed to open compressed disk file (file_compress_open)");
        ffpmsg(filename);
        return(status);
    }

    /* name of the output uncompressed file is stored in the */
    /* global variable called 'file_outfile'.                */

    cptr = file_outfile;
    if (*cptr == '!')
    {
        /* clobber any existing file with the same name */
        clobber = 1;
        cptr++;
        remove(cptr);
    }
    else
    {
        outdiskfile = fopen(file_outfile, "r"); /* does file already exist? */

        if (outdiskfile)
        {
          ffpmsg("uncompressed file already exists: (file_compress_open)");
          ffpmsg(file_outfile);
          fclose(outdiskfile);         /* close file and exit with error */
          return(FILE_NOT_CREATED); 
        }
    }

    outdiskfile = fopen(cptr, "w+b"); /* create new file */
    if (!outdiskfile)
    {
        ffpmsg("could not create uncompressed file: (file_compress_open)");
        ffpmsg(file_outfile);
        return(FILE_NOT_CREATED); 
    }

    /* uncompress file into another file */
    uncompress2file(filename, indiskfile, outdiskfile, &status);
    fclose(indiskfile);
    fclose(outdiskfile);

    if (status)
    {
        ffpmsg("error in file_compress_open: failed to uncompressed file:");
        ffpmsg(filename);
        ffpmsg(" into new output file:");
        ffpmsg(file_outfile);
        return(status);
    }

    strcpy(filename, cptr);  /* switch the names */
    file_outfile[0] = '\0';

    status = file_open(filename, rwmode, hdl);

    return(status);
}
/*--------------------------------------------------------------------------*/
int file_is_compressed(char *filename) /* I - FITS file name          */
/*
  Test if the disk file is compressed.  Returns 1 if compressed, 0 if not.
  This may modify the filename string by appending a compression suffex.
*/
{
    FILE *diskfile;
    unsigned char buffer[2];
    char tmpfilename[FLEN_FILENAME];

    /* Open file.  Try various suffix combinations */  
    if (file_openfile(filename, 0, &diskfile))
    {
      strcpy(tmpfilename,filename);
      strcat(filename,".gz");
      if (file_openfile(filename, 0, &diskfile))
      {
        strcpy(filename, tmpfilename);
        strcat(filename,".Z");
        if (file_openfile(filename, 0, &diskfile))
        {
          strcpy(filename, tmpfilename);
          strcat(filename,".z");   /* it's often lower case on CDROMs */
          if (file_openfile(filename, 0, &diskfile))
          {
            strcpy(filename, tmpfilename);
            strcat(filename,".zip");
            if (file_openfile(filename, 0, &diskfile))
            {
              strcpy(filename, tmpfilename);
              strcat(filename,"-z");      /* VMS suffix */
              if (file_openfile(filename, 0, &diskfile))
              {
                strcpy(filename, tmpfilename);
                strcat(filename,"-gz");    /* VMS suffix */
                if (file_openfile(filename, 0, &diskfile))
                {
                  strcpy(filename,tmpfilename);  /* restore original name */
                  return(0);    /* file not found */
                }
              }
            }
          }
        }
      }
    }

    if (fread(buffer, 1, 2, diskfile) != 2)  /* read 2 bytes */
    {
        fclose(diskfile);   /* error reading file so just return */
        return(0);
    }

    fclose(diskfile);

       /* see if the 2 bytes have the magic values for a compressed file */
    if ( (memcmp(buffer, "\037\213", 2) == 0) ||  /* GZIP  */
         (memcmp(buffer, "\120\113", 2) == 0) ||  /* PKZIP */
         (memcmp(buffer, "\037\036", 2) == 0) ||  /* PACK  */
         (memcmp(buffer, "\037\235", 2) == 0) ||  /* LZW   */
         (memcmp(buffer, "\037\240", 2) == 0) )   /* LZH   */
        {
            return(1);  /* this is a compressed file */
        }
    else
        {
            return(0);  /* not a compressed file */
        }
}
/*--------------------------------------------------------------------------*/
int file_checkfile (char *urltype, char *infile, char *outfile) 
{
    /* special case: if file:// driver, check if the file is compressed */
    if ( file_is_compressed(infile) )
    {
      /* if output file has been specified, save the name for future use: */
      /* This is the name of the uncompressed file to be created on disk. */
      if (strlen(outfile))
      {
        strcpy(urltype, "compressfile://");  /* use special driver */
        strcpy(file_outfile, outfile); /* an output file is specified */
      }
      else
      {
        /* uncompress the file in memory */
        strcpy(urltype, "compress://");  /* use special driver */
        *file_outfile = '\0';  /* no output file was specified */
      }
    }

    return 0;
}



