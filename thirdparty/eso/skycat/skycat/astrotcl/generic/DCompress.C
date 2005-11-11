 /*
 * E.S.O. - VLT project/ESO Archive 
 * $Id: Compress.C,v 1.4 2005/02/02 01:43:04 brighton Exp $
 *
 * Compress.C - method definitions for class Compress
 *             (based on Archive/CADC press routines)
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  2 Aug 96  Created
 */
static const char* const rcsId="@(#) $Id: Compress.C,v 1.4 2005/02/02 01:43:04 brighton Exp $";


#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <iostream>
#include <fstream>
#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <unistd.h>
#include "error.h"
#include "Mem.h"
#include "Compress.h"

extern "C" {
#include "gen_types.h"
#include "press.h"
}

// must correspond to enum CompressType
static char* types_[] = {"NONE", "UCMP", "HCMP", "ULDA", "GZIP"};


/* 
 * local util to report an error
 */
static int compress_error(int compress_flag)
{
    if (compress_flag)
	return error("could not compress data: ", pr_msg);
    else
	return error("could not decompress data: ", pr_msg);
}


/*
 * compress (or decompress) from read_fd and write results to write_fd
 * If compress_flag is true, compress, otherwise decompress the file.
 */
int Compress::compress(int read_fd, int write_fd, CompressType ctype, 
		       int compress_flag)
{
    if (ctype == NO_COMPRESS) {
	// no compression ? just copy
	int n;
	char buf[8*1024];
	while((n = read(read_fd, buf, sizeof(buf))) > 0) {
	    write(write_fd, buf, n);
	}
	return 0;
    }

    char* type = types_[ctype];
    int status = (compress_flag ? 
		  press_f2f(read_fd, write_fd, type) :
		  unpress_f2f(read_fd, write_fd, type));

    if (status != 0) 
	return compress_error(compress_flag);

    return 0;
}


/*
 * compress (or decompress) infile using the given compress type and
 * put the results in outfile.
 * If compress_flag is true, compress, otherwise decompress the file.
 * If mmap_flag is true, use mmap to map the file to memory.
 *
 * Note: we can just open the file and use the fd, but the "press" C 
 * routines do unbuffered I/O on each char, which is slow. We can
 * mmap the file to memory and use the "mem to mem" version to improve
 * speed somewhat...
 */
int Compress::compress(const char* infile, const char* outfile, CompressType ctype, 
		       int compress_flag, int mmap_flag)
{
    if (ctype == NO_COMPRESS)
	return 0;

    int status = 0;
    char* type = types_[ctype];
    
    int write_fd = open(outfile, O_WRONLY|O_CREAT|O_TRUNC, 0777);
    if (write_fd < 0 ) {
	return sys_error("can't create output file: ", outfile);
    }

    if (mmap_flag) {
	// map input file to memory
	Mem inbuf(infile);
	if (inbuf.status() != 0) {
	    close(write_fd);
	    return ERROR;
	}

	unsigned char* outbuf = NULL;
	int outsize = inbuf.size();
	int factor = 2;		// estimated comp/decomp factor
    
	if (compress_flag) {
	    outsize /= factor;
	    status = press_m2m((unsigned char*)inbuf.ptr(), inbuf.size(), &outbuf, &outsize, type);
	}
	else {
	    outsize *= factor;
	    status = unpress_m2m((unsigned char*)inbuf.ptr(), inbuf.size(), &outbuf, &outsize, type);
	}
	if (status == 0) {
	    if (write(write_fd, outbuf, outsize) != outsize)
		status = 1;
	    close(write_fd);
	    free(outbuf);
	    if (status != 0)
		return sys_error("error writing file: ", outfile);
	}
    }
    else {
	// use standard read/write sys calls
	int read_fd = open(infile, O_RDONLY);
	if (read_fd < 0 ) {
	    close(write_fd);
	    return sys_error("can't open file: ", infile);
	}

	status = (compress_flag ? 
		  press_f2f(read_fd, write_fd, type) :
		  unpress_f2f(read_fd, write_fd, type));
	close(read_fd);
	close(write_fd);
    }

    if (status != 0) 
	return compress_error(compress_flag);

    return 0;
}


/*
 * compress (decompress) the file in place using the given compress type.
 * If compress_flag is true, compress, otherwise decompress the file.
 */
int Compress::compress(const char* file, CompressType ctype, int compress_flag,
		       int mmap_flag)
{
    char tmpfile[1024];
    sprintf(tmpfile, "%s.comp", file);
    int status = compress(file, tmpfile, ctype, compress_flag, mmap_flag);
   
    if (status != 0) {
	unlink(tmpfile);
	return status;
    }

    if (rename(tmpfile, file) != 0)
	return sys_error("rename failed for: ", file);

    return 0;
}


/*
 * compress (or decompress) the contents of inbuf using the given compress type and
 * allocate the results to outbuf. 
 *
 * - inbufsz is the size of the input buffer. 
 *
 * - outbufsz is an estimate of the outbuf size on input and the actual size
 *   on output.
 *
 * If compress_flag is true, compress, otherwise decompress the file.
 *
 * It is the caller's responsibility to free() the outbuf when no longer needed.
 */
int Compress::compress(const char* inbuf, int inbufsz, char*& outbuf, int& outbufsz, 
		       CompressType ctype, int compress_flag)
{
    if (ctype == NO_COMPRESS)
	return 0;

    char* type = types_[ctype];

    int status = (compress_flag ? 
		  press_m2m((unsigned char*)inbuf, inbufsz, (unsigned char**)&outbuf, &outbufsz, type) :
		  unpress_m2m((unsigned char*)inbuf, inbufsz, (unsigned char**)&outbuf, &outbufsz, type));
   
    if (status != 0) 
	return compress_error(compress_flag);

    return 0;
}


/*
 * Optionally set global compress options. 
 * The first argument indicates the compression type. 
 * "scale" is supported by gzip and h_compress.
 * "do_smoothing" (boolean) is only supported for h_compress.
 */
int Compress::set_options(CompressType ctype, int scale, int do_smoothing)
{
    char* type = types_[ctype];
    switch(ctype) {
    case H_COMPRESS:
	press_setopt(type, 0, do_smoothing, scale, "fits", 0, 0);
	break;
    case GZIP_COMPRESS:
	press_setopt(type, 0, scale);
	break;
    default:
	break;
    }
    
    return 0;
}

