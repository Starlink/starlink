// -*-c++-*-
#ifndef _Compress_h_
#define _Compress_h_

/*
 * E.S.O. - VLT project 
 * $Id$
 *
 * DCompress.h - utility class for compressing/decompressing FITS files
 * 
 * (wrapper for archive/CADC "press" routines)
 * 
 * who             when       what 
 * --------------  --------   ----------------------------------------
 * Allan Brighton  2 Aug 96  Created
 *                 2 Jan 06  Renamed file to avoid name conflict with fitsio's 
 *                           "compress.h" on file systems that ignore case.
 */

/*
 * Class Compress
 *
 */
class Compress {
protected:

public:
    // constructor
    Compress() {}

    // types of compression
    enum CompressType {
	NO_COMPRESS,		// no compression
	UNIX_COMPRESS,		// Compressed FITS file (UNIX)
	H_COMPRESS,		// Hcompressed FITS file
	ULDA_COMPRESS,		// ULDA compressed FITS file
	GZIP_COMPRESS		// GZIPed FITS file
    };


    // compress (or decompress) from read_fd and write results to write_fd
    int compress(int read_fd, int write_fd, CompressType type, 
		 int compress_flag = 1);

    // decompress from read_fd and write results to write_fd
    int decompress(int read_fd, int write_fd, CompressType type) {
	return compress(read_fd, write_fd, type, 0);
    }


    // compress (or decompress) infile and put the result in outfile
    int compress(const char* infile, const char* outfile, CompressType type, 
		 int compress_flag = 1, int mmap_flag = 1);

    // decompress infile and put the result in outfile
    int decompress(const char* infile, const char* outfile, CompressType type,
		   int mmap_flag = 1) {
	return compress(infile, outfile, type, 0, mmap_flag);
    }


    // compress (or decompress) the file in place
    int compress(const char* file, CompressType type, int compress_flag = 1, 
		 int mmap_flag = 1);

    // decompress the file in place
    int decompress(const char* file, CompressType type, int mmap_flag = 1) {
	return compress(file, type, 0, mmap_flag);
    }


    // compress (or decompress) inbuf and allocate results in outbuf
    int compress(const char* inbuf, int inbufsz, char*& outbuf, int& outbufsz, 
		       CompressType ctype, int compress_flag = 1);

    // decompress inbuf and allocate results in outbuf
    int decompress(const char* inbuf, int inbufsz, char*& outbuf, int& outbufsz, 
		   CompressType ctype) {
	return compress(inbuf, inbufsz, outbuf, outbufsz, ctype, 0);
    }

    // optionally set global compress options for scale and smoothing
    static int set_options(CompressType ctype, int scale = 10, int do_smoothing = 0);
};



#endif /* _Compress_h_ */
