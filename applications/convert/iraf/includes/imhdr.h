# IMHDR.H -- Image Header Definitions.  The structure definitions in this file
# assume two or less chars per struct unit (e.g., a one or two byte char).

define	IM_MAXDIM	7			# maximum dimension of image
define	SZ_IMMAGIC	5			# used to verify structure
define	SZ_IMPIXFILE	79			# name of pixel storage file
define	SZ_IMHDRFILE	79			# name of header storage file
define	SZ_IMTITLE	79			# image title string
define	SZ_IMHIST	511			# image history record
define	SZ_BUNIT	9			# brightness units string
define	SZ_CTYPE	9			# coord axes units string

# The IMIO image header structure.

# Parameters.
define	LEN_IMDES	200			# length of image descriptor
define	LEN_IMHDR	513			# length of std header
define	IMD		LEN_IMDES		# offset to std header fields
define	IMU		713			# offset to user fields
define	IM_USERAREA	(P2C($1+IMU))		# user area (database)

# Disk resident header.
define	IM_MAGIC	Memi[$1+IMD]		# contains the string "imhdr"
define	IM_HDRLEN	Memi[$1+IMD+3]		# length of image header
define	IM_PIXTYPE	Memi[$1+IMD+4]		# datatype of the pixels
define	IM_NDIM		Memi[$1+IMD+5]		# number of dimensions
define	IM_LEN		Meml[$1+IMD+$2+6-1]	# length of the dimensions
define	IM_PHYSLEN	Meml[$1+IMD+$2+13-1]	# physical length (as stored)
define	IM_SSMTYPE	Meml[$1+IMD+20]		# type of subscript mapping
define	IM_LUTOFF	Meml[$1+IMD+21]		# offset to subscript map luts
define	IM_PIXOFF	Meml[$1+IMD+22]		# offset of the pixels
define	IM_HGMOFF	Meml[$1+IMD+23]		# offset of hgm pixels
define	IM_BLIST	Meml[$1+IMD+24]		# offset of bad pixel list
define	IM_SZBLIST	Meml[$1+IMD+25]		# size of bad pixel list
define	IM_NBPIX	Meml[$1+IMD+26]		# number of bad pixels
define	IM_CTIME	Meml[$1+IMD+27]		# time of image creation
define	IM_MTIME	Meml[$1+IMD+28]		# time of last modify
define	IM_LIMTIME	Meml[$1+IMD+29]		# time min,max computed
define	IM_MAX		Memr[$1+IMD+30]		# max pixel value
define	IM_MIN		Memr[$1+IMD+31]		# min pixel value
define	IM_HGM		($1+IMD+33)		# histogram descriptor
define	IM_CTRAN	($1+IMD+52)		# coordinate transformations
define	IM_PIXFILE	Memc[P2C($1+IMD+103)]	# name of pixel storage file
define	IM_HDRFILE	Memc[P2C($1+IMD+143)]	# name of header storage file
define	IM_TITLE	Memc[P2C($1+IMD+183)]	# image name string
define	IM_HISTORY	Memc[P2C($1+IMD+223)]	# history comment string

# The Histogram structure (field IM_HGM)
define	LEN_HGMSTRUCT	20
define	HGM_TIME	Meml[$1]		# time when hgm was computed
define	HGM_LEN		Meml[$1+1]		# number of bins in hgm
define	HGM_NPIX	Meml[$1+2]		# npix used to compute hgm
define	HGM_MIN		Memr[$1+3]		# min hgm value
define	HGM_MAX		Memr[$1+4]		# max hgm value
define	HGM_INTEGRAL	Memr[$1+5]		# integral of hgm
define	HGM_MEAN	Memr[$1+6]		# mean value
define	HGM_VARIANCE	Memr[$1+7]		# variance about mean
define	HGM_SKEWNESS	Memr[$1+8]		# skewness of hgm
define	HGM_MODE	Memr[$1+9]		# modal value of hgm
define	HGM_LCUT	Memr[$1+10]		# low cutoff value
define	HGM_HCUT	Memr[$1+11]		# high cutoff value
# next available field: ($1+12)


# The Coordinate Transformation Structure (IM_CTRAN)
define	LEN_CTSTRUCT	50
define	CT_VALID	Memi[$1]		# (y/n) is structure valid?
define	CT_BSCALE	Memr[$1+1]		# pixval scale factor
define	CT_BZERO	Memr[$1+2]		# pixval offset
define	CT_CRVAL	Memr[$1+$2+3-1]		# value at pixel
define	CT_CRPIX	Memr[$1+$2+10-1]	# index of pixel
define	CT_CDELT	Memr[$1+$2+17-1]	# increment along axis
define	CT_CROTA	Memr[$1+$2+24-1]	# rotation angle
define	CT_BUNIT	Memc[P2C($1+31)]	# pixval ("brightness") units
define	CT_CTYPE	Memc[P2C($1+36)]	# coord units string
# next available field: ($1+41)
