# IRAF script to calculate a mean and subtract
# it from an image.

# Load images package.
images

# Initialize the cl list variable.
list = " "

# Store a temporary file name in string s3.
s3 = "temp.file"

# Configure imstat to display only the mean
imstat.format=no
imstat.fields="mean"

# Redirect imstat output to temporary file.
imstat "ccdframe" > (s3)

# Associate temporary file with list variable.
list = (s3)

# Read the value in the file into parameter 'x'
i= fscan (list,x)

# Use imarith to subtract x from the image.
imarith ("ccdframe","-",x,"newframe")

# Reconfigure imstat and then look at statistics of
# new image to see if the subtraction worked.
imstat.format=yes
imstat.fields="image,npix,mean,stddev,min,max"

imstat ("newframe")

# Delete the temporary file.
delete ("temp.file", verify=no)

# Print an output message
print("Subtracted ",x,"to create file newframe.")
