#
##
#
proc PathVarImport {pathvar {default ./}} {
  global env

# Load path environment variable 
  if [info exists env($pathvar)] {
    set lpath $env($pathvar)
  } else {
    set lpath $default
    }

# Convert path to list
  return [split $lpath :]
  }


#
# Search a list of directories for a file
#
proc PathSearch {file dirs} {
      
# Search each component of path for the file specified. Return immediately
# if found
  foreach dir $dirs {
    set ftest "$dir/$file"
    if [file exists $ftest] {
      return $ftest
      }
    }
    
  return ""
  }
