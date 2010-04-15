PROC FLATFIELD

{ Obtain the name of the flat-field NDF.  If it does not have a
{ leading @ insert one.
   INPUT "Which flat field frame?: " (FF)
   IF SUBSTR( FF, 1, 1 ) <> '@'
      FF = '@' & (FF)
   END IF

{ Loop until there are no further NDFs to flat field.
   MOREDATA = TRUE
   LOOP WHILE MOREDATA

{ Obtain the frame to flat field.  Assume that it will not have
{ an @ prefix. Generate a title for the flattened frame.
      INPUT "Enter frame to flat field (!! to exit): " (IMAGE)
      MOREDATA = IMAGE <> '!!'
      IF MOREDATA
         TITLE = 'Flat field of ' & (IMAGE)
         IMAGE = '@' & (IMAGE)

{ Generate the name of the flattened NDF.
         IMAGEOUT = (IMAGE) & 'F'
         PRINT Writing to (IMAGEOUT)

{ Divide the image by the flat field.

         DIV IN1=(IMAGE) IN2=(FF) OUT=(IMAGEOUT) ~
             TITLE=(TITLE)
      END IF
   END LOOP
END PROC
