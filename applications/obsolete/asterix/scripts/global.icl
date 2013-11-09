HIDDEN PROC GetaGlob ParmText ParmName OuText
   value='                                                           '
   OuText = ParmText & ParmName
   GETGLOBAL (ParmName) (Value)
   IF LGT((Value)," ")
     OuText = OuText & Value
   ELSE
     OuText = OuText & "** Not Set **"
   END IF
   EXCEPTION adamerr
      CREATEGLOBAL (ParmName) _CHAR*200
      Value = '** Not Defined **'
      OuText = OuText & Value
   END EXCEPTION
END PROC

HIDDEN PROC Global
   PRINT 'GLOBAL Version 1.0-2'
   PRINT
   PRINT    'Global Variable         Global Name      Value'
   PRINT
   GetaGlob 'Binned dataset          ' 'BINDS            ' (x)
   PRINT (x)
   GetaGlob 'Event dataset           ' 'EVDS             ' (x)
   PRINT (x)
   GetaGlob 'HDS editor object       ' 'HDSOBJ           ' (x)
   PRINT (x)
   GetaGlob 'Source search results   ' 'SSDS             ' (x)
   PRINT (x)
   GetaGlob 'Fit model object        ' 'FIT_MODEL        ' (x)
   PRINT (x)
   GetaGlob 'Fit data object         ' 'FIT_DATA         ' (x)
   PRINT (x)
END PROC
