      real function determinant(m)
      real m(2,2)
      determinant = m(1,1) * m(2,2) - m(1,2) * m(2,1)
      return
      end
