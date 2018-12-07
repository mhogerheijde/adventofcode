val greek = Range('Α', 'Ω')

greek.filterNot(_.toChar.isUpper).map(_.toChar)