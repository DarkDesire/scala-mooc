package homeworks.collections

object task_caesar {
  /**
   * В данном задании Вам предлагается реализовать функции,
   * реализующие кодирование/декодирование строки шифром Цезаря.
   * https://ru.wikipedia.org/wiki/Шифр_Цезаря
   * Алфавит - прописные латинские буквы от A до Z.
   * Сдвиг   - неотрицательное целое число.
   * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
   */
  /**
   * @param word   входное слово, которое необходимо зашифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return зашифрованное слово
   */
  def encrypt(word: String, offset: Int): String = shiftWord(word, offset % alphabetSize)
  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = shiftWord(cipher, alphabetSize - offset % alphabetSize)
  def shiftWord(word: String, offset: Int): String = {
    word
      .map(_.toInt + offset)
      .map(ch => if (ch > 'Z') ch - alphabetSize else ch)
      .map(_.toChar)
      .mkString
  }
  val alphabetSize: Int = ('A' to 'Z').size
}
