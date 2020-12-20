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
  def encrypt(word: String, offset: Int): String = {
    val alphabet = 'A' to 'Z' // size=26
    // reduce offset
    val shift = (offset + alphabet.size) % alphabet.size
    // map + upper bounds
    word.map {
      case c if alphabet.indexOf(c.toUpper) + shift >= alphabet.size =>
        alphabet(alphabet.indexOf(c.toUpper) + shift - alphabet.size)
      case c =>
        alphabet(alphabet.indexOf(c.toUpper) + shift)
    }
  }

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = {
    val alphabet = 'A' to 'Z' // size=26
    // reduce offset
    val shift = (offset + alphabet.size) % alphabet.size
    // map + lower bounds
    cipher.map {
      case c if alphabet.indexOf(c.toUpper) - shift < 0 =>
        alphabet(alphabet.indexOf(c.toUpper) - shift + alphabet.size)
      case c =>
        alphabet(alphabet.indexOf(c.toUpper) - shift)
    }
  }
}
