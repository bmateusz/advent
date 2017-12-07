package hu.borcsok.advent.y2017.d04

import org.specs2.Specification
import hu.borcsok.advent.y2017.Helpers._

class PassphraseSpecs extends Specification {

  val passphrases: Seq[Seq[String]] = {
    readResourceLines("2017/passphrase.txt").map(Passphrase.fromString)
  }

  val sortedPassphrases: Seq[Seq[String]] = {
    passphrases.map(Passphrase.sort)
  }

  def is =
  s2"""
Day 04

A new system policy has been put in place that requires all accounts to use a passphrase instead of simply a password.
A passphrase consists of a series of words (lowercase letters) separated by spaces.

To ensure security, a valid passphrase must contain no duplicate words.

For example:

aa bb cc dd ee is valid.
aa bb cc dd aa is not valid - the word aa appears more than once.
aa bb cc dd aaa is valid - aa and aaa count as different words.

${passphrases.map(w => Passphrase.isUnique(w)).count(_ == true) === 477}
${sortedPassphrases.map(w => Passphrase.isUnique(w)).count(_ == true) === 167}
  """
}
