package hu.borcsok.advent.y2017.d08

case class Cpu(registers: Map[String, Int] = Map.empty) {

  def get(reg: String): Int = registers.getOrElse(reg, 0)

  def operation(cmd: String): Cpu = {
    // b inc 5 if a > 1
    val cmds = cmd.split("\\s+")
    val register = cmds(0)
    val operation = cmds(1)
    val value = cmds(2).toInt
    val mustBeIf = cmds(3)
    val conditionRegister = cmds(4)
    val condition = cmds(5)
    val conditionValue = cmds(6).toInt

    val conditionResult = condition match {
      case "==" => get(conditionRegister) == conditionValue
      case "!=" => get(conditionRegister) != conditionValue
      case ">=" => get(conditionRegister) >= conditionValue
      case "<=" => get(conditionRegister) <= conditionValue
      case ">" => get(conditionRegister) > conditionValue
      case "<" => get(conditionRegister) < conditionValue
    }

    if (conditionResult) {
      val newValue = operation match {
        case "inc" => get(register) + value
        case "dec" => get(register) - value
      }
      Cpu(registers + (register -> newValue))
    } else {
      this
    }
  }

  lazy val largestRegisterValue: Int = registers.values.max

}
