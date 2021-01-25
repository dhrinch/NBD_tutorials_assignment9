import scala.runtime.Nothing$
import scala.util.Try

object Assignment9 {

  def main(args: Array[String]): Unit = {
    /*1. Implement a Container class parametrized with type A.
    Class constructor should accept a single private value of the given type. The class should implement the following methods:
    a. getContent returning the stored value
    b. applyFunction accepting function A=>R and returning the result of application of this function to stored value*/

    println("**************")
    val cont = new Container(10)
    println("Type of " +cont.getContent + " = " + cont.applyFunction(cont.getContent))

    val cont1 = new Container("Hello World!")
    println("Type of " +cont1.getContent + " = " + cont1.applyFunction(cont1.getContent))

    val cont2 = new Container(3.14159265359)
    println("Type of " +cont2.getContent + " = " + cont2.applyFunction(cont2.getContent))

    /*2. Implement trait Maybe parametrized with type A and two classes extending it –
    class No extending Maybe[Nothing] and class Yes parametrized with type A and storing
    a single value of a given type. Create objects of both classes.
    Check if they both are subtypes of Maybe[_] (use isInstanceOf method parametrized with checked type)*/

    println("**************")
    val no = new No
    val yes = new Yes("indeed")
    println("no is an instance of Maybe: " + no.isInstanceOf[Maybe[_]])
    println("yes is an instance of Maybe: " + yes.isInstanceOf[Maybe[String]])

    /*3. Extend solution of task 3 with function applyFunction of type A=>R returning:
    a. for No: No
    b. for Yes: Yes(f(stored value))*/

    println("**************")
    println(no.ApplyFunction())
    println(yes.ApplyFunction("indeed"))

    /*4. Extend solution of task 3 with function getOrElse parametrized with type and returning:
    a. for No – parameter of getOrElse
    b. for Yes – content of Yes*/
    println("**************")
    val nope: getOrElse[No[_]] = new getOrElse[No[_]](new No)
    println(nope.getOrElse)
    val yep: getOrElse[Yes[String]] = new getOrElse[Yes[String]](new Yes("Yup, that's a yes"))
    println(yep.getOrElse)
  }
}

class Container[A](private val value : A) {
  private val _value: A = value

  def getContent: A = _value

  def applyFunction(value:A) : Unit = println(plusOne(value))

  private def plusOne(a: A): String = (a.toString + " plus one!")
}

trait Maybe[A] {
  def ApplyFunction[A](a : A*): String
}

class No[A] extends Maybe[Nothing] {
  override def ApplyFunction[A](a : A*): String = {
    if (a.isEmpty) "No"
    else "Yes, " + a + " " + a.getClass
    }
}

class Yes[A](val value: A) extends Maybe[A] {
  val _value: A = value

  override def ApplyFunction[A](a : A*): String = {
    if (a.isEmpty) "No"
    else "Yes, " + a + " " + a.getClass
  }
}

class getOrElse[A](value: A) {
  val _value: A = value
  def getOrElse[B]: B =
    _value match {
      case _: Yes[_] => _value.asInstanceOf[Yes[A]]._value.asInstanceOf[B]
      case _: No[A] => "Nothing to see here".asInstanceOf[B]
    }
}
