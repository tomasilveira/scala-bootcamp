package com.evolutiongaming.bootcamp.adt

import com.evolutiongaming.bootcamp.adt.AlgebraicDataTypes.GameLevel.GameLevelExtra

object AlgebraicDataTypes {

  // ALGEBRAIC DATA TYPES

  // Algebraic Data Types or ADTs is a commonly used way of structuring data, used in many programming
  // languages (so this is not something unique to Scala).
  //
  // While the definition may sound scientific and complex, in reality we have already been using this
  // concept in `basics` package. For example, see `sealed trait Shape` in `ClassesAndTraits`. We will now
  // look into the concept and its use cases in more detail.

  // ADTs are widely used in Scala for multiple reasons:
  // - to ensure it is hard or even impossible to represent invalid data;
  // - because pattern matching and ADTs play nicely together.

  // Two common classes of ADTs are:
  // 1. product types: case classes and tuples;
  // 2. sum types: sealed traits and abstract classes.

  // PRODUCT TYPES

  //A x B = {(a, b), a:A, b:B}
  //E = (A,B)
  //How many instances of E exist?
  //#E = #A * #B
  // MyType = (Int, Int, Boolean)
  //myType1 = (1,2,true)
  //myType2 = (2,3,false)
  //#E = #Int * #Int * #Boolean

  // A product type allows to combine multiple values into one. Canonical Scala examples of product types are
  // case classes and tuples. See `Basics` and `ClassesAndTraits` for their introduction.

  // A product type is called like that because one can calculate how many different values it can possibly
  // have by multiplying the number of such possibilities for the types it combines. The resulting number
  // is called the arity of the product type.

  // Question. What is the arity of the product type `(Boolean, Boolean)`?
  type DoubleBoolean = (Boolean, Boolean) //2*2=4 true and false for each

  // Question. What is the arity of the product type `Person`?
  final case class Person(name: String, surname: String, age: Int)
  //#String = #Array[Byte]
  //#Int = 2^32 -1
  //#Person = #String * #String * #Int = very huge


  final class PersonOther(val name: String, val surname: String, val age: Int) {
    override def equals(obj: Any): Boolean = ???
    override def hashCode(): Int = ???

    override def toString: String = super.toString

    def copy(name: String =this.name, surname:String = this.surname, age:Int =this.age):PersonOther = ???
  }

  object PersonOther {
    def apply(name:String, surname:String, age: Int): PersonOther =
      new PersonOther(name, surname, age)
  }

  val personOther = /*new*/ PersonOther("marco", "kustra", 28) //if we implement the object we can take the new from the instanciation
  val personOther2 = PersonOther("marco", "kustra", 28)
  val marcoTotomas = PersonOther("Tomas", personOther.surname, personOther.age)
  val marcoTotomas2 = personOther.copy(name = "Tomas") // write copy method
  //personOther == personOther2 its false because they point to different references of the object
  //to do a print it is needed to override toString method


  val person = Person("marco", "kustra", 28)
  val personErr = Person("kustra", "", -100)
  // para evitar parametros vazios, trocados ou com valores impossiveis para o dominio usamos VALUE CLASSES


  // Question. `Int`, `Double`, `String`, etc. are useful types from the Scala standard library, which can
  // represent a wide range of data. In the product type `Person`, both the name and the surname are
  // represented by `String`. Is that a good idea?

  // VALUE CLASSES

  // Value classes are a mechanism in Scala to avoid allocating runtime objects, while still providing
  // additional type safety. Runtime objects are not allocated in most cases, but there are notable
  // exceptions, see the following link for more details:
  // https://docs.scala-lang.org/overviews/core/value-classes.html

  // `Age` has a single, public val parameter that is the underlying runtime representation. The type at
  // compile time is `Age`, but at runtime, the representation is `Int`. Case classes can also be used to
  // define value classes, see `Name`.
  class Age(val value: Int)            extends AnyVal
  type AgeAlias =Int
  final case class Name(value: String) extends AnyVal {
    def greeting: String = s"Hello, $value!"
  }
  type NameAlias = String

  // Type aliases may seem similar to value classes, but they provide no additional type safety. They can,
  // however, increase readability of the code in certain scenarios.
  final case class Surname(value: String) extends AnyVal
  type SurnameAlias = String // No additional type safety in comparison to `String`, arguably a bad example!

  val name: Name = Name("Marco")
  val surname: Surname = Surname("Kustra")
  //val nameAsSurname:Name = Surname("Kustra") compiler does not allow

  val nameAlias: NameAlias = "Marco"
  val surnameAlias: SurnameAlias = "Kustra"
  val nameAsSurname: SurnameAlias = nameAlias


  type NameByAge = Map[Age, List[Name]] //Simple example but easier for the developer to understand the intent




  // Question. Can you come up with an example, where using type aliases would make sense?

  // Exercise. Rewrite the product type `Person`, so that it uses value classes.
  final case class Person1(name: Name, surname: Surname, age: Age)

  val nameEmpty = Name("")
  val surnameEmpty = Surname("")

  // SMART CONSTRUCTORS

  // Smart constructor is a pattern, which allows creating only valid instances of a class.

  // Exercise. Create a smart constructor for `GameLevel` that only permits levels from 1 to 80 (inclusive).
  final case class GameLevel private (value: Int) extends AnyVal
//  object GameLevel {
//    def create(value: Int): Option[GameLevel] = value match {
//      case value if 1 <= value && 80 >= value => Some(GameLevel(value))
//      case _ => None
//    }
//  }

  object GameLevel {
    def create(value: Int): Option[GameLevel] = {
      Option.when(value >= 1 && value <= 80)(GameLevel(value))
    }
    val gamelevel1: GameLevel = GameLevel.create(1).get
    gamelevel1.copy(value = 100) // 100 > 80

    sealed abstract class GameLevelExtra private (val value: Int)

    object GameLevelExtra {
      def create(value: Int): Option[GameLevelExtra] =
      Option.when(value >= 1 && value <= 80)(new GameLevelExtra(value){})
    }

  }

  val gamelevel2: GameLevelExtra= GameLevelExtra.create(1).get

  // To disable creating case classes in any other way besides smart constructor, the following pattern
  // can be used. However, it is rather syntax-heavy and cannot be combined with value classes.
  sealed abstract case class Time private (hour: Int, minute: Int)
  object Time {
    def create(hour: Int, minute: Int): Either[String, Time] = {
      for{
        hourParsed <- if(hour >= 0 && hour <= 23) Right(hour) else Left("Invalid hour value")
        minuteParsed <- if(minute>= 0 && minute <= 59) Right(minute) else Left("Invalid minute value")
      }yield new Time(hourParsed, minuteParsed){}
    }
  }

//  sealed trait TimeError
//
//
//  final case object MinuteError extends TimeError
//  final case object HourError extends TimeError
//
//  Time.create(10, 61)match{
//    case Left(MinuteError) => println("Wrong minute, dude")
//    case Left(_) => ???
//    case Right(_) => ???
//
//  }


  // Exercise. Implement the smart constructor for `Time` that only permits values from 00:00 to 23:59 and
  // returns "Invalid hour value" or "Invalid minute value" strings in `Left` when appropriate.

  // Question. Is using `String` to represent `Left` a good idea? Why?
  //Let's now assume we want to represent various currencies.
  //EUR, USD, BTC
  // How can we model the following class with those three variants
  final case class Currency(isEuro: Boolean, isUSD: Boolean, isBTC: Boolean)

  object Currency{
    val BTC = Currency(false, false, true)
     // ...
  }

  // SUM TYPES

  // A sum type is an enumerated type. To define it one needs to enumerate all its possible variants.
  // A custom boolean type `Bool` can serve as a canonical example.
  sealed trait Bool
  object Bool {
    final case object True  extends Bool // True -> product type - 0 other type. #True = 1
    final case object False extends Bool // False -> product type - 0 other type. #False = 1
  }

  // Bool type = sum type of True and False
  // #Bool =  #True + #False = 2

  val boolV = Bool.True

  // Note that sealed keyword means that `Bool` can only be extended in the same file as its declaration.
  // Question. Why do you think sealed keyword is essential to define sum types?

  // A sum type is called like that because one can calculate how many different values it can possibly
  // have by adding the number of such possibilities for the types it enumerates. The resulting number
  // is called the arity of the sum type.

  // Question. What is the arity of the sum type `Bool`?

  // The power of sum and product types is unleashed when they are combined together. For example, consider a
  // case where multiple different payment methods need to be supported. (This is an illustrative example and
  // should not be considered complete.)
  final case class AccountNumber(value: String) extends AnyVal
  final case class CardNumber(value: String)    extends AnyVal
  final case class ValidityDate(month: Int, year: Int)
  sealed trait PaymentMethod
  object PaymentMethod {
    final case class BankAccount(accountNumber: AccountNumber)                      extends PaymentMethod
    final case class CreditCard(cardNumber: CardNumber, validityDate: ValidityDate) extends PaymentMethod
    final case object Cash                                                          extends PaymentMethod
  }

  import PaymentMethod._
  import com.evolutiongaming.bootcamp.adt.AlgebraicDataTypes.GameLevel

  final case class PaymentStatus(value: String) extends AnyVal
  trait BankAccountService {
    def processPayment(amount: BigDecimal, accountNumber: AccountNumber): PaymentStatus
  }
  trait CreditCardService {
    def processPayment(amount: BigDecimal, creditCard: CreditCard): PaymentStatus
  }
  trait CashService {
    def processPayment(amount: BigDecimal): PaymentStatus
  }

  // Exercise. Implement `PaymentService.processPayment` using pattern matching and ADTs.
  class PaymentService(
    bankAccountService: BankAccountService,
    creditCardService: CreditCardService,
    cashService: CashService,
  ) {
    def processPayment(amount: BigDecimal, method: PaymentMethod): PaymentStatus = {
      method match {
        case BankAccount(accountNumber) => bankAccountService.processPayment(amount, accountNumber)
        //case CreditCard(cardNumber, validityDate) => creditCardService.processPayment(amount, CreditCard(cardNumber, validityDate))
        case credit: CreditCard => creditCardService.processPayment(amount, credit)
        case PaymentMethod.Cash => cashService.processPayment(amount)
      }
    }
  }

  // Let's compare that to `NaivePaymentService.processPayment` implementation, which does not use ADTs, but
  // provides roughly the same features as `PaymentService`.
  // Question. What are disadvantages of `NaivePaymentService`? Are there any advantages?
  trait NaivePaymentService { // Obviously a bad example!
    def processPayment(
      amount: BigDecimal,
      bankAccountNumber: Option[String],
      validCreditCardNumber: Option[String],
      isCash: Boolean,
    ): String = ???
  }

  // Attributions and useful links:
  // https://nrinaudo.github.io/scala-best-practices/definitions/adt.html
  // https://alvinalexander.com/scala/fp-book/algebraic-data-types-adts-in-scala/
  // https://en.wikipedia.org/wiki/Algebraic_data_type
}
