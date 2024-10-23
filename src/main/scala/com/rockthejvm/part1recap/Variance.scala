package com.rockthejvm.part1recap

object Variance {

  // OOP - substitution
  class Animal
  class Dog(name: String) extends Animal

  // Variance question for List: if Dog <: Animal, then should List[Dog] <: List[Animal]?

  // YES - COVARIANT
  val lassie = new Dog("lassie")
  val hachi = new Dog("Hachi")
  val laika = new Dog("Laika")

  val anAnimal: Animal = lassie
  val someAnimal: List[Animal] = List(lassie, hachi, laika)

  class MyList[+A] // MyList is COVARIANT in A
  val myAnimalList: MyList[Animal] = new MyList[Dog]

  // NO - then the type is INVARIANT
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  // all generics in JAVA
  //  val aJavaList: java.util.ArrayList[Animal] = new java.util.ArrayList[Dog]() // not ok

  // HELL NO - CONTRAVARIANCE
  trait Vet[-A] {
    def heal(animal: A): Boolean
  }

  // Vet[Animal] is "better" than a Vet[Dog]: she/he can treat ANY animal, therefore my dog as well
  // Dog <: Animal , then Vet[Dog] >: Vet[Animal]
  val myVet: Vet[Dog] = new Vet[Animal]{
    override def heal(animal: Animal): Boolean =
      println("Here tou go, you're good now...")
      true
  }

  val healingLassie = myVet.heal(lassie)


  /*
    Rule of thumb:
    - if the type PRODUCES or RETRIEVES values of type A (e.g. lists), then the type should be COVARIANT
    - if the type CONSUMES or ACTS ON values of type A (e.g. a vet), then the type should be CONTRAVARIANT
    - otherwise, INVARIANT
   */


  /**
   *
   * VARIANCE POSITIONS
   */
  /* A)---------------
    class Vet2[-A](val favoriteAnimal: A) <-- the types of val field are in COVARIANT position

    val garfield = new Cat
    val theVet: Vet2[Animal] = new Vet2[Animal](garfield)
    val dogVet: Vet2[Dog] = theVet
    val favAnimal: Dog = dogVet.favoriteAnimal // must be a Dog - type conflict !
   */

  // B)--------
  // var field are also in COVARIANT position(same)

  /*
    class MutableContainer[+A](var contents: A)

    val containerAnimal: MutableContainer[Animal] = new MutableContainer[Dog](new Dog)
    containerAnimal.contents = new Cat // type conflict !
   */

  // C)-----
  // types of method arguments are in CONTRAVARIANT position
  /*
    class MyList2[+A]{
      def add(element: A): MyList2[A]
    }

    val animals: MyList2[Animal] = new MyList2[Cat]
    val biggerListOfAnimals: MyList2[Animal] = animals.add(new Dog) // type conflict !
   */

  // solution: WIDEN the type argument
  class MyList2[+A]{
    def add[B >:A](element: B): MyList2[B] = ???
  }

  // D)---------
  // Method return types are in COVARIANT Postition
  /*
    abstract class Vet2[-A] {
      def rescueAnimal(): A
    }

    val vet: Vet2[Animal] = new Vet2[Animal] {
      def rescueAnimal(): Animal =  new Cat
    }

    val lassieVet: Vet2[Dog]: vet
    val rescueDog: Dog = lassieVet.rescueAnimal() // must return a Dog, but it returns a Cat - type conflict !
   */

  // solution: NARROW the type argument
  abstract class Vet2[-A] {
    def rescueAnimal[B <: A](): B
  }


  def main(args: Array[String]): Unit = {

  }
}
