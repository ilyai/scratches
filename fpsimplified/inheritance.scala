sealed trait Animal
final case class Dog(name: String) extends Animal
final case class Cat(name: String) extends Animal
final case class Bird(name: String) extends Animal

trait BehavesLikeHuman[A] {
  def speak(a: A): Unit
}

object BehavesLikeHumanInstances {
  implicit val dogBehavesLikeHuman = new BehavesLikeHuman[Dog] {
    override def speak(dog: Dog): Unit = println(s"I'm a Dog, my name is ${dog.name}")
  }
}

import BehavesLikeHumanInstances.dogBehavesLikeHuman

object BehavesLikeHuman {
  def speak[A](a: A)(implicit blh: BehavesLikeHuman[A]) = blh.speak(a)
}

val rover = Dog("Rover")
BehavesLikeHuman.speak(rover)

object BehavesLikeHumanSyntax {
  implicit class BehavesLikeHumanOps[A](value: A) {
    def speak(implicit behavesLikeHumanInstance: BehavesLikeHuman[A]) =
      behavesLikeHumanInstance.speak(value)
  }
}

import BehavesLikeHumanSyntax.BehavesLikeHumanOps

rover.speak