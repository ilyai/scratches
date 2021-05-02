trait Monad[A] {
  def flatMap[A,B](f: A => Monad[B]): Monad[B]
  def map[A,B](f: A => B): Monad[B]
  def lift[A](a: A): Monad[A]
}
