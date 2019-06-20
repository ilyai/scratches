// Domain layer - capture business concepts and rules and encapsulate state that will be repsisted to an external relation database
// Application layer - API organized in terms of the services the application offers to clients (including the user interface layer)

abstract class Food(val name: String) {
  override def toString: String = name
}

class Recipe(
            val name: String,
            val ingredients: List[Food],
            val instructions: String
            ) {
  override def toString = name
}

object Apple extends Food("Apple")
object Orange extends Food("Orange")
object Cream extends Food("Cream")
object Sugar extends Food("Sugar")

object FruitSalad extends Recipe(
  "fruit salad",
  List(Apple, Orange, Cream, Sugar),
  "Stir it all together."
)


//val apple = SimpleDatabase.foodNamed("Apple").get
//SimpleBrowser.recipesUsing(apple)

abstract class Browser {
  val database: Database
  def recipesUsing(food: Food) =
    database.allRecipes.filter(recipe =>
      recipe.ingredients.contains(food))
  def displayCategory(category: database.FoodCategory) =
    println(category)
}

abstract class Database extends FoodCategories {
  def allFoods: List[Food]
  def allRecipes: List[Recipe]

  def foodNamed(name: String) =
    allFoods.find(f => f.name == name)
}

object SimpleDatabase extends Database with SimpleFoods {
  override def allFoods = List(Apple, Orange, Cream, Sugar)

  def allRecipes: List[Recipe] = List(FruitSalad)

  case class FoodCategory(name: String, foods: List[Food])

  private var categories = List(
    FoodCategory("fruits", List(Apple, Orange)),
    FoodCategory("misc", List(Cream, Sugar))
  )

  override def allCategories: List[FoodCategory] = categories
}
//
//val apple = SimpleDatabase.foodNamed("Apple").get
//
//object SimpleBrowser extends Browser {
//  val database = SimpleDatabase
//}

//SimpleBrowser.recipesUsing(apple)


object StudentDatabase extends Database {
  object FrozenFood extends Food("FrozenFood")

  object HeatItUp extends Recipe(
    "heat it up",
    List(FrozenFood),
    "Microwave the 'food' for 10 min."
  )

  override def allFoods = List(FrozenFood)
  override def allRecipes = List(HeatItUp)

  override def allCategories = List(
    FoodCategory("edible", List(FrozenFood))
  )
}

object StudentBrowser extends Browser {
  val database = StudentDatabase
}

trait FoodCategories {
  case class FoodCategory(name: String, foods: List[Food])
  def allCategories: List[FoodCategory]
}

trait SimpleFoods {
  object Pear extends Food("Pear")
  def allFoods = List(Apple, Pear)
  def allCategories = Nil
}

trait SimpleRecipes { self: SimpleFoods =>
  object FruitSalad extends Recipe(
    "fruit salad",
    List(Apple, Pear),
    "Mix it all together"
  )
  def allRecipes = List(FruitSalad)
}

object GotApples {
  def main(args: Array[String]): Unit = {
    val db: Database =
      if (args(0) == "student")
        StudentDatabase
      else
        StudentDatabase
    object browser extends Browser {
      val database: db.type = db
    }
    val apple = StudentDatabase.foodNamed("FrozenFood").get
    for (recipe <- browser.recipesUsing(apple))
      println(recipe)
  }
}

GotApples.main(Array("student"))