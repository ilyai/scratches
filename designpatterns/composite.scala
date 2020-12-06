abstract class Equipment(name: String) {
  var equipment: List[Equipment] = List.empty
  def netPrice: Int
  def add(e: Equipment) = equipment ::= e
}

class FloppyDisk extends Equipment("Floppy") {
  override def netPrice: Int = 20
}

class CompositeEquipment(name: String) extends Equipment(name) {
  override def netPrice: Int = equipment.map(_.netPrice).sum
}

class Chassis extends CompositeEquipment("Chassis") {

}

val floppy = new FloppyDisk
floppy.netPrice

val chassis = new Chassis
chassis.add(new FloppyDisk)
chassis.add(new FloppyDisk)
chassis.netPrice
