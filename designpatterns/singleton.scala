class USTax

object USTax {
  var instance: USTax = _

  def getInstance = {
    if (instance == null) {
      instance = new USTax
    }
    instance
  }
}

USTax.getInstance
USTax.getInstance