//
// Creational
//

// Abstract factory

class Client


class ScrollBar
class PMScrollBar extends ScrollBar
class MotifScrollBar extends ScrollBar

class Window
class PMWindodw extends Window
class MotifWindow extends Window

abstract class WidgetFactory extends Client {
  def createWindow(): Unit
  def createScollBar(): Unit
}

abstract class MotifFactory extends WidgetFactory {
  def createFactory(): Unit
  def createScollBar(): Unit
}

abstract class PMFactory extends WidgetFactory {
  def createFactory(): Unit
  def createScollBar(): PMScrollBar
}

// Builder

class Director {
  def costruct() = {
    val builder = new ConcreteBuilder1()
    builder.buildStep1()
    builder.buildStep2()
    builder.getObject()
  }
}

class Builder {
  def buildStep1()
  def buildStep2()
  def getObject()
}

class ConcreteBuilder1 extends Builder {
  def buildStep1()
  def buildStep2()
  def getObject()
}

class ConcreteBuilder2 extends Builder {
  def buildStep1()
  def buildStep2()
  def getObject()
}

// Factory method

class Document
class MyDoc extends Document

// factory object
abstract class Application {
  def createDocument()    // factory method
}

class MyApp extends Application {
  def createDocument()
}

// Prototype

class Client
class Prototype {
  def clone()
}

class ConcretePrototype1 extends Prototype {
  def clone()
}

class ConcretePrototype2 extends Prototype {
  def clone()
}

// Singleton

class Singleton {
  protected Singleton()
  def Singleton(): Unit = {

  }
}

//
// Structural
//

// Adapter
// Normally don’t worry about interfaces here, so don’t usually think about it. However, if you know some existing code is going to be incorporated into your system, it is likely that an adapter will be needed since it is unlikely this pre-existing code will have the correct interface.

// Bridge
// There are a set of related objects using another set of objects. This second set represents an implementation of the first set.

// Composite
// There are single things and groups of things that you want to treat the same way. The groups of things are made up of other groups and of single things (i.e., they are hierarchically related).

// Facade
// A complex system will be used which will likely not be utilized to its full extent.

// Proxy-virtual
// Performance issues (speed or memory) can be foreseen because of the cost of having objects around before they are actually used.

//
// Behavioral patterns
//

// Decorator
// There is some action that is always done, there are other actions that may need to be done.

// Proxy-adding function
//  We need some particular action to occur before some object we already have is called.

// State
// We have behaviors that change, depending upon the state we are in.

// Strategy
// There are different implementations of a business rule.

// Template
// There are different procedures that are followed that are essentially the same, except that each step does things differently.

// Visitor
// You have a reasonably stable set of classes for which you need to add new functions. You can add tasks to be performed on this set without having to change it.

//
// DECOUPLING PATTERNS
//

// Chain of responsi- bility
// We have the several actions that may be done by different things.

// Iterator
// We have a collection of things but aren’t clear what the right type of collection to use is.

// Mediator
// Many objects need to communicate with many other objects yet this communication cannot be handled with the observer pattern.

// Memento
// The state of an object needs to be remembered so we can go back to it (e.g., undo an action).

// Observer
//  Different things (objects) that need to know when an event has occurred. This list of objects may vary from time to time or from case to case.

// Proxy – access- ability
// Are any of the things we work with remote (i.e., on other machines)? An existing object needs to use an object on another machine and doesn’t want to have to worry about making the connection (or even know about the remote connection).