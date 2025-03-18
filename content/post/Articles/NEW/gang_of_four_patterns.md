---
title: The Gang of Four Patterns
description: Gang of Foure Code Examples in Java
slug: the-gang-of-four-patterns-explained
date: 2023-12-15
image: post/Articles/IMAGES/gofwide.png
categories:
  - Design Patterns
  - Algorithms
  - Java
tags:
  - Design
  - Patterns
  - Gang
  - Of
  - Four
  - Software
  - Engineering
  - Java
  - Object-Oriented
  - Programming
  - DesignPaterns
  - DesignPatterns
draft: false
weight: 33
categories_ref:
  - Design Patterns
  - Algorithms
  - Java
slug_calculated: https://brianbraatz.github.io/p/the-gang-of-four-patterns-explained
lastmod: 2025-03-14T16:40:21.012Z
---
# The Gang of Four Patterns Explained

You ever feel like your code is a tangled mess of spaghetti, and not the good kind with marinara sauce?

Well, The **Gang of Four** (GoF) is here to save the day with their legendary **design patterns**.

These four brilliant minds tackled the chaos of object-oriented design and compiled a set of reusable, battle-tested patterns that developers still use today.

## Who Are the Gang of Four?

No, it's not a rock band (though that would be cool). The Gang of Four consists of **Erich Gamma, Richard Helm, Ralph Johnson, and John Vlissides**—four computer scientists who, back in 1994, wrote *Design Patterns: Elements of Reusable Object-Oriented Software*.

Basically, they looked at common software problems, went all Sherlock Holmes on them, and came up with solutions that developers could use over and over again.

Their motivation? Simple: **software was turning into a dumpster fire**. Large-scale applications were becoming increasingly complex, and developers kept reinventing the wheel (badly).....

<!-- 
Instead of letting everyone struggle, they identified best practices and documented them into patterns that could be applied universally.
-->

The "four" organized the common ideas into a set of patterns and wrote a famous book about the ideas in hopes of standardizing how we think about design patterns in software...

And pattern nerds **rejoiced**!!!!...

Below is each pattern implemented in Java.

## The Gang of Four Design Patterns

The GoF patterns are categorized into **three groups**:

1. **Creational Patterns** – Focus on object creation mechanisms.
2. **Structural Patterns** – Deal with object composition and structure.
3. **Behavioral Patterns** – Address communication between objects.

***

# Design Patterns Organized by Type

## Creational Patterns

### 1. Factory Method Pattern

**Intent:** Define an interface for creating an object, but let subclasses decide which class to instantiate. Factory Method lets a class defer instantiation to subclasses.

#### Example in Java:

```java
// Product interface
interface Product {
    void use();
}

// ConcreteProductA class
class ConcreteProductA implements Product {
    public void use() {
        System.out.println("Using Product A");
    }
}

// ConcreteProductB class
class ConcreteProductB implements Product {
    public void use() {
        System.out.println("Using Product B");
    }
}

// Creator abstract class
abstract class Creator {
    public abstract Product factoryMethod();

    public void someOperation() {
        Product product = factoryMethod();
        product.use();
    }
}

// ConcreteCreatorA class
class ConcreteCreatorA extends Creator {
    public Product factoryMethod() {
        return new ConcreteProductA();
    }
}

// ConcreteCreatorB class
class ConcreteCreatorB extends Creator {
    public Product factoryMethod() {
        return new ConcreteProductB();
    }
}

// Usage
public class FactoryMethodDemo {
    public static void main(String[] args) {
        Creator creatorA = new ConcreteCreatorA();
        creatorA.someOperation();

        Creator creatorB = new ConcreteCreatorB();
        creatorB.someOperation();
    }
}
```

***

## Structural Patterns

### 2. Adapter Pattern

**Intent:** Convert the interface of a class into another interface clients expect. Adapter lets classes work together that couldn't otherwise because of incompatible interfaces.

#### Example in Java:

```java
// Target interface
interface Target {
    void request();
}

// Adaptee class
class Adaptee {
    public void specificRequest() {
        System.out.println("Called specificRequest()");
    }
}

// Adapter class
class Adapter implements Target {
    private Adaptee adaptee;

    public Adapter(Adaptee adaptee) {
        this.adaptee = adaptee;
    }

    public void request() {
        adaptee.specificRequest();
    }
}

// Usage
public class AdapterPatternDemo {
    public static void main(String[] args) {
        Adaptee adaptee = new Adaptee();
        Target target = new Adapter(adaptee);
        target.request();
    }
}
```

***

### 3. Composite Pattern

**Intent:** Compose objects into tree structures to represent part-whole hierarchies. Composite lets clients treat individual objects and compositions of objects uniformly.

#### Example in Java:

```java
import java.util.ArrayList;
import java.util.List;

// Component interface
interface Component {
    void operation();
}

// Leaf class
class Leaf implements Component {
    public void operation() {
        System.out.println("Leaf operation");
    }
}

// Composite class
class Composite implements Component {
    private List<Component> children = new ArrayList<>();

    public void add(Component component) {
        children.add(component);
    }

    public void remove(Component component) {
        children.remove(component);
    }

    public void operation() {
        System.out.println("Composite operation");
        for (Component child : children) {
            child.operation();
        }
    }
}

// Usage
public class CompositePatternDemo {
    public static void main(String[] args) {
        Leaf leaf1 = new Leaf();
        Leaf leaf2 = new Leaf();

        Composite composite = new Composite();
        composite.add(leaf1);
        composite.add(leaf2);

        composite.operation();
    }
}
```

***

### 4. Decorator Pattern

**Intent:** Attach additional responsibilities to an object dynamically. Decorators provide a flexible alternative to subclassing for extending functionality.

#### Example in Java:

```java
// Component interface
interface Component {
    void operation();
}

// ConcreteComponent class
class ConcreteComponent implements Component {
    public void operation() {
        System.out.println("ConcreteComponent operation");
    }
}

// Decorator abstract class
abstract class Decorator implements Component {
    protected Component component;

    public Decorator(Component component) {
        this.component = component;
    }

    public void operation() {
        component.operation();
    }
}

// ConcreteDecoratorA class
class ConcreteDecoratorA extends Decorator {
    public ConcreteDecoratorA(Component component) {
        super(component);
    }

    public void operation() {
        super.operation();
        System.out.println("ConcreteDecoratorA operation");
    }
}

// Usage
public class DecoratorPatternDemo {
    public static void main(String[] args) {
        Component component = new ConcreteComponent();
        Component decoratorA = new ConcreteDecoratorA(component);
        decoratorA.operation();
    }
}
```

***

### 5. Facade Pattern

**Intent:** Provide a unified interface to a set of interfaces in a subsystem. Facade defines a higher-level interface that makes the subsystem easier to use.

#### Example in Java:

```java
// Subsystem classes
class SubsystemA {
    public void operationA() {
        System.out.println("SubsystemA operation");
    }
}

class SubsystemB {
    public void operationB() {
        System.out.println("SubsystemB operation");
    }
}

// Facade class
class Facade {
    private SubsystemA subsystemA;
    private SubsystemB subsystemB;

    public Facade() {
        subsystemA = new SubsystemA();
        subsystemB = new SubsystemB();
    }

    public void operation() {
        subsystemA.operationA();
        subsystemB.operationB();
    }
}

// Usage
public class FacadePatternDemo {
    public static void main(String[] args) {
        Facade facade = new Facade();
        facade.operation();
    }
}
```

# Design Patterns: Structural and Behavioral

## Structural Patterns

### 1. Flyweight Pattern

**Intent:** Minimize memory usage by sharing as much data as possible with similar objects; it is a way to use objects in large numbers when a simple repeated representation would use an unacceptable amount of memory.

#### Example in Java:

```java
import java.util.HashMap;
import java.util.Map;

// Flyweight interface
interface Vehicle {
    void start();
}

// Concrete Flyweight
class Car implements Vehicle {
    private final String color;

    public Car(String color) {
        this.color = color;
    }

    @Override
    public void start() {
        System.out.println("Starting a " + color + " car.");
    }
}

// Flyweight Factory
class VehicleFactory {
    private static final Map<String, Vehicle> vehicles = new HashMap<>();

    public static Vehicle getCar(String color) {
        Car car = (Car) vehicles.get(color);
        if (car == null) {
            car = new Car(color);
            vehicles.put(color, car);
            System.out.println("Creating a " + color + " car.");
        }
        return car;
    }
}

// Usage
public class FlyweightPatternDemo {
    public static void main(String[] args) {
        Vehicle redCar1 = VehicleFactory.getCar("Red");
        redCar1.start();

        Vehicle redCar2 = VehicleFactory.getCar("Red");
        redCar2.start();

        Vehicle blueCar = VehicleFactory.getCar("Blue");
        blueCar.start();
    }
}
```

***

### 2. Proxy Pattern

**Intent:** Provide a surrogate or placeholder for another object to control access to it.

#### Example in Java:

```java
// Subject interface
interface Image {
    void display();
}

// RealSubject
class RealImage implements Image {
    private final String filename;

    public RealImage(String filename) {
        this.filename = filename;
        loadFromDisk();
    }

    private void loadFromDisk() {
        System.out.println("Loading " + filename);
    }

    @Override
    public void display() {
        System.out.println("Displaying " + filename);
    }
}

// Proxy
class ProxyImage implements Image {
    private final String filename;
    private RealImage realImage;

    public ProxyImage(String filename) {
        this.filename = filename;
    }

    @Override
    public void display() {
        if (realImage == null) {
            realImage = new RealImage(filename);
        }
        realImage.display();
    }
}

// Usage
public class ProxyPatternDemo {
    public static void main(String[] args) {
        Image image = new ProxyImage("test_image.jpg");

        // Image will be loaded from disk
        image.display();
        System.out.println("");

        // Image will not be loaded from disk
        image.display();
    }
}
```

***

## Behavioral Patterns

### 1. Chain of Responsibility Pattern

**Intent:** Avoid coupling the sender of a request to its receiver by giving multiple objects a chance to handle the request. Chain the receiving objects and pass the request along the chain until an object handles it.

#### Example in Java:

```java
abstract class Logger {
    public static int INFO = 1;
    public static int DEBUG = 2;
    public static int ERROR = 3;

    protected int level;
    protected Logger nextLogger;

    public void setNextLogger(Logger nextLogger) {
        this.nextLogger = nextLogger;
    }

    public void logMessage(int level, String message) {
        if (this.level <= level) {
            write(message);
        }
        if (nextLogger != null) {
            nextLogger.logMessage(level, message);
        }
    }

    protected abstract void write(String message);
}

class ConsoleLogger extends Logger {
    public ConsoleLogger(int level) {
        this.level = level;
    }

    @Override
    protected void write(String message) {
        System.out.println("Standard Console::Logger: " + message);
    }
}

class ErrorLogger extends Logger {
    public ErrorLogger(int level) {
        this.level = level;
    }

    @Override
    protected void write(String message) {
        System.out.println("Error Console::Logger: " + message);
    }
}

class FileLogger extends Logger {
    public FileLogger(int level) {
        this.level = level;
    }

    @Override
    protected void write(String message) {
        System.out.println("File::Logger: " + message);
    }
}

// Usage
public class ChainPatternDemo {
    private static Logger getChainOfLoggers() {
        Logger errorLogger = new ErrorLogger(Logger.ERROR);
        Logger fileLogger = new FileLogger(Logger.DEBUG);
        Logger consoleLogger = new ConsoleLogger(Logger.INFO);

        errorLogger.setNextLogger(fileLogger);
        fileLogger.setNextLogger(consoleLogger);

        return errorLogger;
    }

    public static void main(String[] args) {
        Logger loggerChain = getChainOfLoggers();

        loggerChain.logMessage(Logger.INFO, "This is an information.");
        loggerChain.logMessage(Logger.DEBUG, "This is a debug level information.");
        loggerChain.logMessage(Logger.ERROR, "This is an error information.");
    }
}
```

# Design Patterns: Behavioral

## 1. Command Pattern

**Intent:** Encapsulate a request as an object, thereby allowing for parameterization of clients with different requests, queuing of requests, and logging of requests. It also allows for the support of undoable operations.

### Example in Java:

```java
// Command Interface
interface Command {
    void execute();
}

// Receiver Class
class Light {
    public void turnOn() {
        System.out.println("The light is on");
    }

    public void turnOff() {
        System.out.println("The light is off");
    }
}

// Concrete Command to turn on the light
class TurnOnCommand implements Command {
    private Light light;

    public TurnOnCommand(Light light) {
        this.light = light;
    }

    public void execute() {
        light.turnOn();
    }
}

// Concrete Command to turn off the light
class TurnOffCommand implements Command {
    private Light light;

    public TurnOffCommand(Light light) {
        this.light = light;
    }

    public void execute() {
        light.turnOff();
    }
}

// Invoker Class
class RemoteControl {
    private Command command;

    public void setCommand(Command command) {
        this.command = command;
    }

    public void pressButton() {
        command.execute();
    }
}

// Usage
public class CommandPatternDemo {
    public static void main(String[] args) {
        Light livingRoomLight = new Light();
        Command turnOn = new TurnOnCommand(livingRoomLight);
        Command turnOff = new TurnOffCommand(livingRoomLight);

        RemoteControl remote = new RemoteControl();

        // Turn the light on
        remote.setCommand(turnOn);
        remote.pressButton();

        // Turn the light off
        remote.setCommand(turnOff);
        remote.pressButton();
    }
}
```

***

## 2. Interpreter Pattern

**Intent:** Given a language, define a representation for its grammar along with an interpreter that uses the representation to interpret sentences in the language.

### Example in Java:

```java
import java.util.Map;

// Abstract Expression
interface Expression {
    int interpret(Map<String, Integer> context);
}

// Number Expression
class Number implements Expression {
    private int number;

    public Number(int number) {
        this.number = number;
    }

    public int interpret(Map<String, Integer> context) {
        return number;
    }
}

// Addition Expression
class Add implements Expression {
    private Expression leftOperand;
    private Expression rightOperand;

    public Add(Expression left, Expression right) {
        leftOperand = left;
        rightOperand = right;
    }

    public int interpret(Map<String, Integer> context) {
        return leftOperand.interpret(context) + rightOperand.interpret(context);
    }
}

// Usage
public class InterpreterPatternDemo {
    public static void main(String[] args) {
        // Represents the expression '5 + 10'
        Expression expression = new Add(new Number(5), new Number(10));

        // Interpret the expression
        int result = expression.interpret(Map.of());
        System.out.println("Result: " + result); // Output: Result: 15
    }
}
```

***

## 3. Iterator Pattern

**Intent:** Provide a way to access the elements of an aggregate object sequentially without exposing its underlying representation.

### Example in Java:

```java
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

// Collection Class
class NameCollection implements Iterable<String> {
    private List<String> names = new ArrayList<>();

    public void addName(String name) {
        names.add(name);
    }

    public Iterator<String> iterator() {
        return names.iterator();
    }
}

// Usage
public class IteratorPatternDemo {
    public static void main(String[] args) {
        NameCollection collection = new NameCollection();
        collection.addName("Alice");
        collection.addName("Bob");
        collection.addName("Charlie");

        for (String name : collection) {
            System.out.println(name);
        }
    }
}
```

***

## 4. Mediator Pattern

**Intent:** Define an object that encapsulates how a set of objects interact. Mediator promotes loose coupling by keeping objects from referring to each other explicitly, and it lets you vary their interaction independently.

### Example in Java:

```java
import java.util.ArrayList;
import java.util.List;

// Mediator Interface
interface ChatMediator {
    void sendMessage(String message, User user);
    void addUser(User user);
}

// Concrete Mediator
class ChatMediatorImpl implements ChatMediator {
    private List<User> users = new ArrayList<>();

    public void addUser(User user) {
        users.add(user);
    }

    public void sendMessage(String message, User user) {
        for (User u : users) {
            if (u != user) {
                u.receive(message);
            }
        }
    }
}

// User Class
abstract class User {
    protected ChatMediator mediator;
    protected String name;

    public User(ChatMediator mediator, String name) {
        this.mediator = mediator;
        this.name = name;
    }

    public abstract void send(String message);
    public abstract void receive(String message);
}

// Concrete User
class UserImpl extends User {
    public UserImpl(ChatMediator mediator, String name) {
        super(mediator, name);
    }

    public void send(String message) {
        System.out.println(this.name + ": Sending Message = " + message);
        mediator.sendMessage(message, this);
    }

    public void receive(String message) {
        System.out.println(this.name + ": Received Message = " + message);
    }
}

// Usage
public class MediatorPatternDemo {
    public static void main(String[] args) {
        ChatMediator mediator = new ChatMediatorImpl();

        User user1 = new UserImpl(mediator, "Alice");
        User user2 = new UserImpl(mediator, "Bob");
        User user3 = new UserImpl(mediator, "Charlie");

        mediator.addUser(user1);
        mediator.addUser(user2);
        mediator.addUser(user3);

        user1.send("Hello, everyone!");
    }
}
```

# Design Patterns: Behavioral

## 1. Memento Pattern

**Intent:** Without violating encapsulation, capture and externalize an object's internal state so that the object can be restored to this state later.

### Example in Java:

```java
// Memento class
class Memento {
    private String state;

    public Memento(String state) {
        this.state = state;
    }

    public String getState() {
        return state;
    }
}

// Originator class
class Originator {
    private String state;

    public void setState(String state) {
        this.state = state;
    }

    public String getState() {
        return state;
    }

    public Memento saveStateToMemento() {
        return new Memento(state);
    }

    public void getStateFromMemento(Memento memento) {
        state = memento.getState();
    }
}

// Caretaker class
class Caretaker {
    private List<Memento> mementoList = new ArrayList<>();

    public void add(Memento state) {
        mementoList.add(state);
    }

    public Memento get(int index) {
        return mementoList.get(index);
    }
}

// Usage
public class MementoPatternDemo {
    public static void main(String[] args) {
        Originator originator = new Originator();
        Caretaker caretaker = new Caretaker();

        originator.setState("State #1");
        originator.setState("State #2");
        caretaker.add(originator.saveStateToMemento());

        originator.setState("State #3");
        caretaker.add(originator.saveStateToMemento());

        originator.setState("State #4");
        System.out.println("Current State: " + originator.getState());

        originator.getStateFromMemento(caretaker.get(0));
        System.out.println("First saved State: " + originator.getState());
        originator.getStateFromMemento(caretaker.get(1));
        System.out.println("Second saved State: " + originator.getState());
    }
}
```

***

## 2. Observer Pattern

**Intent:** Define a one-to-many dependency between objects so that when one object changes state, all its dependents are notified and updated automatically.

### Example in Java:

```java
import java.util.ArrayList;
import java.util.List;

// Subject class
class Subject {
    private List<Observer> observers = new ArrayList<>();
    private int state;

    public int getState() {
        return state;
    }

    public void setState(int state) {
        this.state = state;
        notifyAllObservers();
    }

    public void attach(Observer observer) {
        observers.add(observer);
    }

    public void notifyAllObservers() {
        for (Observer observer : observers) {
            observer.update();
        }
    }
}

// Observer abstract class
abstract class Observer {
    protected Subject subject;
    public abstract void update();
}

// Concrete Observer classes
class BinaryObserver extends Observer {
    public BinaryObserver(Subject subject) {
        this.subject = subject;
        this.subject.attach(this);
    }

    @Override
    public void update() {
        System.out.println("Binary String: " + Integer.toBinaryString(subject.getState()));
    }
}

class HexaObserver extends Observer {
    public HexaObserver(Subject subject) {
        this.subject = subject;
        this.subject.attach(this);
    }

    @Override
    public void update() {
        System.out.println("Hex String: " + Integer.toHexString(subject.getState()).toUpperCase());
    }
}

// Usage
public class ObserverPatternDemo {
    public static void main(String[] args) {
        Subject subject = new Subject();

        new HexaObserver(subject);
        new BinaryObserver(subject);

        System.out.println("First state change: 15");
        subject.setState(15);
        System.out.println("Second state change: 10");
        subject.setState(10);
    }
}
```

***

## 3. State Pattern

**Intent:** Allow an object to alter its behavior when its internal state changes. The object will appear to change its class.

### Example in Java:

```java
// State interface
interface State {
    void doAction(Context context);
}

// Concrete State classes
class StartState implements State {
    public void doAction(Context context) {
        System.out.println("Player is in start state");
        context.setState(this);
    }

    public String toString() {
        return "Start State";
    }
}

class StopState implements State {
    public void doAction(Context context) {
        System.out.println("Player is in stop state");
        context.setState(this);
    }

    public String toString() {
        return "Stop State";
    }
}

// Context class
class Context {
    private State state;

    public Context() {
        state = null;
    }

    public void setState(State state) {
        this.state = state;
    }

    public State getState() {
        return state;
    }
}

// Usage
public class StatePatternDemo {
    public static void main(String[] args) {
        Context context = new Context();

        StartState startState = new StartState();
        startState.doAction(context);
        System.out.println(context.getState().toString());

        StopState stopState = new StopState();
        stopState.doAction(context);
        System.out.println(context.getState().toString());
    }
}
```

# Design Patterns: Behavioral

## 1. Strategy Pattern

**Intent:** Define a family of algorithms, encapsulate each one, and make them interchangeable. Strategy lets the algorithm vary independently from clients that use it.

### Example in Java:

```java
// Strategy interface
interface Strategy {
    int doOperation(int num1, int num2);
}

// Concrete Strategies
class OperationAdd implements Strategy {
    public int doOperation(int num1, int num2) {
        return num1 + num2;
    }
}

class OperationSubtract implements Strategy {
    public int doOperation(int num1, int num2) {
        return num1 - num2;
    }
}

class OperationMultiply implements Strategy {
    public int doOperation(int num1, int num2) {
        return num1 * num2;
    }
}

// Context class
class Context {
    private Strategy strategy;

    public Context(Strategy strategy) {
        this.strategy = strategy;
    }

    public int executeStrategy(int num1, int num2) {
        return strategy.doOperation(num1, num2);
    }
}

// Usage
public class StrategyPatternDemo {
    public static void main(String[] args) {
        Context context = new Context(new OperationAdd());
        System.out.println("10 + 5 = " + context.executeStrategy(10, 5));

        context = new Context(new OperationSubtract());
        System.out.println("10 - 5 = " + context.executeStrategy(10, 5));

        context = new Context(new OperationMultiply());
        System.out.println("10 * 5 = " + context.executeStrategy(10, 5));
    }
}
```

***

## 2. Template Method Pattern

**Intent:** Define the skeleton of an algorithm in an operation, deferring some steps to subclasses. Template Method lets subclasses redefine certain steps of an algorithm without changing the algorithm's structure.

### Example in Java:

```java
// Abstract class with template method
abstract class Game {
    abstract void initialize();
    abstract void startPlay();
    abstract void endPlay();

    // Template method
    public final void play() {
        initialize();
        startPlay();
        endPlay();
    }
}

// Concrete classes
class Cricket extends Game {
    void initialize() {
        System.out.println("Cricket Game Initialized! Start playing.");
    }
    void startPlay() {
        System.out.println("Cricket Game Started. Enjoy the game!");
    }
    void endPlay() {
        System.out.println("Cricket Game Finished!");
    }
}

class Football extends Game {
    void initialize() {
        System.out.println("Football Game Initialized! Start playing.");
    }
    void startPlay() {
        System.out.println("Football Game Started. Enjoy the game!");
    }
    void endPlay() {
        System.out.println("Football Game Finished!");
    }
}

// Usage
public class TemplatePatternDemo {
    public static void main(String[] args) {
        Game game = new Cricket();
        game.play();
        System.out.println();
        game = new Football();
        game.play();
    }
}
```

***

## 3. Visitor Pattern

**Intent:** Represent an operation to be performed on the elements of an object structure. Visitor lets you define a new operation without changing the classes of the elements on which it operates.

### Example in Java:

```java
// Element interface
interface ComputerPart {
    void accept(ComputerPartVisitor computerPartVisitor);
}

// Concrete Elements
class Keyboard implements ComputerPart {
    public void accept(ComputerPartVisitor computerPartVisitor) {
        computerPartVisitor.visit(this);
    }
}

class Monitor implements ComputerPart {
    public void accept(ComputerPartVisitor computerPartVisitor) {
        computerPartVisitor.visit(this);
    }
}

class Mouse implements ComputerPart {
    public void accept(ComputerPartVisitor computerPartVisitor) {
        computerPartVisitor.visit(this);
    }
}

class Computer implements ComputerPart {
    ComputerPart[] parts;

    public Computer() {
        parts = new ComputerPart[] {new Mouse(), new Keyboard(), new Monitor()};
    }

    public void accept(ComputerPartVisitor computerPartVisitor) {
        for (int i = 0; i < parts.length; i++) {
            parts[i].accept(computerPartVisitor);
        }
        computerPartVisitor.visit(this);
    }
}

// Visitor interface
interface ComputerPartVisitor {
    void visit(Computer computer);
    void visit(Mouse mouse);
    void visit(Keyboard keyboard);
    void visit(Monitor monitor);
}

// Concrete Visitor
class ComputerPartDisplayVisitor implements ComputerPartVisitor {
    public void visit(Computer computer) {
        System.out.println("Displaying Computer.");
    }

    public void visit(Mouse mouse) {
        System.out.println("Displaying Mouse.");
    }

    public void visit(Keyboard keyboard) {
        System.out.println("Displaying Keyboard.");
    }

    public void visit(Monitor monitor) {
        System.out.println("Displaying Monitor.");
    }
}

// Usage
public class VisitorPatternDemo {
    public static void main(String[] args) {
        ComputerPart computer = new Computer();
        computer.accept(new ComputerPartDisplayVisitor());
    }
}
```

## Comparison Table of GoF Patterns

| Pattern   | Type       | Purpose                                              |
| --------- | ---------- | ---------------------------------------------------- |
| Singleton | Creational | Ensures a single instance of a class                 |
| Factory   | Creational | Creates objects without specifying exact class       |
| Adapter   | Structural | Bridges incompatible interfaces                      |
| Proxy     | Structural | Controls access to another object                    |
| Observer  | Behavioral | Allows objects to react to changes in another object |

## Summary of Key Ideas

* The Gang of Four wrote the **design patterns bible**.
* Their patterns solve **common software design problems**.
* Patterns are categorized into **Creational, Structural, and Behavioral**.
* Java examples make them **easy to understand and use**.

## References

* <https://en.wikipedia.org/wiki/Design_Patterns>
* [https://en.wikipedia.org/wiki/Gang\_of\_Four\_(design\_patterns)](https://en.wikipedia.org/wiki/Gang_of_Four_%28design_patterns%29)
* <https://en.wikipedia.org/wiki/Singleton_pattern>
* <https://en.wikipedia.org/wiki/Adapter_pattern>
* <https://en.wikipedia.org/wiki/Observer_pattern>
