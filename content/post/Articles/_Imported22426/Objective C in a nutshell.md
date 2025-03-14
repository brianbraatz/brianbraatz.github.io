---
title: Objective-C in a Nutshell
description: With Code Examples
slug: objective-c-in-a-nutshell-with-10-code-examples
date: 2015-11-03
image: post/Articles/IMAGES/objectivec.png
categories:
  - Programming
  - Objective-C
  - Code Examples
  - CPP
  - iPhone
  - Mobile
tags:
  - Programming
  - Objective-C
  - Examples
  - iOS
  - MacOS
  - Syntax
  - Apple
  - NeXT
draft: false
weight: 35
categories_ref:
  - Programming
  - Objective-C
  - Code Examples
  - CPP
  - iPhone
  - Mobile
lastmod: 2025-03-14T15:45:17.580Z
---
Objective-C is an object-oriented programming language developed in the early 1980s.

It was created by Brad Cox and Tom Love and was later adopted by NeXT, the company founded by Steve Jobs after leaving Apple in 1985.

When Apple acquired NeXT in 1996, Objective-C became the primary language for macOS and iOS development.

## A Brief History of Objective-C

Objective-C was designed as an extension of the C programming language, adding Smalltalk-style messaging to bring object-oriented capabilities.

It became the foundation of Apple's software ecosystem, including macOS and iOS applications, until Swift emerged as its successor in 2014.

NeXT played a crucial role in popularizing Objective-C through its NeXTSTEP OS, which later evolved into macOS.

Steve Jobs’ vision  for a modern computing platform heavily influenced Apple's transition to macOS and iOS,  making Objective-C's popular for decades.

Also you will notice a lot of "NS" prefixes- Thats for NextStep..

So to keep backwards compatibility , with the NextStep code.. they held on to the NS prefix for quite awhile..

I guess the positive of this would be- out of the gate - when Mac OS was released- they could already leverage some of the Software Companies making NextStep apps, to make Mac OS apps..

It is a little weird though.. :)

## 1. Hello, World!

```objective-c
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSLog(@"Hello, World!");
    }
    return 0;
}
```

The `NSLog` function prints text to the console, and `@autoreleasepool` manages memory for Objective-C objects.

## 2. Variables and Constants

```objective-c
NSString *name = @"Objective-C";
int age = 10;
age += 1;
```

Objective-C uses `NSString *` for string objects and standard C types for primitives.

## 3. Functions

```objective-c
NSString *greet(NSString *name) {
    return [NSString stringWithFormat:@"Hello, %@!", name];
}
```

Functions in Objective-C use the `NSString` class for string manipulation.

## 4. Pointers and Memory Management

```objective-c
NSString *message = [[NSString alloc] initWithFormat:@"Memory Management in Objective-C"];
NSLog(@"%@", message);
[message release];
```

Before Automatic Reference Counting (ARC), manual memory management required using `alloc` and `release`.

## 5. Conditional Expressions

```objective-c
int number = 10;
NSString *result = (number > 0) ? @"Positive" : @"Negative";
```

Objective-C supports ternary conditional expressions, similar to C.

## 6. Loops

```objective-c
for (int i = 1; i <= 5; i++) {
    NSLog(@"%d", i);
}
```

Objective-C supports traditional C-style loops for iteration.

## 7. Switch Statement

```objective-c
char grade = 'A';
switch (grade) {
    case 'A':
        NSLog(@"Excellent!");
        break;
    case 'B':
        NSLog(@"Good job!");
        break;
    default:
        NSLog(@"Keep trying!");
        break;
}
```

`switch` statements in Objective-C follow C syntax.

## 8. Classes and Objects

```objective-c
@interface Person : NSObject
@property NSString *name;
@property int age;
@end

@implementation Person
@end

Person *person = [Person new];
person.name = @"Alice";
NSLog(@"%@", person.name);
```

Objective-C classes use `@interface` and `@implementation` keywords.

## 9. Categories (Extensions)

```objective-c
@interface NSString (Shouting)
- (NSString *)shout;
@end

@implementation NSString (Shouting)
- (NSString *)shout {
    return [self uppercaseString];
}
@end

NSLog(@"%@", [@"hello" shout]);
```

Categories allow adding methods to existing classes without modifying them.

## 10. Blocks (Closures)

```objective-c
void (^printMessage)(void) = ^{
    NSLog(@"Objective-C Blocks Example");
};
printMessage();
```

Blocks provide inline function expressions similar to Swift closures.

***

## Key Ideas Table

| Key Idea             | Summary                                                                            |
| -------------------- | ---------------------------------------------------------------------------------- |
| Objective-C Overview | A C-based object-oriented language used for macOS and iOS development              |
| History              | Developed in the 1980s, adopted by NeXT, and later became Apple’s primary language |
| Hello World          | Uses `NSLog` to print messages to the console                                      |
| Variables            | Uses `NSString *` for strings and C types for primitives                           |
| Functions            | Objective-C functions use `NSString` for string handling                           |
| Memory Management    | Used manual `retain` and `release` before ARC                                      |
| Conditionals         | Supports ternary operators and `switch` statements                                 |
| Classes              | Uses `@interface` and `@implementation` for defining classes                       |
| Categories           | Allows extending existing classes without modification                             |
| Blocks               | Provides inline function expressions for callbacks and functional programming      |

***

## References

1. [Apple's Objective-C Guide](https://developer.apple.com/documentation/objectivec)
2. [History of Objective-C](https://www.cocoawithlove.com/2010/07/brief-history-of-objective-c.html)
3. [NeXT and Objective-C](https://www.folklore.org/StoryView.py?project=Macintosh\&story=NeXT.txt)
4. [Steve Jobs and NeXT](https://www.macworld.com/article/224502/steve-jobs-next-apple.html)
5. [Objective-C Programming](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/Introduction/Introduction.html)

***
