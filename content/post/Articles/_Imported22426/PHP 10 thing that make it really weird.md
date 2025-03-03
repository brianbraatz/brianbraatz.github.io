---
title: "PHP is WEIRD: 10 Things That Are Weird About PHP"
description: Weird PHP quirks that you won't find in most other programming languages.
slug: php-weird-10-things-most-languages-dont-do
date: 2017-05-18
image: post/Articles/IMAGES/29.jpg
categories:
  - PHP
  - Programming Humor
  - Coding Oddities
  - SQL
  - Cloud
tags:
  - PHP
  - Weirdness
  - Quirks
  - Programming
  - Oddities
  - Strange
  - Behavior
draft: false
weight: 532
lastmod: 2025-03-03T03:54:22.348Z
---
PHP is the lovable, chaotic gremlin of programming languages.\
It runs most of the web, powers countless CMSes, and yet—sometimes—it makes absolutely no sense.\
If you’ve ever written PHP, you’ve probably thought, *"Why does it do this?!"*

***

## 1. `$0 == "0"` but `$0 === "0"` is false 🤯

PHP has *loose* and *strict* comparison.\
But why does `$0 == "0"` evaluate to `true`, while `$0 === "0"` evaluates to `false`?

```php
<?php
var_dump(0 == "0");   // true
var_dump(0 === "0");  // false
?>
```

Loose comparisons in PHP convert types behind your back, sometimes in hilariously unexpected ways.

***

## 2. `"php" + 5 == 5`

Most languages would scream in agony if you tried to add a string to a number.\
PHP? It just shrugs and says, "Sure, let’s pretend that string is a zero."

```php
<?php
var_dump("php" + 5); // int(5)
?>
```

No error, just PHP being PHP. 🤡

***

## 3. Arrays Can Have Non-Sequential Integer Keys

PHP arrays are secretly maps. That means you can do this:

```php
<?php
$array = [
    0 => "zero",
    2 => "two",
    100 => "one hundred"
];
var_dump($array);
?>
```

Most languages would make you define a dictionary or map for this. PHP? Nah.

***

## 4. `NULL` == `0` but `NULL > 0` is `false`

How can NULL be equal to zero but also not greater than it? 😵‍💫

```php
<?php
var_dump(NULL == 0); // true
var_dump(NULL > 0);  // false
?>
```

PHP’s comparison logic makes no sense, but at this point, we just roll with it.

***

## 5. The `+` Operator Only Works on Numbers, but `.` Concatenates Strings

Want to add two numbers? Use `+`. Want to concatenate two strings? Use `.`.\
Try using `+` for strings and… PHP laughs at you.

```php
<?php
var_dump("Hello" + "World"); // Fatal error!
var_dump("Hello" . "World"); // Works fine
?>
```

***

## 6. Functions Can Return Different Types for No Reason

A function can return an integer… or an array… or a string… all in one function.\
PHP doesn’t mind, but your brain might.

```php
<?php
function magic() {
    if (rand(0, 1)) return "String";
    return [1, 2, 3];
}
var_dump(magic());
?>
```

Type safety? We don’t do that here. (Unless you're using PHP 8’s strict types.)

***

## 7. The `empty()` Function

`empty()` is supposed to check if a value is "empty," but what counts as empty?

```php
<?php
var_dump(empty(0));      // true
var_dump(empty("0"));    // true
var_dump(empty([]));     // true
var_dump(empty(false));  // true
?>
```

In PHP, "empty" means "basically anything falsy, including some things you might expect to be valid."

***

## 8. Magic Quotes (RIP but Never Forgotten)

Once upon a time, PHP **automatically** escaped every input.

```php
<?php
// Before PHP 5.4, this would automatically escape input!
$name = $_GET["name"];
?>
```

This led to countless double-escaping issues before it was finally removed.\
Still, some servers live in the past and keep this horror alive. 😱

***

## 9. Constants Aren’t Always Constant

Constants should, well, stay constant, right? Not in PHP!

```php
<?php
define("FOO", "bar");
define("FOO", "baz"); // No error! (Before PHP 7.3)
?>
```

Before PHP 7.3, redefining constants wouldn’t even trigger an error. Chaos reigned supreme.

***

## 10. The `goto` Keyword Exists (And It Works)

Yes, PHP has `goto`. And yes, it works.\
But no, please never, ever use it.

```php
<?php
goto skip;
echo "This will be skipped.";
skip:
echo "PHP has goto!";
?>
```

Most modern languages avoid `goto`, but PHP keeps it just in case someone wants to ruin their own day.

***

## Conclusion

PHP is weird. It has bizarre quirks, strange inconsistencies, and rules that feel completely arbitrary.\
Yet, despite all of that, it **powers a massive chunk of the internet**.

If nothing else, PHP keeps us on our toes—and gives us plenty to laugh about. 😆

***

## Key Ideas

| Quirk                       | Explanation                                         |
| --------------------------- | --------------------------------------------------- |
| Loose vs Strict Comparisons | `0 == "0"` is `true`, but `0 === "0"` is `false`    |
| String Math                 | `"php" + 5` results in `5`                          |
| Non-Sequential Array Keys   | PHP allows arrays with gaps in their keys           |
| NULL Comparisons            | `NULL == 0` but `NULL > 0` is `false`               |
| `+` vs `.` Operators        | `+` is for numbers, `.` is for string concatenation |
| Dynamic Return Types        | Functions can return multiple types unpredictably   |
| `empty()` Function          | Determines "emptiness" in a strange way             |
| Magic Quotes                | Automatically escaped input (now deprecated)        |
| Mutable Constants           | Constants could be redefined in older PHP versions  |
| `goto` Exists               | Yes, PHP actually has `goto`                        |

## References

* [PHP Manual: Comparison Operators](https://www.php.net/manual/en/language.operators.comparison.php)
* [PHP `goto` Statement](https://www.php.net/manual/en/control-structures.goto.php)
* [History of PHP Magic Quotes](https://www.php.net/manual/en/security.magicquotes.php)
