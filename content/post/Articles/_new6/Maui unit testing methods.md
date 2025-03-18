---
title: Maui Unit Testing Strategies
description: Testing and How to Structure Your Project
slug: best-unit-testing-strategies-for-maui
date: 2022-04-21
image: post/Articles/IMAGES/mauilogo1.png
categories:
  - WPF
  - XAML
  - Maui
  - Unit Testing
  - CSharp
  - DotNet
  - Testing
  - Mobile
tags:
  - Unit
  - Testing
  - Maui
  - Testing
  - Strategies
  - Mocking
  - Mstest
  - Xunit
  - Nunit
  - Moq
  - Fakeiteasy
  - Justmock
  - Dependency
  - Injection
  - Ui
  - Testing
draft: false
weight: 246
categories_ref:
  - WPF
  - XAML
  - Maui
  - Unit Testing
  - CSharp
  - DotNet
  - Testing
  - Mobile
slug_calculated: https://brianbraatz.github.io/p/best-unit-testing-strategies-for-maui:-alternatives-and-how-to-structure-your-project
lastmod: 2025-03-18T19:28:38.150Z
---
<!--
# Best Unit Testing Strategies for MAUI: Alternatives and How to Structure Your Project
-->

## Introduction

So, you’re building a **.NET MAUI** app and want to **test it properly**? First off, **good call!** Unit testing helps you avoid bugs before they reach production, but **testing MAUI apps can be tricky**.

### The problem?

* UI elements don't play nicely with unit tests.
* MAUI apps rely on **dependency injection** and **platform-specific code**.
* Mocking frameworks have **limitations** in mobile development. i.e iPhone - Android

### The solution?

* Use **the right testing framework** (MSTest, NUnit, or xUnit).
* **Mock dependencies** using Moq, FakeItEasy, or JustMock.
* **Structure your project** to **separate UI from logic**.

***

## A Brief History of Unit Testing in .NET MAUI

### **Unit Testing in .NET: The Early Days**

Before **MAUI**, developers built cross-platform apps with **Xamarin.Forms**. Unit testing was already a challenge due to **tight coupling** between UI and logic.

With **MAUI**, Microsoft **introduced dependency injection** as a core feature, making unit testing **easier** but still requiring **proper architecture**.

### **Why Unit Testing is Hard in MAUI**

1. **UI Testing is difficult** → You can't test UI elements in traditional unit tests.
2. **Dependency Injection (DI)** → Services must be mocked correctly.
3. **Platform-Specific Code** → Native features need abstraction.

***

## Choosing the Right Unit Testing Framework

| Framework  | Pros                         | Cons                 |
| ---------- | ---------------------------- | -------------------- |
| **MSTest** | Official Microsoft framework | Verbose syntax       |
| **NUnit**  | Simple and widely supported  | Needs extra setup    |
| **xUnit**  | Modern and flexible          | Less tooling support |

***

## Best Testing Strategies for MAUI

### **1. Separate UI from Logic** (MVVM)

The **Model-View-ViewModel (MVVM) pattern** makes unit testing easier by **moving logic away from the UI**.

#### **Bad Example: Logic Inside Code-Behind (Hard to Test)**

```csharp
public partial class MainPage : ContentPage
{
    public MainPage()
    {
        InitializeComponent();
        Button.Clicked += (s, e) => Label.Text = "Clicked!";
    }
}
```

#### **Good Example: Logic in a ViewModel (Testable)**

```csharp
public class MainViewModel : INotifyPropertyChanged
{
    public Command ClickCommand { get; }
    private string _text = "Click me!";

    public string Text 
    { 
        get => _text; 
        set { _text = value; OnPropertyChanged(); } 
    }

    public MainViewModel()
    {
        ClickCommand = new Command(() => Text = "Clicked!");
    }

    public event PropertyChangedEventHandler PropertyChanged;
    protected void OnPropertyChanged([CallerMemberName] string name = "") =>
        PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(name));
}
```

#### **Unit Test for ViewModel (Using xUnit and Moq)**

```csharp
public class MainViewModelTests
{
    [Fact]
    public void ClickCommand_ShouldUpdateText()
    {
        var vm = new MainViewModel();
        vm.ClickCommand.Execute(null);
        Assert.Equal("Clicked!", vm.Text);
    }
}
```

***

### **2. Use Dependency Injection and Mock Services**

#### **Without DI (Hard to Test)**

```csharp
public class LoginService
{
    public bool Login(string username, string password) => username == "admin";
}
```

#### **With DI (Easier to Mock)**

```csharp
public interface ILoginService
{
    bool Login(string username, string password);
}

public class LoginService : ILoginService
{
    public bool Login(string username, string password) => username == "admin";
}
```

#### **Mocking the Service in Unit Tests (Using Moq)**

```csharp
public class LoginViewModelTests
{
    [Fact]
    public void Login_ShouldCallLoginService()
    {
        var mockService = new Mock<ILoginService>();
        mockService.Setup(s => s.Login("admin", "password")).Returns(true);

        var vm = new LoginViewModel(mockService.Object);
        vm.LoginCommand.Execute("admin");

        mockService.Verify(s => s.Login("admin", "password"), Times.Once);
    }
}
```

***

### **3. UI Testing: When Unit Tests Aren’t Enough**

Unit tests **can't** verify UI elements, but **UI testing frameworks like Appium and Playwright** can.

#### **UI Test Example Using Playwright**

```csharp
[Test]
public async Task ShouldClickButtonAndVerifyText()
{
    await Page.ClickAsync("#submit-button");
    string text = await Page.InnerTextAsync("#result-label");
    Assert.AreEqual("Clicked!", text);
}
```

***

## Project Structure for Testable MAUI Apps

### **Best Practice: Separate UI, Logic, and Tests**

```plaintext
/MyMauiApp
  ├── MyMauiApp (Main Project)
  ├── MyMauiApp.Tests (Unit Tests)
  ├── MyMauiApp.Services (Business Logic)
  ├── MyMauiApp.UI (UI Layer)
```

**Why?**

* **Unit tests** should **never touch UI code**.
* **Mock dependencies** easily using **dependency injection**.
* **Separate concerns** to avoid **tight coupling**.

***

## Alternative Approaches and Their Pros & Cons

| Approach                            | Pros                         | Cons                      |
| ----------------------------------- | ---------------------------- | ------------------------- |
| **MVVM + Unit Testing**             | Clean separation of concerns | Requires more setup       |
| **Code-Behind Testing**             | Simpler for small projects   | Hard to test UI logic     |
| **UI Testing (Appium, Playwright)** | Tests full app behavior      | Slower and requires setup |
| **Snapshot Testing**                | Catches UI changes           | No deep logic validation  |

***

## Key Ideas

* **Use MVVM** to make unit testing easier.
* **Use Dependency Injection** to mock services.
* **Choose the right testing framework** (MSTest, NUnit, xUnit).
* **UI testing** should be separate from **unit testing**.
* **Structure your project properly** to avoid **tight coupling**.

***

## References

4. [MAUI Unit Testing Guide](https://learn.microsoft.com/en-us/dotnet/maui/)
5. [MVVM Best Practices](https://devblogs.microsoft.com/dotnet/mvvm-in-maui/)
6. [MSTest vs NUnit vs xUnit](https://www.unit-testing-frameworks.com/mstest-vs-nunit-vs-xunit/)
7. [Using Moq for Dependency Injection](https://docs.microsoft.com/en-us/aspnet/core/testing/unit-testing)
8. [UI Testing with Playwright](https://playwright.dev/)
