---
title: Angular Cheatsheet
description: 
slug: Angular-file-cheatsheet
date: 2014-05-06
image: post/Articles/IMAGES/18.jpg
categories: 
tags:
  - Cheatsheet
  - Angular
  - Typescript
  - WebDevelopment
weight: 10
draft: false
lastmod: 2025-01-31T17:35:24.485Z
---
### Angular Cheatsheet

| **Concept**              | **Syntax/Example**                                                                                                            | **Description**                                           |                                          |
| ------------------------ | ----------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------- | ---------------------------------------- |
| **Component**            | `@Component({ selector: 'app-root', ... })`                                                                                   | Defines an Angular component                              |                                          |
| **Template**             | `<h1>{{title}}</h1>`                                                                                                          | Defines the HTML template for a component                 |                                          |
| **Interpolation**        | `{{ expression }}`                                                                                                            | Binds data to the template using double curly braces      |                                          |
| **Property Binding**     | `[property]="expression"`                                                                                                     | Binds a property of an HTML element to an expression      |                                          |
| **Event Binding**        | `(event)="handler"`                                                                                                           | Binds an event of an HTML element to an event handler     |                                          |
| **Two-way Binding**      | `[(ngModel)]="property"`                                                                                                      | Binds a property to a form input for two-way data binding |                                          |
| **Directive**            | `@Directive({ selector: '[appDirective]' })`                                                                                  | Defines a custom directive                                |                                          |
| **Built-in Directives**  | `*ngIf`, `*ngFor`, `*ngSwitch`, `ngClass`, `ngStyle`                                                                          | Common built-in directives                                |                                          |
| **Services**             | `@Injectable({ providedIn: 'root' })`                                                                                         | Defines a service for dependency injection                |                                          |
| **Dependency Injection** | `constructor(private service: MyService) { }`                                                                                 | Injects a service into a component or other service       |                                          |
| **Routing**              | `RouterModule.forRoot(routes)`                                                                                                | Configures routing for an Angular application             |                                          |
| **Module**               | `@NgModule({ declarations: [...], imports: [...] })`                                                                          | Defines an Angular module                                 |                                          |
| **Lifecycle Hooks**      | `ngOnInit()`, `ngOnDestroy()`, `ngAfterViewInit()`                                                                            | Lifecycle hooks for components                            |                                          |
| **Pipes**                | \`{{ value                                                                                                                    | pipe }}\`                                                 | Transforms data in templates using pipes |
| **Custom Pipes**         | `@Pipe({ name: 'myPipe' })`                                                                                                   | Defines a custom pipe                                     |                                          |
| **Forms**                | `ReactiveFormsModule`, `FormBuilder`, `FormGroup`, `FormControl`                                                              | Handling forms with reactive forms                        |                                          |
| **HttpClient**           | `HttpClientModule`, `HttpClient.get()`, `HttpClient.post()`                                                                   | Making HTTP requests                                      |                                          |
| **Guards**               | `@Injectable({ providedIn: 'root' }) export class AuthGuard implements CanActivate {}`                                        | Defines route guards for authentication and authorization |                                          |
| **Animations**           | `@Component({ animations: [ trigger('animation', [ state('state', style({ ... })), transition('...', animate('...')) ]) ] })` | Adds animations to components                             |                                          |
