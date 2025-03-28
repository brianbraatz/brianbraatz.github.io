---
title: Android Cheatsheet
description: Buzzword Decoder + important bits of the Android SDK
slug: Android-SDK-cheatsheet
date: 2014-05-06
image: post/Articles/IMAGES/androidwide.jpg
categories:
  - Android
  - Java
  - Mobile
tags:
  - Cheatsheet
  - Angular
  - Typescript
  - WebDevelopment
  - Android
  - Java
  - Javascript
weight: 200
draft: false
categories_ref:
  - Android
  - Java
  - Mobile
slug_calculated: https://brianbraatz.github.io/p/Android-SDK-cheatsheet
lastmod: 2025-03-14T16:40:18.364Z
---
| **Category**             | **Description**                                                        | **Important Classes/Components**                                       |
| ------------------------ | ---------------------------------------------------------------------- | ---------------------------------------------------------------------- |
| **App Structure**        | Structure of an Android project, including important directories.      | `src/`, `res/`, `AndroidManifest.xml`                                  |
| **Activities**           | Represents a screen in an app; controls UI interaction.                | `Activity`, `Intent`, `onCreate()`, `onStart()`, `onResume()`          |
| **Views & Widgets**      | UI components like buttons, text fields, images.                       | `TextView`, `Button`, `ImageView`, `RecyclerView`, `EditText`          |
| **Layouts**              | Defines how views are arranged on the screen.                          | `LinearLayout`, `RelativeLayout`, `ConstraintLayout`                   |
| **Intents**              | Used for communication between components (e.g., starting activities). | `Intent`, `BroadcastReceiver`, `PendingIntent`                         |
| **Resources**            | Store application resources such as strings, images, and colors.       | `res/`, `strings.xml`, `drawable/`, `values/colors.xml`                |
| **Permissions**          | Manage app permissions for accessing device features.                  | `AndroidManifest.xml`, `checkSelfPermission()`, `requestPermissions()` |
| **Storage**              | Data storage options for saving and retrieving data.                   | `SharedPreferences`, `SQLiteDatabase`, `File`, `ContentProvider`       |
| **Networking**           | Communication with external servers and APIs.                          | `HttpURLConnection`, `Retrofit`, `Volley`, `OkHttp`                    |
| **Background Tasks**     | Perform tasks in the background (e.g., downloading data).              | `AsyncTask`, `Handler`, `ExecutorService`, `WorkManager`               |
| **Fragments**            | Reusable UI components that can be combined within an activity.        | `Fragment`, `FragmentTransaction`, `FragmentManager`                   |
| **Services**             | For long-running operations in the background (e.g., music playback).  | `Service`, `IntentService`, `JobIntentService`                         |
| **Broadcast Receivers**  | Listen for and respond to system-wide broadcast announcements.         | `BroadcastReceiver`, `sendBroadcast()`, `LocalBroadcastManager`        |
| **UI Thread & Handlers** | Manage UI updates and tasks in the background.                         | `Handler`, `Looper`, `runOnUiThread()`, `MessageQueue`                 |
| **Notifications**        | Display notifications to users outside the app's UI.                   | `Notification`, `NotificationManager`, `NotificationChannel`           |
| **Testing**              | Tools for testing Android applications.                                | `JUnit`, `Mockito`, `Espresso`, `Robolectric`                          |
