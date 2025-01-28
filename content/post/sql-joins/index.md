---
title: Sql Joins
description: Illustrated SQL Joins
slug: sql-joins-illustrated
date: 2012-03-06 00:00:00+0000
image: cover-sql-joins.png
categories: []
tags:
  - SQL
  - Mentoring
weight: 5
imag: cover.jpg
lastmod: 2025-01-28T22:09:33.659Z
---
Details

# Sql Joins Illustrated

#### *Brian Braatz*

* 1 SQL Joins Illustrated - MySql
  * 1.1 Database Setup
  * 1.2 Sample Data
  * 1.3 Dataset - Table
  * 1.4 Dataset - Venn Diagram
  * 1.5 Inner Join
  * 1.6 Left Outer Join
  * 1.7 Right Outer Join
  * 1.8 Cartesian Join \ Cross Join

# 1 SQL Joins Illustrated - MySql

## 1.1 Database Setup

First, lets setup a simple script to create a test database:

```sql
-- Database setup
DROP DATABASE IF EXISTS sqljoins;       -- Delete if it exists
CREATE DATABASE sqljoins;               -- Create a new database
USE sqljoins;                           -- Set the default (current) database

-- Table Setup
DROP TABLE IF EXISTS test;

-- Table to hold Soccer Team
CREATE TABLE SoccerPlayer
(
BaseballPlayerID int,
Name varchar(255)
); 

DROP TABLE IF EXISTS Address;

-- Table to hold Basketball team
CREATE TABLE BasketballPlayer
(
BasketballPlayerID int,
Name varchar(255)
); 
```

## 1.2 Sample Data

In the below sample, we have modelled a basketball and a soccer team.

For example simplicity, we will be joining on the name field.

```sql
-- Insert Basketball Players
INSERT INTO `BasketballPlayer` VALUES (1,'Alan');
INSERT INTO `BasketballPlayer` VALUES (2,'Amanda');
INSERT INTO `BasketballPlayer` VALUES (3,'Tay');
INSERT INTO `BasketballPlayer` VALUES (4,'Sally');

-- Insert Soccer Players
INSERT INTO `SoccerPlayer` VALUES (1,'Amanda');
INSERT INTO `SoccerPlayer` VALUES (2,'Sally');
INSERT INTO `SoccerPlayer` VALUES (3,'Jose');
INSERT INTO `SoccerPlayer` VALUES (4,'Ian');
```

Looking at the data you will notice that only Amanda and Sally play both Soccer and Basketball.

## 1.3 Dataset - Table

```sql
      BasketballPlayer TABLE              SoccerPlayer TABLE
+ -------------------- + --------- +    + --------------- + --------- +
| BasketballPlayerID   | Name      |    | SoccerPlayerID  | Name      |
+ ---------------------+ --------- +    + ----------------+ --------- +
| 1                    | Alan      |    | 1               | Amanda    |
| 2                    | Amanda    |    | 2               | Sally     |
| 3                    | Tay       |    | 3               | Jose      |
| 4                    | Sally     |    | 4               | Ian       |
+ -------------------- + --------- +    + --------------- + --------- +
```

## 1.4 Dataset - Venn Diagram

![post/sql-joins/sqljoins1.png](/post/sql-joins/sqljoins1.png)\
![](/post/sql-joins/sqljoins1.png)

## 1.5 Inner Join

The Inner Join shows only the rows that exist in both tables. Visualize it as the inner section of the Venn diagram.

> select \* from BasketballPlayer BP INNER JOIN SoccerPlayer SP on BP.Name = SP.Name

**OR**

> select \* from BasketballPlayer BP , SoccerPlayer SP where BP.Name = SP.Name;

```sql
+ ----------------------- + --------- + ------------------- + --------- +
| BasketballPlayerID      | Name      | SoccerPlayerID      | Name      |
+ ----------------------- + --------- + ------------------- + --------- +
| 2                       | Amanda    | 1                   | Amanda    |
| 4                       | Sally     | 2                   | Sally     |
+ ----------------------- + --------- + ------------------- + --------- +
2 rows
```

![](/post/sql-joins/sqljoins2.png)

## 1.6 Left Outer Join

Left Outer Join will give us ALL the records from the LEFT table AND the records that match to the left table. Empty fields will be null.

> select \* from BasketballPlayer BP LEFT OUTER JOIN SoccerPlayer SP on BP.Name = SP.Name

```sql
+ ----------------------- + --------- + ------------------- + --------- +
| BasketballPlayerID      | Name      | SoccerPlayerID      | Name      |
+ ----------------------- + --------- + ------------------- + --------- +
| 1                       | Alan      |                     |           |
| 2                       | Amanda    | 1                   | Amanda    |
| 3                       | Tay       |                     |           |
| 4                       | Sally     | 2                   | Sally     |
+ ----------------------- + --------- + ------------------- + --------- +
4 rows
```

![](/post/sql-joins/sqljoins3.png)

## 1.7 Right Outer Join

The Right Outer Join will give us ALL the records from the RIGHT table AND the records that match to the left table. Empty fields will be null.

> select \* from BasketballPlayer BP RIght OUTER JOIN SoccerPlayer SP on BP.Name = SP.Name

```sql
+ ----------------------- + --------- + ------------------- + --------- +
| BasketballPlayerID      | Name      | SoccerPlayerID      | Name      |
+ ----------------------- + --------- + ------------------- + --------- +
| 2                       | Amanda    | 1                   | Amanda    |
| 4                       | Sally     | 2                   | Sally     |
|                         |           | 3                   | Jose      |
|                         |           | 4                   | Ian       |
+ ----------------------- + --------- + ------------------- + --------- +
4 rows
```

![](/post/sql-joins/sqljoins4.png)

## 1.8 Cartesian Join \ Cross Join

The Cartesian Join or Cross Join has very little realistic use. It returns the Cartesian product of the sets of records from the two or more joined tables. This result is usually encountered when someone learning SQL forgets to put in a where clause :) .

> select \* from BasketballPlayer, SoccerPlayer

```sql
+ ----------------------- + --------- + ------------------- + --------- +
| BasketballPlayerID      | Name      | SoccerPlayerID      | Name      |
+ ----------------------- + --------- + ------------------- + --------- +
| 1                       | Alan      | 1                   | Amanda    |
| 2                       | Amanda    | 1                   | Amanda    |
| 3                       | Tay       | 1                   | Amanda    |
| 4                       | Sally     | 1                   | Amanda    |
| 1                       | Alan      | 2                   | Sally     |
| 2                       | Amanda    | 2                   | Sally     |
| 3                       | Tay       | 2                   | Sally     |
| 4                       | Sally     | 2                   | Sally     |
| 1                       | Alan      | 3                   | Jose      |
| 2                       | Amanda    | 3                   | Jose      |
| 3                       | Tay       | 3                   | Jose      |
| 4                       | Sally     | 3                   | Jose      |
| 1                       | Alan      | 4                   | Ian       |
| 2                       | Amanda    | 4                   | Ian       |
| 3                       | Tay       | 4                   | Ian       |
| 4                       | Sally     | 4                   | Ian       |
+ ----------------------- + --------- + ------------------- + --------- +
16 rows
```
