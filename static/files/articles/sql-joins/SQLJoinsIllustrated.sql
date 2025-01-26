-- Table to hold Basketball team
CREATE TABLE BasketballPlayer
(
BasketballPlayerID int,
Name varchar(255)
); 

-- Insert Basketball Players
INSERT INTO `BasketballPlayer` VALUES (1,'Alan');
INSERT INTO `BasketballPlayer` VALUES (2,'Amanda');
INSERT INTO `BasketballPlayer` VALUES (3,'Tay');
INSERT INTO `BasketballPlayer` VALUES (4,'Sally');

-- List the basketball players
select * from BasketballPlayer;

-- Insert Soccer Players
INSERT INTO `SoccerPlayer` VALUES (1,'Amanda');
INSERT INTO `SoccerPlayer` VALUES (2,'Sally');
INSERT INTO `SoccerPlayer` VALUES (3,'Jose');
INSERT INTO `SoccerPlayer` VALUES (4,'Ian');

-- List the soccer players
select * from soccerplayer;

select "Inner Join shows only the rows that exist in both tables.  Visualize it as the inner section of the Venn diagram.";

-- Inner Join 
select * from BasketballPlayer BP INNER JOIN SoccerPlayer SP  on BP.Name = SP.Name;

select "Left Outer Join will give us ALL the records from the LEFT table AND the records that match to the left table. Empty fields will be null.";

-- Left Outer
select * from BasketballPlayer BP LEFT JOIN SoccerPlayer SP  on BP.Name = SP.Name;

select "Right Outer Join will give us ALL the records from the RIGHT table AND the records that match to the left table. Empty fields will be null.";

-- Right Outer
select * from BasketballPlayer BP RIght OUTER JOIN SoccerPlayer SP  on BP.Name = SP.Name;

select "The CARTESIAN JOIN or CROSS JOIN has very little realistic use. It returns the Cartesian product of the sets of records from the two or more joined tables. This result is usually encountered when someone learning SQL forgets to put in a where clause :) . ";

select * from BasketballPlayer, SoccerPlayer ;
