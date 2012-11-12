



CREATE TABLE Agent
( 
  ID serial,
  name varchar(120) NOT NULL DEFAULT '',
  image_uri varchar(2048) NOT NULL,
  extension varchar(120) NOT NULL,
  email varchar(256) NOT NULL,
  PRIMARY KEY (ID)
);


CREATE TABLE Agent_Organization
(
  Agent_ID int,
  Org_ID int,
  Priority int NOT NULL DEFAULT 0,
  PRIMARY KEY (Agent_ID, Org_ID),
  FOREIGN KEY (Agent_ID) REFERENCES Agent (ID)        MATCH SIMPLE ON UPDATE CASCADE ON DELETE CASCADE
  FOREIGN KEY (Org_ID)   REFERENCES organization (ID) MATCH SIMPLE ON UPDATE CASCADE ON DELETE CASCADE
);
