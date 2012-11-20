--Music
--It contains all the music classes.
CREATE TABLE music (
  name varchar(80) PRIMARY KEY,
  directory varchar(255) NOT NULL default '',
  application varchar(255) NOT NULL default '',
  mode varchar(80) NOT NULL default '',
  digit char(1) NOT NULL default '',
  sort varchar(16) NOT NULL default '',
  format varchar(16) NOT NULL default ''
);

--queue_settings
--this table have the settings about a queue, that is specific for an organization.
--It has a Foreign Key to music on musicclass which also have a default value to 'default'. 
--  This require that music have a row with name default
CREATE TABLE queue_settings
(
  id integer NOT NULL,
  musicclass character varying(256) NOT NULL DEFAULT 'default'::character varying,
  context character varying(256) NOT NULL DEFAULT 'LocalSets'::character varying,
  PRIMARY KEY (id),
  FOREIGN KEY (id)
      REFERENCES organization (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE,
  FOREIGN KEY (musicclass)
      REFERENCES music (name) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE SET NULL
);

--queue_view
--This is the view Asterisk will see
CREATE VIEW queue_view AS
SELECT 
  'org_id_'::text || org.id AS name, 
  coalesce(qs.musicclass,'default') AS musiconhold,
  coalesce(qs.context,'LocalSets') AS context,
  'true'::character varying(5) AS joinempty, 
  'false'::character varying(5) AS leavewhenempty
FROM 
  organization org LEFT OUTER JOIN queue_settings qs 
    on org.id = qs.id;

--extension also known as Dialplan
CREATE TABLE extension (
  id serial,
  context varchar(20) NOT NULL default '',
  exten varchar(20) NOT NULL default '',
  priority smallint NOT NULL default '0',
  app varchar(20) NOT NULL default '',
  appdata varchar(128) NOT NULL default '',
  PRIMARY KEY (context,exten,priority),
  UNIQUE (id)
);

--sippeers
--it contains the information about a sip client.
CREATE TABLE sippers
(
  id serial NOT NULL,
  name character varying(80),
  host character varying(31),
  nat character varying(5),
  context character varying(80),
  secret character varying(80) DEFAULT NULL::character varying,
  disallow character varying(100) DEFAULT 'all'::character varying,
  allow character varying(100) DEFAULT 'g729;ilbc;gsm;ulaw;alaw'::character varying,
  defaultuser character varying(80),
  regseconds integer NOT NULL DEFAULT 0,
  regserver character varying(80),
  type character varying(80),
  ipaddr character varying(80),
  port integer,
  fullcontact character varying(180),
  lastms character varying(80),
  useragent character varying(80),
  CONSTRAINT sippers_pkey PRIMARY KEY (id),
  CONSTRAINT sippers_name_key UNIQUE (name)
);