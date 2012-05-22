--  An organization.
--  SHOULD contain information relevant to describing an organization, like one
--  or more addresses, phone numbers, products and similar. All this data is
--  kept in the json column which MUST contain a valid JSON object string.
--
--  Note that the COLLATE parameters should be set according to the environment.
--
--     org_id     : Unique identifier for an organization.
--     org_name	  : Name of the organization. SHOULD contain the same value as
--                  the "org_name" JSON object from the json column.
--     identifier : MUST contain a unique identifier for an organization. This
--                  does not serve the same role as org_id, as identifier SHOULD
--                  provide an identifier that makes sense in the context of the
--                  system, ie. a unique SIP URI or a phonenumber.
--     json       : The actual organization data.
CREATE TABLE organization
(
  org_id serial NOT NULL,
  org_name character varying(256) NOT NULL,
  identifier character varying(256) NOT NULL,
  json character varying(10000) NOT NULL,
  CONSTRAINT organization_pkey PRIMARY KEY (org_id ),
  CONSTRAINT organization_identifier_key UNIQUE (identifier )
)
WITH (
  OIDS=FALSE
);
ALTER TABLE organization
  OWNER TO alice;


--  A contact entity.
--  SHOULD contain information relevant to describing a contact entity, like
--  email addresses, phone numbers, titles and similar. All this data is kept
--  in the json column which MUST contain a valid JSON object string.
--
--  Note that the COLLATE parameter should be set according to the environment.
--
--     ce_id    : Unique identifier for a contact entity.
--     ce_name  : SHOULD contain the actual name of the contact entity. This is
--                used primarily for sorting and presentation, to avoid parsing
--                the JSON object.
--     json     : The actual contact entity data.
--     is_human : If True the contact entity is an actual human, if False then
--     		  it's a function/group.
CREATE TABLE contactentity
(
  ce_id serial NOT NULL,
  ce_name character varying(256) NOT NULL,
  json character varying(10000) NOT NULL,
  is_human boolean NOT NULL DEFAULT true,
  CONSTRAINT contactentity_pkey PRIMARY KEY (ce_id )
)
WITH (
  OIDS=FALSE
);
ALTER TABLE contactentity
  OWNER TO alice;


--  The Organization <-> Contact Entity relations.
--  Which contact entities belong to which organizations. Obviously a relation
--  is only possible if both the organization and the contact entity exists.
--
--     org_id : Identifies an organization.
--     ce_id  : Identifies a contact entity.
CREATE TABLE organization_contactentities
(
  org_id integer NOT NULL,
  ce_id integer NOT NULL,
  CONSTRAINT organization_contactentities_pkey PRIMARY KEY (org_id , ce_id ),
  CONSTRAINT organization_contactentities_ce_id_fkey FOREIGN KEY (ce_id)
      REFERENCES contactentity (ce_id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT organization_contactentities_org_id_fkey FOREIGN KEY (org_id)
      REFERENCES organization (org_id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE
)
WITH (
  OIDS=FALSE
);
ALTER TABLE organization_contactentities
  OWNER TO alice;


--  Contact entity attributes.
--  SHOULD contain such data as name, emailaddresses, phone numbers, tags and
--  other relevant information about a contact entity.
--
--  Note that the constraints make sure that there can only be one set of
--  attributes per org_id <-> ce_id relation, and that this set is removed if
--  the organization, contact entity or organization <-> contact relation is
--  deleted from the database.
--
--     org_id : Identifies an organization.
--     ce_id  : Identifies a contact entity.
--     json   : The actual attributes for a contact entity.
CREATE TABLE contactentity_attributes
(
  org_id integer NOT NULL,
  ce_id integer NOT NULL,
  json character varying(10000) NOT NULL,
  CONSTRAINT contactentity_attributes_pkey PRIMARY KEY (ce_id , org_id ),
  CONSTRAINT contactentity_attributes_ce_and_org_id_fkey FOREIGN KEY (ce_id, org_id)
      REFERENCES organization_contactentities (ce_id, org_id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT contactentity_attributes_ce_id_fkey FOREIGN KEY (ce_id)
      REFERENCES contactentity (ce_id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT contactentity_attributes_org_id_fkey FOREIGN KEY (org_id)
      REFERENCES organization (org_id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE
)
WITH (
  OIDS=FALSE
);
ALTER TABLE contactentity_attributes
  OWNER TO alice;

