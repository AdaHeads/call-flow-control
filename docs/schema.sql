--  An organization.
--  SHOULD contain information relevant to describing an organization, like one
--  or more addresses, phone numbers, products and similar. All this data is
--  kept in the json column which MUST be a valid JSON object.
--
--    id         : Unique identifier for an organization.
--    name       : SHOULD contain the full name of the organization.
--    identifier : MUST contain a unique identifier for an organization. This
--                 does not serve the same role as id, as identifier SHOULD
--                 provide an identifier that makes sense in the context of the
--                 system, ie. a unique SIP URI or a phonenumber.
--    json       : The actual organization data.
CREATE TABLE organization
(
  id serial NOT NULL,
  name character varying(256) NOT NULL,
  identifier character varying(256) NOT NULL,
  json json,
  CONSTRAINT organization_pkey PRIMARY KEY (id),
  CONSTRAINT organization_identifier_key UNIQUE (identifier)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE organization
  OWNER TO alice;

CREATE INDEX organization_id_idx
  ON organization
  USING btree
  (id);

--  A contact entity.
--  SHOULD contain information relevant to describing a very basic contact entity.
--
--    id       : Unique identifier for a contact entity.
--    name     : SHOULD contain the full name of the contact entity.
--    is_human : If True the contact entity is an actual human, if False then
--               it's a function/group.
CREATE TABLE contactentity
(
  id serial NOT NULL,
  full_name character varying(256) NOT NULL,
  is_human boolean NOT NULL DEFAULT true,
  CONSTRAINT contactentity_pkey PRIMARY KEY (id)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE contactentity
  OWNER TO alice;

CREATE INDEX contactentity_id_idx
  ON contactentity
  USING btree
  (id);

--  The Organization <-> Contact Entity relations.
--  Which contact entities belong to which organizations. Obviously a relation
--  is only possible if both the organization and the contact entity exists.
--
--    organization_id  : Identifies an organization.
--    contactentity_id : Identifies a contact entity.
CREATE TABLE organization_contactentities
(
  organization_id integer NOT NULL,
  contactentity_id integer NOT NULL,
  CONSTRAINT organization_contactentities_pkey PRIMARY KEY (organization_id, contactentity_id),
  CONSTRAINT organization_contactentities_contactentity_id_fkey FOREIGN KEY (contactentity_id)
      REFERENCES contactentity (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT organization_contactentities_organization_id_fkey FOREIGN KEY (organization_id)
      REFERENCES organization (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE
)
WITH (
  OIDS=FALSE
);
ALTER TABLE organization_contactentities
  OWNER TO alice;

CREATE INDEX organization_contactentities_contactentity_id_idx
  ON organization_contactentities
  USING btree
  (contactentity_id);

CREATE INDEX organization_contactentities_organization_id_idx
  ON organization_contactentities
  USING btree
  (organization_id);

--  Contact entity attributes.
--  SHOULD contain such data as names, emailaddresses, phone numbers, tags and
--  other relevant information about a contact entity.
--
--  Note that the constraints make sure that there can only be one set of
--  attributes per organization_id <-> contactentity_id relation, and that this
--  set is removed if the organization, contact entity or organization <-> contact
--  relation is deleted from the database.
--
--    organization_id  : Identifies an organization.
--    contactentity_id : Identifies a contact entity.
--    json             : The actual attributes for a contact entity.
CREATE TABLE contactentity_attributes
(
  organization_id integer NOT NULL,
  contactentity_id integer NOT NULL,
  json json NOT NULL,
  CONSTRAINT contactentity_attributes_pkey PRIMARY KEY (organization_id, contactentity_id),
  CONSTRAINT contactentity_attributes_contactentity_id_fkey FOREIGN KEY (contactentity_id)
      REFERENCES contactentity (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT contactentity_attributes_organization_id_fkey FOREIGN KEY (organization_id)
      REFERENCES organization (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT contactentity_attributes_organization_id_fkey1 FOREIGN KEY (organization_id, contactentity_id)
      REFERENCES organization_contactentities (organization_id, contactentity_id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE
)
WITH (
  OIDS=FALSE
);
ALTER TABLE contactentity_attributes
  OWNER TO alice;

CREATE INDEX contactentity_attributes_contactentity_id_idx
  ON contactentity_attributes
  USING btree
  (contactentity_id);

CREATE INDEX contactentity_attributes_organization_id_idx
  ON contactentity_attributes
  USING btree
  (organization_id);

--  The Contact Entity <-> Recipient relations.
--  Which recipients belong to which contact entities. Obviously a relation
--  is only possible if both the recipient and the contact entity exists.
CREATE TABLE contactentity_recipient
(
  contactentity_id integer NOT NULL,
  recipient_id integer NOT NULL,
  CONSTRAINT contactentity_recipient_pkey PRIMARY KEY (contactentity_id, recipient_id),
  CONSTRAINT contactentity_recipient_contactentity_id_fkey FOREIGN KEY (contactentity_id)
      REFERENCES contactentity (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT contactentity_recipient_recipient_id_fkey FOREIGN KEY (recipient_id)
      REFERENCES recipient (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE
)
WITH (
  OIDS=FALSE
);
ALTER TABLE contactentity_recipient
  OWNER TO alice;

CREATE INDEX contactentity_recipient_contactentity_id_idx
  ON contactentity_recipient
  USING btree
  (contactentity_id);

CREATE INDEX contactentity_recipient_recipient_id_idx
  ON contactentity_recipient
  USING btree
  (recipient_id);

--  A message recipient.
--  MUST contain a valid emailaddress for a message recipient.
--    id      : Identifies a recipient.
--    kind_id : The kind of recipient, eg. To, Cc or Bcc.
CREATE TABLE recipient
(
  id serial NOT NULL,
  kind_id integer NOT NULL,
  full_name text,
  email_address text NOT NULL,
  CONSTRAINT recipient_pkey PRIMARY KEY (id),
  CONSTRAINT recipient_kind_id_fkey FOREIGN KEY (kind_id)
      REFERENCES recipient_kind (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT recipient_kind_id_full_name_email_address_key UNIQUE (kind_id, full_name, email_address)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE recipient
  OWNER TO alice;

CREATE INDEX recipient_id_idx
  ON recipient
  USING btree
  (id);

--  A recipient kind.
--  Enumeration table containing the valid types of recipients, such as To, Cc,
--  and Bcc.
--    id   : Identifies a recipient kind.
--    kind : The kind of recipient.
CREATE TABLE recipient_kind
(
  id smallint NOT NULL,
  kind character varying(3) NOT NULL,
  CONSTRAINT recipient_kind_pkey PRIMARY KEY (id),
  CONSTRAINT recipient_kind_kind_key UNIQUE (kind)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE recipient_kind
  OWNER TO alice;
