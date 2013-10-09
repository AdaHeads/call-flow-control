-------------------------------------------------------------------------------
--  Dial-plans:

CREATE TABLE dial_plans (
   phone_number TEXT NOT NULL PRIMARY KEY,
   dial_plan    XML  NOT NULL
);

-------------------------------------------------------------------------------
--  Calendar of special days:

CREATE TABLE kinds (
   id          TEXT NOT NULL PRIMARY KEY,
   description TEXT
);

CREATE TABLE special_days (
   kind TEXT NOT NULL REFERENCES kinds (id) ON UPDATE CASCADE ON DELETE CASCADE,
   day  DATE NOT NULL,

   PRIMARY KEY (kind, day)
);

-------------------------------------------------------------------------------
--  Contacts and organizations:

CREATE TABLE contact_types (value TEXT NOT NULL PRIMARY KEY);
INSERT INTO contact_types VALUES ('human'), ('function'), ('invisible');

CREATE TABLE contacts (
   id           INTEGER NOT NULL PRIMARY KEY, --  AUTOINCREMENT
   full_name    TEXT    NOT NULL,
   contact_type TEXT    NOT NULL REFERENCES contact_types (value)
);

CREATE TABLE organizations (
   id        INTEGER NOT NULL PRIMARY KEY, --  AUTOINCREMENT
   full_name TEXT    NOT NULL,
   uri       TEXT    NOT NULL UNIQUE,
   json      JSON    NOT NULL
);

CREATE INDEX organization_uri_index ON organizations (uri);

CREATE TABLE organization_contacts (
   organization_id      INTEGER NOT NULL REFERENCES organizations (id) ON UPDATE CASCADE ON DELETE CASCADE,
   contact_id           INTEGER NOT NULL REFERENCES contacts (id) ON UPDATE CASCADE ON DELETE CASCADE,
   wants_messages       BOOLEAN NOT NULL DEFAULT TRUE,
   distribution_list_id INTEGER, --  Reference constraint added further down
   attributes           JSON,

   PRIMARY KEY (organization_id, contact_id)
);

CREATE INDEX organization_contacts_contact_id_index      ON organization_contacts (contact_id);
CREATE INDEX organization_contacts_organization_id_index ON organization_contacts (organization_id);

-------------------------------------------------------------------------------
--  Addresses and messaging:

CREATE TABLE messaging_address_types (value TEXT NOT NULL PRIMARY KEY);
INSERT INTO messaging_address_types VALUES ('e-mail'), ('sms');

CREATE TABLE messaging_addresses (
   id           INTEGER NOT NULL PRIMARY KEY, --  AUTOINCREMENT
   address      TEXT    NOT NULL,
   address_type TEXT    NOT NULL REFERENCES messaging_address_types (value),

   UNIQUE (address, address_type)
);

CREATE TABLE messaging_end_points (
   contact_id      INTEGER NOT NULL,
   organization_id INTEGER NOT NULL,
   address_id      INTEGER NOT NULL REFERENCES messaging_addresses (id),
   confidential    BOOLEAN NOT NULL DEFAULT TRUE,
   enabled         BOOLEAN NOT NULL DEFAULT FALSE,
   priority        INTEGER NOT NULL DEFAULT 0,

   PRIMARY KEY (contact_id, organization_id, address_id),

   FOREIGN KEY (contact_id, organization_id)
      REFERENCES organization_contacts (contact_id, organization_id)
      ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE recipient_visibilities (value TEXT NOT NULL PRIMARY KEY);
INSERT INTO recipient_visibilities VALUES ('to'), ('cc'), ('bcc');

CREATE TABLE distribution_lists (
   id                      INTEGER NOT NULL PRIMARY KEY, --  AUTOINCREMENT
   send_to_contact_id      INTEGER NOT NULL,
   send_to_organization_id INTEGER NOT NULL,
   recipient_visibility    TEXT    NOT NULL REFERENCES recipient_visibilities (value),

   FOREIGN KEY (send_to_contact_id, send_to_organization_id)
      REFERENCES organization_contacts (contact_id, organization_id)
      ON UPDATE CASCADE ON DELETE CASCADE
);

-------------------------------------------------------------------------------
--  Late reference from organization_contacts to distribution_lists:

ALTER TABLE organization_contacts
  ADD CONSTRAINT organization_contacts_distribution_list_id_foreign_key
    FOREIGN KEY (distribution_list_id)
      REFERENCES distribution_lists (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE SET DEFAULT;

-------------------------------------------------------------------------------
--  System users:

CREATE TABLE users (
   name             TEXT    NOT NULL PRIMARY KEY,
   is_receptionist  BOOLEAN NOT NULL,
   is_service_agent BOOLEAN NOT NULL,
   is_administrator BOOLEAN NOT NULL
);

CREATE TABLE user_ids (
   name     TEXT    NOT NULL REFERENCES users (name) ON UPDATE CASCADE ON DELETE CASCADE,
   openid   TEXT    NOT NULL PRIMARY KEY,
   priority INTEGER NOT NULL
);

-------------------------------------------------------------------------------
