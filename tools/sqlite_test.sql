-- Schema and test-data for Alice.  When interacting with the database
-- using the sqlite3 CLI tool then be sure to set this pragma:
--     PRAGMA foreign_keys=ON;
-- If you don't, then SQLite does not honor the foreign keys.
--
-- Feed this data into a SQLite database like this:
--     sqlite3 test.db < sqlite_test.db

DROP TABLE contact;
DROP TABLE organization;
DROP TABLE organization_contacts;
DROP TABLE contact_attributes;

CREATE TABLE contact (
       id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
       full_name TEXT NOT NULL,
       is_human BOOLEAN NOT NULL DEFAULT 1
);

CREATE TABLE organization (
    id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    full_name TEXT NOT NULL,
    uri TEXT UNIQUE NOT NULL,
    json json NOT NULL
);

CREATE TABLE organization_contacts (
       organization_id INTEGER NOT NULL,
       contact_id INTEGER NOT NULL,
       PRIMARY KEY (organization_id, contact_id),
       FOREIGN KEY(contact_id) REFERENCES contact(id) ON UPDATE CASCADE ON DELETE CASCADE,
       FOREIGN KEY(organization_id) REFERENCES organization(id) ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE contact_attributes (
       organization_id INTEGER NOT NULL,
       contact_id INTEGER NOT NULL,
       json json NOT NULL,
       PRIMARY KEY(organization_id, contact_id),
       FOREIGN KEY(organization_id) REFERENCES organization(id) ON UPDATE CASCADE ON DELETE CASCADE,
       FOREIGN KEY(contact_id) REFERENCES contact(id) ON UPDATE CASCADE ON DELETE CASCADE,
       FOREIGN KEY(organization_id, contact_id) REFERENCES organization_contacts (organization_id, contact_id) ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE INDEX contact_id_idx ON contact(id);
CREATE INDEX contact_attributes_contact_id_idx ON contact_attributes(contact_id);
CREATE INDEX contact_attributes_organization_id_idx ON contact_attributes(organization_id);
CREATE INDEX organization_id_idx ON organization(id);
CREATE INDEX organization_uri_idx ON organization(uri);
CREATE INDEX organization_contacts_contact_id_idx ON organization_contacts(contact_id);
CREATE INDEX organization_contacts_organization_id_idx ON organization_contacts(organization_id);

INSERT INTO organization VALUES (null, 'AdaHeads K/S', 'adaheads_ks_1', '{"website":"http://adaheads.com","greeting":"Velkommen til AdaHeads, hvad kan jeg hjælpe med?"}');
INSERT INTO organization VALUES (null, 'Fishermans Friends A/S','fishermans_friends_as_2','{"website":"http://fishermansfriends.dk","greeting":"Fishermans Friends du taler med ... - hvad kan jeg hjælpe med?"}');
INSERT INTO organization VALUES (null, 'Responsum K/S','responsum_ks_3','{"website":"http://responsum.dk","greeting":"Velkommen til Responsum - du taler med ..."}');
INSERT INTO organization VALUES (null, 'Hansen VVS A/S','hansen_vvs_4','{"website":"http://hansenvvs.dk","greeting":"Hansen VVS goddag"}');

INSERT INTO contact VALUES (null, 'Thomas Løcke', 1);
INSERT INTO contact VALUES (null, 'Trine Løcke', 1);
INSERT INTO contact VALUES (null, 'Steen Løcke', 1);
INSERT INTO contact VALUES (null, 'Kim Rostgaard Christensen', 1);
INSERT INTO contact VALUES (null, 'Jacob Sparre Andersen', 1);
INSERT INTO contact VALUES (null, 'Sidsel Schomacker', 1);
INSERT INTO contact VALUES (null, 'Ulrik Hørlyk Hjort', 1);
INSERT INTO contact VALUES (null, 'Support', 0);

INSERT INTO organization_contacts VALUES (1,1);
INSERT INTO organization_contacts VALUES (1,2);
INSERT INTO organization_contacts VALUES (1,3);
INSERT INTO organization_contacts VALUES (1,4);
INSERT INTO organization_contacts VALUES (1,5);
INSERT INTO organization_contacts VALUES (1,6);
INSERT INTO organization_contacts VALUES (1,7);
INSERT INTO organization_contacts VALUES (2,1);
INSERT INTO organization_contacts VALUES (2,4);
INSERT INTO organization_contacts VALUES (2,8);
INSERT INTO organization_contacts VALUES (3,1);
INSERT INTO organization_contacts VALUES (3,2);
INSERT INTO organization_contacts VALUES (3,3);
INSERT INTO organization_contacts VALUES (3,4);

INSERT INTO contact_attributes VALUES (1,1,'{"email":"tl@adaheads.com","phone":"60431992","tags":["AWS","Slackware"]}');
INSERT INTO contact_attributes VALUES (1,2,'{"email":"trine@responsum.dk","phone":"60431993","tags":["Ada","Slackware","Linux"]}');
INSERT INTO contact_attributes VALUES (1,3,'{"email":"steen@adaheads.com","phone":"60431990"}');
INSERT INTO contact_attributes VALUES (1,4,'{"email":"krc@adaheads.com","phone":"555-78787878"}');
INSERT INTO contact_attributes VALUES (1,5,'{"email":"jsa@adaheads.com","phone":"555 666 777"}');
INSERT INTO contact_attributes VALUES (1,6,'{"email":"ss@adaheads.com","phone":""}');
INSERT INTO contact_attributes VALUES (1,7,'{"email":"","phone":"12345678","tags":["Granvej","Mosekrogen"]}');
INSERT INTO contact_attributes VALUES (2,1,'{"email":"thomas@responsum.dk","phone":"88329100","tags":["Ada","Slackware","Linux"]}');
INSERT INTO contact_attributes VALUES (2,4,'{"email":"krc@retrospekt.dk","phone":"444-555-66","tags":["Ada","Slackware","Linux"]}');
INSERT INTO contact_attributes VALUES (3,1,'{"email":"thomas@loecke.dk","phone":"33488201"}');
INSERT INTO contact_attributes VALUES (3,2,'{"email":"trine@adaheads.com","phone":"60431993","tags":["IT","Support","Printer"]}');
INSERT INTO contact_attributes VALUES (3,3,'{"email":"steen@responsum.dk","phone":"88329100","tags":["jobansøger","2730","3660","3520"]}');
INSERT INTO contact_attributes VALUES (3,4,'{"email":"krc@retrospekt.dk","phone":"444-555-66","tags":["Ny kunde","Salg","Uadresserede"]}');
