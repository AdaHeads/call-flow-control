BEGIN TRANSACTION;

INSERT INTO contacts (id, full_name)
VALUES (1, 'Thomas Løcke'),
       (2, 'Trine Løcke'),
       (3, 'Steen Løcke'),
       (4, 'Kim Rostgaard Christensen'),
       (5, 'Jacob Sparre Andersen'),
       (6, 'Sidsel Schomacker'),
       (7, 'Ulrik Hørlyk Hjort');

INSERT INTO contacts (id, full_name, is_human)
VALUES (8, 'Support', FALSE);

INSERT INTO organizations (id, full_name, uri, json)
VALUES (1, 'AdaHeads K/S', 'adaheads_ks_1', '{"website":"http://adaheads.com","greeting":"Velkommen til AdaHeads, hvad kan jeg hjælpe med?"}'),
       (2, 'Fishermans Friends A/S', 'fishermans_friends_as_2', '{"website":"http://fishermansfriends.dk","greeting":"Fishermans Friends du taler med ... - hvad kan jeg hjælpe med?"}'),
       (3, 'Responsum K/S', 'responsum_ks_3', '{"website":"http://responsum.dk","greeting":"Velkommen til Responsum - du taler med ..."}'),
       (4, 'Hansen VVS A/S', 'hansen_vvs_4', '{"website":"http://hansenvvs.dk","greeting":"Hansen VVS goddag"}');

INSERT INTO organization_contacts (organization_id, contact_id, free_form_attributes)
VALUES --  Adaheads
       (1, 1, '{"tags":["AWS","Slackware"]}'),
       (1, 2, '{"tags":["usability"]}'),
       (1, 3, '{"tags":["CFO"]}'),
       (1, 4, '{"tags":["Ada","Linux"]}'),
       (1, 5, '{"tags":["Ada","physics"]}'),
       (1, 6, ''),
       (1, 7, '{"tags":["embedded"]}'),
       --  Fishermans Friends
       (2, 1, '{"tags":["Ada","Slackware","Linux"]}'),
       (2, 4, '{"tags":["Ada","Slackware","Linux"]}'),
       (2, 8, ''),
       --  Responsum
       (3, 1, '}'),
       (3, 2, '{"tags":["IT","Support","Printer"]}'),
       (3, 3, '{"tags":["jobansøger","2730","3660","3520"]}'),
       (3, 4, '{"tags":["Ny kunde","Salg","Uadresserede"]}');

INSERT INTO end_points (contact_id, organization_id,
                        address_type, address,
                        confidential, messaging)
VALUES --  Adaheads
       (1, 1, 'e-mail', 'tl@adaheads.com',       FALSE, TRUE),
       (1, 1, 'sms',    '+4560431992',           FALSE, FALSE),
       (1, 4, 'e-mail', 'jsa@adaheads.com',      FALSE, TRUE),
       (1, 4, 'sms',    '+4521490804',           FALSE, TRUE),
       (1, 4, 'e-mail', 'jacob@jacob-sparre.dk', TRUE,  FALSE),
       --  Fishermans Friends
       (2, 1, 'e-mail', 'thomas@responsum.dk',   FALSE, TRUE),
       (2, 1, 'sms',    '+4588329100',           FALSE, FALSE),
       --  Responsum
       (3, 1, 'e-mail', 'thomas@responsum.dk',   FALSE, TRUE),
       (3, 2, 'e-mail', 'trine@responsum.dk',    FALSE, TRUE);

INSERT INTO distribution_lists (owner_contact_id,   owner_organization_id,
                                send_to_contact_id, send_to_organization_id,
                                recipient_visibility)
VALUES (1, 1, 1, 1, 'to'),
       (1, 1, 1, 2, 'cc'),
       (1, 2, 1, 2, 'to'),
       (1, 2, 1, 1, 'cc'),
       (1, 4, 1, 4, 'to'),
       (2, 8, 3, 1, 'to'),
       (2, 8, 3, 4, 'to');

INSERT INTO kinds (id)
VALUES ('helligdag');

INSERT INTO special_days (kind, day)
VALUES ('helligdag', '2013-12-25'),
       ('helligdag', '2013-12-26'),
       ('helligdag', '2014-01-01');

INSERT INTO dial_plans (phone_number, dial_plan)
VALUES ('+4521490804', '<xml/>');

INSERT INTO users (name)
VALUES ('Tux'),
       ('AdaHeads Test User One'),
       ('AdaHeads Test User Two'),
       ('AdaHeads Test User Three');

INSERT INTO user_ids (name, openid, rank)
VALUES ('Tux','https://tux.myopenid.com/', 1), 
       ('Tux','https://accounts.google.com/we-love-tux/', 2),
       ('AdaHeads Test User One', 'https://adaheads1.myopenid.com/', 1),
       ('AdaHeads Test User Two', 'https://adaheads2.myopenid.com/', 1),
       ('AdaHeads Test User Three','https://adaheads3.myopenid.com/', 1);

COMMIT;
