BEGIN TRANSACTION;

INSERT INTO contacts (id, full_name, contact_type)
VALUES (1, 'Thomas Løcke', 'human'),
       (2, 'Trine Løcke', 'human'),
       (3, 'Steen Løcke', 'human'),
       (4, 'Kim Rostgaard Christensen', 'human'),
       (5, 'Jacob Sparre Andersen', 'human'),
       (6, 'Sidsel Schomacker', 'human'),
       (7, 'Ulrik Hørlyk Hjort', 'human'),
       (8, 'Support', 'function');

INSERT INTO organizations (id, full_name, uri, json)
VALUES (1, 'AdaHeads K/S', 'adaheads_ks_1', '{"website":"http://adaheads.com","greeting":"Velkommen til AdaHeads, hvad kan jeg hjælpe med?"}'),
       (2, 'Fishermans Friends A/S', 'fishermans_friends_as_2', '{"website":"http://fishermansfriends.dk","greeting":"Fishermans Friends du taler med ... - hvad kan jeg hjælpe med?"}'),
       (3, 'Responsum K/S', 'responsum_ks_3', '{"website":"http://responsum.dk","greeting":"Velkommen til Responsum - du taler med ..."}'),
       (4, 'Hansen VVS A/S', 'hansen_vvs_4', '{"website":"http://hansenvvs.dk","greeting":"Hansen VVS goddag"}');

INSERT INTO organization_contacts (organization_id, contact_id, attributes, distribution_list_id)
VALUES --  Adaheads
       (1, 1, '{"tags":["AWS","Slackware"]}', 1),
       (1, 2, '{"tags":["usability"]}', 1),
       (1, 3, '{"tags":["CFO"]}', 2),
       (1, 4, '{"tags":["Ada","Linux"]}', 2),
       (1, 5, '{"tags":["Ada","physics"]}', 2),
       (1, 6, '', 3),
       (1, 7, '{"tags":["embedded"]}', 3),
       --  Fishermans Friends
       (2, 1, '{"tags":["Ada","Slackware","Linux"]}', 4),
       (2, 4, '{"tags":["Ada","Slackware","Linux"]}', 4),
       (2, 8, '', 5),
       --  Responsum
       (3, 1, '}', 5),
       (3, 2, '{"tags":["IT","Support","Printer"]}', 5),
       (3, 3, '{"tags":["jobansøger","2730","3660","3520"]}', 5),
       (3, 4, '{"tags":["Ny kunde","Salg","Uadresserede"]}', 5);

INSERT INTO messaging_addresses (id, address_type, address)
VALUES (1, 'e-mail', 'tl@adaheads.com'),
       (2, 'sms',    '+4560431992'),
       (3, 'e-mail', 'jsa@adaheads.com'),
       (4, 'sms',    '+4521490804'),
       (5, 'e-mail', 'jacob@jacob-sparre.dk'),
       (6, 'e-mail', 'thomas@responsum.dk'),
       (7, 'sms',    '+4588329100'),
       (9, 'e-mail', 'trine@responsum.dk');

INSERT INTO messaging_end_points (contact_id, organization_id,
                                  address_id,
                                  confidential, enabled)
VALUES --  Adaheads
       (1, 1, 1, FALSE, TRUE),
       (2, 1, 2, FALSE, FALSE),
       (3, 4, 3, FALSE, TRUE),
       (4, 4, 4, FALSE, TRUE),
       (5, 4, 5, TRUE,  FALSE),
       --  Fishermans Friends                
       (6, 1, 6, FALSE, TRUE),
       (7, 1, 7, FALSE, FALSE),
       --  Responsum
       (8, 1, 8, FALSE, TRUE),
       (9, 2, 9, FALSE, TRUE);

INSERT INTO distribution_lists (id,
                                          send_to_contact_id, send_to_organization_id,
                                          recipient_visibility)
VALUES (1, 1, 1, 'to'),
       (2, 1, 2, 'cc'),
       (3, 1, 2, 'to'),
       (4, 1, 1, 'cc'),
       (5, 1, 4, 'to'),
       (6, 3, 1, 'to'),
       (7, 3, 4, 'to');

INSERT INTO kinds (id)
VALUES ('helligdag');

INSERT INTO special_days (kind, day)
VALUES ('helligdag', '2013-12-25'),
       ('helligdag', '2013-12-26'),
       ('helligdag', '2014-01-01');

INSERT INTO dial_plans (phone_number, dial_plan)
VALUES ('+4521490804', '<?xml version="1.0"?> <!DOCTYPE dial-plan> <dial-plan title="Jacob: Hang up on anonymous callers"> <start do="Start"/> <decision-tree title="Start"> <branch> <conditions> <caller number=""/> </conditions> <action do="Hang up"/> </branch> <fall-back do="Pass through"/> </decision-tree> <end-point title="Hang up"> <hang-up/> </end-point> <end-point title="Pass through"> <redirect to="+45 21 49 08 04"/> </end-point> </dial-plan>');

INSERT INTO users (name, is_receptionist, is_service_agent, is_administrator)
VALUES ('Tux',                      FALSE, FALSE, TRUE),
       ('AdaHeads Test User One',   TRUE,  FALSE, FALSE),
       ('AdaHeads Test User Two',   TRUE,  TRUE,  FALSE),
       ('AdaHeads Test User Three', TRUE,  TRUE,  TRUE);

INSERT INTO user_ids (name, openid, priority)
VALUES ('Tux','https://tux.myopenid.com/', 1), 
       ('Tux','https://accounts.google.com/we-love-tux/', 2),
       ('AdaHeads Test User One', 'https://adaheads1.myopenid.com/', 1),
       ('AdaHeads Test User Two', 'https://adaheads2.myopenid.com/', 1),
       ('AdaHeads Test User Three','https://adaheads3.myopenid.com/', 1);

COMMIT;
