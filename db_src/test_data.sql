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

INSERT INTO organizations (id, full_name, uri, attributes)
VALUES (1, 'AdaHeads K/S', 'adaheads_ks_1', '{"addresses":[{"value":"For enden af regnbuen","priority":1}],"alternatenames":[{"value":"Code monkeys","priority":1}],"bankinginformation":[{"value":"Amagerbank 123456789","priority":1}],"crapcallhandling":[{"value":"Håndter dem som....","priority":1}],"customertype":"","emailaddresses":[{"value":"mail@adaheads.com","priority":1}],"greeting":"Velkommen til AdaHeads, hvad kan jeg hjælpe med?","handlings":[{"value":"Lad tlf. ringe 4-5 gange.","priority":2},{"value":"Indgang til deres kontor ligger i gården.","priority":3},{"value":"Kunder skal tiltales formelt, med både fornavn og efternavn.","priority":1}],"openinghours":[{"value":"08:00:00 - 17:00:00","priority":1}],"other":"Bonus info","product":"Software produkter","registrationnumbers":[{"value":"123456789","priority":1}],"telephonenumbers":[{"value":"+45 10 20 30 40","priority":1},{"value":"+45 20 40 60 80","priority":1}],"websites":[{"value":"http://adaheads.com","priority":1},{"value":"http://adaheads.org","priority":2}]}'),
       (2, 'Fishermans Friends A/S', 'fishermans_friends_as_2', '{"addresses":[{"value":"Lofthouse of Fleetwood Ltd. Maritime Street Fleetwood Lancs. FY7 7LP UK","priority":1},{"value":"Valora Trade Denmark A/S Transformervej 16 2730 Herlev","priority":2},{"value":"Et sted ude på atlandterhavet","priority":3}],"alternatenames":[{"value":"Fiskernes venner","priority":1}],"bankinginformation":[{"value":"En kiste ude på en øde ø","priority":1},{"value":"Nogle englændere har pt. \"deres\" guld","priority":2}],"crapcallhandling":[{"value":"Stil dem videre til marketings afdelingen","priority":1}],"customertype":"","emailaddresses":[{"value":"info@fiskermans.com","priority":1}],"greeting":"Fishermans Friends du taler med... hvad kan jeg gøre for dig?","handlings":[{"value":"Lad tlf. ringe 4-5 gange og spørg så: skal poppedreng ha'' noget.","priority":2},{"value":"Indgang til deres kontor ligger ved siden af kabyssen.","priority":3},{"value":"Kunder skal tiltales med pirat stemme, med både klo og klap for øjet.","priority":1}],"openinghours":[{"value":"Solopgang - Solnedgang","priority":1}],"other":"Bonus info","product":"ORIGINAL, MINT SUKKERFRI, SALMIAK SUKKERFRI, SØD LAKRIDS SUKKERFRI, EXSTRA STÆRK","registrationnumbers":[{"value":"Reg no. 781277","priority":1}],"telephonenumbers":[{"value":"+45 11 22 33 44","priority":1},{"value":"+45 21 32 43 55","priority":1}],"websites":[{"value":"http://www.fishermansfriend.com/","priority":1}]}'),
       (3, 'Responsum K/S', 'responsum_ks_3', '{"addresses":[{"value":"Farum gydevej 87","priority":3}],"alternatenames":[{"value":"Stemmen i dit øre","priority":1}],"bankinginformation":[{"value":"Danske bank 222 333 444 555","priority":1},{"value":"Nordea 999 888 777 666","priority":2}],"crapcallhandling":[{"value":"Stil dem videre til Thomas Løcke","priority":1}],"customertype":"","emailaddresses":[{"value":"info@responsum.com","priority":1}],"greeting":"Velkommen til Responsum - du taler med ...","handlings":[{"value":"De kender godt til stavefejlen på deres reklamebanner på køge bugt motorvejen","priority":2},{"value":"Man kan ikke møde op på adressen, før der er aftalt et møde.","priority":3},{"value":"Køb af produkter stilles videre til Steen","priority":1}],"openinghours":[{"value":"08:00 - 17:00","priority":1}],"other":"De har byens eneste mandelige receptionister","product":"Extern reception","registrationnumbers":[{"value":"Reg no. 123456","priority":1}],"telephonenumbers":[{"value":"sip:thomas@responsum.dk","priority":1},{"value":"+45 13 37 13 37","priority":1}],"websites":[{"value":"http://responsum.dk","priority":1}]}'),
       (4, 'Hansen VVS A/S', 'hansen_vvs_4', '{"addresses":[],"alternatenames":[],"bankinginformation":[],"crapcallhandling":[],"customertype":"","emailaddresses":[],"greeting":"Hansen vvs. Hvad vil du?","handlings":[],"openinghours":[],"other":"","product":"","registrationnumbers":[],"telephonenumbers":[],"websites":[]}');

INSERT INTO organization_contacts(organization_id, contact_id, attributes) 
VALUES /*Adaheads*/
       (1, 1, '{"backup":[{"value":"Trine Løcke","priority":1},{"value":"Kim Rostgaard Christensen","priority":2},{"value":"Steen Løcke","priority":3},{"value":"Jacob Sparre Andersen","priority":4}],"emailaddresses":[{"value":"tl@adaheads.com","priority":1},{"value":"tl@adaheads.org","priority":2}],"handling":[{"value":"Bær over med hans gode humør","priority":1}],"telephonenumbers":[{"value":"+45 60 43 19 92","priority":1}],"workhours":[{"value":"Hverdage 07:00 – 18:00","priority":1},{"value":"Weekend: 10:00 - 14:00","priority":2}],"tags":["AWS","SIP","Slackware","Linux","Yolk"],"department":"Development","info":"Yolk forfatter","position":"Software udvikler","relations":"Gift med Trine Løcke","responsibility":"Alice og Bob"}'),
       (1, 2, '{"backup":[{"value":"Thomas Løcke","priority":1}],"emailaddresses":[{"value":"trine@responsum.com","priority":1}],"handling":[{"value":"Bær over med hendes gode humør","priority":1}],"telephonenumbers":[{"value":"+45 60 43 19 92","priority":1}],"workhours":[{"value":"Hverdage 07:00 – 18:00","priority":1},{"value":"Weekend: 10:00 - 14:00","priority":2}],"tags":["Grafik","SIP","Linux"],"department":"Development","info":"Laver alt det flotte I programmet","position":"Designer","relations":"Gift med Thomas","responsibility":"Bob"}'),
       (1, 3, '{"backup":[{"value":"Thomas Løcke", "priority": 1}],"emailaddresses":[{"value":"steen@adaheads.com", "priority": 1}],"handling":[{"value":"Bær over med hans gode humør", "priority": 1}],"telephonenumbers":[{"value":"+45 60 43 19 90", "priority": 1}],"workhours":[{"value":"Hverdage 07:00 – 18:00", "priority": 1},{"value":"Weekend: 10:00 - 14:00", "priority": 2}],"tags":["Grafik","SIP","Linux"],"department":"Regnskab","info":"Kigger efter pengene","position":"CFO","relations":"Far til Thomas Løcke","responsibility":"Regnskab"}'),
       (1, 4, '{"backup":[{"value":"Thomas Løcke", "priority": 1},{"value":"Jacob Sparre Anders", "priority": 2}],"emailaddresses":[{"value":"krc@adaheads.com", "priority": 1}],"handling":[{"value":"Husk at slutte af med: Du må have en god dag", "priority": 1}],"telephonenumbers":[{"value":"555-78787878", "priority": 1}],"workhours":[{"value":"Hverdage 07:00 – 18:00", "priority": 1},{"value":"Weekend: 10:00 - 14:00", "priority": 2}],"tags":["mail","SIP","Linux"],"department":"Development","info":"Kigger efter koden","position":"Software udvikler","relations":"Børn med Sidsel Schomacker","responsibility":"Alice, Bob og telefonen"}'),
       (1, 5, '{"backup":[{"value":"Thomas Løcke", "priority": 1},{"value":"Kim rostgaard Christensen", "priority": 2}],"emailaddresses":[{"value":"jsa@adaheads.com", "priority": 1}],"handling":[{"value":"Hans telefon har ofte dårlig forbindelse på grund af, han befinder sig I de tyndere luftlag", "priority": 1}],"telephonenumbers":[{"value":"555 666 777", "priority": 1}],"workhours":[{"value":"Mandag-Tirsdag 09:00 16", "priority": 1},{"value":"Torsdag-Fredag 10:00 – 15:00", "priority": 2}],"tags":["Ada","SIP","Linux","Fysik"],"department":"Development","info":"Kigger efter koden","position":"Software udvikler","relations":"Har engang haft en hund","responsibility":"Alice og Cloe"}'),
       (1, 6, '{"backup":[{"value":"Kim Rostgaard Christensen","priority":1}],"emailaddresses":[{"value":"ss@adaheads.com","priority":1}],"handling":[],"telephonenumbers":[],"workhours":[],"tags":["Grafik"],"department":"Design","info":"","position":"Designer","relations":"Børn med Kim Rostgaard Christensen","responsibility":"Bob design"}'),
       (1, 7, '{"backup":[{"value":"Kim Rostgaard Christensen","priority":1}],"emailaddresses":[],"handling":[],"telephonenumbers":[{"value":"12345678","priority":1}],"workhours":[],"tags":["Granvej","Mosekrogen"],"department":"","info":"","position":"","relations":"","responsibility":""}'),
       /*Fishermans Friends*/
       (2, 1, '{"backup":[{"value":"Steen Løcke","priority":1}],"emailaddresses":[{"value":"tl@ff.dk","priority":1}],"handling":[{"value":"spørg ikke ind til ekstra stærk varianten","priority":1}],"telephonenumbers":[{"value":"87654321","priority":1}],"workhours":[],"tags":["Fisker","sømand","pirat"],"department":"Fangst","info":"Tidligere fisker I militæret","position":"Key fishing manager","relations":"Gift med Trine Løcke","responsibility":"Fersk fisk"}'),
       (2, 4, '{"backup":[{"value":"Sidsel Schomacker", "priority": 1}],"emailaddresses":[{"value":"krc@retrospekt.dk", "priority": 1}],"handling":[{"value":"Pas på hans skæg", "priority": 1}],"telephonenumbers":[{"value":"+45 31 41 59 26", "priority": 1}],"workhours":[{"value":"Hele tiden", "priority": 1}],"tags":["Linux","Tux","Pingvinen"],"department":"Båden","info":"Klap for den venstre øje","position":"CFO (Cheif fishing officer)","relations":"Papegøjen Dieco ","responsibility":"Saltvands fisk"}'),
       (2, 8, null),
       /*Responsum*/
       (3, 1, '{"backup":[{"value":"Trine Løcke","priority":1},{"value":"Steen Løcke","priority":2}],"emailaddresses":[{"value":"tl@responsum.dk","priority":1}],"handling":[{"value":"Bær over med hans gode humør","priority":1}],"telephonenumbers":[{"value":"+45 33 48 82 01","priority":1}],"workhours":[{"value":"Hverdage 07:00 – 18:00","priority":1},{"value":"Weekend: 10:00 - 14:00","priority":2}],"tags":["AWS","SIP","Slackware","Linux"],"department":"HQ","info":"Something","position":"CTO","relations":"Gift med Trine Løcke","responsibility":"IT afdellingen"}'),
       (3, 2, '{"backup":[{"value":"Thomas Løcke","priority":1}],"emailaddresses":[{"value":"trine@adaheads.com","priority":1}],"handling":[],"telephonenumbers":[{"value":"60431993","priority":1}],"workhours":[{"value":"Hverdage 08:00 – 12:00 & 13:00 – 17:00","priority":1},{"value":"Lørdag Hele dagen","priority":2}],"tags":["Linux","Printer","Support","IT","Speaker"],"department":"Produktion","info":"Går altid I blå sko","position":"CRO (Cheif receptionist officer)","relations":"Gift med Thomas Løcke","responsibility":"Printeren"}'),
       (3, 3, '{"backup":[{"value":"Thomas Løcke","priority":1}],"emailaddresses":[{"value":"steen@responsum.dk","priority":1}],"handling":[],"telephonenumbers":[{"value":"88329100","priority":1}],"workhours":[{"value":"Hverdage 08:00 – 17:00","priority":1}],"tags":["jobansøger","2730","3660","3520"],"department":"Produktion","info":"Ham I glasburet","position":"CEO & CFO","relations":"Far til Thomas Løcke","responsibility":"Regnskab"}'),
       (3, 4, '{"backup":[{"value":"Jacob Sparre Andersen", "priority": 1}],"emailaddresses":[{"value":"krc@retrospekt.dk", "priority": 1}],"handling":[{"value":"Spørg ikke ind til hvor god han er til at parkere", "priority": 1}],"telephonenumbers":[{"value":"88329100", "priority": 1}],"workhours":[{"value":"Hverdage 09:00 – 18:00", "priority": 1}],"tags":["Ny kunde","Salg","Uadresserede"],"department":"Produktion","info":"Ham med håret","position":"Backup software maintainer","relations":"ven med alle","responsibility":"mail"}');

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
       (3, 1, 3, FALSE, TRUE),
       (4, 1, 4, FALSE, TRUE),
       (5, 1, 5, TRUE,  FALSE),
       --  Fishermans Friends                
       (6, 1, 6, FALSE, TRUE),
       (7, 1, 7, FALSE, FALSE),
       --  Responsum
       (8, 2, 2, FALSE, TRUE),
       (4, 3, 9, FALSE, TRUE);

INSERT INTO distribution_lists (id,
                                send_to_contact_id, send_to_organization_id,
                                recipient_visibility)
VALUES (1, 1, 1, 'to'),
       (2, 1, 2, 'cc'),
       (3, 1, 2, 'to'),
       (4, 1, 1, 'cc'),
       (5, 1, 3, 'to'),
       (6, 3, 1, 'to'),
       (7, 3, 3, 'to');

INSERT INTO kinds (id)
VALUES ('helligdag');

INSERT INTO special_days (kind, day)
VALUES ('helligdag', '2013-12-25'),
       ('helligdag', '2013-12-26'),
       ('helligdag', '2014-01-01');

INSERT INTO dial_plans (phone_number, dial_plan)
VALUES ('+4521490804', '<dial-plan title="Jacob: Hang up on anonymous callers"> <start do="Start"/> <decision-tree title="Start"> <branch> <conditions> <caller number=""/> </conditions> <action do="Hang up"/> </branch> <fall-back do="Pass through"/> </decision-tree> <end-point title="Hang up"> <hang-up/> </end-point> <end-point title="Pass through"> <redirect to="+45 21 49 08 04"/> </end-point> </dial-plan>');

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
