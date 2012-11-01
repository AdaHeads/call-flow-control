--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: contactentity; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE contactentity (
    id integer NOT NULL,
    full_name character varying(256) NOT NULL,
    is_human boolean DEFAULT true NOT NULL
);


ALTER TABLE public.contactentity OWNER TO alice;

--
-- Name: contactentity_attributes; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE contactentity_attributes (
    organization_id integer NOT NULL,
    contactentity_id integer NOT NULL,
    json json NOT NULL
);


ALTER TABLE public.contactentity_attributes OWNER TO alice;

--
-- Name: contactentity_id_seq; Type: SEQUENCE; Schema: public; Owner: alice
--

CREATE SEQUENCE contactentity_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.contactentity_id_seq OWNER TO alice;

--
-- Name: contactentity_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alice
--

ALTER SEQUENCE contactentity_id_seq OWNED BY contactentity.id;


--
-- Name: contactentity_recipient; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE contactentity_recipient (
    contactentity_id integer NOT NULL,
    recipient_id integer NOT NULL
);


ALTER TABLE public.contactentity_recipient OWNER TO alice;

--
-- Name: organization; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE organization (
    id integer NOT NULL,
    name character varying(256) NOT NULL,
    identifier character varying(256) NOT NULL,
    json json
);


ALTER TABLE public.organization OWNER TO alice;

--
-- Name: organization_contactentities; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE organization_contactentities (
    organization_id integer NOT NULL,
    contactentity_id integer NOT NULL
);


ALTER TABLE public.organization_contactentities OWNER TO alice;

--
-- Name: organization_id_seq; Type: SEQUENCE; Schema: public; Owner: alice
--

CREATE SEQUENCE organization_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.organization_id_seq OWNER TO alice;

--
-- Name: organization_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alice
--

ALTER SEQUENCE organization_id_seq OWNED BY organization.id;


--
-- Name: recipient; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE recipient (
    id integer NOT NULL,
    kind_id integer NOT NULL,
    full_name text,
    email_address text NOT NULL
);


ALTER TABLE public.recipient OWNER TO alice;

--
-- Name: recipient_id_seq; Type: SEQUENCE; Schema: public; Owner: alice
--

CREATE SEQUENCE recipient_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.recipient_id_seq OWNER TO alice;

--
-- Name: recipient_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alice
--

ALTER SEQUENCE recipient_id_seq OWNED BY recipient.id;


--
-- Name: recipient_kind; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE recipient_kind (
    id smallint NOT NULL,
    kind character varying(3) NOT NULL
);


ALTER TABLE public.recipient_kind OWNER TO alice;

--
-- Name: id; Type: DEFAULT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity ALTER COLUMN id SET DEFAULT nextval('contactentity_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: alice
--

ALTER TABLE ONLY organization ALTER COLUMN id SET DEFAULT nextval('organization_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: alice
--

ALTER TABLE ONLY recipient ALTER COLUMN id SET DEFAULT nextval('recipient_id_seq'::regclass);


--
-- Name: contactentity_attributes_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY contactentity_attributes
    ADD CONSTRAINT contactentity_attributes_pkey PRIMARY KEY (organization_id, contactentity_id);


--
-- Name: contactentity_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY contactentity
    ADD CONSTRAINT contactentity_pkey PRIMARY KEY (id);


--
-- Name: contactentity_recipient_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY contactentity_recipient
    ADD CONSTRAINT contactentity_recipient_pkey PRIMARY KEY (contactentity_id, recipient_id);


--
-- Name: organization_contactentities_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY organization_contactentities
    ADD CONSTRAINT organization_contactentities_pkey PRIMARY KEY (organization_id, contactentity_id);


--
-- Name: organization_identifier_key; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY organization
    ADD CONSTRAINT organization_identifier_key UNIQUE (identifier);


--
-- Name: organization_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY organization
    ADD CONSTRAINT organization_pkey PRIMARY KEY (id);


--
-- Name: recipient_kind_id_full_name_email_address_key; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY recipient
    ADD CONSTRAINT recipient_kind_id_full_name_email_address_key UNIQUE (kind_id, full_name, email_address);


--
-- Name: recipient_kind_kind_key; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY recipient_kind
    ADD CONSTRAINT recipient_kind_kind_key UNIQUE (kind);


--
-- Name: recipient_kind_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY recipient_kind
    ADD CONSTRAINT recipient_kind_pkey PRIMARY KEY (id);


--
-- Name: recipient_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY recipient
    ADD CONSTRAINT recipient_pkey PRIMARY KEY (id);


--
-- Name: contactentity_attributes_contactentity_id_idx; Type: INDEX; Schema: public; Owner: alice; Tablespace: 
--

CREATE INDEX contactentity_attributes_contactentity_id_idx ON contactentity_attributes USING btree (contactentity_id);


--
-- Name: contactentity_attributes_organization_id_idx; Type: INDEX; Schema: public; Owner: alice; Tablespace: 
--

CREATE INDEX contactentity_attributes_organization_id_idx ON contactentity_attributes USING btree (organization_id);


--
-- Name: contactentity_id_idx; Type: INDEX; Schema: public; Owner: alice; Tablespace: 
--

CREATE INDEX contactentity_id_idx ON contactentity USING btree (id);


--
-- Name: contactentity_recipient_contactentity_id_idx; Type: INDEX; Schema: public; Owner: alice; Tablespace: 
--

CREATE INDEX contactentity_recipient_contactentity_id_idx ON contactentity_recipient USING btree (contactentity_id);


--
-- Name: contactentity_recipient_recipient_id_idx; Type: INDEX; Schema: public; Owner: alice; Tablespace: 
--

CREATE INDEX contactentity_recipient_recipient_id_idx ON contactentity_recipient USING btree (recipient_id);


--
-- Name: organization_contactentities_contactentity_id_idx; Type: INDEX; Schema: public; Owner: alice; Tablespace: 
--

CREATE INDEX organization_contactentities_contactentity_id_idx ON organization_contactentities USING btree (contactentity_id);


--
-- Name: organization_contactentities_organization_id_idx; Type: INDEX; Schema: public; Owner: alice; Tablespace: 
--

CREATE INDEX organization_contactentities_organization_id_idx ON organization_contactentities USING btree (organization_id);


--
-- Name: organization_id_idx; Type: INDEX; Schema: public; Owner: alice; Tablespace: 
--

CREATE INDEX organization_id_idx ON organization USING btree (id);


--
-- Name: recipient_id_idx; Type: INDEX; Schema: public; Owner: alice; Tablespace: 
--

CREATE INDEX recipient_id_idx ON recipient USING btree (id);


--
-- Name: contactentity_attributes_contactentity_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_attributes
    ADD CONSTRAINT contactentity_attributes_contactentity_id_fkey FOREIGN KEY (contactentity_id) REFERENCES contactentity(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contactentity_attributes_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_attributes
    ADD CONSTRAINT contactentity_attributes_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES organization(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contactentity_attributes_organization_id_fkey1; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_attributes
    ADD CONSTRAINT contactentity_attributes_organization_id_fkey1 FOREIGN KEY (organization_id, contactentity_id) REFERENCES organization_contactentities(organization_id, contactentity_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contactentity_recipient_contactentity_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_recipient
    ADD CONSTRAINT contactentity_recipient_contactentity_id_fkey FOREIGN KEY (contactentity_id) REFERENCES contactentity(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contactentity_recipient_recipient_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_recipient
    ADD CONSTRAINT contactentity_recipient_recipient_id_fkey FOREIGN KEY (recipient_id) REFERENCES recipient(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: organization_contactentities_contactentity_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY organization_contactentities
    ADD CONSTRAINT organization_contactentities_contactentity_id_fkey FOREIGN KEY (contactentity_id) REFERENCES contactentity(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: organization_contactentities_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY organization_contactentities
    ADD CONSTRAINT organization_contactentities_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES organization(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: recipient_kind_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY recipient
    ADD CONSTRAINT recipient_kind_id_fkey FOREIGN KEY (kind_id) REFERENCES recipient_kind(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

