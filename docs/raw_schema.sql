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
    ce_id integer NOT NULL,
    ce_name text COLLATE pg_catalog."da_DK.utf8" NOT NULL,
    json text NOT NULL,
    is_human boolean DEFAULT true NOT NULL
);


ALTER TABLE public.contactentity OWNER TO alice;

--
-- Name: contactentity_attributes; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE contactentity_attributes (
    org_id integer NOT NULL,
    ce_id integer NOT NULL,
    json text NOT NULL
);


ALTER TABLE public.contactentity_attributes OWNER TO alice;

--
-- Name: contactentity_ce_id_seq; Type: SEQUENCE; Schema: public; Owner: alice
--

CREATE SEQUENCE contactentity_ce_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.contactentity_ce_id_seq OWNER TO alice;

--
-- Name: contactentity_ce_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alice
--

ALTER SEQUENCE contactentity_ce_id_seq OWNED BY contactentity.ce_id;


--
-- Name: contactentity_tags; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE contactentity_tags (
    org_id integer NOT NULL,
    ce_id integer NOT NULL,
    json text NOT NULL
);


ALTER TABLE public.contactentity_tags OWNER TO alice;

--
-- Name: organization; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE organization (
    org_id integer NOT NULL,
    org_name text COLLATE pg_catalog."da_DK.utf8" NOT NULL,
    identifier text COLLATE pg_catalog."da_DK.utf8" NOT NULL,
    json text NOT NULL
);


ALTER TABLE public.organization OWNER TO alice;

--
-- Name: organization_contactentities; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE organization_contactentities (
    org_id integer NOT NULL,
    ce_id integer NOT NULL
);


ALTER TABLE public.organization_contactentities OWNER TO alice;

--
-- Name: organization_org_id_seq; Type: SEQUENCE; Schema: public; Owner: alice
--

CREATE SEQUENCE organization_org_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.organization_org_id_seq OWNER TO alice;

--
-- Name: organization_org_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alice
--

ALTER SEQUENCE organization_org_id_seq OWNED BY organization.org_id;


--
-- Name: ce_id; Type: DEFAULT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity ALTER COLUMN ce_id SET DEFAULT nextval('contactentity_ce_id_seq'::regclass);


--
-- Name: org_id; Type: DEFAULT; Schema: public; Owner: alice
--

ALTER TABLE ONLY organization ALTER COLUMN org_id SET DEFAULT nextval('organization_org_id_seq'::regclass);


--
-- Name: contactentity_attributes_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY contactentity_attributes
    ADD CONSTRAINT contactentity_attributes_pkey PRIMARY KEY (ce_id, org_id);


--
-- Name: contactentity_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY contactentity
    ADD CONSTRAINT contactentity_pkey PRIMARY KEY (ce_id);


--
-- Name: contactentity_tags_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY contactentity_tags
    ADD CONSTRAINT contactentity_tags_pkey PRIMARY KEY (ce_id, org_id);


--
-- Name: organization_contactentities_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY organization_contactentities
    ADD CONSTRAINT organization_contactentities_pkey PRIMARY KEY (org_id, ce_id);


--
-- Name: organization_identifier_key; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY organization
    ADD CONSTRAINT organization_identifier_key UNIQUE (identifier);


--
-- Name: organization_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY organization
    ADD CONSTRAINT organization_pkey PRIMARY KEY (org_id);


--
-- Name: contactentity_attributes_ce_and_org_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_attributes
    ADD CONSTRAINT contactentity_attributes_ce_and_org_id_fkey FOREIGN KEY (ce_id, org_id) REFERENCES organization_contactentities(ce_id, org_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contactentity_attributes_ce_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_attributes
    ADD CONSTRAINT contactentity_attributes_ce_id_fkey FOREIGN KEY (ce_id) REFERENCES contactentity(ce_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contactentity_attributes_org_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_attributes
    ADD CONSTRAINT contactentity_attributes_org_id_fkey FOREIGN KEY (org_id) REFERENCES organization(org_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contactentity_tags_ce_and_org_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_tags
    ADD CONSTRAINT contactentity_tags_ce_and_org_id_fkey FOREIGN KEY (ce_id, org_id) REFERENCES organization_contactentities(ce_id, org_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contactentity_tags_ce_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_tags
    ADD CONSTRAINT contactentity_tags_ce_id_fkey FOREIGN KEY (ce_id) REFERENCES contactentity(ce_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contactentity_tags_org_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_tags
    ADD CONSTRAINT contactentity_tags_org_id_fkey FOREIGN KEY (org_id) REFERENCES organization(org_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: organization_contactentities_ce_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY organization_contactentities
    ADD CONSTRAINT organization_contactentities_ce_id_fkey FOREIGN KEY (ce_id) REFERENCES contactentity(ce_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: organization_contactentities_org_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY organization_contactentities
    ADD CONSTRAINT organization_contactentities_org_id_fkey FOREIGN KEY (org_id) REFERENCES organization(org_id) ON UPDATE CASCADE ON DELETE CASCADE;


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
    ce_id integer NOT NULL,
    ce_name text COLLATE pg_catalog."da_DK.utf8" NOT NULL,
    json text NOT NULL,
    is_human boolean DEFAULT true NOT NULL
);


ALTER TABLE public.contactentity OWNER TO alice;

--
-- Name: contactentity_attributes; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE contactentity_attributes (
    org_id integer NOT NULL,
    ce_id integer NOT NULL,
    json text NOT NULL
);


ALTER TABLE public.contactentity_attributes OWNER TO alice;

--
-- Name: contactentity_ce_id_seq; Type: SEQUENCE; Schema: public; Owner: alice
--

CREATE SEQUENCE contactentity_ce_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.contactentity_ce_id_seq OWNER TO alice;

--
-- Name: contactentity_ce_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alice
--

ALTER SEQUENCE contactentity_ce_id_seq OWNED BY contactentity.ce_id;


--
-- Name: organization; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE organization (
    org_id integer NOT NULL,
    org_name text COLLATE pg_catalog."da_DK.utf8" NOT NULL,
    identifier text COLLATE pg_catalog."da_DK.utf8" NOT NULL,
    json text NOT NULL
);


ALTER TABLE public.organization OWNER TO alice;

--
-- Name: organization_contactentities; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE organization_contactentities (
    org_id integer NOT NULL,
    ce_id integer NOT NULL
);


ALTER TABLE public.organization_contactentities OWNER TO alice;

--
-- Name: organization_org_id_seq; Type: SEQUENCE; Schema: public; Owner: alice
--

CREATE SEQUENCE organization_org_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.organization_org_id_seq OWNER TO alice;

--
-- Name: organization_org_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alice
--

ALTER SEQUENCE organization_org_id_seq OWNED BY organization.org_id;


--
-- Name: ce_id; Type: DEFAULT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity ALTER COLUMN ce_id SET DEFAULT nextval('contactentity_ce_id_seq'::regclass);


--
-- Name: org_id; Type: DEFAULT; Schema: public; Owner: alice
--

ALTER TABLE ONLY organization ALTER COLUMN org_id SET DEFAULT nextval('organization_org_id_seq'::regclass);


--
-- Name: contactentity_attributes_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY contactentity_attributes
    ADD CONSTRAINT contactentity_attributes_pkey PRIMARY KEY (ce_id, org_id);


--
-- Name: contactentity_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY contactentity
    ADD CONSTRAINT contactentity_pkey PRIMARY KEY (ce_id);


--
-- Name: organization_contactentities_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY organization_contactentities
    ADD CONSTRAINT organization_contactentities_pkey PRIMARY KEY (org_id, ce_id);


--
-- Name: organization_identifier_key; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY organization
    ADD CONSTRAINT organization_identifier_key UNIQUE (identifier);


--
-- Name: organization_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY organization
    ADD CONSTRAINT organization_pkey PRIMARY KEY (org_id);


--
-- Name: contactentity_attributes_ce_and_org_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_attributes
    ADD CONSTRAINT contactentity_attributes_ce_and_org_id_fkey FOREIGN KEY (ce_id, org_id) REFERENCES organization_contactentities(ce_id, org_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contactentity_attributes_ce_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_attributes
    ADD CONSTRAINT contactentity_attributes_ce_id_fkey FOREIGN KEY (ce_id) REFERENCES contactentity(ce_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contactentity_attributes_org_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_attributes
    ADD CONSTRAINT contactentity_attributes_org_id_fkey FOREIGN KEY (org_id) REFERENCES organization(org_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: organization_contactentities_ce_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY organization_contactentities
    ADD CONSTRAINT organization_contactentities_ce_id_fkey FOREIGN KEY (ce_id) REFERENCES contactentity(ce_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: organization_contactentities_org_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY organization_contactentities
    ADD CONSTRAINT organization_contactentities_org_id_fkey FOREIGN KEY (org_id) REFERENCES organization(org_id) ON UPDATE CASCADE ON DELETE CASCADE;


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
    ce_id integer NOT NULL,
    ce_name character varying(256) NOT NULL,
    json character varying(10000) NOT NULL,
    is_human boolean DEFAULT true NOT NULL
);


ALTER TABLE public.contactentity OWNER TO alice;

--
-- Name: contactentity_attributes; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE contactentity_attributes (
    org_id integer NOT NULL,
    ce_id integer NOT NULL,
    json character varying(10000) NOT NULL
);


ALTER TABLE public.contactentity_attributes OWNER TO alice;

--
-- Name: contactentity_ce_id_seq; Type: SEQUENCE; Schema: public; Owner: alice
--

CREATE SEQUENCE contactentity_ce_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.contactentity_ce_id_seq OWNER TO alice;

--
-- Name: contactentity_ce_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alice
--

ALTER SEQUENCE contactentity_ce_id_seq OWNED BY contactentity.ce_id;


--
-- Name: organization; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE organization (
    org_id integer NOT NULL,
    org_name character varying(256) NOT NULL,
    identifier character varying(256) NOT NULL,
    json character varying(10000) NOT NULL
);


ALTER TABLE public.organization OWNER TO alice;

--
-- Name: organization_contactentities; Type: TABLE; Schema: public; Owner: alice; Tablespace: 
--

CREATE TABLE organization_contactentities (
    org_id integer NOT NULL,
    ce_id integer NOT NULL
);


ALTER TABLE public.organization_contactentities OWNER TO alice;

--
-- Name: organization_org_id_seq; Type: SEQUENCE; Schema: public; Owner: alice
--

CREATE SEQUENCE organization_org_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.organization_org_id_seq OWNER TO alice;

--
-- Name: organization_org_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alice
--

ALTER SEQUENCE organization_org_id_seq OWNED BY organization.org_id;


--
-- Name: ce_id; Type: DEFAULT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity ALTER COLUMN ce_id SET DEFAULT nextval('contactentity_ce_id_seq'::regclass);


--
-- Name: org_id; Type: DEFAULT; Schema: public; Owner: alice
--

ALTER TABLE ONLY organization ALTER COLUMN org_id SET DEFAULT nextval('organization_org_id_seq'::regclass);


--
-- Name: contactentity_attributes_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY contactentity_attributes
    ADD CONSTRAINT contactentity_attributes_pkey PRIMARY KEY (ce_id, org_id);


--
-- Name: contactentity_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY contactentity
    ADD CONSTRAINT contactentity_pkey PRIMARY KEY (ce_id);


--
-- Name: organization_contactentities_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY organization_contactentities
    ADD CONSTRAINT organization_contactentities_pkey PRIMARY KEY (org_id, ce_id);


--
-- Name: organization_identifier_key; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY organization
    ADD CONSTRAINT organization_identifier_key UNIQUE (identifier);


--
-- Name: organization_pkey; Type: CONSTRAINT; Schema: public; Owner: alice; Tablespace: 
--

ALTER TABLE ONLY organization
    ADD CONSTRAINT organization_pkey PRIMARY KEY (org_id);


--
-- Name: contactentity_attributes_ce_and_org_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_attributes
    ADD CONSTRAINT contactentity_attributes_ce_and_org_id_fkey FOREIGN KEY (ce_id, org_id) REFERENCES organization_contactentities(ce_id, org_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contactentity_attributes_ce_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_attributes
    ADD CONSTRAINT contactentity_attributes_ce_id_fkey FOREIGN KEY (ce_id) REFERENCES contactentity(ce_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contactentity_attributes_org_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY contactentity_attributes
    ADD CONSTRAINT contactentity_attributes_org_id_fkey FOREIGN KEY (org_id) REFERENCES organization(org_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: organization_contactentities_ce_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY organization_contactentities
    ADD CONSTRAINT organization_contactentities_ce_id_fkey FOREIGN KEY (ce_id) REFERENCES contactentity(ce_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: organization_contactentities_org_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alice
--

ALTER TABLE ONLY organization_contactentities
    ADD CONSTRAINT organization_contactentities_org_id_fkey FOREIGN KEY (org_id) REFERENCES organization(org_id) ON UPDATE CASCADE ON DELETE CASCADE;


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

